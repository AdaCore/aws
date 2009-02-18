------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2009, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

with AWS.Parameters;
with AWS.Status.Set;

package body AWS.Services.Web_Block.Registry is

   use Ada;

   Internal_Context_Var     : constant String := "=&= CTX_WB =&=";
   Internal_Context_Var_Old : constant String := "=&= CTX_OLD_WB =&=";
   Context_Var              : constant String := "CTX_WB";
   Context_Var_Old          : constant String := "CTX_OLD_WB";
   Context_Var_To_Copy      : constant String := "CTX_WB_COPY";

   type Lazy_Handler is new Templates.Dynamic.Lazy_Tag with record
      Request      : aliased Status.Data;
      --  Current request made to the server
      Translations : Templates.Translate_Set;
      --  Global translations table
   end record;

   overriding procedure Value
     (Lazy_Tag     : not null access Lazy_Handler;
      Var_Name     : in              String;
      Translations : in out          Templates.Translate_Set);
   --  Handle lazy tags

   type Web_Object (Callback_Template : Boolean := False) is record
      Content_Type     : Unbounded_String;
      Context_Required : Boolean;
      Data_CB          : Data_Callback;

      case Callback_Template is
         when False =>
            Template    : Unbounded_String;
         when True =>
            Template_CB : Template_Callback;
      end case;
   end record;

   package Web_Object_Maps is new Containers.Indefinite_Hashed_Maps
     (String, Web_Object, Strings.Hash, "=");
   use Web_Object_Maps;

   WO_Map : Map;

   package Prefix_URI is new Containers.Vectors (Positive, Unbounded_String);

   Prefix_URI_Vector : Prefix_URI.Vector;

   type Context_Data is record
      Id     : Context.Id;
      Old_Id : Context.Id;
      Is_New : Boolean;
   end record;

   function Get_Context
     (Request : not null access Status.Data) return Context_Data;
   --  Gets the proper context id for this request

   -----------
   -- Build --
   -----------

   function Build
     (Key           : in String;
      Request       : in Status.Data;
      Translations  : in Templates.Translate_Set;
      Status_Code   : in Messages.Status_Code := Messages.S200;
      Cache_Control : in Messages.Cache_Option := Messages.Unspecified;
      Context_Error : in String := "") return Response.Data
   is
      P    : constant Page :=
               Parse (Key, Request, Translations, Context_Error);
      Data : Response.Data;
   begin
      if P = No_Page then
         Data := Response.Build
           (MIME.Text_HTML, "", Status_Code => Messages.S404);

      else
         Data := Response.Build
           (To_String (P.Content_Type),
            To_String (P.Content),
            Status_Code   => Status_Code,
            Cache_Control => Cache_Control);
      end if;

      return Data;
   end Build;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (Key : in String) return String is
      Position : constant Web_Object_Maps.Cursor := WO_Map.Find (Key);
   begin
      if Position = No_Element then
         return "";
      else
         return To_String (Web_Object_Maps.Element (Position).Content_Type);
      end if;
   end Content_Type;

   -----------------
   -- Get_Context --
   ------------------

   function Get_Context
     (Request : not null access Status.Data) return Web_Block.Context.Object
   is
      C_Data : constant Context_Data := Get_Context (Request);
   begin
      return Web_Block.Context.Get (C_Data.Id);
   end Get_Context;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (Request : not null access Status.Data) return Context_Data
   is
      use type Context.Id;

      function Create_New_Context return Context.Id;
      --  Create a new context, register it and return the corresponding Id

      ------------------------
      -- Create_New_Context --
      ------------------------

      function Create_New_Context return Context.Id is
         C : constant Context.Id := Context.Create;
      begin
         Status.Set.Add_Parameter
           (Request.all, Internal_Context_Var, Context.Image (C));
         return C;
      end Create_New_Context;

      CID : Context_Data;

   begin
      if Parameters.Get
        (Status.Parameters (Request.all), Internal_Context_Var) = ""
      then
         --  No context known

         if Parameters.Get
           (Status.Parameters (Request.all), Context_Var) = ""
         then
            --  No context sent with the request, create a new context for
            --  this request.

            CID.Id     := Create_New_Context;
            CID.Is_New := True;
            CID.Old_Id := Context.Copy (CID.Id);
            Status.Set.Add_Parameter
              (Request.all, Internal_Context_Var_Old,
               Context.Image (CID.Old_Id));
         else
            --  A context has been sent with this request

            declare
               C_Str     : constant String :=
                             Parameters.Get (Status.Parameters (Request.all),
                                             Context_Var);
               Old_C_Str : constant String :=
                             Parameters.Get (Status.Parameters (Request.all),
                                             Context_Var_Old);

            begin
               --  First check that it is a known context (i.e. still a valid
               --  context recorded in the context database).

               CID.Id     := Context.Value (C_Str);
               CID.Old_Id := Context.Value (Old_C_Str);
               CID.Is_New := False;

               if Context.Exist (CID.Id) then
                  --  This context is known, record it as the current
                  --  working context.

                  if Parameters.Get
                    (Status.Parameters (Request.all),
                     Context_Var_To_Copy) /= ""
                  then
                     --  This context must be copied

                     Copy_Context : declare
                        New_Id : Context.Id;
                        --  Create a new context
                     begin
                        New_Id := Context.Copy (CID.Id);

                        --  Discard last modification to CID.Id
                        --  We want to revert the previous action in old
                        --  context as the context can be referenced in another
                        --  page which has no knowlegde of this new status.
                        Context.Copy (CID.Old_Id, CID.Id);

                        --  Return the new context and create a new old context
                        CID.Id     := New_Id;
                        CID.Old_Id := Context.Copy (New_Id);

                        Status.Set.Add_Parameter
                          (Request.all, Internal_Context_Var,
                           Context.Image (CID.Id));
                        Status.Set.Add_Parameter
                          (Request.all, Internal_Context_Var_Old,
                           Context.Image (CID.Old_Id));
                     end Copy_Context;

                  else
                     --  Returns the current working context
                     --  Creates a copy in old_context

                     Context.Copy (CID.Id, CID.Old_Id);

                     Status.Set.Add_Parameter
                       (Request.all, Internal_Context_Var, C_Str);
                     Status.Set.Add_Parameter
                       (Request.all, Internal_Context_Var_Old, Old_C_Str);
                  end if;

               else
                  --  Unknown or expired context, create a new one

                  CID.Id     := Create_New_Context;
                  CID.Old_Id := Context.Copy (CID.Id);
                  CID.Is_New := True;
               end if;
            end;
         end if;

      else
         --  Context already recorded, just retrieve it

         if Parameters.Get (Status.Parameters (Request.all), Context_Var)
           = Parameters.Get
               (Status.Parameters (Request.all), Internal_Context_Var)
         then
            --  Internal context object and context stored into the session are
            --  identical, this is an old known context object.

            CID := (Id     => Context.Value
                    (Parameters.Get
                       (Status.Parameters
                          (Request.all), Internal_Context_Var)),
                    Old_Id => Context.Value
                      (Parameters.Get
                         (Status.Parameters
                            (Request.all), Internal_Context_Var_Old)),
                    Is_New => False);
         else
            --  In this case the context variable in the Web page has not been
            --  yet evaluated. This means that no page have been rendered with
            --  this context. In this case it means that we really have a new
            --  context. This case arise when an end-user get the context
            --  directly in a callback using Get_Context public routine. The
            --  Internal_Context_Var is set to the new context value but
            --  Context_Var still has some old context found into the page. The
            --  only point where Context_Var is set is into Value callback
            --  below.

            CID := (Id     => Context.Value
                    (Parameters.Get
                       (Status.Parameters
                          (Request.all), Internal_Context_Var)),
                    Old_Id => Context.Value
                      (Parameters.Get
                         (Status.Parameters
                            (Request.all), Internal_Context_Var_Old)),
                    Is_New => True);
         end if;
      end if;

      return CID;
   end Get_Context;

   -----------
   -- Parse --
   -----------

   function Parse
     (Key           : in String;
      Request       : in Status.Data;
      Translations  : in Templates.Translate_Set;
      Context_Error : in String := "") return Page
   is
      LT       : aliased Lazy_Handler :=
                   Lazy_Handler'(Templates.Dynamic.Lazy_Tag
                                 with Request => Request,
                                      Translations => Translations);
      Position : Web_Object_Maps.Cursor;

      function Get_Matching_Key (Search_Key : in String) return String;
      --  Get the Prefix Key matching Search_Key in Prefix_URI_Vector

      ----------------------
      -- Get_Matching_Key --
      ----------------------

      function Get_Matching_Key (Search_Key : in String) return String is
         use Prefix_URI;
         Cursor : Prefix_URI.Cursor := Prefix_URI.First (Prefix_URI_Vector);
      begin
         while Cursor /= Prefix_URI.No_Element loop
            declare
               K : constant String := To_String (Prefix_URI.Element (Cursor));
            begin
               if K'Length <= Search_Key'Length and then
                 Search_Key
                 (Search_Key'First .. Search_Key'First + K'Length - 1) = K then
                  return K;
               end if;
            end;
            Prefix_URI.Next (Cursor);
         end loop;
         return "";
      end Get_Matching_Key;

      Parsed_Page : Page := No_Page;

   begin
      --  Get Web Object

      Position := WO_Map.Find (Key);

      if Position = No_Element then
         --  Search key in Prefix_URI_Vector

         declare
            Matching_Key : constant String := Get_Matching_Key (Key);
         begin
            if Matching_Key /= "" then
               Position := WO_Map.Find (Matching_Key);
            end if;
         end;
      end if;

      if Position /= No_Element then
         declare
            C_Data        : constant Context_Data :=
                              Get_Context (LT.Request'Access);
            Context       : aliased Web_Block.Context.Object :=
                              Web_Block.Context.Get (C_Data.Id);
            T             : Templates.Translate_Set;
            Template_Name : Unbounded_String;
         begin
            --  Get translation set for this tag

            if C_Data.Is_New
              and then Element (Position).Context_Required
            then
               --  No context but it is required
               return Parse (Context_Error, Request, Translations);

            else
               Templates.Insert (T, Translations);

               if Element (Position).Data_CB /= null then
                  Element (Position).Data_CB (LT.Request, Context'Access, T);
               end if;

               if Element (Position).Callback_Template then
                  Template_Name := To_Unbounded_String
                    (Element (Position).Template_CB (Request));
               else
                  Template_Name := Element (Position).Template;
               end if;

               LT.Translations := T;

               Parsed_Page :=
                 Page'(Content_Type => Element (Position).Content_Type,
                       Content      => Templates.Parse
                         (To_String (Template_Name), T,
                          Lazy_Tag => LT'Unchecked_Access),
                       Set          => Templates.Null_Set);
            end if;
         end;
      end if;
      return Parsed_Page;
   end Parse;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key              : in String;
      Template         : in String;
      Data_CB          : in Data_Callback;
      Content_Type     : in String  := MIME.Text_HTML;
      Prefix           : in Boolean := False;
      Context_Required : in Boolean := False)
   is
      WO : Web_Object;
   begin
      --  WO := (To_Unbounded_String (Template), Data_CB);
      --  ??? problem with GNAT GPL 2006.

      WO.Content_Type     := To_Unbounded_String (Content_Type);
      WO.Template         := To_Unbounded_String (Template);
      WO.Data_CB          := Data_CB;
      WO.Context_Required := Context_Required;

      --  Register Tag

      WO_Map.Include (Key, WO);

      if Prefix then
         Prefix_URI.Append (Prefix_URI_Vector, To_Unbounded_String (Key));
      end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key              : in String;
      Template_CB      : in Template_Callback;
      Data_CB          : in Data_Callback;
      Content_Type     : in String := MIME.Text_HTML;
      Context_Required : in Boolean := False)
   is
      WO : Web_Object (True);
   begin
      WO.Content_Type     := To_Unbounded_String (Content_Type);
      WO.Template_CB      := Template_CB;
      WO.Data_CB          := Data_CB;
      WO.Context_Required := Context_Required;

      --  Register Tag

      WO_Map.Include (Key, WO);
      Prefix_URI.Append (Prefix_URI_Vector, To_Unbounded_String (Key));
   end Register;

   -----------
   -- Value --
   -----------

   overriding procedure Value
     (Lazy_Tag     : not null access Lazy_Handler;
      Var_Name     : in              String;
      Translations : in out          Templates.Translate_Set)
   is
      Position : Web_Object_Maps.Cursor;
   begin
      --  Specific case for the contextual var

      if Var_Name = Context_Var then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Context_Var,
               Context.Image (Get_Context (Lazy_Tag.Request'Access).Id)));

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Context_Var_Old,
               Context.Image (Get_Context (Lazy_Tag.Request'Access).Old_Id)));
      else
         --  Get Web Object

         Position := WO_Map.Find (Var_Name);

         if Position /= No_Element then
            declare
               C_Data        : constant Context_Data :=
                                 Get_Context (Lazy_Tag.Request'Access);
               Context       : aliased Web_Block.Context.Object :=
                                 Web_Block.Context.Get (C_Data.Id);
               T             : Templates.Translate_Set;
               Template_Name : Unbounded_String;
            begin
               --  Get translation set for this tag

               Templates.Insert (T, Translations);
               Templates.Insert (T, Lazy_Tag.Translations);

               if Element (Position).Data_CB /= null then
                  Element (Position).Data_CB
                    (Lazy_Tag.Request, Context'Access, T);
               end if;

               if Element (Position).Callback_Template then
                  Template_Name := To_Unbounded_String
                    (Element (Position).Template_CB (Lazy_Tag.Request));
               else
                  Template_Name := Element (Position).Template;
               end if;

               Lazy_Tag.Translations := T;

               Templates.Insert
                 (Translations,
                  Templates.Assoc
                    (Var_Name,
                     Unbounded_String'(Templates.Parse
                       (To_String (Template_Name), T,
                          Lazy_Tag =>
                            Templates.Dynamic.Lazy_Tag_Access (Lazy_Tag)))));
            end;
         end if;
      end if;
   end Value;

end AWS.Services.Web_Block.Registry;
