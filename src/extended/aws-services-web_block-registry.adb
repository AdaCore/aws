------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2010, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with AWS.Parameters;

with GNAT.Regpat;

package body AWS.Services.Web_Block.Registry is

   use Ada;

   Context_Var : constant String := "CTX_WB";

   type Lazy_Handler is new Templates.Dynamic.Lazy_Tag with record
      Request      : aliased Status.Data;
      --  Current request made to the server
      Translations : Templates.Translate_Set;
      --  Global translations table
      Ctx          : aliased Context.Object;
      --  Current context
   end record;

   overriding procedure Value
     (Lazy_Tag     : not null access Lazy_Handler;
      Var_Name     : String;
      Translations : in out          Templates.Translate_Set);
   --  Handle lazy tags

   type Web_Object_Data_Callback (With_Params : Boolean := False) is record
      case With_Params is
         when False =>
            Callback : Data_Callback;
         when True =>
            Callback_With_Parameters : Data_With_Param_Callback;
      end case;
   end record;

   type Web_Object (Callback_Template : Boolean := False) is record
      Content_Type     : Unbounded_String;
      Context_Required : Boolean;

      Data_CB          : Web_Object_Data_Callback;

      case Callback_Template is
         when False =>
            Template    : Unbounded_String;
         when True =>
            Template_CB : Template_Callback;
      end case;
   end record;

   package Web_Object_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Web_Object, Strings.Hash, "=");
   use Web_Object_Maps;

   WO_Map : Map;

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;

   type URL_Pattern (With_Matcher : Boolean := False) is record
      Prefix  : Unbounded_String;
      case With_Matcher is
         when True =>
            Matcher : Pattern_Matcher_Access;
            Key     : Unbounded_String;
         when False =>
            null;
      end case;
   end record;

   package Pattern_URL_Container is new Ada.Containers.Vectors
      (Positive, URL_Pattern);

   Pattern_URL_Vector : Pattern_URL_Container.Vector;

   -----------
   -- Build --
   -----------

   function Build
     (Key           : String;
      Request       : Status.Data;
      Translations  : Templates.Translate_Set;
      Status_Code   : Messages.Status_Code := Messages.S200;
      Cache_Control : Messages.Cache_Option := Messages.Unspecified;
      Context       : access Web_Block.Context.Object := null;
      Context_Error : String := "") return Response.Data
   is

      function Get_Context return Web_Block.Context.Object;
      --  Returns the context object

      -----------------
      -- Get_Context --
      -----------------

      function Get_Context return Web_Block.Context.Object is
      begin
         if Context = null then
            return Web_Block.Context.Empty;
         else
            return Context.all;
         end if;
      end Get_Context;

      P    : constant Page :=
               Parse (Key, Request, Translations, Get_Context, Context_Error);
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

      --  Return the new context

      if Context /= null then
         Context.all := Web_Block.Context.Get (P.Ctx_Id);
      end if;

      return Data;
   end Build;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (Key : String) return String is
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
     (Request : Status.Data) return Web_Block.Context.Object
   is
      Ctx : constant String :=
              Parameters.Get (Status.Parameters (Request), Context_Var);
   begin
      if Ctx = "" then
         return Context.Empty;
      else
         return Context.Get (Context.Value (Ctx));
      end if;
   end Get_Context;

   -----------
   -- Parse --
   -----------

   function Parse
     (Key           : String;
      Request       : Status.Data;
      Translations  : Templates.Translate_Set;
      Context       : Web_Block.Context.Object := Web_Block.Context.Empty;
      Context_Error : String := "") return Page
   is
      use type Web_Block.Context.Object;

      function Get_Context return Web_Block.Context.Object;
      --  Returns the context as passed to Parse routine if not empty, or the
      --  one retrieved from the Web page.

      -----------------
      -- Get_Context --
      -----------------

      function Get_Context return Web_Block.Context.Object is
      begin
         if Context = Web_Block.Context.Empty then
            return Get_Context (Request);
         else
            return Context;
         end if;
      end Get_Context;

      Tag_Context_Var : constant String :=
                          Templates.Tag_From_Name (Context_Var);
      Ctx             : constant String :=
                          Parameters.Get
                            (Status.Parameters (Request), Context_Var);
      LT              : aliased Lazy_Handler :=
                          Lazy_Handler'(Templates.Dynamic.Lazy_Tag
                                        with Request      => Request,
                                        Translations      => Translations,
                                        Ctx               => Get_Context);
      Position        : Web_Object_Maps.Cursor;

      function Get_Matching_Web_Object
         (Search_Key : String) return Callback_Parameters;
      --  Get the Web_Object matching Search_Key in Pattern_URL_Vector
      --  Returns the Parameters extracted from the URL patterns.

      -----------------------------
      -- Get_Matching_Web_Object --
      -----------------------------

      function Get_Matching_Web_Object (Search_Key : String)
         return Callback_Parameters is
      begin
         Position := WO_Map.Find (Key);

         if Position /= No_Element then
            return Empty_Callback_Parameters;
         end if;

         declare
            use Pattern_URL_Container;
            use GNAT.Regpat;
            Vector_Cursor : Pattern_URL_Container.Cursor :=
                              First (Pattern_URL_Vector);
         begin
            while Vector_Cursor /= Pattern_URL_Container.No_Element loop
               declare
                  P_URI : constant URL_Pattern := Element (Vector_Cursor);
                  K     : constant String := To_String (P_URI.Prefix);
               begin
                  if K'Length <= Search_Key'Length
                  and then
                     Search_Key
                        (Search_Key'First ..
                               Search_Key'First + K'Length - 1) = K
                  then

                     --  If a regexp is defined, check whether it matched
                     if P_URI.With_Matcher then
                        declare
                           Count   : constant Natural :=
                                       Paren_Count (P_URI.Matcher.all);
                           Matched : Match_Array (0 .. Count);
                        begin
                           Match (Self    => P_URI.Matcher.all,
                                 Data    => Search_Key,
                                 Matches => Matched);
                           if Matched (0) /= No_Match then
                              --  Returns the registered web object
                              --  Registered with a key = Prefix + Regexp
                              Position := WO_Map.Find (To_String (P_URI.Key));
                              declare
                                 Params : Callback_Parameters (1 .. Count);
                              begin
                                 for J in 1 .. Count loop
                                    Params (J) :=
                                       To_Unbounded_String (Search_Key
                                          (Matched (J).First ..
                                             Matched (J).Last));
                                 end loop;
                                 return Params;
                              end;
                           end if;
                        end;
                     else
                        --  Only a prefix is defined.
                        --  No need to search for other candidates
                        Position := WO_Map.Find (K);
                        return Empty_Callback_Parameters;
                     end if;
                  end if;
               end;
               Next (Vector_Cursor);
            end loop;

            return Empty_Callback_Parameters;
         end;
      end Get_Matching_Web_Object;

      Parsed_Page : Page := No_Page;
      Parameters  : constant Callback_Parameters :=
                      Get_Matching_Web_Object (Key);

   begin
      --  Use provided context if a user's defined one

      if Context /= Web_Block.Context.Empty then
         LT.Ctx := Context;
      end if;

      if Position /= No_Element then
         declare
            T             : Templates.Translate_Set;
            Template_Name : Unbounded_String;
            Content       : Unbounded_String;
            C_Index       : Natural;
            CID           : Web_Block.Context.Id;
            CT            : Unbounded_String
                              renames Element (Position).Content_Type;
         begin
            --  Get translation set for this tag

            if Ctx = "" and then Element (Position).Context_Required then
               --  No context but it is required
               return Parse (Context_Error, Request, Translations);

            else
               Templates.Insert (T, Translations);

               --  Call the Data_CB
               if not Element (Position).Data_CB.With_Params then
                  if Element (Position).Data_CB.Callback /= null then
                     Element (Position).Data_CB.Callback
                       (LT.Request, LT.Ctx'Access, T);
                  end if;
               else
                  if Element (Position).Data_CB.Callback_With_Parameters
                    /= null then
                     Element (Position).Data_CB.Callback_With_Parameters
                        (LT.Request, LT.Ctx'Access, Parameters, T);
                  end if;
               end if;

               if Element (Position).Callback_Template then
                  Template_Name := To_Unbounded_String
                    (Element (Position).Template_CB (Request));
               else
                  Template_Name := Element (Position).Template;
               end if;

               --  Page is now parsed, we need to create the context id for
               --  this page.

               LT.Translations := T;

               Content := Templates.Parse
                 (To_String (Template_Name), T,
                  Lazy_Tag => LT'Unchecked_Access);

               CID := Web_Block.Context.Register (LT.Ctx);

               --  Finaly inject the context Id into the result

               --  Note that any change in the format of the context below
               --  will affect the Web Block Javascript runtime. So a
               --  corresponding change must be done into aws_kernel.tjs.

               --  Check if we have an explicite context in the template. In
               --  this case we inject the context into this variable. If not
               --  we inject the context into HTML and XML document as follow.

               C_Index := Index (Content, Tag_Context_Var);

               if CT = MIME.Text_HTML and then C_Index = 0 then
                  --  A web page, we insert the context just after the
                  --  <body> tag, format:
                  --
                  --  <div id="CTX_WB" style="display:none">CID</div>
                  --

                  C_Index := Index (Content, "<body>");

                  if C_Index = 0 then
                     --  If not found, look for a body with some attributes
                     C_Index := Index (Content, "<body ");
                  end if;

                  if C_Index /= 0 then
                     --  Look for the end of the body tag
                     C_Index := Index (Content, ">", From => C_Index);

                     if C_Index /= 0 then
                        Insert
                          (Content, C_Index + 1,
                           "<div id=""CTX_WB"" style=""display:none"">"
                             & Web_Block.Context.Image (CID) & "</div>");
                     end if;
                  end if;

               elsif CT = MIME.Text_XML and then C_Index = 0 then
                  --  Inject context into the XML response, format:
                  --
                  --  <ctx id="CID"/>
                  --

                  C_Index := Index (Content, "</response>");

                  if C_Index /= 0 then
                     Insert
                       (Content, C_Index,
                        "<ctx id="""
                          & Web_Block.Context.Image (CID) & """/>");
                  end if;

               elsif C_Index /= 0 then
                  --  Replace all context variables
                  Replace_Contexts : loop
                     Replace_Slice
                       (Content,
                        Low  => C_Index,
                        High => C_Index + Tag_Context_Var'Length - 1,
                        By   => Web_Block.Context.Image (CID));
                     C_Index := Index
                       (Content, Tag_Context_Var, From => C_Index + 1);
                     exit Replace_Contexts when C_Index = 0;
                  end loop Replace_Contexts;
               end if;

               Parsed_Page :=
                 Page'(Content_Type => Element (Position).Content_Type,
                       Content      => Content,
                       Set          => Templates.Null_Set,
                       Ctx_Id       => CID);
            end if;
         end;
      end if;
      return Parsed_Page;
   end Parse;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key              : String;
      Template         : String;
      Data_CB          : Data_Callback;
      Content_Type     : String  := MIME.Text_HTML;
      Prefix           : Boolean := False;
      Context_Required : Boolean := False)
   is
      WO : constant Web_Object :=
             (Callback_Template => False,
              Content_Type      => To_Unbounded_String (Content_Type),
              Template          => To_Unbounded_String (Template),
              Data_CB           => Web_Object_Data_Callback'(
                 With_Params => False, Callback => Data_CB),
              Context_Required  => Context_Required);
   begin
      --  Register Tag

      WO_Map.Include (Key, WO);

      if Prefix then
         Pattern_URL_Container.Append
           (Pattern_URL_Vector,
            URL_Pattern'(Prefix       => To_Unbounded_String (Key),
                         With_Matcher => False));
      end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key              : String;
      Template_CB      : Template_Callback;
      Data_CB          : Data_Callback;
      Content_Type     : String := MIME.Text_HTML;
      Context_Required : Boolean := False)
   is
      WO : constant Web_Object :=
             (Callback_Template => True,
              Content_Type      => To_Unbounded_String (Content_Type),
              Template_CB       => Template_CB,
              Data_CB           => Web_Object_Data_Callback'(
                 With_Params => False, Callback => Data_CB),
              Context_Required  => Context_Required);
   begin
      --  Register Tag

      WO_Map.Include (Key, WO);
   end Register;

   --------------------------
   -- Register_Pattern_URL --
   --------------------------

   procedure Register_Pattern_URL
     (Prefix           : String;
      Regexp           : String;
      Template         : String;
      Data_CB          : Data_With_Param_Callback;
      Content_Type     : String  := MIME.Text_HTML;
      Context_Required : Boolean := False)
   is
      WO : constant Web_Object :=
             (Callback_Template => False,
              Content_Type      => To_Unbounded_String (Content_Type),
              Template          => To_Unbounded_String (Template),
              Data_CB           => Web_Object_Data_Callback'(
                 With_Params => True, Callback_With_Parameters => Data_CB),
              Context_Required  => Context_Required);
      Key     : constant String := Prefix & Regexp;
      Matcher : constant Pattern_Matcher_Access :=
         new GNAT.Regpat.Pattern_Matcher'(
            GNAT.Regpat.Compile (Key, GNAT.Regpat.Case_Insensitive));
   begin
      --  Register Tag

      WO_Map.Include (Key, WO);
      Pattern_URL_Container.Append
         (Pattern_URL_Vector,
            URL_Pattern'(Prefix       => To_Unbounded_String (Prefix),
                         With_Matcher => True,
                         Key          => To_Unbounded_String (Key),
                         Matcher      => Matcher));
   end Register_Pattern_URL;

   procedure Register_Pattern_URL
     (Prefix           : String;
      Regexp           : String;
      Template_CB      : Template_Callback;
      Data_CB          : Data_With_Param_Callback;
      Content_Type     : String  := MIME.Text_HTML;
      Context_Required : Boolean := False)
   is
      WO : constant Web_Object :=
             (Callback_Template => True,
              Content_Type      => To_Unbounded_String (Content_Type),
              Template_CB       => Template_CB,
              Data_CB           => Web_Object_Data_Callback'(
                 With_Params => True, Callback_With_Parameters => Data_CB),
              Context_Required  => Context_Required);
      Key     : constant String := Prefix & Regexp;
      Matcher : constant Pattern_Matcher_Access :=
         new GNAT.Regpat.Pattern_Matcher'(
            GNAT.Regpat.Compile (Key, GNAT.Regpat.Case_Insensitive));
   begin
      --  Register Tag

      WO_Map.Include (Key, WO);
      Pattern_URL_Container.Append
         (Pattern_URL_Vector,
            URL_Pattern'(Prefix       => To_Unbounded_String (Prefix),
                         With_Matcher => True,
                         Key          => To_Unbounded_String (Key),
                         Matcher      => Matcher));
   end Register_Pattern_URL;

   -----------
   -- Value --
   -----------

   overriding procedure Value
     (Lazy_Tag     : not null access Lazy_Handler;
      Var_Name     : String;
      Translations : in out          Templates.Translate_Set)
   is
      Position : Web_Object_Maps.Cursor;
   begin
      --  Specific case for the contextual var

      if Var_Name = Context_Var then
         --  We do not want to remove the context var now, just replace it by
         --  the corresponding context var name. The proper context will be
         --  injected into the Web page later.

         Templates.Insert
           (Translations,
            Templates.Assoc
              (Context_Var, Templates.Tag_From_Name (Context_Var)));

      else
         --  Get Web Object

         Position := WO_Map.Find (Var_Name);

         if Position /= No_Element then
            declare
               Keep          : constant Templates.Translate_Set :=
                                 Lazy_Tag.Translations;
               T             : Templates.Translate_Set;
               Template_Name : Unbounded_String;
            begin
               --  Get translation set for this tag

               Templates.Insert (T, Translations);
               Templates.Insert (T, Lazy_Tag.Translations);

               if not Element (Position).Data_CB.With_Params and then
                  Element (Position).Data_CB.Callback /= null
               then
                  Element (Position).Data_CB.Callback
                    (Lazy_Tag.Request, Lazy_Tag.Ctx'Access, T);
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

               --  We restore the lazy tag state as we do not want to
               --  propagate changes to siblings.

               Lazy_Tag.Translations := Keep;
            end;
         end if;
      end if;
   end Value;

end AWS.Services.Web_Block.Registry;
