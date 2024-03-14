------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2015, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

with AWS.Parameters;

with GNAT.Regpat;

package body AWS.Services.Web_Block.Registry is

   use GNAT;

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

   --  Wrap access to the shared map with a protected object for safe
   --  concurrent access.

   protected WO_Store is

      procedure Include (Key : String; WO : Web_Object);
      --  Include an element in the map

      procedure Find (Key : String; Position : out Web_Object_Maps.Cursor);
      --  Returns a cursor pointing to element Key (or No_Element)

      procedure Element
        (Position : Web_Object_Maps.Cursor;
         WO       : out Web_Object);
      --  Returns element pointed to by Position

   private
      WO_Map : Map;
   end WO_Store;

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;

   type URL_Pattern (With_Matcher : Boolean := False) is record
      Prefix : Unbounded_String;
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
      Position : Web_Object_Maps.Cursor;
   begin
      WO_Store.Find (Key, Position);

      if Position = No_Element then
         return "";
      else
         declare
            WO : Web_Object;
         begin
            WO_Store.Element (Position, WO);
            return To_String (WO.Content_Type);
         end;
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

      function Get_Matching_Web_Object return Callback_Parameters;
      --  Get the Web_Object matching Search_Key in Pattern_URL_Vector
      --  Returns the Parameters extracted from the URL patterns.

      -----------------------------
      -- Get_Matching_Web_Object --
      -----------------------------

      function Get_Matching_Web_Object return Callback_Parameters is
      begin
         WO_Store.Find (Key, Position);

         if Position /= No_Element then
            return Empty_Callback_Parameters;
         end if;

         for Vector_Cursor in Pattern_URL_Vector.Iterate loop
            declare
               use GNAT.Regpat;
               use Pattern_URL_Container;
               P_URI : constant URL_Pattern := Element (Vector_Cursor);
               K     : constant String := To_String (P_URI.Prefix);
            begin
               if K'Length <= Key'Length
                 and then Key (Key'First .. Key'First + K'Length - 1) = K
               then
                  --  If a regexp is defined, check whether it matched

                  if P_URI.With_Matcher then
                     declare
                        Count   : constant Natural :=
                                    Paren_Count (P_URI.Matcher.all);
                        Matched : Match_Array (0 .. Count);
                     begin
                        Match (Self    => P_URI.Matcher.all,
                               Data    => Key,
                               Matches => Matched);

                        if Matched (0) /= No_Match then
                           --  Returns the registered web object
                           --  Registered with a key = Prefix + Regexp
                           WO_Store.Find (To_String (P_URI.Key), Position);

                           declare
                              Params : Callback_Parameters (1 .. Count);
                           begin
                              for J in 1 .. Count loop
                                 Params (J) :=
                                    To_Unbounded_String
                                      (Key (Matched (J).First
                                            .. Matched (J).Last));
                              end loop;
                              return Params;
                           end;
                        end if;
                     end;

                  else
                     --  Only a prefix is defined.
                     --  No need to search for other candidates
                     WO_Store.Find (K, Position);
                     return Empty_Callback_Parameters;
                  end if;
               end if;
            end;
         end loop;

         return Empty_Callback_Parameters;
      end Get_Matching_Web_Object;

      Parsed_Page : Page := No_Page;
      Parameters  : constant Callback_Parameters := Get_Matching_Web_Object;
   begin
      --  Use provided context if a user's defined one

      if Position /= No_Element then
         declare
            T             : Templates.Translate_Set;
            Template_Name : Unbounded_String;
            Content       : Unbounded_String;
            C_Index       : Natural;
            CID           : Web_Block.Context.Id;
            Element       : Web_Object;
         begin
            WO_Store.Element (Position, Element);

            --  Get translation set for this tag

            if Ctx = "" and then Element.Context_Required then
               --  No context but it is required
               return Parse (Context_Error, Request, Translations);

            else
               Templates.Insert (T, Translations);

               --  Call the Data_CB
               if not Element.Data_CB.With_Params then
                  if Element.Data_CB.Callback /= null then
                     Element.Data_CB.Callback (LT.Request, LT.Ctx'Access, T);
                  end if;
               else
                  if Element.Data_CB.Callback_With_Parameters /= null then
                     Element.Data_CB.Callback_With_Parameters
                        (LT.Request, LT.Ctx'Access, Parameters, T);
                  end if;
               end if;

               if Element.Callback_Template then
                  Template_Name := To_Unbounded_String
                    (Element.Template_CB (Request));
               else
                  Template_Name := Element.Template;
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

               if Element.Content_Type = MIME.Text_HTML
                 and then C_Index = 0
               then
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

               elsif Element.Content_Type = MIME.Text_XML
                 and then C_Index = 0
               then
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
                 Page'(Content_Type => Element.Content_Type,
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
              Data_CB           => Web_Object_Data_Callback'
                                     (With_Params => False,
                                      Callback    => Data_CB),
              Context_Required  => Context_Required);
   begin
      --  Register Tag

      WO_Store.Include (Key, WO);

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
              Data_CB           => Web_Object_Data_Callback'
                                     (With_Params => False,
                                      Callback    => Data_CB),
              Context_Required  => Context_Required);
   begin
      --  Register Tag

      WO_Store.Include (Key, WO);
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
              Data_CB           => Web_Object_Data_Callback'
                                     (With_Params              => True,
                                      Callback_With_Parameters => Data_CB),
              Context_Required  => Context_Required);
      Key     : constant String := Prefix & Regexp;
      Matcher : constant Pattern_Matcher_Access :=
                  new Regpat.Pattern_Matcher'
                    (Regpat.Compile (Key, Regpat.Case_Insensitive));
   begin
      --  Register Tag

      WO_Store.Include (Key, WO);

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
              Data_CB           => Web_Object_Data_Callback'
                                     (With_Params              => True,
                                      Callback_With_Parameters => Data_CB),
              Context_Required  => Context_Required);
      Key     : constant String := Prefix & Regexp;
      Matcher : constant Pattern_Matcher_Access :=
                  new Regpat.Pattern_Matcher'
                    (Regpat.Compile (Key, Regpat.Case_Insensitive));
   begin
      --  Register Tag

      WO_Store.Include (Key, WO);

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

         WO_Store.Find (Var_Name, Position);

         if Position /= No_Element then
            declare
               Keep          : constant Templates.Translate_Set :=
                                 Lazy_Tag.Translations;
               T             : Templates.Translate_Set;
               Template_Name : Unbounded_String;
               Element       : Web_Object;
            begin
               --  Get translation set for this tag

               Templates.Insert (T, Translations);
               Templates.Insert (T, Lazy_Tag.Translations);

               WO_Store.Element (Position, Element);

               if not Element.Data_CB.With_Params
                  and then Element.Data_CB.Callback /= null
               then
                  Element.Data_CB.Callback
                    (Lazy_Tag.Request, Lazy_Tag.Ctx'Access, T);
               end if;

               if Element.Callback_Template then
                  Template_Name := To_Unbounded_String
                    (Element.Template_CB (Lazy_Tag.Request));
               else
                  Template_Name := Element.Template;
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

   --------------
   -- WO_Store --
   --------------

   protected body WO_Store is

      -------------
      -- Element --
      -------------

      procedure Element
        (Position : Web_Object_Maps.Cursor;
         WO       : out Web_Object) is
      begin
         WO := Web_Object_Maps.Element (Position);
      end Element;

      ----------
      -- Find --
      ----------

      procedure Find (Key : String; Position : out Web_Object_Maps.Cursor) is
      begin
         Position := WO_Map.Find (Key);
      end Find;

      -------------
      -- Include --
      -------------

      procedure Include (Key : String; WO : Web_Object) is
      begin
         WO_Map.Include (Key, WO);
      end Include;

   end WO_Store;

end AWS.Services.Web_Block.Registry;
