------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

with Ada.Strings.Unbounded;

with AWS.Containers.Tables;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Services.Web_Block.Context;
with AWS.Status;
with AWS.Templates;

package AWS.Services.Web_Block.Registry is

   use Ada;
   use Ada.Strings.Unbounded;

   type Page is record
      Content      : Unbounded_String;
      --  Rendered page
      Content_Type : Unbounded_String;
      --  The page's content type
      Set          : Templates.Translate_Set;
      --  The translate set used to render the page
      Ctx_Id       : Context.Id;
      --  The page context id
   end record;

   No_Page : constant Page;

   type Data_Callback is access procedure
     (Request      : Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   type Callback_Parameters is new Containers.Tables.VString_Array;
   Empty_Callback_Parameters : Callback_Parameters (1 .. 0);

   type Data_With_Param_Callback is access procedure
     (Request      : Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Parameters   : Callback_Parameters;
      Translations : in out Templates.Translate_Set);

   type Template_Callback is access
     function (Request : Status.Data) return String;

   procedure Register
     (Key              : String;
      Template         : String;
      Data_CB          : Data_Callback;
      Content_Type     : String  := MIME.Text_HTML;
      Prefix           : Boolean := False;
      Context_Required : Boolean := False);
   --  Key is a Lazy_Tag or template page name. Template is the corresponding
   --  template file. Data_CB is the callback used to retrieve the translation
   --  table to render the page. If Context_Required is True a proper context
   --  must be present when rendering the page otherwise Context_Error callback
   --  (see Build below) is called.

   procedure Register
     (Key              : String;
      Template_CB      : Template_Callback;
      Data_CB          : Data_Callback;
      Content_Type     : String := MIME.Text_HTML;
      Context_Required : Boolean := False);
   --  Key is a Lazy_Tag or template page name. Template_CB is the callback
   --  used to retrieve the corresponding template file name. Data_CB is the
   --  callback used to retrieve the translation table to render the page.

   procedure Register_Pattern_URL
     (Prefix           : String;
      Regexp           : String;
      Template         : String;
      Data_CB          : Data_With_Param_Callback;
      Content_Type     : String  := MIME.Text_HTML;
      Context_Required : Boolean := False);
   --  Prefix is the prefix key to match
   --  Then the rest of the url is a regular expression defined by Regexp
   --  All regular-expression groups (inside parenthesis) is captured and pass
   --  to the Data_CB in the Parameters vector
   --  For instance, with:
   --      Prefix = '/page/'
   --      Regexp = '([0-9]+)/section-([a-z]+)/.*'
   --  The url '/page/42/section-b/part2' will be matched and Data_CB will
   --  be called with Parameters = <42, "b">

   procedure Register_Pattern_URL
     (Prefix           : String;
      Regexp           : String;
      Template_CB      : Template_Callback;
      Data_CB          : Data_With_Param_Callback;
      Content_Type     : String  := MIME.Text_HTML;
      Context_Required : Boolean := False);
   --  Same as above but takes a Template_Callback

   function Parse
     (Key           : String;
      Request       : Status.Data;
      Translations  : Templates.Translate_Set;
      Context       : Web_Block.Context.Object := Web_Block.Context.Empty;
      Context_Error : String := "") return Page;
   --  Parse the Web page registered under Key. Context_Error is the key
   --  of the registered template to use when a required context is not
   --  present.

   function Content_Type (Key : String) return String;
   --  Returns the Content_Type recorded for the web object

   function Build
     (Key           : String;
      Request       : Status.Data;
      Translations  : Templates.Translate_Set;
      Status_Code   : Messages.Status_Code := Messages.S200;
      Cache_Control : Messages.Cache_Option := Messages.Unspecified;
      Context       : access Web_Block.Context.Object := null;
      Context_Error : String := "") return Response.Data;
   --  Same as above but returns a standard Web page. If Context is set it
   --  is the initial value and will be setup at the end to correspond to
   --  the recorded new context.

   function Get_Context
     (Request : Status.Data) return Web_Block.Context.Object;
   --  Gets the proper context object for this request. Note that if the
   --  context object is modified outside of the Web_Block framework it must be
   --  passed to the Build or Parse procedure above.

private

   No_Page : constant Page :=
               Page'(Content      => Null_Unbounded_String,
                     Content_Type => Null_Unbounded_String,
                     Set          => Templates.Null_Set,
                     Ctx_Id       => <>);

end AWS.Services.Web_Block.Registry;
