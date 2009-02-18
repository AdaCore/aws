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

with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Services.Web_Block.Context;
with AWS.Status;
with AWS.Templates;

package AWS.Services.Web_Block.Registry is

   use Ada.Strings.Unbounded;

   type Page is record
      Content      : Unbounded_String;
      --  Rendered page
      Content_Type : Unbounded_String;
      --  The page's content type
      Set          : Templates.Translate_Set;
      --  The translate set used to render the page
   end record;

   No_Page : constant Page;

   type Data_Callback is access procedure
     (Request      : in Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   type Template_Callback is access function
     (Request : in Status.Data) return String;

   procedure Register
     (Key              : in String;
      Template         : in String;
      Data_CB          : in Data_Callback;
      Content_Type     : in String  := MIME.Text_HTML;
      Prefix           : in Boolean := False;
      Context_Required : in Boolean := False);
   --  Key is a Lazy_Tag or template page name. Template is the corresponding
   --  template file. Data_CB is the callback used to retrieve the translation
   --  table to render the page. If Context_Required is True a proper context
   --  must be present when rendering the page otherwise Context_Error callback
   --  (see Build below) is called.

   procedure Register
     (Key              : in String;
      Template_CB      : in Template_Callback;
      Data_CB          : in Data_Callback;
      Content_Type     : in String := MIME.Text_HTML;
      Context_Required : in Boolean := False);
   --  Key is a Lazy_Tag or template page name. Template_CB is the callback
   --  used to retrieve the corresponding template file name. Data_CB is the
   --  callback used to retrieve the translation table to render the page.

   function Parse
     (Key           : in String;
      Request       : in Status.Data;
      Translations  : in Templates.Translate_Set;
      Context_Error : in String := "") return Page;
   --  Parse the Web page registered under Key. Context_Error is the key
   --  of the registered template to use when a required context is not
   --  present.

   function Content_Type (Key : in String) return String;
   --  Returns the Content_Type recorded for the web object

   function Build
     (Key           : in String;
      Request       : in Status.Data;
      Translations  : in Templates.Translate_Set;
      Status_Code   : in Messages.Status_Code := Messages.S200;
      Cache_Control : in Messages.Cache_Option := Messages.Unspecified;
      Context_Error : in String := "") return Response.Data;
   --  Same as above but returns a standard Web page

   function Get_Context
     (Request : not null access Status.Data) return Web_Block.Context.Object;
   --  Gets the proper context id for this request

private

   No_Page : constant Page :=
               Page'(Content      => Null_Unbounded_String,
                     Content_Type => Null_Unbounded_String,
                     Set          => Templates.Null_Set);

end AWS.Services.Web_Block.Registry;
