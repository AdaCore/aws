------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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

with AWS.Messages;
with AWS.MIME;
with AWS.Services.Web_Block.Registry;
with AWS.Utils;

package body Web_Callbacks is

   --------------------
   -- Widget_Counter --
   --------------------

   procedure Widget_Counter
     (Request      : in              Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      N : Natural := 0;
   begin
      if Context.Exist ("N") then
         N := Natural'Value (Context.Get_Value ("N"));
      end if;

      Templates.Insert
        (Translations, Templates.Assoc ("COUNTER", N));
   end Widget_Counter;

   ------------------
   -- Onclick_Next --
   ------------------

   procedure Onclick_Next
     (Request      : in              Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      N : Natural := 0;
   begin
      if Context.Exist ("N") then
         N := Natural'Value (Context.Get_Value ("N"));
      end if;

      N := N + 1;

      Context.Set_Value ("N", Utils.Image (N));

      Templates.Insert
        (Translations, Templates.Assoc ("COUNTER", N));
   end Onclick_Next;

   ----------------------
   -- Onclick_Previous --
   ----------------------

   procedure Onclick_Previous
     (Request      : in              Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      N : Natural := 0;
   begin
      if Context.Exist ("N") then
         N := Natural'Value (Context.Get_Value ("N"));
      end if;

      if N > 0 then
         N := N - 1;
      end if;

      Context.Set_Value ("N", Utils.Image (N));

      Templates.Insert
        (Translations, Templates.Assoc ("COUNTER", N));
   end Onclick_Previous;

   ----------
   -- Main --
   ----------

   function Main (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI'Length > 7
        and then URI (URI'First .. URI'First + 6) = "/we_js/"
      then
         --  Handle the Ajax framework files
         return Response.Build
           (Content_Type => MIME.Text_Javascript,
            Message_Body =>
              Templates.Parse
                ("../../web_elements/javascripts"
                 & URI (URI'First + 6 .. URI'Last),
                 Templates.Null_Set));

      else
         return Web_Block.Registry.Build
           (Key          => URI,
            Request      => Request,
            Translations => Templates.Null_Set);
      end if;
   end Main;

end Web_Callbacks;
