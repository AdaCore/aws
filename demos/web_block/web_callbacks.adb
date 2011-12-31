------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with AWS.Messages;
with AWS.MIME;
with AWS.Services.Web_Block.Registry;
with AWS.Utils;

package body Web_Callbacks is

   -------------
   -- Counter --
   -------------

   procedure Counter
     (Request      : in              Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      N : Natural := 0;
   begin
      if Context.Exist ("N") then
         N := Natural'Value (Context.Get_Value ("N"));
      end if;

      if Templates.Get (Templates.Get (Translations, "OP")) = "ADD" then
         N := N + 1;
      elsif Templates.Get (Templates.Get (Translations, "OP")) = "SUB"
        and then N > 0
      then
         N := N - 1;
      end if;

      Context.Set_Value ("N", Utils.Image (N));

      Templates.Insert
        (Translations, Templates.Assoc ("COUNTER", N));
   end Counter;

   ----------
   -- Main --
   ----------

   function Main (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
      Set : Templates.Translate_Set;
   begin
      if URI = "/add" then
         Templates.Insert (Set, Templates.ASSOC ("OP", "ADD"));
      elsif URI = "/sub" then
         Templates.Insert (Set, Templates.ASSOC ("OP", "SUB"));
      end if;

      return Web_Block.Registry.Build
        (Key          => "/",
         Request      => Request,
         Translations => Set);
   end Main;

end Web_Callbacks;
