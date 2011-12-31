------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Text_IO;

package body WSDL_8 is

   use Ada.Text_IO;

   ----------
   -- Proc --
   ----------

   procedure Proc
     (Name  : String;
      Files : Set_Of_Files) is
   begin
      Put_Line ("Name : " & Name);

      for K in Files'Range loop
         Put_Line ("K = " & Integer'Image (K));
         Put_Line ("   filename = " & To_String (Files (K).Filename));
         Put_Line ("   content  = " & To_String (Files (K).Content));
      end loop;
   end Proc;

end WSDL_8;
