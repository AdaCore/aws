------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

--  $Id$

package body AWS.Messages is

   type String_Access is access constant String;

   S100_Code : aliased constant String := "100";
   S101_Code : aliased constant String := "101";
   S200_Code : aliased constant String := "200";
   S201_Code : aliased constant String := "201";

   S100_Message : aliased constant String := "Continue";
   S101_Message : aliased constant String := "Switching Protocols";
   S200_Message : aliased constant String := "OK";
   S201_Message : aliased constant String := "Create";

   type Status_Data is record
      Code          : String_Access;
      Reason_Phrase : String_Access;
   end record;

   Status_Messages : array (Status_Code) of Status_Data
     := (S100 => (S100_Code'Access, S100_Message'Access),
         S101 => (S101_Code'Access, S101_Message'Access),
         S200 => (S200_Code'Access, S200_Message'Access),
         S201 => (S201_Code'Access, S201_Message'Access));

   -----------------
   -- Status_Line --
   -----------------

   function Status_Line (Code : in Status_Code) return String is
   begin
      return HTTP_Version & ' '
        & Status_Messages (Code).Code.all & ' '
        & Status_Messages (Code).Reason_Phrase.all;
   end Status_Line;

end AWS.Messages;
