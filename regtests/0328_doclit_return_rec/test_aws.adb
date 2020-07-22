------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2020, AdaCore                         --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with SOAP.Utils;

package body Test_AWS is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   -----------
   -- Test1 --
   -----------

   procedure Test1 (Rec : Types_AWS.Rec_Type) is
   begin
      Put_Line ("@ Begin Test1");
      Put_Line ("@   Input Rec.Status = "
                & Types_AWS.Status_Type'Image (Rec.Status));
      Put_Line ("@   Input Rec.Message = " & To_String (Rec.Message));
      Put_Line ("@ End Test1");
   end Test1;

   -----------
   -- Test2 --
   -----------

   procedure Test2 (Stat : Types_AWS.Status_Type) is
   begin
      Put_Line ("@ Begin Test2");
      Put_Line ("@   Input Stat = " & Types_AWS.Status_Type'Image (Stat));
      Put_Line ("@ End Test2");
   end Test2;

   -----------
   -- Test3 --
   -----------

   function Test3 (X : Integer) return Types_AWS.Rec_Type is
      Rec : constant Types_AWS.Rec_Type :=
              (Types_AWS.Success,
               To_Unbounded_String ("A test message from server."));

   begin
      Put_Line ("@ Begin Test3");
      Put_Line ("@   Output Rec.Status = "
                & Types_AWS.Status_Type'Image (Rec.Status));
      Put_Line ("@   Output Rec.Message = " & To_String (Rec.Message));
      Put_Line ("@ End Test3");
      return Rec;
   end Test3;

end Test_AWS;
