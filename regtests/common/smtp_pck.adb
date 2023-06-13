------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Headers;

package body SMTP_Pck is

   use Ada;
   use AWS;

   --------------
   -- Callback --
   --------------

   protected body Callback is

      ------------
      -- Signal --
      ------------

      entry Signal when not Status is
      begin
         Status := True;
      end Signal;

      ----------
      -- Wait --
      ----------

      entry Wait when Status is
      begin
         Status := False;
      end Wait;

   end Callback;

   ---------------
   -- Dump_Mail --
   ---------------

   procedure Dump_Mail (Message : SMTP.Messages.Data) is

      function Set_Boundary (Content : String) return String;
      --  Change the boundary number to be stable

      ------------------
      -- Set_Boundary --
      ------------------

      function Set_Boundary (Content : String) return String is
         Pattern   : constant String := "----=_NextPart_";
         L_Content : String := Content;
         K         : Natural := L_Content'First;
      begin
         loop
            K := Strings.Fixed.Index (L_Content, Pattern, From => K);
            exit when K = 0;

            L_Content (K + Pattern'Length .. K + Pattern'Length + 9) :=
              "9999999999";
            K := K + Pattern'Length + 9;
         end loop;
         return L_Content;
      end Set_Boundary;

      H : constant Headers.List := SMTP.Messages.Headers (Message);
   begin
      Text_IO.Put_Line
        ("From         " & Headers.Get_Values (H, SMTP.Messages.From_Token));
      Text_IO.Put_Line
        ("To           " & Headers.Get_Values (H, SMTP.Messages.To_Token));
      Text_IO.Put_Line
        ("Subject      "
         & Headers.Get_Values (H, SMTP.Messages.Subject_Token));
      Text_IO.Put_Line
        ("Content-Type "
         & Set_Boundary
           (Headers.Get_Values (H, SMTP.Messages.Content_Type_Token)));

      Text_IO.Put_Line (Set_Boundary (SMTP.Messages.Message_Body (Message)));

      Callback.Signal;
   end Dump_Mail;

end SMTP_Pck;
