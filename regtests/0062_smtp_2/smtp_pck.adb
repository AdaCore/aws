------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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

   procedure Dump_Mail (Message : in SMTP.Messages.Data) is

      function Set_Boundary (Content : in String) return String;
      --  Change the boundary number to be stable

      ------------------
      -- Set_Boundary --
      ------------------

      function Set_Boundary (Content : in String) return String is
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
