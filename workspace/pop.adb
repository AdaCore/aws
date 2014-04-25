------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

--  Simple POP demo, it depends on the mailbox content

with Ada.Text_IO;

with AWS.POP;

procedure POP is

   use Ada;
   use AWS;

   Mailbox : AWS.POP.Mailbox;

   M       : AWS.POP.Message;
   A       : AWS.Pop.Attachment;
   Counter : Integer := 32;

   procedure Display
     (M     : AWS.POP.Message;
      Index : Positive;
      Quit  : in out Boolean) is
   begin
      Text_IO.Put_Line ("From    " & AWS.POP.From (M));
      Text_IO.Put_Line ("Subject " & AWS.POP.Subject (M));
      Counter := Counter - 1;
      Quit := Counter <= 0;
   end Display;

   procedure List_Message is new AWS.POP.For_Every_Message (Display);

   procedure List_Message_Header is
      new AWS.POP.For_Every_Message_Header (Display);

begin
   Mailbox := AWS.POP.Initialize
     ("host", "username", "password", AWS.POP.Clear_Text);

   Text_IO.Put_Line ("M " & Natural'Image (AWS.POP.Message_Count (Mailbox)));
   Text_IO.Put_Line ("S " & Natural'Image (AWS.POP.Size (Mailbox)));

   M := AWS.POP.Get (Mailbox, 3);

--     A := AWS.POP.Get (M, 2);

--     AWS.POP.Write (A, ".");

   Text_IO.Put_Line ("From    " & AWS.POP.From (M));
   Text_IO.Put_Line ("Subject " & AWS.POP.Subject (M));

   Text_IO.Put_Line ("Attachments : " & AWS.POP.Attachment_Count (M)'Img);

   Text_IO.New_Line;
   Text_IO.Put_Line ("Iterator:");

   List_Message (Mailbox, Remove => False);

   Text_IO.New_Line;
   Text_IO.Put_Line ("Header Iterator:");

   Counter := 32;
   List_Message_Header (Mailbox);

   AWS.POP.Close (Mailbox);
end POP;
