
--  Simple POP demo, it depends on the mailbox content

with Ada.Text_IO;

with AWS.POP;

procedure POP is

   use Ada;
   use AWS;

   Mailbox : AWS.POP.Mailbox;

   M       : AWS.POP.Message;
   A       : AWS.Pop.Attachment;

   procedure Display
     (M     : in     AWS.POP.Message;
      Index : in     Positive;
      Quit  : in out Boolean) is
   begin
      Text_IO.Put_Line ("From    " & AWS.POP.From (M));
      Text_IO.Put_Line ("Subject " & AWS.POP.Subject (M));
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

   List_Message_Header (Mailbox);

   AWS.POP.Close (Mailbox);
end POP;
