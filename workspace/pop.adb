
--  $Id$

--  Simple POP demo, it depends on the mailbox content

with Ada.Text_IO;

with AWS.POP;

procedure POP is

   use Ada;
   use AWS;

   Server : AWS.POP.Server;

   M      : AWS.POP.Message;
   A      : AWS.Pop.Attachment;

   procedure Display (M : in AWS.POP.Message; Quit : in out Boolean) is
   begin
      Text_IO.Put_Line ("From    " & AWS.POP.From (M));
      Text_IO.Put_Line ("Subject " & AWS.POP.Subject (M));
   end Display;

   procedure List_Message is new AWS.POP.For_Every_Message (Display);

begin
   Server := AWS.POP.Initialize
     ("pop.xyz.cc", "username", "password", AWS.POP.Clear_Text);

   Text_IO.Put_Line ("M " & Natural'Image (AWS.POP.Message_Count (Server)));
   Text_IO.Put_Line ("S " & Natural'Image (AWS.POP.Mailbox_Size (Server)));

   M := AWS.POP.Get (Server, 3);

   A := AWS.POP.Get (M, 2);

   AWS.POP.Write (A, ".");

   Text_IO.Put_Line ("From    " & AWS.POP.From (M));
   Text_IO.Put_Line ("Subject " & AWS.POP.Subject (M));

   Text_IO.Put_Line ("Attachments : " & AWS.POP.Attachment_Count (M)'Img);

   Text_IO.New_Line;
   Text_IO.Put_Line ("Iterator:");

   List_Message (Server, Remove => False);

   AWS.POP.Close (Server);
end POP;
