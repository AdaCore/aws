with Ada.Streams;
with Ada.Text_IO;
with AWS.Net;

procedure SPair_Proc (Security : Boolean) is
   use Ada.Streams;
   use Ada.Text_IO;

   S1, S2 : AWS.Net.Socket_Type'Class := AWS.Net.Socket (Security);
   Sample : Stream_Element_Array (1 .. 128);
   Buffer : Stream_Element_Array (1 .. 130);
   Last   : Stream_Element_Offset;

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class);

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class) is
   begin
      if Security then
         --  SSL socket need handshake during send/receive.
         --  we need different tasks to do this.

         declare
            task Send;
            task body Send is
            begin
               AWS.Net.Send (S2, Sample);
            end;
         begin
            AWS.Net.Receive (S1, Buffer, Last);
         end;

      else
         AWS.Net.Send (S2, Sample);
         AWS.Net.Receive (S1, Buffer, Last);
      end if;

      if Sample = Buffer (1 .. Last) then
         Put_Line ("OK");
      else
         Put_Line ("Error");
      end if;
   end Test;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element (J);
   end loop;

   AWS.Net.Socket_Pair (S1, S2);

   Test (S1, S2);
   Test (S2, S1);
   AWS.Net.Shutdown (S1);
   AWS.Net.Shutdown (S2);
   AWS.Net.Free (S1);
   AWS.Net.Free (S2);
end SPair_Proc;
