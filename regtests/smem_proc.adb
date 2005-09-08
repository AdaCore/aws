with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.Std;

procedure SMem_Proc (Security : in Boolean) is
   use AWS.Net;
   use Ada.Streams;
   use Ada.Text_IO;

   Server : Std.Socket_Type;
   S1, S2 : Socket_Type'Class := Socket (Security);
   C1, C2 : Socket_Type'Class := Socket (Security);

   Sample : Stream_Element_Array (1 .. 128);

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class);

   -------------
   -- Connect --
   -------------

   procedure Connect is
      task Connector;
      task body Connector is
      begin
         Connect (S1, "127.0.0.1", Std.Get_Port (Server));
      exception
         when E : others =>
            Put_Line
              ("On connect " & Ada.Exceptions.Exception_Information (E));
      end Connector;
   begin
      Accept_Socket (Server, S2);
   end Connect;

   ----------
   -- Test --
   ----------

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class) is
      Buffer : Stream_Element_Array (Sample'First .. Sample'Last + 2);
      Last   : Stream_Element_Offset;

      task Send;

      task body Send is
      begin
         AWS.Net.Send (S2, Sample);
      exception
         when E : others =>
            Put_Line ("On send " & Ada.Exceptions.Exception_Information (E));
      end Send;

   begin
      AWS.Net.Receive (S1, Buffer, Last);

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

   Std.Bind (Server, 0);
   Std.Listen (Server);

   Connect;

   Test (S1, S2);
   Test (S2, S1);

   C1 := S1;
   C2 := S2;

   Connect;

   Test (S1, S2);
   Test (S2, S1);

   Shutdown (C1);
   Free (C1);
   Shutdown (C2);
   Free (C2);

   Test (S1, S2);
   Test (S2, S1);

   Shutdown (S2);
   Free (S2);
   Shutdown (S1);
   Free (S1);
   Std.Shutdown (Server);
   Std.Free (Server);
end SMem_Proc;
