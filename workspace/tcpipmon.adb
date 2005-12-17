
--  TCP monitor, display all exchanged data

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Streams;         use Ada.Streams;
with GNAT.Sockets;        use GNAT.Sockets;

procedure Tcpipmon is

   use Ada;

   task type Transport is
      entry Start (Incoming : Socket_Type);
   end Transport;

   type Transporter is access Transport;

   K  : Natural := 0;

   procedure Put (Item : in Streams.Stream_Element) is
   begin
      K := K + 1;

      if K = 1 then
         Integer_Text_IO.Put (K, Width => 5);
         Text_IO.Put ("> ");
      end if;

      Put (Character'Val (Item));

      if Item = 10 then
         Integer_Text_IO.Put (K + 1, Width => 5);
         Text_IO.Put ("> ");
      end if;
   end Put;

   procedure Move (A, B : Socket_Type) is
      Data      : Streams.Stream_Element_Array (1 .. 1000);
      Data_Last : Streams.Stream_Element_Offset;
      Send_Last : Streams.Stream_Element_Offset;
   begin
      loop
         Receive_Socket (A, Data, Data_Last);
         for I in Data'First .. Data_Last loop
            Put (Data (I));
         end loop;
         Send_Last := Data'First - 1;
         loop
            Send_Socket (B, Data (Send_Last + 1 .. Data_Last), Send_Last);
            exit when Data_Last = Send_Last;
         end loop;
      end loop;
   exception when Socket_Error =>
      begin
         Close_Socket (A);
      exception when Socket_Error => null;
      end;
      begin
         Close_Socket (B);
      exception when Socket_Error => null;
      end;
   end Move;

   task body Transport is
      Socket_In : Socket_Type;
      Address   : Sock_Addr_Type;
      Socket_Out : Socket_Type;
   begin
      New_Line;
      Put_Line ("------- Link opened ---------------------------------------");
      New_Line;

      accept Start (Incoming : Socket_Type) do
         Socket_In := Incoming;
      end Start;

      Address.Addr := Addresses (Get_Host_By_Name (Argument (2)), 1);
      Address.Port := Port_Type'Value (Argument (3));

      Create_Socket (Socket_Out);

      Set_Socket_Option
        (Socket_Out,
         Socket_Level,
         (Reuse_Address, True));

      Set_Socket_Option
        (Socket_Out,
         Socket_Level,
         (No_Delay, True));

      Connect_Socket (Socket_Out, Address);

      declare
         task Move_In_To_Out;
         task body Move_In_To_Out is
         begin
            Move (Socket_In, Socket_Out);
         end Move_In_To_Out;
      begin
         Move (Socket_Out, Socket_In);
      end;

      New_Line;
      Put_Line ("------- Link close ----------------------------------------");
      New_Line;
   exception when E : others =>
      Put_Line ("Exception " & Exception_Information (E));
   end Transport;

   Address   : Sock_Addr_Type;
   Server    : Socket_Type;
   Socket    : Socket_Type;
begin
   if Argument_Count = 3 then
      Initialize;

      Address.Addr := Addresses (Get_Host_By_Name ("localhost"), 1);
      Address.Port := Port_Type'Value (Argument (1));

      Create_Socket (Server);

      Set_Socket_Option
        (Server,
         Socket_Level,
         (Reuse_Address, True));

      Set_Socket_Option
        (Server,
         Socket_Level,
         (No_Delay, True));

      Bind_Socket (Server, Address);
      Listen_Socket (Server);

      loop
         Accept_Socket (Server, Socket, Address);
         declare
            Tsk : Transporter := new Transport;
         begin
            Tsk.Start (Socket);
         end;
      end loop;

   else
      Put_Line ("tcpipmon needs 3 arguments:");
      Put_Line ("    tcpipmon inport oname oport");
      Put_Line ("where:");
      Put_Line ("    inport = input tcp/ip port number");
      Put_Line ("    oname  = name of computer to connect to");
      Put_Line ("    oport  = tcp/ip port number on that computer");
      Put_Line ("Example:  tcpipmon 8080 localhost 80");
   end if;
end Tcpipmon;
