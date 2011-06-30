------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                   Copyright (C) 2005-2011, AdaCore                       --
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

with Ada.Streams;

package body AWS.Net.Acceptors is

   Server_Index   : constant := 1;
   Signal_Index   : constant := 2;
   First_Index    : constant := 3;

   Socket_Command : constant := 1;

   ---------
   -- Get --
   ---------

   procedure Get
     (Acceptor : in out Acceptor_Type;
      Socket   : out    Socket_Access;
      On_Error : access procedure
        (E : Ada.Exceptions.Exception_Occurrence) := null)
   is
      use type Sets.Socket_Count;

      procedure Add_Sockets;
      --  Add sockets to the acceptor either from Accept_Socket or from
      --  Give_Back.

      procedure Shutdown;
      pragma No_Return (Shutdown);

      Too_Many_FD  : Boolean := False;
      Ready, Error : Boolean;

      -----------------
      -- Add_Sockets --
      -----------------

      procedure Add_Sockets is
      begin
         --  Save Acceptor.Last to do not try to get status of new arrived
         --  sockets until it wouldn't in the Wait call.

         Acceptor.Last := Sets.Count (Acceptor.Set);

         Sets.Is_Read_Ready (Acceptor.Set, Server_Index, Ready, Error);

         if Error then
            Raise_Socket_Error
              (Acceptor.Server.all, "Accepting socket error.");

         elsif Ready then
            declare
               use Ada.Real_Time;
               New_Socket : Socket_Type'Class := Acceptor.Constructor (False);
            begin
               --  We could not accept SSL socket because SSL handshake could
               --  take a long time inside Accept_Socket. We would make socket
               --  SSL later outside acceptor if necessary.

               Accept_Socket (Acceptor.Server.all, New_Socket);

               Sets.Add
                 (Acceptor.Set,
                  New_Socket,
                  Data => (Time => Clock, First => True),
                  Mode => Sets.Input);

            exception
               when E : Socket_Error =>
                  if On_Error /= null then
                     On_Error (E);

                     --  Most probable that Accept_Socket error is because the
                     --  number of sockets per process exceeded.
                     --  Set the flag Too_Many_FD to be able to use shorter
                     --  timeouts in the next sockets expiration check.

                     Too_Many_FD := True;

                  else
                     raise;
                  end if;
            end;
         end if;

         Sets.Is_Read_Ready (Acceptor.Set, Signal_Index, Ready, Error);

         if Error then
            Shutdown;

         elsif Ready then
            declare
               use Ada.Real_Time;

               Socket : Socket_Access;
               Bytes  : Ada.Streams.Stream_Element_Array (1 .. 16);
               Last   : Ada.Streams.Stream_Element_Offset;
            begin
               --  Read bytes from signalling socket and take sockets from
               --  mailbox.

               begin
                  Receive (Acceptor.R_Signal.all, Bytes, Last);
               exception
                  when Socket_Error =>
                     Shutdown;
               end;

               for J in 1 .. Last loop
                  if Bytes (J) = Socket_Command then
                     Acceptor.Box.Get (Socket);
                     Sets.Add
                       (Acceptor.Set,
                        Socket,
                        Data  => (Time => Clock, First => False),
                        Mode  => Sets.Input);
                  else
                     raise Constraint_Error;
                  end if;
               end loop;
            end;
         end if;

         Acceptor.Index := First_Index;
      end Add_Sockets;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         while Sets.Count (Acceptor.Set) > 0 loop
            Sets.Remove_Socket (Acceptor.Set, 1, Socket);
            Shutdown (Socket.all);

            --  We can free other sockets, because it is not
            --  used anywhere else when it is in socket set.

            Free (Socket);
         end loop;

         raise Socket_Error;
      end Shutdown;

      First        : constant Boolean := True;
      Timeout      : array (Boolean) of Real_Time.Time_Span;
      Oldest_Idx   : Sets.Socket_Count;

      Wait_Timeout : Real_Time.Time_Span;

   begin
      if Sets.Count (Acceptor.Set) = 0 then
         --  After shutdown of the server socket
         raise Socket_Error;
      end if;

      loop
         if Sets.Count (Acceptor.Set) > Acceptor.Force_Length
           or else Too_Many_FD
         then
            Timeout (First)     := Acceptor.Force_First_Timeout;
            Timeout (not First) := Acceptor.Force_Timeout;
         else
            Timeout (First)     := Acceptor.First_Timeout;
            Timeout (not First) := Acceptor.Timeout;
         end if;

         Wait_Timeout := Timeout (not First);
         Oldest_Idx   := 0;

         Read_Ready : loop
            exit Read_Ready when Acceptor.Index > Acceptor.Last;

            Sets.Is_Read_Ready (Acceptor.Set, Acceptor.Index, Ready, Error);

            if Error or else Ready then
               Sets.Remove_Socket (Acceptor.Set, Acceptor.Index, Socket);

               Acceptor.Last := Acceptor.Last - 1;

               if Error then
                  Shutdown (Socket.all);
                  Free (Socket);

               elsif Ready then
                  return;
               else
                  raise Program_Error;
               end if;

            else
               --  Check for timeout

               declare
                  use Ada.Real_Time;
                  Data : constant Socket_Data_Type
                    := Sets.Get_Data (Acceptor.Set, Acceptor.Index);
                  Diff : constant Time_Span
                    := Timeout (Data.First) - (Clock - Data.Time);
               begin
                  if Diff <= Time_Span_Zero then
                     Sets.Remove_Socket (Acceptor.Set, Acceptor.Index, Socket);
                     Acceptor.Last := Acceptor.Last - 1;
                     Shutdown (Socket.all);
                     Free (Socket);
                  else
                     if Diff < Wait_Timeout then
                        Wait_Timeout := Diff;
                        Oldest_Idx   := Acceptor.Index;
                     end if;

                     Acceptor.Index := Acceptor.Index + 1;
                  end if;
               end;
            end if;
         end loop Read_Ready;

         if Oldest_Idx > 0 and then Acceptor.Last > Acceptor.Close_Length then
            Sets.Remove_Socket (Acceptor.Set, Oldest_Idx, Socket);
            Acceptor.Last := Acceptor.Last - 1;
            Shutdown (Socket.all);
            Free (Socket);
         end if;

         begin
            Sets.Wait (Acceptor.Set, Real_Time.To_Duration (Wait_Timeout));
            Error := False;
         exception
            when E : Socket_Error =>
               Error := True;

               if On_Error /= null then
                  On_Error (E);

                  --  Most probable that Wait error is because the
                  --  number of sockets per process exceeded.
                  --  Set the flag Too_Many_FD to be able to use shorter
                  --  timeouts in the next sockets expiration check.

                  Too_Many_FD := True;

               else
                  raise;
               end if;
         end;

         if not Error then
            Add_Sockets;
         end if;
      end loop;
   end Get;

   ---------------
   -- Give_Back --
   ---------------

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket : Socket_Access;
      Success : out Boolean) is
   begin
      Acceptor.Box.Add (Socket, Positive (Acceptor.Back_Queue_Size), Success);

      if Success then
         Acceptor.W_Signal.Send ((1 => Socket_Command));
      end if;
   end Give_Back;

   procedure Give_Back
     (Acceptor : in out Acceptor_Type; Socket : Socket_Access)
   is
      Success : Boolean;
   begin
      Acceptor.Box.Add (Socket, Positive'Last, Success);

      if Success then
         Acceptor.W_Signal.Send ((1 => Socket_Command));
      else
         raise Program_Error;
      end if;
   end Give_Back;

   ------------
   -- Length --
   ------------

   function Length (Acceptor : Acceptor_Type) return Natural is
   begin
      return Natural (Sets.Count (Acceptor.Set));
   end Length;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Acceptor            : in out Acceptor_Type;
      Host                : String;
      Port                : Natural;
      Queue_Size          : Positive;
      Timeout             : Duration := Forever;
      First_Timeout       : Duration := Forever;
      Force_Timeout       : Duration := Forever;
      Force_First_Timeout : Duration := Forever;
      Force_Length        : Positive := Positive'Last;
      Close_Length        : Positive := Positive'Last;
      Reuse_Address       : Boolean  := False)
   is
      use type Sets.Socket_Count;

      function Correct_2 (Item : Positive) return Sets.Socket_Count;
      pragma Inline (Correct_2);
      --  Take in account 2 auxiliary sockets, Server and R_Signal

      function New_Socket return Socket_Access;
      pragma Inline (New_Socket);

      ---------------
      -- Correct_2 --
      ---------------

      function Correct_2 (Item : Positive) return Sets.Socket_Count is
      begin
         if Item >= Positive'Last - 2 then
            return Sets.Socket_Count (Item);
         else
            return Sets.Socket_Count (Item + 2);
         end if;
      end Correct_2;

      ----------------
      -- New_Socket --
      ----------------

      function New_Socket return Socket_Access is
      begin
         return new Socket_Type'Class'(Acceptor.Constructor (False));
      end New_Socket;

      use Real_Time;

   begin
      Acceptor.Server := New_Socket;
      Bind
        (Acceptor.Server.all, Host => Host, Port => Port,
         Reuse_Address => Reuse_Address);
      Listen (Acceptor.Server.all, Queue_Size => Queue_Size);

      Acceptor.R_Signal := New_Socket;
      Acceptor.W_Signal := New_Socket;
      Socket_Pair (Acceptor.W_Signal.all, Acceptor.R_Signal.all);
      Set_Timeout (Acceptor.R_Signal.all, 10.0);

      Sets.Reset (Acceptor.Set);
      Sets.Add (Acceptor.Set, Acceptor.Server, Sets.Input);
      Sets.Add (Acceptor.Set, Acceptor.R_Signal, Sets.Input);

      Acceptor.Index               := First_Index;
      Acceptor.Last                := Sets.Count (Acceptor.Set);
      Acceptor.Timeout             := To_Time_Span (Timeout);
      Acceptor.Force_Timeout       := To_Time_Span (Force_Timeout);
      Acceptor.First_Timeout       := To_Time_Span (First_Timeout);
      Acceptor.Force_First_Timeout := To_Time_Span (Force_First_Timeout);
      Acceptor.Back_Queue_Size     := Queue_Size;

      Acceptor.Force_Length := Correct_2 (Force_Length);
      Acceptor.Close_Length := Correct_2 (Close_Length);
   end Listen;

   -------------------
   -- Server_Socket --
   -------------------

   function Server_Socket
     (Acceptor : Acceptor_Type) return Socket_Type'Class is
   begin
      return Acceptor.Server.all;
   end Server_Socket;

   ----------------------------
   -- Set_Socket_Constructor --
   ----------------------------

   procedure Set_Socket_Constructor
     (Acceptor : in out Acceptor_Type; Constructor : Socket_Constructor) is
   begin
      Acceptor.Constructor := Constructor;
   end Set_Socket_Constructor;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Acceptor : in out Acceptor_Type) is
   begin
      if Acceptor.W_Signal /= null then
         Shutdown (Acceptor.W_Signal.all);
         Free (Acceptor.W_Signal);
      end if;
   end Shutdown;

   ----------------
   -- Socket_Box --
   ----------------

   protected body Socket_Box is

      ---------
      -- Add --
      ---------

      procedure Add
        (S : Socket_Access; Max_Size : Positive; Success : out Boolean) is
      begin
         Success := Natural (Buffer.Length) < Max_Size;

         if Success then
            Buffer.Append (S);
         end if;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get
        (S : out Socket_Access) when Ada.Containers.">" (Buffer.Length, 0) is
      begin
         S := Buffer.First_Element;
         Buffer.Delete_First;
      end Get;

      ----------
      -- Size --
      ----------

      function Size return Natural is
      begin
         return Natural (Buffer.Length);
      end Size;

   end Socket_Box;

end AWS.Net.Acceptors;
