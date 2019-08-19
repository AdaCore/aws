------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                   Copyright (C) 2005-2019, AdaCore                       --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

package body AWS.Net.Acceptors is

   Signal_Index   : constant := 1;
   First_Index    : constant := 2;

   Socket_Command : constant := 1;

   procedure Shutdown_Internal (Acceptor : in out Acceptor_Type);

   -------------------
   -- Add_Listening --
   -------------------

   procedure Add_Listening
     (Acceptor      : in out Acceptor_Type;
      Host          : String;
      Port          : Natural;
      Family        : Family_Type := Family_Unspec;
      Reuse_Address : Boolean     := False;
      IPv6_Only     : Boolean     := False)
   is
      Server : constant Socket_Access :=
                 new Socket_Type'Class'(Acceptor.Constructor (False));
   begin
      Server.Bind
        (Host => Host, Port => Port, Family => Family, IPv6_Only => IPv6_Only,
         Reuse_Address => Reuse_Address);
      Server.Listen (Queue_Size => Acceptor.Back_Queue_Size);

      Give_Back (Acceptor, Server);

      Acceptor.Servers.Add (Server);
   end Add_Listening;

   ---------
   -- Get --
   ---------

   procedure Get
     (Acceptor : in out Acceptor_Type;
      Socket   : out    Socket_Access;
      To_Close : out    Socket_List;
      On_Error : access procedure (E : Exception_Occurrence) := null)
   is
      use type Sets.Socket_Count;

      function Accept_Listening return Boolean;

      procedure Add_Sockets;
      --  Add sockets to the acceptor either from Accept_Socket or from
      --  Give_Back.

      procedure Shutdown with No_Return;

      procedure Finalize;

      Too_Many_FD  : Boolean := False;
      Ready, Error : Boolean;

      ----------------------
      -- Accept_Listening --
      ----------------------

      function Accept_Listening return Boolean is
         Server : constant Socket_Type'Class :=
                    Sets.Get_Socket (Acceptor.Set, Acceptor.Index);
      begin
         if not Server.Is_Listening then
            return False;
         end if;

         if Error then
            Server.Raise_Socket_Error ("Accepting socket error");

         elsif Ready then
            declare
               New_Socket : Socket_Type'Class := Acceptor.Constructor (False);
            begin
               --  We could not accept SSL socket because SSL handshake could
               --  take a long time inside Accept_Socket. We would make socket
               --  SSL later outside acceptor if necessary.

               Server.Accept_Socket (New_Socket);

               Sets.Add
                 (Acceptor.Set,
                  New_Socket,
                  Data => (Time => Real_Time.Clock, First => True),
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

         return True;
      end Accept_Listening;

      -----------------
      -- Add_Sockets --
      -----------------

      procedure Add_Sockets is
      begin
         Sets.Is_Read_Ready (Acceptor.Set, Signal_Index, Ready, Error);

         if Error then
            Shutdown;

         elsif Ready then
            declare
               Socket : Socket_Access;
               Bytes  : Stream_Element_Array (1 .. 16);
               Last   : Stream_Element_Offset;
            begin
               --  Read bytes from signalling socket and take sockets from
               --  mailbox.

               begin
                  Acceptor.R_Signal.Receive (Bytes, Last);
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
                        Data  => (Time => Real_Time.Clock, First => False),
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
      -- Finalize --
      --------------

      procedure Finalize is
      begin
         Acceptor.Semaphore.Release;
      end Finalize;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         Shutdown_Internal (Acceptor);
         raise Socket_Error;
      end Shutdown;

      First        : constant Boolean := True;
      Timeout      : array (Boolean) of Real_Time.Time_Span;
      Oldest_Idx   : Sets.Socket_Count;
      Finalizer    : AWS.Utils.Finalizer (Finalize'Access) with Unreferenced;

      Wait_Timeout : Real_Time.Time_Span;

   begin
      Acceptor.Semaphore.Seize;

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

         Read_Ready : while Acceptor.Index <= Acceptor.Last loop
            Sets.Is_Read_Ready (Acceptor.Set, Acceptor.Index, Ready, Error);

            if Accept_Listening then
               Acceptor.Index := Acceptor.Index + 1;

            elsif Error or else Ready then
               Sets.Remove_Socket (Acceptor.Set, Acceptor.Index, Socket);

               Acceptor.Last := Acceptor.Last - 1;

               if Error then
                  To_Close.Append (Socket);

               elsif Ready then
                  return;
               else
                  raise Program_Error;
               end if;

            else
               --  Check for timeout

               declare
                  use Real_Time;
                  Data : constant Socket_Data_Type :=
                           Sets.Get_Data (Acceptor.Set, Acceptor.Index);
                  Diff : constant Real_Time.Time_Span :=
                           Timeout (Data.First) - (Clock - Data.Time);
               begin
                  if Diff <= Time_Span_Zero then
                     Sets.Remove_Socket (Acceptor.Set, Acceptor.Index, Socket);
                     Acceptor.Last := Acceptor.Last - 1;
                     To_Close.Append (Socket);

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
            To_Close.Append (Socket);
         end if;

         declare
            use type Containers.Count_Type;
            Count : Sets.Socket_Count;
            S     : Socket_Access;
         begin
            loop
               if To_Close.Length = 0 then
                  Sets.Wait
                    (Acceptor.Set, Real_Time.To_Duration (Wait_Timeout));
                  exit;
               end if;

               --  We could not wait too long when we have sockets to close.
               --  Otherwise client could not understand that server closed
               --  its keep-alive socket on inactivity timeout.

               Sets.Wait (Acceptor.Set, 0.0, Count);
               exit when Count > 0;

               S := To_Close.First_Element;
               To_Close.Delete_First;
               S.Shutdown;
               Free (S); -- Don't use S.Free, it does not deallocate S
            end loop;

            --  Save Acceptor.Last to do not try to get status of new arrived
            --  sockets until it wouldn't in the Wait call.

            Acceptor.Last := Sets.Count (Acceptor.Set);

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

   procedure Get
     (Acceptor : in out Acceptor_Type;
      Socket   : out    Socket_Access;
      On_Error : access procedure (E : Exception_Occurrence) := null)
   is
      To_Close : Socket_List;
   begin
      Get (Acceptor, Socket, To_Close, On_Error);
      Shutdown_And_Free (To_Close);
   exception
      when others =>
         Shutdown_And_Free (To_Close);
         raise;
   end Get;

   ---------------
   -- Give_Back --
   ---------------

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket   : not null access Socket_Type'Class;
      Success  : out Boolean) is
   begin
      Acceptor.Box.Add (Socket, Acceptor.Back_Queue_Size, Success);
   end Give_Back;

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket   : not null access Socket_Type'Class)
   is
      Success : Boolean;
   begin
      Acceptor.Box.Add (Socket, Positive'Last, Success);

      if not Success then
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
      Family              : Family_Type := Family_Unspec;
      Timeout             : Duration    := Forever;
      First_Timeout       : Duration    := Forever;
      Force_Timeout       : Duration    := Forever;
      Force_First_Timeout : Duration    := Forever;
      Force_Length        : Positive    := Positive'Last;
      Close_Length        : Positive    := Positive'Last;
      Reuse_Address       : Boolean     := False;
      IPv6_Only           : Boolean     := False)
   is
      use type Sets.Socket_Count;

      function Correct_2 (Item : Positive) return Sets.Socket_Count
        with Inline;
      --  Take in account 2 auxiliary sockets, Server and R_Signal

      function New_Socket return Socket_Access with Inline;

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

      Server : constant Socket_Access := New_Socket;

   begin
      if Sets.Count (Acceptor.Set) > 0 then
         raise Constraint_Error with "Acceptor is not clear";
      end if;

      Server.Bind
        (Host => Host, Port => Port, Family => Family, IPv6_Only => IPv6_Only,
         Reuse_Address => Reuse_Address);
      Server.Listen (Queue_Size => Queue_Size);

      Acceptor.Servers.Add (Server);

      Acceptor.R_Signal := New_Socket;
      Acceptor.W_Signal := New_Socket;
      Acceptor.W_Signal.Socket_Pair (Acceptor.R_Signal.all);
      Acceptor.R_Signal.Set_Timeout (10.0);

      Sets.Add (Acceptor.Set, Acceptor.R_Signal, Sets.Input);
      Sets.Add (Acceptor.Set, Server, Sets.Input);

      Acceptor.Last                := Sets.Count (Acceptor.Set);
      Acceptor.Index               := Acceptor.Last + 1;
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
      return Acceptor.Servers.Get.First_Element.all;
   end Server_Socket;

   --------------------
   -- Server_Sockets --
   --------------------

   function Server_Sockets (Acceptor : Acceptor_Type) return Socket_List is
   begin
      return Acceptor.Servers.Get;
   end Server_Sockets;

   ------------------------
   -- Server_Sockets_Set --
   ------------------------

   protected body Server_Sockets_Set is

      ---------
      -- Add --
      ---------

      procedure Add (S : not null access Socket_Type'Class) is
      begin
         Sockets.Append (Socket_Access (S));
      end Add;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Sockets.Clear;
      end Clear;

      ---------
      -- Get --
      ---------

      function Get return Socket_List is
      begin
         return Sockets;
      end Get;

      ----------------
      -- Wait_Empty --
      ----------------

      entry Wait_Empty when Sockets.Is_Empty is
      begin
         null;
      end Wait_Empty;

   end Server_Sockets_Set;

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
         Acceptor.W_Signal.Shutdown;
         Free (Acceptor.W_Signal);
      end if;

      Acceptor.Box.Clear;

      --  Loop trying to shutdown directly or over Get routine

      for J in 1 .. 4 loop
         --  If acceptor is not inside Get routine, we should make shutdown
         --  directly.

         select
            Acceptor.Semaphore.Seize;
            Shutdown_Internal (Acceptor);
            Acceptor.Semaphore.Release;
            return;
         or
            delay 0.0;
         end select;

         --  Close of signal socket could be detected inside Get routine and
         --  shoutdown would be processed over there.

         select
            Acceptor.Servers.Wait_Empty;
            return;
         or
            delay 1.0;
         end select;
      end loop;

      raise Program_Error with
        "Could not shutdown acceptor " & Sets.Count (Acceptor.Set)'Img
        & Acceptor.Last'Img & Acceptor.Index'Img;
   end Shutdown;

   -----------------------
   -- Shutdown_And_Free --
   -----------------------

   procedure Shutdown_And_Free (Set : Socket_List) is
      S : Socket_Access;
   begin
      for C in Set.Iterate loop
         S := Socket_Lists.Element (C);
         S.Shutdown;
         Free (S); -- Don't use S.Free, it does not deallocate S
      end loop;
   end Shutdown_And_Free;

   -----------------------
   -- Shutdown_Internal --
   -----------------------

   procedure Shutdown_Internal (Acceptor : in out Acceptor_Type) is
      use type Sets.Socket_Count;
      Socket : Socket_Access;
   begin
      while Sets.Count (Acceptor.Set) > 0 loop
         Sets.Remove_Socket (Acceptor.Set, 1, Socket);
         Socket.Shutdown;

         --  We can free other sockets, because it is not used anywhere else
         --  when it is in socket set.

         Free (Socket);
      end loop;

      Acceptor.Servers.Clear;
   end Shutdown_Internal;

   ----------------
   -- Socket_Box --
   ----------------

   protected body Socket_Box is

      ---------
      -- Add --
      ---------

      procedure Add
        (S        : not null access Socket_Type'Class;
         Max_Size : Positive;
         Success  : out Boolean) is
      begin
         Success := Natural (Buffer.Length) < Max_Size
                      and then Acceptor.W_Signal /= null;

         if Success then
            Buffer.Append (Socket_Access (S));
            Acceptor.W_Signal.Send ((1 => Socket_Command));
         end if;
      end Add;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Shutdown_And_Free (Buffer);
      end Clear;

      ---------
      -- Get --
      ---------

      entry Get (S : out Socket_Access) when Size > 0 is
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
