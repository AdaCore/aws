------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with Ada.Unchecked_Deallocation;
with GNAT.Regpat;

with AWS.Config;
with AWS.Net.Generic_Sets;
with AWS.Net.Memory;
with AWS.Net.Poll_Events;
with AWS.Net.Std;
with AWS.Utils;

package body AWS.Net.WebSocket.Registry is

   use GNAT;
   use GNAT.Regpat;
   use type Ada.Containers.Count_Type;

   --  Container for URI based registered constructors

   package Constructors is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Factory);

   Factories : Constructors.Map;

   --  Container for Pattern based registered constructors

   type P_Data (Size : Regpat.Program_Size) is record
      Pattern : Regpat.Pattern_Matcher (Size);
      Factory : Registry.Factory;
   end record;

   package Pattern_Constructors is
     new Ada.Containers.Indefinite_Vectors (Positive, P_Data);

   Pattern_Factories : Pattern_Constructors.Vector;

   --  A queue for WebSocket with pending messages to be read

   package WebSocket_Queue is new Utils.Mailbox_G (Object_Class);
   type Queue_Ref is access WebSocket_Queue.Mailbox;

   --  A list of all WebSockets in the registry, this list is used to send or
   --  broadcast messages.

   function Same_WS (Left, Right : Object_Class) return Boolean is
     (Left.Get.Id = Right.Get.Id);
   --  Equality is based on the unique id

   package WebSocket_Map is
     new Ada.Containers.Ordered_Maps (UID, Object_Class, "=" => Same_WS);

   package WebSocket_Set is new Ada.Containers.Ordered_Sets (UID);

   package WebSocket_List is new Ada.Containers.Doubly_Linked_Lists (UID);

   --  The socket set with all sockets to wait for data

   package FD_Set is new Net.Generic_Sets (Object_Class);
   use type FD_Set.Socket_Count;

   task type Watcher with Priority => Config.WebSocket_Priority is
   end Watcher;

   task type Message_Sender with Priority => Config.WebSocket_Priority is
   end Message_Sender;

   type Message_Sender_Set is array (Positive range <>) of Message_Sender;
   type Message_Sender_Set_Ref is access all Message_Sender_Set;

   type Watcher_Ref is access all Watcher;
   --  This task is in charge of watching the WebSocket for incoming messages.
   --  It then places the WebSocket into a job queue to be processed by the
   --  reader tasks.

   task type Message_Reader with Priority => Config.WebSocket_Priority is
   end Message_Reader;
   --  Wait for WebSocket message to be ready, read them and call the Received
   --  callback. The a message has been read, the WebSocket is added back into
   --  the list of watched sockets.

   type Message_Reader_Set is array (Positive range <>) of Message_Reader;
   type Message_Reader_Set_Ref is access all Message_Reader_Set;

   --  Task objects

   Message_Queue   : Queue_Ref;

   Message_Watcher : Watcher_Ref;

   Message_Readers : Message_Reader_Set_Ref;

   Message_Senders : Message_Sender_Set_Ref;

   Shutdown_Signal : Boolean := False with Atomic;

   --  Concurrent access to Set above

   protected DB is

      procedure Initialize;
      --  Initialize the socket set by inserting a signaling socket

      procedure Finalize;
      --  Close signaling socket

      function Create_Set return FD_Set.Socket_Set_Type;
      --  Returns the set of watched WebSockets

      procedure Watch (WebSocket : Object'Class);
      --  Add a new Websocket into the set, release the current FD_Set.Wait
      --  call if any to ensure this new WebSocket will be watched too.

      procedure Remove (WebSocket : Object'Class);
      --  Remove WebSocket from the watched list

      entry Get_Socket (WebSocket : out Object_Class);
      --  Get a WebSocket having some data to be sent

      procedure Release_Socket (WebSocket : Object_Class);
      --  Release a socket retrieved with Get_Socket above, this socket will be
      --  then available again.

      entry Not_Empty;
      --  Returns if the Set is not empty

      procedure Send
        (To           : Recipient;
         Message      : String;
         Except_Peer  : String;
         Timeout      : Duration := Forever;
         Asynchronous : Boolean := False;
         Error        : access procedure (Socket : Object'Class;
                                          Action : out Action_Kind) := null);
      --  Send the given message to all matching WebSockets

      procedure Send
        (Socket       : in out Object'Class;
         Message      : String;
         Is_Binary    : Boolean := False;
         Timeout      : Duration := Forever;
         Asynchronous : Boolean := False);

      procedure Close
        (To          : Recipient;
         Message     : String;
         Except_Peer : String;
         Timeout     : Duration := Forever;
         Error       : Error_Type := Normal_Closure);
      --  Close all matching Webockets

      procedure Close
        (Socket  : in out Object'Class;
         Message : String;
         Timeout : Duration := Forever;
         Error   : Error_Type := Normal_Closure);

      procedure Register (WebSocket : Object_Class; Success : out Boolean) with
        Pre => WebSocket /= No_Object;
      --  Register a new WebSocket

      procedure Unregister (WebSocket : Object'Class);
      --  Unregister a WebSocket

      function Is_Registered (Id : UID) return Boolean;
      --  Returns True if the WebSocket Id is registered and False otherwise

      procedure Signal_Socket;
      --  Send a signal to the wait call

      procedure Shutdown_Signal;
      --  Signal when a shutdown is requested

      procedure Receive
        (WebSocket : Object'Class;
         Data      : out Stream_Element_Array;
         Last      : out Stream_Element_Offset);
      --  Get data from WebSocket

   private
      Sig1, Sig2  : Net.Std.Socket_Type; -- Signaling sockets
      Signal      : Boolean := False;    -- Transient signal, release Not_Emtpy
      S_Signal    : Boolean := False;    -- Shutdown is in progress
      New_Pending : Boolean := False;    -- New pending socket
      Count       : Natural := 0;        -- Not counting signaling socket
      Registered  : WebSocket_Map.Map;   -- Contains all the WebSocket ref
      Sending     : WebSocket_Set.Set;   -- Socket being handed to Sender task

      Pending     : WebSocket_List.List; -- Pending messages to be sent

      Watched     : WebSocket_Set.Set;
      --  All WebSockets are registered into this set to check for incoming
      --  messages. When a message is ready the WebSocket is placed into the
      --  Message_Queue for being handled. When the message has been read
      --  and handled the WebSocket is put back into this set.
   end DB;

   -------------
   -- Watcher --
   -------------

   task body Watcher is
      Count : FD_Set.Socket_Count;
      WS    : Object_Class;
   begin
      loop
         DB.Not_Empty;
         exit when Shutdown_Signal;

         declare
            Set : FD_Set.Socket_Set_Type := DB.Create_Set;
            --  Note that the very first one is a signaling socket used to
            --  release the wait call. This first entry is not a WebSocket and
            --  should be ignored in most code below.
         begin
            --  Wait indefinitely, this call will be released either by an
            --  incoming message in a WebSocket or because the signaling socket
            --  has been used due to a new WebSocket registered. In this later
            --  case no message will be read, but on the next iteration the new
            --  WebSockets will be watched.

            FD_Set.Wait (Set, Duration'Last, Count);

            --  Queue all WebSocket having some data to read, skip the
            --  signaling socket.

            declare
               --  Skip first entry as it is not a websocket
               K : FD_Set.Socket_Count := 2;
            begin
               while K <= FD_Set.Count (Set) loop
                  if FD_Set.Is_Read_Ready (Set, K) then
                     WS := FD_Set.Get_Data (Set, K);
                     DB.Remove (WS.Get);
                     Message_Queue.Add (WS);
                  end if;

                  K := K + 1;
               end loop;
            end;

         exception
            when E : others =>
               --  Send a On_Error message to all registered clients

               for K in 2 .. FD_Set.Count (Set) loop
                  WS := FD_Set.Get_Data (Set, K);
                  WS.Get.State.Errno := Error_Code (Internal_Server_Error);
                  WS.Get.On_Error
                    ("WebSocket Watcher server error, "
                     & Exception_Message (E));
               end loop;
         end;
      end loop;
   end Watcher;

   --------------------
   -- Message_Reader --
   --------------------

   task body Message_Reader is

      procedure Do_Receive
        (WebSocket : Object'Class;
         Data      : out Stream_Element_Array;
         Last      : out Stream_Element_Offset);

      procedure Do_Register (WebSocket : Object'Class);
      procedure Do_Unregister (WebSocket : Object'Class);

      ----------------
      -- Do_Receive --
      ----------------

      procedure Do_Receive
        (WebSocket : Object'Class;
         Data      : out Stream_Element_Array;
         Last      : out Stream_Element_Offset) is
      begin
         DB.Receive (WebSocket, Data, Last);
      end Do_Receive;

      -----------------
      -- Do_Register --
      -----------------

      procedure Do_Register (WebSocket : Object'Class) is
      begin
         DB.Watch (WebSocket);
      end Do_Register;

      -------------------
      -- Do_Unregister --
      -------------------

      procedure Do_Unregister (WebSocket : Object'Class) is
      begin
         DB.Unregister (WebSocket);
      end Do_Unregister;

      function Read_Message is new AWS.Net.WebSocket.Read_Message
         (Receive    => Do_Receive,
          On_Success => Do_Register,
          On_Error   => Do_Unregister);

   begin
      Handle_Message : loop
         declare
            WebSocket : Object_Class;
            Message   : Unbounded_String;
         begin
            Message_Queue.Get (WebSocket);

            --  A WebSocket is null when termination is requested

            exit Handle_Message when WebSocket = No_Object;

            --  A message can be sent in multiple chunks and/or multiple
            --  frames with possibly some control frames in between text or
            --  binary ones. This loop handles those cases.

            loop
               exit when Read_Message (WebSocket.Get, Message);
            end loop;

         exception
            when E : others =>
               declare
                  WS : constant Object_References.Reference_Type :=
                         WebSocket.Get;
               begin
                  Do_Unregister (WS);
                  WebSocket_Exception
                    (WS, Exception_Message (E), Protocol_Error);
                  WS.Shutdown;
               end;
         end;
      end loop Handle_Message;
   end Message_Reader;

   --------
   -- DB --
   --------

   protected body DB is

      ----------
      -- Close --
      ----------

      procedure Close
        (To          : Recipient;
         Message     : String;
         Except_Peer : String;
         Timeout     : Duration := Forever;
         Error       : Error_Type := Normal_Closure)
      is

         procedure Close_To (Position : WebSocket_Map.Cursor);

         -------------
         -- Close_To --
         -------------

         procedure Close_To (Position : WebSocket_Map.Cursor) is
            WebSocket : constant Object_References.Reference_Type :=
                          WebSocket_Map.Element (Position).Get;
         begin
            if (Except_Peer = "" or else WebSocket.Peer_Addr /= Except_Peer)
              and then
                (not To.URI_Set
                 or else GNAT.Regexp.Match (WebSocket.URI, To.URI))
              and then
                (not To.Origin_Set
                 or else GNAT.Regexp.Match (WebSocket.Origin, To.Origin))
            then
               DB.Unregister (WebSocket);
               WebSocket.State.Errno := Error_Code (Error);

               --  If an error occurs, we don't want to fail, shutdown the
               --  socket silently.

               begin
                  WebSocket.Set_Timeout (Timeout);
                  WebSocket.Close (Message, Error);
                  WebSocket.On_Close (Message);
               exception
                  when others =>
                     null;
               end;

               WebSocket.Shutdown;
            end if;
         end Close_To;

         Registered_Before : constant WebSocket_Map.Map := Registered;

      begin
         case To.Kind is
            when K_UID =>
               if Registered.Contains (To.WS_Id) then
                  declare
                     WebSocket : constant Object_References.Reference_Type :=
                                   Registered (To.WS_Id).Get;
                  begin
                     WebSocket.Set_Timeout (Timeout);
                     WebSocket.Close (Message, Error);
                     WebSocket.On_Close (Message);
                  exception
                     when others =>
                        null;
                  end;

               else
                  --  This WebSocket is not registered anymore

                  raise Socket_Error
                    with "WebSocket " & Utils.Image (Natural (To.WS_Id))
                         & " is not registered";
               end if;

            when K_URI =>
               Registered_Before.Iterate (Close_To'Access);
         end case;
      end Close;

      procedure Close
        (Socket  : in out Object'Class;
         Message : String;
         Timeout : Duration := Forever;
         Error   : Error_Type := Normal_Closure)
      is
         W : Object_Class;
      begin
         --  Look for WebSocket into the registered set, unregisted it if
         --  present.

         if Registered.Contains (Socket.Id) then
            W := Registered (Socket.Id);
            Unregister (W.Get);
         end if;

         Socket.State.Errno := Error_Code (Error);
         Socket.Set_Timeout (Timeout);
         Socket.Close (Message, Error);
         Socket.On_Close (Message);
         Socket.Shutdown;
      end Close;

      ----------------
      -- Create_Set --
      ----------------

      function Create_Set return FD_Set.Socket_Set_Type is
      begin
         return Result : FD_Set.Socket_Set_Type do
            --  Add the signaling socket

            FD_Set.Add (Result, Sig1, No_Object, FD_Set.Input);

            --  Add watched sockets

            for Id of Watched loop
               declare
                  WS : constant Object_Class := Registered (Id);
               begin
                  FD_Set.Add (Result, WS.Get.Socket.all, WS, FD_Set.Input);
               end;
            end loop;
         end return;
      end Create_Set;

      --------------
      -- Finalize --
      --------------

      procedure Finalize is

         procedure On_Close (Position : WebSocket_Map.Cursor);

         --------------
         -- On_Close --
         --------------

         procedure On_Close (Position : WebSocket_Map.Cursor) is
            WebSocket : constant Object_References.Reference_Type :=
                          WebSocket_Map.Element (Position).Get;
         begin
            WebSocket.State.Errno := Error_Code (Going_Away);

            --  We do not want to block if the peer is not responding, just
            --  allow 10 seconds for the close message to be accepted.
            --  In any case, we do not want to raise an exception. If an
            --  error occurs just close the socket silently.

            begin
               WebSocket.Set_Timeout (10.0);
               WebSocket.On_Close ("AWS server going down");
            exception
               when others =>
                  null;
            end;

            WebSocket.Shutdown;
         end On_Close;

      begin
         Net.Std.Shutdown (Sig1);
         Net.Std.Shutdown (Sig2);

         --  Finally send a On_Close message to all registered WebSocket

         Registered.Iterate (On_Close'Access);
         Registered.Clear;
      end Finalize;

      ----------------
      -- Get_Socket --
      ----------------

      entry Get_Socket (WebSocket : out Object_Class)
        when New_Pending or else S_Signal is
      begin
         WebSocket := No_Object;

         --  Shutdown requested, just return now

         if S_Signal then
            return;
         end if;

         --  No pending message on the queue, this can happen because
         --  New_Pending is set when giving back a WebSocket into the
         --  registry. See Release_Socket.

         if Pending.Length = 0 then
            New_Pending := False;
            requeue Get_Socket;
         end if;

         --  Look for a socket not yet being handled

         declare
            use type WebSocket_List.Cursor;
            Pos : WebSocket_List.Cursor := Pending.First;
            Id  : UID;
            WS  : Object_Class;
         begin
            while Pos /= WebSocket_List.No_Element loop
               Id := Pending (Pos);

               --  Check if this socket is not yet being used by a sender task

               if not Sending.Contains (Id) then
                  WS := Registered (Id);

                  --  Check that some messages are to be sent. This is needed
                  --  as some messages could have been dropped if the list was
                  --  too long to avoid congestion.

                  if WS.Get.Messages.Length > 0 then
                     Pending.Delete (Pos);
                     WebSocket := WS;
                     Sending.Insert (Id);
                     return;
                  end if;
               end if;

               Pos := WebSocket_List.Next (Pos);
            end loop;

            --  Finally no more socket found to be handled, wait for new to
            --  arrive.

            New_Pending := False;
            requeue Get_Socket;
         end;
      end Get_Socket;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         --  Create a signaling socket that will be used to exit from the
         --  infinite wait when a new WebSocket arrives.
         Net.Std.Socket_Pair (Sig1, Sig2);
      end Initialize;

      -------------------
      -- Is_Registered --
      -------------------

      function Is_Registered (Id : UID) return Boolean is
      begin
         return Registered.Contains (Id);
      end Is_Registered;

      ---------------
      -- Not_Empty --
      ---------------

      entry Not_Empty when Count > 0 or else Signal or else S_Signal is
      begin
         --  If shutdown is in process, return now

         if S_Signal then
            return;
         end if;

         --  It was a signal, consume the one by sent

         if Signal then
            Signal := False;

            declare
               Data : Stream_Element_Array (1 .. 1);
               Last : Stream_Element_Offset;
            begin
               AWS.Net.Std.Receive (Sig1, Data, Last);
            end;
         end if;
      end Not_Empty;

      -------------
      -- Receive --
      -------------

      procedure Receive
        (WebSocket : Object'Class;
         Data      : out Stream_Element_Array;
         Last      : out Stream_Element_Offset) is
      begin
         WebSocket.Receive (Data, Last);
      end Receive;

      --------------
      -- Register --
      --------------

      procedure Register (WebSocket : Object_Class; Success : out Boolean) is
      begin
         --  Check if maximum number of WebSocket has been reached

         if Natural (Registered.Length) = Config.Max_WebSocket then
            --  Let's try to close a WebSocket for which the activity has
            --  timed out.

            declare
               use type Calendar.Time;
               Timeout : constant Calendar.Time :=
                           Calendar.Clock - Config.WebSocket_Timeout;
               W       : Object_Class;
            begin
               for WS of Registered loop
                  if WS.Get.State.Last_Activity < Timeout then
                     W := WS;
                     exit;
                  end if;
               end loop;

               --  If no WebSocket can be closed

               if W = No_Object then
                  Success := False;
                  return;

               else
                  Close
                    (Socket  => W.Get,
                     Message => "activity timeout reached",
                     Timeout => 1.0,
                     Error   => Abnormal_Closure);
               end if;
            end;
         end if;

         Registered.Insert (WebSocket.Get.Id, WebSocket);
         Success := True;
      end Register;

      --------------------
      -- Release_Socket --
      --------------------

      procedure Release_Socket (WebSocket : Object_Class) is
      begin
         Sending.Exclude (WebSocket.Get.Id);
         New_Pending := True;
      end Release_Socket;

      ------------
      -- Remove --
      ------------

      procedure Remove (WebSocket : Object'Class) is
         CID : WebSocket_Set.Cursor := Watched.Find (WebSocket.Id);
      begin
         if WebSocket_Set.Has_Element (CID) then
            Watched.Delete (CID);
            Count := Count - 1;
         end if;
      end Remove;

      ----------
      -- Send --
      ----------

      procedure Send
        (To           : Recipient;
         Message      : String;
         Except_Peer  : String;
         Timeout      : Duration := Forever;
         Asynchronous : Boolean := False;
         Error        : access procedure (Socket : Object'Class;
                                          Action : out Action_Kind) := null)
      is
         type Object_Set is array (1 .. Natural (Registered.Length)) of
           Object_Class;

         procedure Initialize_Recipients (Position : WebSocket_Map.Cursor);
         --  Count recipients and initialize the message's raw data

         procedure Send_To_Recipients (Recipients : Object_Set);
         --  Send message to all recipients

         Recipients : Object_Set;
         Last       : Natural := 0;

         ---------------------------
         -- Initialize_Recipients --
         ---------------------------

         procedure Initialize_Recipients (Position : WebSocket_Map.Cursor) is
            WebSocket : constant Object_Class :=
                          WebSocket_Map.Element (Position);
            WS        : constant Object_References.Reference_Type :=
                          WebSocket.Get;
         begin
            if (Except_Peer = "" or else WS.Peer_Addr /= Except_Peer)
              and then
                (not To.URI_Set
                 or else GNAT.Regexp.Match (WS.URI, To.URI))
              and then
                (not To.Origin_Set
                 or else GNAT.Regexp.Match (WS.Origin, To.Origin))
            then
               --  Send all data to a memory socket. This is a special
               --  circuitry where all sent data are actually stored into
               --  a buffer. This is necessary to be able to get raw data
               --  depending on the protocol.
               --
               --  ??? for supporting a large set of WebSocket it would be
               --  good to share the memory buffer. There is actually one
               --  for each WebSocket format.

               WS.Mem_Sock := new Memory.Socket_Type;

               WS.In_Mem := True;
               WS.Send (Message);
               WS.In_Mem := False;

               --  Not that it is not possible to honor the synchronous
               --  sending if the socket is already being handled by a
               --  send task (Asynchronously).

               if Asynchronous or else Sending.Contains (WS.Id) then
                  declare
                     M : constant Message_Data := (WS.Mem_Sock, Timeout);
                  begin
                     WS.Messages.Append (M);
                     WS.Mem_Sock := null;
                     Pending.Append (WS.Id);
                     New_Pending := True;
                  end;

               else
                  Last := Last + 1;
                  WS.Set_Timeout (Timeout);
                  Recipients (Last) := WebSocket;
               end if;
            end if;
         end Initialize_Recipients;

         ------------------------
         -- Send_To_Recipients --
         ------------------------

         procedure Send_To_Recipients (Recipients : Object_Set) is
            Wait_Events : constant Wait_Event_Set :=
                            (Input => False, Output => True);
            Set         : Poll_Events.Set (Recipients'Length);
            Socks       : Object_Set := Recipients;
            Sock_Index  : Positive;
            Count       : Natural;
            Pending     : Stream_Element_Count;
         begin
            --  Register the sockets to be handled

            for S of Socks loop
               Set.Add (S.Get.Get_FD, Wait_Events);
            end loop;

            --  Send actual data to the WebSocket depending on their status

            Send_Message : loop
               Set.Wait (Timeout, Count);

               --  Timeout reached, some sockets are not responding

               if Count = 0 then
                  for K in 1 .. Set.Length loop
                     declare
                        W : constant Object_References.Reference_Type :=
                              Socks (K).Get;
                        A : Action_Kind := None;
                     begin
                        if Error = null then
                           DB.Unregister (W);

                        else
                           Error (W, A);

                           case A is
                              when Close =>
                                 DB.Unregister (W);
                              when None =>
                                 null;
                           end case;
                        end if;
                     end;
                  end loop;

                  exit Send_Message;
               end if;

               Sock_Index := 1;

               for K in 1 .. Count loop
                  Set.Next (Sock_Index);

                  declare
                     WS         : constant Object_References.Reference_Type :=
                                    Socks (Sock_Index).Get;
                     Chunk_Size : Stream_Element_Offset := WS.Output_Space;
                  begin
                     if Chunk_Size = -1 then
                        Chunk_Size := 100 * 1_024;
                     end if;

                     Pending := WS.Mem_Sock.Pending;

                     Chunk_Size := Stream_Element_Offset'Min
                       (Chunk_Size, Pending);

                     Read_Send : declare
                        --  ??? Might blow up if Chunk_Size > 2Mb
                        Data : Stream_Element_Array (1 .. Chunk_Size);
                        Last : Stream_Element_Offset;
                     begin
                        --  ??? Useless copy of the data in memory. Would be
                        --  nice to remove this.
                        WS.Mem_Sock.Receive (Data, Last);
                        pragma Assert (Last = Data'Last);

                        if Last /= 0 then
                           WS.Set_Timeout (Timeout);
                           WS.Send (Data, Last);
                        end if;

                        Pending := Pending - Last;
                     exception
                        when E : others =>
                           Unregister (WS);
                           WebSocket_Exception
                             (WS, Exception_Message (E), Protocol_Error);

                           WS.Close (Exception_Message (E), Going_Away);

                           --  No more data to send from this socket
                           Pending := 0;

                           Socks (Sock_Index) := No_Object;
                     end Read_Send;
                  end;

                  if Pending = 0 then
                     --  No more data for this socket, first free memory

                     if Socks (Sock_Index) /= No_Object then
                        Free (Socks (Sock_Index).Get.Mem_Sock);
                     end if;

                     --  Then the Set.Remove (on the socket set) move the last
                     --  socket in the set to the location of the removed
                     --  one. Do the same for the local data to keep data
                     --  consistency.
                     --
                     --  Note that in this case we do not want to increment the
                     --  socket index. The new loop will check the socket at
                     --  the same position which is now the previous last in
                     --  the set.

                     if Sock_Index /= Set.Length then
                        Socks (Sock_Index) := Socks (Set.Length);
                     end if;

                     Set.Remove (Sock_Index);

                  else
                     --  In this case, and only in this case we move to next
                     --  socket position for next iteration.

                     Sock_Index := Sock_Index + 1;
                  end if;
               end loop;

               exit Send_Message when Set.Length = 0;
            end loop Send_Message;
         end Send_To_Recipients;

      begin
         case To.Kind is
            when K_UID =>
               if Registered.Contains (To.WS_Id) then
                  declare
                     WebSocket : constant Object_References.Reference_Type :=
                                   Registered (To.WS_Id).Get;
                  begin
                     WebSocket.Set_Timeout (Timeout);
                     WebSocket.Send (Message);
                  exception
                     when E : others =>
                        Unregister (WebSocket);
                        WebSocket_Exception
                          (WebSocket, Exception_Message (E), Protocol_Error);

                        WebSocket.Close (Exception_Message (E), Going_Away);
                  end;

               else
                  --  This WebSocket is not registered anymore

                  raise Socket_Error
                    with "WebSocket " & Utils.Image (Natural (To.WS_Id))
                         & " is not registered";
               end if;

            when K_URI =>
               Registered.Iterate (Initialize_Recipients'Access);

               if Last > 0 then
                  Send_To_Recipients (Recipients (1 .. Last));
               end if;
         end case;
      end Send;

      procedure Send
        (Socket       : in out Object'Class;
         Message      : String;
         Is_Binary    : Boolean := False;
         Timeout      : Duration := Forever;
         Asynchronous : Boolean := False) is
      begin
         if Asynchronous then
            Socket.In_Mem := True;
            Socket.Send (Message, Is_Binary);
            Socket.In_Mem := False;

            declare
               M : constant Message_Data := (Socket.Mem_Sock, Timeout);
            begin
               Socket.Messages.Append (M);
               Socket.Mem_Sock := null;
               Pending.Append (Socket.Id);
               New_Pending := True;
            end;

         else
            Socket.Set_Timeout (Timeout);
            Socket.Send (Message, Is_Binary);
         end if;
      end Send;

      ---------------------
      -- Shutdown_Signal --
      ---------------------

      procedure Shutdown_Signal is
      begin
         S_Signal := True;
         Signal_Socket;
      end Shutdown_Signal;

      -------------------
      -- Signal_Socket --
      -------------------

      procedure Signal_Socket is
      begin
         --  If a signal is pending no need to signal again the socket

         if not Signal then
            Net.Send (Sig2, Stream_Element_Array'(1 => 0));

            --  Also activate the signal to release Not_Empty for proper
            --  termination when there is no remaining socket.

            Signal := True;
         end if;
      end Signal_Socket;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (WebSocket : Object'Class) is
         Id : constant UID := WebSocket.Id;
      begin
         Registered.Exclude (Id);
         Sending.Exclude (Id);

         Remove (WebSocket);
         Signal_Socket;
      end Unregister;

      -----------
      -- Watch --
      -----------

      procedure Watch (WebSocket : Object'Class) is
      begin
         if Is_Registered (WebSocket.Id)
           and then not Watched.Contains (WebSocket.Id)
         then
            Watched.Insert (WebSocket.Id);
            Count := Count + 1;
            Signal_Socket;
         end if;

      exception
         when others =>
            Unregister (WebSocket);
            raise;
      end Watch;

   end DB;

   -----------
   -- Close --
   -----------

   procedure Close
     (To          : Recipient;
      Message     : String;
      Except_Peer : String := "";
      Timeout     : Duration := Forever;
      Error       : Error_Type := Normal_Closure) is
   begin
      DB.Close (To, Message, Except_Peer, Timeout, Error);
   exception
      when others =>
         --  Should never fails even if the WebSocket is closed by peer
         null;
   end Close;

   procedure Close
     (Socket  : in out Object'Class;
      Message : String;
      Timeout : Duration := Forever;
      Error   : Error_Type := Normal_Closure) is
   begin
      DB.Close (Socket, Message, Timeout, Error);
   exception
      when others =>
         --  Should never fails even if the WebSocket is closed by peer
         null;
   end Close;

   -----------------
   -- Constructor --
   -----------------

   function Constructor (URI : String) return Registry.Factory is
      Position : constant Constructors.Cursor := Factories.Find (URI);

   begin
      if Constructors.Has_Element (Position) then
         return Constructors.Element (Position);

      else
         for Data of Pattern_Factories loop
            declare
               Count   : constant Natural := Paren_Count (Data.Pattern);
               Matches : Match_Array (0 .. Count);
            begin
               Match (Data.Pattern, URI, Matches);

               if Matches (0) /= No_Match then
                  return Data.Factory;
               end if;
            end;
         end loop;
      end if;

      return Create'Access;
   end Constructor;

   ------------
   -- Create --
   ------------

   function Create (URI : String; Origin : String := "") return Recipient is
      Result : Recipient (K_URI);
   begin
      if URI /= "" then
         Result.URI_Set := True;
         Result.URI     := GNAT.Regexp.Compile (URI);
      end if;

      if Origin /= "" then
         Result.Origin_Set := True;
         Result.Origin     := GNAT.Regexp.Compile (Origin);
      end if;

      return Result;
   end Create;

   function Create (Id : UID) return Recipient is
   begin
      return Result : Recipient (K_UID) do
         Result.WS_Id := Id;
      end return;
   end Create;

   -------------------
   -- Is_Registered --
   -------------------

   function Is_Registered (Id : UID) return Boolean is
   begin
      return DB.Is_Registered (Id);
   end Is_Registered;

   --------------------
   -- Message_Sender --
   --------------------

   task body Message_Sender is

      WSR : Object_Class;
      WS  : access Object'Class;

      procedure Send (Message : Message_Data);

      ----------
      -- Send --
      ----------

      procedure Send (Message : Message_Data) is
         Chunk_Size : Stream_Element_Offset := WS.Output_Space;
         Pending    : Stream_Element_Count;
      begin
         if Chunk_Size = -1 then
            Chunk_Size := 100 * 1_024;
         end if;

         loop
            Pending := Message.Mem_Sock.Pending;
            exit when Pending = 0;

            Chunk_Size := Stream_Element_Offset'Min (Chunk_Size, Pending);

            Read_Send : declare
               Data : Stream_Element_Array (1 .. Chunk_Size);
               Last : Stream_Element_Offset;
            begin
               Message.Mem_Sock.Receive (Data, Last);
               pragma Assert (Last = Data'Last);

               WS.Send (Data, Last);

               Pending := Pending - Chunk_Size;
            exception
               when E : others =>
                  DB.Unregister (WS.all);
                  WebSocket_Exception
                    (WS.all, Exception_Message (E), Protocol_Error);
                  --  No more data to send from this socket
                  Pending := 0;
            end Read_Send;
         end loop;
      end Send;

   begin
      loop
         DB.Get_Socket (WSR);
         WS := WSR.Get.Element;

         exit when Shutdown_Signal;

         --  This WebSocket has a message to be sent

         --  First let's remove too old messages

         while Positive (WS.Messages.Length) >
           Config.WebSocket_Send_Message_Queue_Size
         loop
            WS.Messages.Delete_First;
         end loop;

         --  Then send the oldest message on the list

         declare
            Message : constant Message_Data := WS.Messages.First_Element;
         begin
            Send (Message);
            WS.Messages.Delete_First;

            DB.Release_Socket (WSR);
         end;
      end loop;
   end Message_Sender;

   --------------
   -- Register --
   --------------

   procedure Register (URI : String; Factory : Registry.Factory) is
   begin
      Factories.Insert (URI, Factory);
   end Register;

   function Register (WebSocket : Object'Class) return Object_Class is
      WS      : Object_Class;
      Success : Boolean;
   begin
      WS.Set (WebSocket);
      DB.Register (WS, Success);

      if not Success then
         WS := No_Object;
      end if;

      return WS;
   end Register;

   ----------------------
   -- Register_Pattern --
   ----------------------

   procedure Register_Pattern
     (Pattern : String;
      Factory : Registry.Factory)
   is
      M : constant Regpat.Pattern_Matcher :=
            Regpat.Compile (Pattern, GNAT.Regpat.Case_Insensitive);
   begin
      Pattern_Factories.Append (P_Data'(M.Size, M, Factory));
   end Register_Pattern;

   ----------
   -- Send --
   ----------

   procedure Send
     (To           : Recipient;
      Message      : Unbounded_String;
      Except_Peer  : String := "";
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null)
   is
      use Ada.Strings.Unbounded.Aux;
      S  : Big_String_Access;
      L  : Natural;
   begin
      Get_String (Message, S, L);
      DB.Send (To, S (1 .. L), Except_Peer, Timeout, Asynchronous,  Error);
   exception
      when others =>
         --  Should never fails even if the WebSocket is closed by peer
         null;
   end Send;

   procedure Send
     (To           : Recipient;
      Message      : String;
      Except_Peer  : String := "";
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null) is
   begin
      DB.Send (To, Message, Except_Peer, Timeout, Asynchronous, Error);
   end Send;

   procedure Send
     (To           : Recipient;
      Message      : Unbounded_String;
      Request      : AWS.Status.Data;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null)
   is
      use Ada.Strings.Unbounded.Aux;
      S  : Big_String_Access;
      L  : Natural;
   begin
      Get_String (Message, S, L);
      Send (To, S (1 .. L), Request, Timeout, Asynchronous, Error);
   end Send;

   procedure Send
     (To           : Recipient;
      Message      : String;
      Request      : AWS.Status.Data;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null) is
   begin
      Send
        (To, Message,
         Except_Peer  => AWS.Status.Socket (Request).Peer_Addr,
         Timeout      => Timeout,
         Asynchronous => Asynchronous,
         Error        => Error);
   end Send;

   procedure Send
     (Socket       : in out Object'Class;
      Message      : String;
      Is_Binary    : Boolean := False;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False) is
   begin
      DB.Send (Socket, Message, Is_Binary, Timeout, Asynchronous);
   end Send;

   procedure Send
     (Socket       : in out Object'Class;
      Message      : Unbounded_String;
      Is_Binary    : Boolean := False;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False)
   is
      use Ada.Strings.Unbounded.Aux;
      S  : Big_String_Access;
      L  : Natural;
   begin
      Get_String (Message, S, L);
      DB.Send (Socket, S (1 .. L), Is_Binary, Timeout, Asynchronous);
   end Send;

   procedure Send
     (Socket       : in out Object'Class;
      Message      : Stream_Element_Array;
      Is_Binary    : Boolean := True;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False)
   is
      A : String (Natural (Message'First) .. Natural (Message'Last))
         with Import;
      for A'Address use Message'Address;
      pragma Compile_Time_Error
         (Character'Size /= Stream_Element'Size,
          "A character size is not the size of a byte");
   begin
      Send (Socket, A, Is_Binary, Timeout, Asynchronous);
   end Send;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
      procedure Unchecked_Free is new
        Unchecked_Deallocation (Watcher, Watcher_Ref);
      procedure Unchecked_Free is new
        Unchecked_Deallocation (Message_Reader_Set, Message_Reader_Set_Ref);
      procedure Unchecked_Free is new
        Unchecked_Deallocation (Message_Sender_Set, Message_Sender_Set_Ref);
      procedure Unchecked_Free is new
        Unchecked_Deallocation (WebSocket_Queue.Mailbox, Queue_Ref);
   begin
      --  Check if a shutdown if not already in progress or if the servers have
      --  not been initialized.

      if Shutdown_Signal
        or else (Message_Watcher = null and then Message_Readers = null)
      then
         return;
      end if;

      --  First shutdown the watcher

      Shutdown_Signal := True;
      DB.Shutdown_Signal;

      --  Wait for proper termination to be able to free the task object

      while not Message_Watcher'Terminated loop
         delay 0.5;
      end loop;

      --  Now shutdown all the message readers

      for K in Message_Readers'Range loop
         Message_Queue.Add (No_Object);
      end loop;

      for K in Message_Readers'Range loop
         while not Message_Readers (K)'Terminated loop
            delay 0.5;
         end loop;
      end loop;

      for K in Message_Senders'Range loop
         while not Message_Senders (K)'Terminated loop
            delay 0.5;
         end loop;
      end loop;

      --  Now we can deallocate the task objects

      Unchecked_Free (Message_Readers);
      Unchecked_Free (Message_Senders);
      Unchecked_Free (Message_Watcher);
      Unchecked_Free (Message_Queue);

      DB.Finalize;
   end Shutdown;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      DB.Initialize;
      Message_Queue :=
        new WebSocket_Queue.Mailbox (Config.WebSocket_Message_Queue_Size);
      Message_Watcher := new Watcher;
      Message_Readers :=
        new Message_Reader_Set (1 .. Config.Max_WebSocket_Handler);
      Message_Senders :=
        new Message_Sender_Set (1 .. Config.Max_WebSocket_Handler);
   end Start;

   -----------
   -- Watch --
   -----------

   procedure Watch (WebSocket : in out Object_Class) is
      WS : constant Object_References.Reference_Type := WebSocket.Get;
   begin
      --  Send a Connection_Open message

      WS.State.Kind := Connection_Open;
      WS.On_Open ("AWS WebSocket connection open");

      DB.Watch (WS);
   end Watch;

end AWS.Net.WebSocket.Registry;
