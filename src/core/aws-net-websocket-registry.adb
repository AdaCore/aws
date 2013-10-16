------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Deallocation;

with AWS.Config;
with AWS.Net.Generic_Sets;
with AWS.Net.Std;
with AWS.Translator;
with AWS.Utils;

package body AWS.Net.WebSocket.Registry is

   use GNAT;

   --  Containers for all registered constructors

   package Constructors is
     new Containers.Indefinite_Ordered_Maps (String, Factory);
   Factories : Constructors.Map;

   --  A queue for WebSocket with pending messages to be read

   package WebSocket_Queue is new Utils.Mailbox_G (Object_Class);
   type Queue_Ref is access WebSocket_Queue.Mailbox;

   --  A list of all WebSockets in the registry, this list is used to send or
   --  broadcast messages.

   function "<" (Left, Right : Object_Class) return Boolean;
   --  Order on the socket file descriptor

   procedure WebSocket_Exception
     (WebSocket : Object_Class; Message : String);
   --  Call when an exception is caught. In this case we want to send the
   --  error message, the close message and shutdown the socket.

   package WebSocket_Set is new Containers.Ordered_Sets (Object_Class);

   --  The socket set with all sockets to wait for data

   package FD_Set is new Net.Generic_Sets (Object_Class);
   use type FD_Set.Socket_Count;

   Set : FD_Set.Socket_Set_Type;
   --  All WebSockets are registered into this set to check for incoming
   --  messages. We a message is ready the WebSocket is placed into the
   --  Message_Queue for being handled. When the message has been read
   --  and handled the WebSocket is put back into this set.
   --
   --  Note that the very first one is a signaling socket used to release the
   --  wait call. This first entry is not a WebSocket and should be ignored in
   --  most code below.

   task type Watcher is
      pragma Priority (Config.WebSocket_Priority);
   end Watcher;

   type Watcher_Ref is access all Watcher;
   --  This task is in charge of watching the WebSocket for incoming messages.
   --  It then places the WebSocket into a job queue to be processed by the
   --  reader tasks.

   task type Message_Reader is
      pragma Priority (Config.WebSocket_Priority);
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

   Shutdown_Signal : Boolean := False;

   --  Concurrent access to Set above

   protected DB is

      procedure Initialize;
      --  Initialize the socket set by inserting a signaling socket

      procedure Finalize;
      --  Close signaling socket

      procedure Watch (WebSocket : Object_Class);
      --  Add a new Websocket into the set, release the current FD_Set.Wait
      --  call if any to ensure this new WebSocket will be watched too.

      procedure Remove (Index : FD_Set.Socket_Index);
      --  Remove WebSocket at the given index

      entry Not_Empty;
      --  Returns if the Set is not empty

      procedure Send
        (To          : Recipient;
         Message     : String;
         Except_Peer : String;
         Timeout     : Duration := Forever);
      --  Send the given message to all matching WebSockets

      procedure Send
        (Socket    : in out Object'Class;
         Message   : String;
         Is_Binary : Boolean := False;
         Timeout   : Duration := Forever);

      procedure Send
        (Socket    : in out Object'Class;
         Message   : Unbounded_String;
         Is_Binary : Boolean := False;
         Timeout   : Duration := Forever);
   --  Same as above but can be used for large messages. The message is
   --  possibly sent fragmented.

      procedure Send
        (Socket    : in out Object'Class;
         Message   : Stream_Element_Array;
         Is_Binary : Boolean := True;
         Timeout   : Duration := Forever);

      procedure Close
        (To          : Recipient;
         Message     : String;
         Except_Peer : String;
         Timeout     : Duration := Forever);
      --  Close all matching Webockets

      procedure Register (WebSocket : Object_Class);
      --  Register a new WebSocket

      procedure Unregister (WebSocket : Object_Class);
      --  Unregister a WebSocket

      procedure Signal_Socket;
      --  Send a signal to the wait call

      procedure Receive
        (WebSocket : Object_Class;
         Data      : out Stream_Element_Array;
         Last      : out Stream_Element_Offset);
      --  Get data from WebSocket

   private
      Sig1, Sig2 : Net.Std.Socket_Type; -- Signaling sockets
      Signal     : Boolean := False;    -- Transient signal, release Not_Emtpy
      Count      : Natural := 0;        -- Not counting signaling socket
      Registered : WebSocket_Set.Set;   -- Contains all the WebSocket ref
   end DB;

   -------------
   -- Watcher --
   -------------

   task body Watcher is
      use type FD_Set.Socket_Count;
      Count : FD_Set.Socket_Count;
      WS    : Object_Class;
   begin
      loop
         DB.Not_Empty;
         exit when Shutdown_Signal;

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
                     DB.Remove (K);
                     Message_Queue.Add (WS);
                     --  Don't increment K if we're removing FDs as it will get
                     --  replaced with last value.
                  else
                     K := K + 1;
                  end if;
               end loop;
            end;

         exception
            when E : others =>
               --  Send a On_Error message to all registered clients

               for K in 2 .. FD_Set.Count (Set) loop
                  WS := FD_Set.Get_Data (Set, K);
                  WS.State.Errno := Error_Code (Internal_Server_Error);
                  WS.On_Error
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
      WebSocket : Object_Class;
      Data      : Stream_Element_Array (1 .. 4_096);
      Last      : Stream_Element_Offset;
      Message   : Unbounded_String;
   begin
      Handle_Message : loop
         begin
            Message := Null_Unbounded_String;

            Message_Queue.Get (WebSocket);

            --  A WebSocket is null when termination is requested

            exit Handle_Message when WebSocket = null;

            --  A message can be sent in multiple chunks and/or multiple
            --  frames with possibly some control frames in between text or
            --  binary ones. This loop handles those cases.

            Read_Message : loop
               DB.Receive (WebSocket, Data, Last);

               case WebSocket.Kind is
                  when Text | Binary =>
                     Append
                       (Message,
                        Translator.To_String (Data (Data'First .. Last)));

                     if WebSocket.End_Of_Message then

                        --  Validate the message as being valid UTF-8 string

                        if WebSocket.Kind = Text
                          and then not Utils.Is_Valid_UTF8 (Message)
                        then
                           DB.Unregister (WebSocket);
                           WebSocket.Shutdown;

                        else
                           WebSocket.On_Message (Message);
                           DB.Watch (WebSocket);
                        end if;

                        exit Read_Message;
                     end if;

                  when Connection_Close =>
                     DB.Unregister (WebSocket);
                     WebSocket.On_Close (To_String (Message));
                     WebSocket.Shutdown;
                     exit Read_Message;

                  when Ping | Pong =>
                     if WebSocket.End_Of_Message then
                        DB.Watch (WebSocket);
                        exit Read_Message;
                     end if;

                  when Connection_Open | Unknown =>
                     --  Note that the On_Open message has been handled at the
                     --  time the WebSocket was registered.
                     exit Read_Message;
               end case;
            end loop Read_Message;

         exception
            when E : others =>
               DB.Unregister (WebSocket);
               WebSocket_Exception (WebSocket, Exception_Message (E));
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
         Timeout     : Duration := Forever)
      is

         procedure Close_To (Position : WebSocket_Set.Cursor);

         -------------
         -- Close_To --
         -------------

         procedure Close_To (Position : WebSocket_Set.Cursor) is
            WebSocket : constant Object_Class :=
                          WebSocket_Set.Element (Position);
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
               WebSocket.State.Errno := Error_Code (Normal_Closure);

               --  If an error occurs, we don't want to fail, shutdown the
               --  socket silently.

               begin
                  WebSocket.Set_Timeout (Timeout);
                  WebSocket.On_Close (Message);
               exception
                  when others =>
                     null;
               end;

               WebSocket.Shutdown;
            end if;
         end Close_To;

         Registered_Before : constant WebSocket_Set.Set := Registered;

      begin
         Registered_Before.Iterate (Close_To'Access);
      end Close;

      --------------
      -- Finalize --
      --------------

      procedure Finalize is

         procedure On_Close (Position : WebSocket_Set.Cursor);

         --------------
         -- On_Close --
         --------------

         procedure On_Close (Position : WebSocket_Set.Cursor) is
            WebSocket : constant Object_Class :=
                          WebSocket_Set.Element (Position);
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
            WebSocket.Free;
         end On_Close;

      begin
         Net.Std.Shutdown (Sig1);
         Net.Std.Shutdown (Sig2);

         FD_Set.Reset (Set);

         --  Finally send a On_Close message to all registered WebSocket

         Registered.Iterate (On_Close'Access);
         Registered.Clear;
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         --  Create a signaling socket that will be used to exit from the
         --  infinite wait when a new WebSocket arrives.
         Net.Std.Socket_Pair (Sig1, Sig2);
         FD_Set.Add (Set, Sig1, null, FD_Set.Input);
      end Initialize;

      ---------------
      -- Not_Empty --
      ---------------

      entry Not_Empty when Count > 0 or else Signal is
      begin
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
        (WebSocket : Object_Class;
         Data      : out Stream_Element_Array;
         Last      : out Stream_Element_Offset) is
      begin
         WebSocket.Receive (Data, Last);
      end Receive;

      --------------
      -- Register --
      --------------

      procedure Register (WebSocket : Object_Class) is
      begin
         Registered.Insert (WebSocket);
      end Register;

      ------------
      -- Remove --
      ------------

      procedure Remove (Index : FD_Set.Socket_Index) is
      begin
         pragma Assert (Index > 1);
         FD_Set.Remove_Socket (Set, Index);
         Count := Count - 1;
      end Remove;

      ----------
      -- Send --
      ----------

      procedure Send
        (To          : Recipient;
         Message     : String;
         Except_Peer : String;
         Timeout     : Duration := Forever)
      is

         procedure Send_To (Position : WebSocket_Set.Cursor);

         -------------
         -- Send_To --
         -------------

         procedure Send_To (Position : WebSocket_Set.Cursor) is
            WebSocket : constant Object_Class :=
                          WebSocket_Set.Element (Position);
         begin
            if (Except_Peer = "" or else WebSocket.Peer_Addr /= Except_Peer)
              and then
                (not To.URI_Set
                 or else GNAT.Regexp.Match (WebSocket.URI, To.URI))
              and then
                (not To.Origin_Set
                 or else GNAT.Regexp.Match (WebSocket.Origin, To.Origin))
            then
               begin
                  WebSocket.Set_Timeout (Timeout);
                  WebSocket.Send (Message);
               exception
                  when E : others =>
                     Unregister (WebSocket);
                     WebSocket_Exception (WebSocket, Exception_Message (E));
               end;
            end if;
         end Send_To;

         Registered_Before : constant WebSocket_Set.Set := Registered;

      begin
         Registered_Before.Iterate (Send_To'Access);
      end Send;

      procedure Send
        (Socket    : in out Object'Class;
         Message   : String;
         Is_Binary : Boolean := False;
         Timeout   : Duration := Forever) is
      begin
         Socket.Set_Timeout (Timeout);
         Socket.Send (Message, Is_Binary);
      end Send;

      procedure Send
        (Socket    : in out Object'Class;
         Message   : Unbounded_String;
         Is_Binary : Boolean := False;
         Timeout   : Duration := Forever) is
      begin
         Socket.Set_Timeout (Timeout);
         Socket.Send (Message, Is_Binary);
      end Send;

      procedure Send
        (Socket    : in out Object'Class;
         Message   : Stream_Element_Array;
         Is_Binary : Boolean := True;
         Timeout   : Duration := Forever) is
      begin
         Socket.Set_Timeout (Timeout);
         Socket.Send (Message, Is_Binary);
      end Send;

      -------------------
      -- Signal_Socket --
      -------------------

      procedure Signal_Socket is
      begin
         Net.Send (Sig2, Stream_Element_Array'(1 => 0));

         --  Also activate the signal to release Not_Empty for proper
         --  termination when there is no remaining socket.

         Signal := True;
      end Signal_Socket;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (WebSocket : Object_Class) is
         Position : WebSocket_Set.Cursor := Registered.Find (WebSocket);
      begin
         if WebSocket_Set.Has_Element (Position) then
            Registered.Delete (Position);
         end if;

         --  And remove it from the set

         for K in 2 .. FD_Set.Count (Set) loop
            if FD_Set.Get_Data (Set, K) = WebSocket then
               --  It's okay to remove from here because we immediately exit
               --  the loop.
               Remove (K);
               Signal_Socket;
               exit;
            end if;
         end loop;
      end Unregister;

      -----------
      -- Watch --
      -----------

      procedure Watch (WebSocket : Object_Class) is
      begin
         FD_Set.Add (Set, WebSocket.all, WebSocket, FD_Set.Input);

         Count := Count + 1;

         --  Signal the wait if there was already some socket

         if Count > 1 then
            Signal_Socket;
         end if;
      end Watch;

   end DB;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Object_Class) return Boolean is
   begin
      return Left.Get_FD < Right.Get_FD;
   end "<";

   -----------
   -- Close --
   -----------

   procedure Close
     (To          : Recipient;
      Message     : String;
      Except_Peer : String := "";
      Timeout     : Duration := Forever) is
   begin
      DB.Close (To, Message, Except_Peer, Timeout);
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
         return Create'Access;
      end if;
   end Constructor;

   ------------
   -- Create --
   ------------

   function Create (URI : String; Origin : String := "") return Recipient is
      Result : Recipient;
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

   --------------
   -- Register --
   --------------

   procedure Register (URI : String; Factory : Registry.Factory) is
   begin
      Factories.Insert (URI, Factory);
   end Register;

   ----------
   -- Send --
   ----------

   procedure Send
     (To          : Recipient;
      Message     : String;
      Except_Peer : String := "";
      Timeout     : Duration := Forever) is
   begin
      DB.Send (To, Message, Except_Peer, Timeout);
   exception
      when others =>
         --  Should never fails even if the WebSocket is closed by peer
         null;
   end Send;

   procedure Send
     (To      : Recipient;
      Message : String;
      Request : AWS.Status.Data;
      Timeout : Duration := Forever) is
   begin
      Send
        (To, Message,
         Except_Peer => AWS.Status.Socket (Request).Peer_Addr,
         Timeout     => Timeout);
   end Send;

   procedure Send
     (Socket    : in out Object'Class;
      Message   : String;
      Is_Binary : Boolean := False;
      Timeout   : Duration := Forever) is
   begin
      DB.Send (Socket, Message, Is_Binary, Timeout);
   end Send;

   procedure Send
     (Socket    : in out Object'Class;
      Message   : Unbounded_String;
      Is_Binary : Boolean := False;
      Timeout   : Duration := Forever) is
   begin
      DB.Send (Socket, Message, Is_Binary, Timeout);
   end Send;

   procedure Send
     (Socket    : in out Object'Class;
      Message   : Stream_Element_Array;
      Is_Binary : Boolean := True;
      Timeout   : Duration := Forever) is
   begin
      DB.Send (Socket, Message, Is_Binary, Timeout);
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
      DB.Signal_Socket;

      --  Wait for proper termination to be able to free the task object

      while not Message_Watcher'Terminated loop
         delay 0.5;
      end loop;

      --  Now shutdown all the message readers

      for K in Message_Readers'Range loop
         Message_Queue.Add (null);
      end loop;

      for K in Message_Readers'Range loop
         while not Message_Readers (K)'Terminated loop
            delay 0.5;
         end loop;
      end loop;

      --  Now we can deallocate the task objects

      Unchecked_Free (Message_Readers);
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
   end Start;

   ----------------
   -- Watch_Data --
   ----------------

   procedure Watch_Data (WebSocket : Object'Class) is
      WS : constant Object_Class := new Object'Class'(WebSocket);
   begin
      --  Send a Connection_Open message

      WS.State.Kind := Connection_Open;
      WS.On_Open ("AWS WebSocket connection open");

      --  Register WebSocket

      DB.Register (WS);
      DB.watch (WS);
   end Watch_Data;

   -------------------------
   -- WebSocket_Exception --
   -------------------------

   procedure WebSocket_Exception
     (WebSocket : Object_Class; Message : String) is
   begin
      WebSocket.State.Errno := Error_Code (Protocol_Error);
      WebSocket.On_Error (Message);
      WebSocket.On_Close (Message);
      WebSocket.Shutdown;
   exception
      when others =>
         --  Never propagate an exception at this point
         null;
   end WebSocket_Exception;

end AWS.Net.WebSocket.Registry;
