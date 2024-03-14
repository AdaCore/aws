------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.Generic_Sets;
with AWS.Net.Log;
with AWS.Net.WebSocket;
with AWS.Translator;
with AWS.Utils;

package body AWS.Server.Push is

   use AWS.Net;

   type Phase_Type is (Available, Going, Waiting);
   --  Available when the socket is not in the waiting poll.
   --  Going when the socket is in process to be placed into waiting poll.
   --  Waiting when the socket is in the waiting poll.

   type Action_Type is (Add, Remove, Shutdown, Deallocate);
   --  Command to server push waiter

   type Client_Holder is record
      Socket      : Net.Socket_Access;
      Kind        : Mode;
      Created     : Real_Time.Time;
      Environment : Client_Environment;
      Groups      : Group_Sets.Set;
      Chunks      : Chunk_Lists.List;
      Chunk_Sent  : Natural;
      Thin        : Thin_Indexes.Map;
      Phase       : Phase_Type;
      Timeout     : Real_Time.Time_Span;
      Errmsg      : Unbounded_String; -- Filled on socket error in waiter
   end record;

   function To_Holder
     (Socket      : Net.Socket_Access;
      Environment : Client_Environment;
      Kind        : Mode;
      Groups      : Group_Set;
      Timeout     : Duration) return Client_Holder_Access;

   procedure Free
     (Holder : in out Client_Holder_Access; Socket : Boolean := True);

   procedure Unchecked_Free is
     new Unchecked_Deallocation (Tables.Map, Map_Access);

   procedure Release
     (Server         : in out Object;
      Close_Sockets  : Boolean;
      Left_Open      : Boolean;
      Get_Final_Data : access function
                         (Holder : not null access Client_Holder)
                          return Stream_Element_Array := null);

   procedure Add_To_Groups
     (Groups     : in out Group_Map;
      Group_Name : String;
      Client_Id  : String;
      Holder     : Client_Holder_Access);

   procedure Register
     (Server         : in out Object;
      Client_Id      : Client_Key;
      Holder         : in out Client_Holder_Access;
      Init_Data      : Stream_Element_Array;
      Content_Type   : String;
      Duplicated_Age : Duration;
      Ext_Sock_Alloc : Boolean);
   --  Internal register routine

   function Data_Chunk
     (Holder       : not null access Client_Holder;
      Data         : Client_Output_Type;
      Content_Type : String) return Stream_Element_Array;

   procedure Get_Data
     (Holder : in out Client_Holder;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   procedure Waiter_Command
     (Server : in out Object;
      Holder : Client_Holder_Access;
      Action : Action_Type);

   procedure Waiter_Signal;

   procedure Waiter_Pause;

   procedure Waiter_Resume;

   No_Servers   : not null access procedure := Null_Procedure'Access;
   First_Server : not null access procedure := Null_Procedure'Access;

   New_Line : constant String := ASCII.CR & ASCII.LF;
   --  HTTP new line

   Byte0 : constant Stream_Element_Array := (1 => 0);

   Boundary  : constant String :=
                 "AWS.Push.Boundary_" & Utils.Random_String (8);
   --  This is the multi-part boundary string used by AWS push server

   Delimiter : constant String := "--" & Boundary & New_Line;
   --  And the corresponding delimiter

   W_Signal  : aliased Net.Socket_Type'Class := Net.Socket (Security => False);
   --  Socket to signal waiter to do to select task entries

   Internal_Error_Handler : Error_Handler := Text_IO.Put_Line'Access;

   type Object_Access is access all Object;

   type Waiter_Queue_Element is record
      Server : Object_Access;
      Holder : Client_Holder_Access;
      Action : Action_Type;
   end record;

   package Waiter_Queues is
     new Ada.Containers.Doubly_Linked_Lists (Waiter_Queue_Element);

   protected Waiter_Queue is
      procedure Add (Item  : Waiter_Queue_Element);
      procedure Add (Queue : Waiter_Queues.List);
      procedure Get (Queue : out Waiter_Queues.List);
   private
      Queue : Waiter_Queues.List;
   end Waiter_Queue;

   protected Waiter_Information is

      procedure Info
        (Size        : out Natural;
         Max_Size    : out Natural;
         Max_Size_DT : out Calendar.Time;
         Counter     : out Wait_Counter_Type);

      entry Empty;

      procedure Set_Size (Size : Positive; Counter : Wait_Counter_Type);

   private
      Size        : Positive := 1;
      Max_Size    : Positive := 1;
      Max_Size_DT : Calendar.Time := Calendar.Clock;
      Counter     : Wait_Counter_Type := 0;
   end Waiter_Information;

   task Waiter with Priority => CNF.Service_Priority is
      entry Resume;
   end Waiter;

   -------------------
   -- Add_To_Groups --
   -------------------

   procedure Add_To_Groups
     (Groups     : in out Group_Map;
      Group_Name : String;
      Client_Id  : String;
      Holder     : Client_Holder_Access)
   is
      Cursor : constant Group_Maps.Cursor := Groups.Find (Group_Name);
      Map    : Map_Access;
   begin
      if Group_Maps.Has_Element (Cursor) then
         Map := Group_Maps.Element (Cursor);
      else
         Map := new Tables.Map;
         Groups.Insert (Group_Name, Map);
      end if;

      Map.Insert (Client_Id, Holder);
   end Add_To_Groups;

   -----------
   -- Count --
   -----------

   function Count (Server : Object) return Natural is
   begin
      return Server.Count;
   end Count;

   ----------------
   -- Data_Chunk --
   ----------------

   function Data_Chunk
     (Holder       : not null access Client_Holder;
      Data         : Client_Output_Type;
      Content_Type : String) return Stream_Element_Array
   is
      Data_To_Send : constant Stream_Element_Array :=
                       To_Stream_Array (Data, Holder.Environment);
      --  Data to send, will be sent with a prefix and suffix

      function Prefix return String;
      --  Message prefix

      function Suffix return String;
      --  Message suffix

      ------------
      -- Prefix --
      ------------

      function Prefix return String is
      begin
         case Holder.Kind is
            when Multipart =>
               Holder.Chunk_Sent := Holder.Chunk_Sent + 1;

               if Holder.Chunk_Sent = 1 then
                  return Delimiter & Messages.Content_Type (Content_Type)
                    & New_Line & New_Line;
               else
                  return Messages.Content_Type (Content_Type)
                    & New_Line & New_Line;
               end if;

            when Chunked =>
               return Utils.Hex (Data_To_Send'Size / System.Storage_Unit)
                 & New_Line;

            when Plain =>
               return "";
         end case;
      end Prefix;

      ------------
      -- Suffix --
      ------------

      function Suffix return String is
      begin
         case Holder.Kind is
            when Multipart =>
               return New_Line & New_Line & Delimiter;

            when Chunked =>
               return New_Line;

            when Plain =>
               return "";
         end case;
      end Suffix;

   begin
      if Data_To_Send'Length > 0 then
         return Translator.To_Stream_Element_Array (Prefix) & Data_To_Send
                & Translator.To_Stream_Element_Array (Suffix);
      else
         return (1 .. 0 => 0);
      end if;
   end Data_Chunk;

   ----------
   -- Free --
   ----------

   procedure Free
     (Holder : in out Client_Holder_Access; Socket : Boolean := True)
   is
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Client_Holder, Client_Holder_Access);
   begin
      if Socket then
         Net.Free (Holder.Socket);
      end if;

      Unchecked_Free (Holder);
   end Free;

   --------------
   -- Get_Data --
   --------------

   procedure Get_Data
     (Holder : in out Client_Holder;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      C : Chunk_Lists.Cursor := Holder.Chunks.First;
   begin
      pragma Assert (Data'First = 1);

      Last := Data'First - 1;

      while Chunk_Lists.Has_Element (C) loop
         declare
            Message : constant Message_Type := Chunk_Lists.Element (C);
            Next    : constant Stream_Element_Offset :=
                        Last + Message.Data'Last;
         begin
            if Next > Data'Last then
               if Last < Data'First then
                  --  Too big single message need to send partially

                  Data := Message.Data (1 .. Data'Last);
                  Last := Data'Last;

                  if Message.Thin /= "" then
                     Holder.Thin.Delete (Message.Thin);
                  end if;

                  Holder.Chunks.Replace_Element
                    (C,
                     (Size      => Message.Data'Length - Data'Length,
                      Thin_Size => 0,
                      Data      =>
                        Message.Data (Last + 1 .. Message.Data'Last),
                      Thin      => ""));
               end if;

               exit;
            end if;

            Data (Last + 1 .. Next) := Message.Data;
            Last := Next;

            if Message.Thin /= "" then
               Holder.Thin.Delete (Message.Thin);
            end if;

            Holder.Chunks.Delete (C);
            C := Holder.Chunks.First;
         end;
      end loop;

   end Get_Data;

   ----------
   -- Info --
   ----------

   procedure Info
     (Size        : out Natural;
      Max_Size    : out Natural;
      Max_Size_DT : out Calendar.Time;
      Counter     : out Wait_Counter_Type) is
   begin
      Waiter_Information.Info
        (Size        => Size,
         Max_Size    => Max_Size,
         Max_Size_DT => Max_Size_DT,
         Counter     => Counter);
   end Info;

   procedure Info
     (Server  : in out Object;
      Clients : out    Natural;
      Groups  : out    Natural;
      Process : access procedure
                  (Client_Id   : Client_Key;
                   Address     : String;
                   State       : String;
                   Environment : Client_Environment;
                   Kind        : Mode;
                   Groups      : Group_Set) := null) is
   begin
      Server.Info (Clients, Group_Count => Groups, Process => Process);
   end Info;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Server : Object) return Boolean is
   begin
      return Server.Is_Open;
   end Is_Open;

   ------------
   -- Object --
   ------------

   protected body Object is

      procedure Send_Data
        (Holder       : in out Client_Holder_Access;
         Data         : Client_Output_Type;
         Content_Type : String;
         Thin_Id      : String);
      --  Send Data to a client identified by Holder.
      --  If Holder out value is not null it mean that socket become busy
      --  waiting for output availability and we have to move it into Waiter.
      --  Holder out value could be not null in case of socket error on
      --  previous socket operations in waiter.

      procedure Unregister (Cursor : in out Tables.Cursor);

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Natural (Container.Length);
      end Count;

      --------------
      -- Get_Data --
      --------------

      procedure Get_Data
        (Holder : Client_Holder_Access;
         Data   : out Stream_Element_Array;
         Last   : out Stream_Element_Offset) is
      begin
         pragma Assert (Holder.Phase = Waiting);

         Get_Data (Holder.all, Data, Last);

         if Last < Data'First then
            Holder.Phase := Available;
         end if;
      end Get_Data;

      ----------
      -- Info --
      ----------

      procedure Info
        (Client_Count : out Natural;
         Group_Count  : out Natural;
         Process      : access procedure
                          (Client_Id   : Client_Key;
                           Address     : String;
                           State       : String;
                           Environment : Client_Environment;
                           Kind        : Mode;
                           Groups      : Group_Set))
      is
         procedure Action (C : Tables.Cursor);

         ------------
         -- Action --
         ------------

         procedure Action (C : Tables.Cursor) is
            CA     : constant Client_Holder_Access := Tables.Element (C);
            Groups : Group_Set (1 .. Integer (CA.Groups.Length));
            CG     : Group_Sets.Cursor := CA.Groups.First;
            G      : Group_Maps.Cursor;

            function Get_Peer_Addr return String;

            -------------------
            -- Get_Peer_Addr --
            -------------------

            function Get_Peer_Addr return String is
            begin
               return Peer_Addr (CA.Socket.all);
            exception
               when E : Net.Socket_Error =>
                  return Exception_Message (E);
            end Get_Peer_Addr;

         begin
            for J in Groups'Range loop
               if Process /= null then
                  Groups (J) := To_Unbounded_String (Group_Sets.Element (CG));
               end if;

               G := Object.Groups.Find (Group_Sets.Element (CG));

               if not Group_Maps.Has_Element (G) then
                  raise Program_Error with
                    "Not found group " & Group_Sets.Element (CG)
                    & " for client";
               end if;

               if Group_Maps.Element (G).Element (Tables.Key (C)) /= CA then
                  raise Program_Error with "Loose client in group";
               end if;

               CG := Group_Sets.Next (CG);
            end loop;

            if Process /= null then
               Process
                 (Tables.Key (C),
                  Get_Peer_Addr,
                  Phase_Type'Image (CA.Phase) & ' ' & To_String (CA.Errmsg),
                  CA.Environment,
                  CA.Kind,
                  Groups);
            end if;
         end Action;

         C : Tables.Cursor := Container.First;
         G : Group_Maps.Cursor;

      begin -- Info
         Container.Iterate (Action'Access);

         G := Groups.First;

         while Group_Maps.Has_Element (G) loop
            C := Group_Maps.Element (G).First;

            while Tables.Has_Element (C) loop
               if
                 not Tables.Element (C).Groups.Contains (Group_Maps.Key (G))
               then
                  raise Program_Error with "Loose group in client";
               end if;

               C := Tables.Next (C);
            end loop;

            G := Group_Maps.Next (G);
         end loop;

         Group_Count  := Integer (Groups.Length);
         Client_Count := Integer (Container.Length);
      end Info;

      -------------
      -- Is_Open --
      -------------

      function Is_Open return Boolean is
      begin
         return Open;
      end Is_Open;

      --------------
      -- Register --
      --------------

      procedure Register
        (Client_Id      : Client_Key;
         Holder         : in out Client_Holder_Access;
         Duplicated     : out Client_Holder_Access;
         Duplicated_Age : Duration;
         Ext_Sock_Alloc : Boolean)
      is
         use Real_Time;

         procedure Add_To_Groups (J : Group_Sets.Cursor);

         -------------------
         -- Add_To_Groups --
         -------------------

         procedure Add_To_Groups (J : Group_Sets.Cursor) is
         begin
            Add_To_Groups (Groups, Group_Sets.Element (J), Client_Id, Holder);
         end Add_To_Groups;

         Cursor  : Tables.Cursor;
         Success : Boolean;

      begin
         if not Open then
            Free (Holder, Socket => not Ext_Sock_Alloc);
            raise Closed;
         end if;

         Container.Insert (Client_Id, Holder, Cursor, Success);

         if Success then
            Duplicated := null;

         else
            Duplicated := Tables.Element (Cursor);

            if To_Time_Span (Duplicated_Age) < Clock - Duplicated.Created then
               Unregister (Cursor);
               Container.Insert (Client_Id, Holder);
            else
               Free (Holder, Socket => not Ext_Sock_Alloc);
               raise Duplicate_Client_Id;
            end if;
         end if;

         Holder.Phase := Going;

         Holder.Groups.Iterate (Add_To_Groups'Access);
      end Register;

      -------------
      -- Restart --
      -------------

      procedure Restart is
      begin
         Open := True;
      end Restart;

      ----------
      -- Send --
      ----------

      procedure Send
        (Data         : Client_Output_Type;
         Group_Id     : String;
         Content_Type : String;
         Thin_Id      : String;
         Queue        : out Tables.Map)
      is
         Cursor : Tables.Cursor;
         Next   : Tables.Cursor;
         Holder : Client_Holder_Access;
      begin
         if Group_Id = "" then
            Cursor := Container.First;

         else
            declare
               use Group_Maps;
               C : constant Group_Maps.Cursor := Groups.Find (Group_Id);
            begin
               if not Has_Element (C) then
                  return;
               end if;

               Cursor := Element (C).First;
            end;
         end if;

         Queue.Clear;

         while Tables.Has_Element (Cursor) loop
            Holder := Tables.Element (Cursor);
            Next   := Tables.Next (Cursor);

            Send_Data (Holder, Data, Content_Type, Thin_Id);

            if Holder /= null then
               Queue.Insert (Tables.Key (Cursor), Holder);

               if Holder.Errmsg /= Null_Unbounded_String then
                  if Group_Id /= "" then
                     --  Cursor have to be from Containers

                     Cursor := Container.Find (Tables.Key (Cursor));
                  end if;

                  Unregister (Cursor);
               end if;
            end if;

            Cursor := Next;
         end loop;
      end Send;

      ---------------
      -- Send_Data --
      ---------------

      procedure Send_Data
        (Holder       : in out Client_Holder_Access;
         Data         : Client_Output_Type;
         Content_Type : String;
         Thin_Id      : String)
      is
         Events : Net.Event_Set;

         procedure To_Buffer;

         ---------------
         -- To_Buffer --
         ---------------

         procedure To_Buffer is
            Chunk : constant Stream_Element_Array :=
                      Data_Chunk (Holder, Data, Content_Type);
            CT    : Thin_Indexes.Cursor;
         begin
            if Thin_Id /= "" then
               CT := Holder.Thin.Find (Thin_Id);
            end if;

            if Thin_Indexes.Has_Element (CT) then
               Holder.Chunks.Replace_Element
                 (Thin_Indexes.Element (CT),
                  (Size      => Chunk'Length,
                   Thin_Size => Thin_Id'Length,
                   Data      => Chunk,
                   Thin      => Thin_Id));
            else
               Holder.Chunks.Append
                 ((Size      => Chunk'Length,
                   Thin_Size => Thin_Id'Length,
                   Data      => Chunk,
                   Thin      => Thin_Id));

               if Thin_Id /= "" then
                  Holder.Thin.Insert (Thin_Id, Holder.Chunks.Last);
               end if;
            end if;
         end To_Buffer;

      begin
         if not Open then
            raise Closed;
         end if;

         if Holder.Phase = Available then
            if Holder.Errmsg /= Null_Unbounded_String then
               return;
            end if;

            if not Holder.Chunks.Is_Empty then
               raise Program_Error;
            end if;

            --  Net.Check is not blocking operation

            begin
               Events := Holder.Socket.Check ((others => True));
            exception
               when E : Socket_Error =>
                  --  !!! Most possible it is ENOBUFS or ENOMEM error
                  --  because of too many open sockets. We should see the exact
                  --  error in the socket error log file.

                  Holder.Errmsg := To_Unbounded_String (Exception_Message (E));
                  Events := (others => False);

                  if Holder.Socket.all in Net.WebSocket.Object'Class
                    and then Holder.Socket.Get_FD = Net.No_Socket
                  then
                     --  Need for free Holder when WebSocket closed
                     --  in WebSocket.Register
                     Holder.Phase := Available;
                     return;
                  end if;
            end;

            if Events (Error) then
               Holder.Errmsg :=
                 To_Unbounded_String
                   ("Check server push output error "
                    & Utils.Image (Net.Errno (Holder.Socket.all)));
               Net.Log.Error (Holder.Socket.all, To_String (Holder.Errmsg));

            elsif Events (Input) then
               if Holder.Socket.all in Net.WebSocket.Object'Class then
                  Holder := null;
                  --  Net.WebSocket.Protocol do read data
                  --  and protocol handling
               else
                  --  Need to understand closed socket from peer
                  declare
                     Unexpect : Stream_Element_Array (1 .. 64);
                     Last     : Stream_Element_Offset;
                  begin
                     Holder.Socket.Receive (Unexpect, Last);

                     Holder.Errmsg :=
                       To_Unbounded_String
                         ("Unexpected data from server push socket: "
                          & Translator.To_String (Unexpect (1 .. Last)));
                     Net.Log.Error
                       (Holder.Socket.all, To_String (Holder.Errmsg));
                  exception
                     when E : Socket_Error =>
                        Holder.Errmsg :=
                          To_Unbounded_String (Exception_Message (E));
                  end;
               end if;

            elsif Events (Output) then
               declare
                  Chunk : constant Stream_Element_Array :=
                            Data_Chunk (Holder, Data, Content_Type);
                  Last  : Stream_Element_Offset;
               begin
                  if Holder.Socket.all in Net.WebSocket.Object'Class then
                     Net.WebSocket.Send
                       (Socket    => Net.WebSocket.Object (Holder.Socket.all),
                        Message   => Chunk,
                        Is_Binary => False);
                     Last := Chunk'Last;
                  else
                     --  It is not blocking Net.Send operation
                     Holder.Socket.Send (Chunk, Last);
                  end if;

                  if Last = Chunk'Last then
                     --  Data sent completely
                     Holder := null;

                  elsif Last in Chunk'Range then
                     --  Data sent partially

                     Holder.Chunks.Append
                       ((Size      => Chunk'Last - Last,
                         Thin_Size => 0,
                         Data      => Chunk (Last + 1 .. Chunk'Last),
                         Thin      => ""));

                     Holder.Phase := Going;

                  else
                     --  Data not sent. Strange, because Check routine did
                     --  show output availability.

                     To_Buffer;
                     Holder.Phase := Going;
                  end if;

               exception
                  when E : Net.Socket_Error =>
                     Holder.Errmsg :=
                       To_Unbounded_String (Exception_Message (E));
               end;

            else
               To_Buffer;

               --  We would return Holder not null to move it to Waiter

               Holder.Phase := Going;
            end if;

         else
            To_Buffer;

            Holder := null;
         end if;
      end Send_Data;

      -------------
      -- Send_To --
      -------------

      procedure Send_To
        (Client_Id    : Client_Key;
         Data         : Client_Output_Type;
         Content_Type : String;
         Thin_Id      : String;
         Holder       : out Client_Holder_Access)
      is
         Cursor : Tables.Cursor := Container.Find (Client_Id);
      begin
         if Tables.Has_Element (Cursor) then
            Holder := Tables.Element (Cursor);
            Send_Data (Holder, Data, Content_Type, Thin_Id);

            if Holder /= null
              and then Holder.Errmsg /= Null_Unbounded_String
            then
               Unregister (Cursor);
            end if;
         else
            raise Client_Gone with "No such client id '" & Client_Id & ''';
         end if;
      end Send_To;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown
        (Final_Data         : Client_Output_Type;
         Final_Content_Type : String;
         Queue              : out Tables.Map) is
      begin
         Send (Final_Data, "", Final_Content_Type, "", Queue);
         Open := False;
      end Shutdown;

      -----------------------
      -- Shutdown_If_Empty --
      -----------------------

      procedure Shutdown_If_Empty (Open : out Boolean) is
         use type Ada.Containers.Count_Type;
      begin
         if Container.Length = 0 then
            Object.Open := False;
         end if;
         Shutdown_If_Empty.Open := Object.Open;
      end Shutdown_If_Empty;

      ---------------
      -- Subscribe --
      ---------------

      procedure Subscribe (Client_Id : Client_Key; Group_Id : String) is

         procedure Modify
           (Key : String; Element : in out Client_Holder_Access);

         ------------
         -- Modify --
         ------------

         procedure Modify
           (Key : String; Element : in out Client_Holder_Access)
         is
            CI      : Group_Sets.Cursor;
            Success : Boolean;
         begin
            Element.Groups.Insert (Group_Id, CI, Success);

            if Success then
               Add_To_Groups (Groups, Group_Id, Key, Element);
            end if;
         end Modify;

         Cursor : constant Tables.Cursor := Container.Find (Client_Id);

      begin
         if Tables.Has_Element (Cursor) then
            Container.Update_Element (Cursor, Modify'Access);
         end if;
      end Subscribe;

      --------------------
      -- Subscribe_Copy --
      --------------------

      procedure Subscribe_Copy (Source : String; Target : String) is
         CG : Group_Maps.Cursor;

         procedure Process (C : Tables.Cursor);

         -------------
         -- Process --
         -------------

         procedure Process (C : Tables.Cursor) is
            Element : constant Client_Holder_Access := Tables.Element (C);
            CI      : Group_Sets.Cursor;
            Success : Boolean;
         begin
            Element.Groups.Insert (Target, CI, Success);

            if Success then
               Add_To_Groups (Groups, Target, Tables.Key (C), Element);
            end if;
         end Process;

      begin
         if Source = "" then
            Container.Iterate (Process'Access);
         else
            CG := Groups.Find (Source);

            if Group_Maps.Has_Element (CG) then
               Group_Maps.Element (CG).Iterate (Process'Access);
            end if;
         end if;
      end Subscribe_Copy;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (Cursor : in out Tables.Cursor) is
         Holder : constant Client_Holder_Access := Tables.Element (Cursor);
         Key    : constant String := Tables.Key (Cursor);

         procedure Delete_Group (J : Group_Sets.Cursor);

         ------------------
         -- Delete_Group --
         ------------------

         procedure Delete_Group (J : Group_Sets.Cursor) is
            use type Ada.Containers.Count_Type;
            C   : Group_Maps.Cursor := Groups.Find (Group_Sets.Element (J));
            Map : Map_Access := Group_Maps.Element (C);
         begin
            Tables.Delete (Map.all, Key);

            if Map.Length = 0 then
               Groups.Delete (C);
               Unchecked_Free (Map);
            end if;
         end Delete_Group;

      begin
         Container.Delete (Cursor);
         Holder.Groups.Iterate (Delete_Group'Access);
      end Unregister;

      procedure Unregister
        (Client_Id : Client_Key; Holder : out Client_Holder_Access)
      is
         Cursor : Tables.Cursor;
      begin
         Cursor := Container.Find (Client_Id);

         if Tables.Has_Element (Cursor) then
            Holder := Tables.Element (Cursor);
            Unregister (Cursor);
         else
            Holder := null;
         end if;
      end Unregister;

      ------------------------
      -- Unregister_Clients --
      ------------------------

      procedure Unregister_Clients (Queue : out Tables.Map; Open : Boolean) is
         Cursor : Tables.Cursor;
      begin
         Object.Open := Unregister_Clients.Open;

         Queue.Clear;

         loop
            Cursor := Container.First;

            exit when not Tables.Has_Element (Cursor);

            Queue.Insert (Tables.Key (Cursor), Tables.Element (Cursor));
            Unregister (Cursor);
         end loop;
      end Unregister_Clients;

      -----------------
      -- Unsubscribe --
      -----------------

      procedure Unsubscribe (Client_Id : Client_Key; Group_Id : String) is

         procedure Modify
           (Key : String; Element : in out Client_Holder_Access);

         ------------
         -- Modify --
         ------------

         procedure Modify
           (Key : String; Element : in out Client_Holder_Access)
         is
            pragma Unreferenced (Key);
            use type Ada.Containers.Count_Type;
            Cursor : Group_Sets.Cursor := Element.Groups.Find (Group_Id);
            CG     : Group_Maps.Cursor;
            Group  : Map_Access;
         begin
            if Group_Sets.Has_Element (Cursor) then
               Element.Groups.Delete (Cursor);

               CG    := Groups.Find (Group_Id);
               Group := Group_Maps.Element (CG);
               Group.Delete (Client_Id);

               if Group.Length = 0 then
                  Groups.Delete (CG);
                  Unchecked_Free (Group);
               end if;
            end if;
         end Modify;

         Cursor : constant Tables.Cursor := Container.Find (Client_Id);

      begin
         if Tables.Has_Element (Cursor) then
            Container.Update_Element (Cursor, Modify'Access);
         end if;
      end Unsubscribe;

      ----------------------
      -- Unsubscribe_Copy --
      ----------------------

      procedure Unsubscribe_Copy (Source : String; Target : String) is
         use type Ada.Containers.Count_Type;

         CG     : Group_Maps.Cursor := Groups.Find (Target);
         CF     : Tables.Cursor;
         Group  : Map_Access;
         Client : Client_Holder_Access;
      begin
         if not Group_Maps.Has_Element (CG) then
            return;
         end if;

         Group := Group_Maps.Element (CG);

         CF := Group.First;

         while Tables.Has_Element (CF) loop
            Client := Tables.Element (CF);

            if Source = "" or else Client.Groups.Contains (Source) then
               declare
                  CN : constant Tables.Cursor := Tables.Next (CF);
               begin
                  Client.Groups.Delete (Target);
                  Group.Delete (CF);
                  CF := CN;
               end;
            else
               CF := Tables.Next (CF);
            end if;
         end loop;

         if Group.Length = 0 then
            Groups.Delete (CG);
            Unchecked_Free (Group);
         end if;

      end Unsubscribe_Copy;

      ------------------
      -- Waiter_Error --
      ------------------

      procedure Waiter_Error
        (Holder : Client_Holder_Access; Message : String) is
      begin
         pragma Assert (Holder.Phase = Waiting);
         Holder.Errmsg := To_Unbounded_String (Message);
         Holder.Phase  := Available;
      end Waiter_Error;

   end Object;

   --------------
   -- Register --
   --------------

   procedure Register
     (Server         : in out Object;
      Client_Id      : Client_Key;
      Holder         : in out Client_Holder_Access;
      Init_Data      : Stream_Element_Array;
      Content_Type   : String;
      Duplicated_Age : Duration;
      Ext_Sock_Alloc : Boolean)
   is
      Duplicated : Client_Holder_Access;

      function Content_Type_Header return String;

      -------------------------
      -- Content_Type_Header --
      -------------------------

      function Content_Type_Header return String is
      begin
         if Content_Type = "" then
            return "";
         else
            return Messages.Content_Type (Content_Type) & New_Line;
         end if;
      end Content_Type_Header;

   begin
      if Holder.Socket.all in Net.WebSocket.Object'Class
        and then Holder.Kind /= Plain
      then
         raise Program_Error
           with "Only 'Plain' Kind for websockets allowed, found "
           & Holder.Kind'Img;
      end if;

      Server.Register
        (Client_Id, Holder, Duplicated, Duplicated_Age, Ext_Sock_Alloc);

      if Duplicated /= null then
         if Duplicated.Phase = Available then
            Duplicated.Socket.Shutdown;
            Free (Duplicated);
         else
            Waiter_Command (Server, Duplicated, Shutdown);
         end if;
      end if;

      begin
         if Holder.Socket.all not in Net.WebSocket.Object'Class then
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               Messages.Status_Line (Messages.S200) & New_Line
                 & "Server: AWS (Ada Web Server) v" & Version & New_Line
                 & Messages.Connection ("Close"));
         end if;

         if Holder.Kind = Chunked then
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               Content_Type_Header & Messages.Transfer_Encoding ("chunked")
               & New_Line);

         elsif Holder.Kind = Multipart then
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               Messages.Content_Type (MIME.Multipart_X_Mixed_Replace, Boundary)
               & New_Line);

         elsif Holder.Socket.all not in Net.WebSocket.Object'Class then
            --  to Net.WebSocket.Object not need to send anything
            Net.Buffered.Put_Line (Holder.Socket.all, Content_Type_Header);
         end if;

         if Holder.Socket.all in Net.WebSocket.Object'Class then
            Net.WebSocket.Send (Socket    => Net.WebSocket.Object
                                               (Holder.Socket.all),
                                Message   => Init_Data,
                                Is_Binary => False);
         else
            Net.Buffered.Write (Holder.Socket.all, Init_Data);
            Net.Buffered.Flush (Holder.Socket.all);
         end if;

         Waiter_Command (Server, Holder, Add);

      exception
         when E : others =>
            Server.Unregister (Client_Id, Holder);

            Net.Log.Error
              (Holder.Socket.all,
               "Server push write header error " & Exception_Information (E));

            Holder.Socket.Shutdown;
            Free (Holder);
      end;

      Socket_Taken;
   end Register;

   procedure Register
     (Server            : in out Object;
      Client_Id         : Client_Key;
      Socket            : Net.Socket_Access;
      Environment       : Client_Environment;
      Init_Data         : Client_Output_Type;
      Init_Content_Type : String             := "";
      Kind              : Mode               := Plain;
      Duplicated_Age    : Duration           := Duration'Last;
      Groups            : Group_Set          := Empty_Group;
      Timeout           : Duration           := Default.Send_Timeout)
   is
      Holder : Client_Holder_Access :=
                 To_Holder (Socket, Environment, Kind, Groups, Timeout);
   begin
      Register
        (Server,
         Client_Id,
         Holder,
         Data_Chunk (Holder, Init_Data, Init_Content_Type),
         Init_Content_Type,
         Duplicated_Age,
         Ext_Sock_Alloc => True);
   end Register;

   procedure Register
     (Server            : in out Object;
      Client_Id         : Client_Key;
      Socket            : Net.Socket_Type'Class;
      Environment       : Client_Environment;
      Init_Data         : Client_Output_Type;
      Init_Content_Type : String             := "";
      Kind              : Mode               := Plain;
      Duplicated_Age    : Duration           := Duration'Last;
      Groups            : Group_Set          := Empty_Group;
      Timeout           : Duration           := Default.Send_Timeout)
   is
      Holder : Client_Holder_Access :=
                 To_Holder
                   (new Socket_Type'Class'(Socket), Environment, Kind, Groups,
                    Timeout);
   begin
      Register
        (Server,
         Client_Id,
         Holder,
         Data_Chunk (Holder, Init_Data, Init_Content_Type),
         Init_Content_Type,
         Duplicated_Age,
         Ext_Sock_Alloc => False);
   end Register;

   procedure Register
     (Server         : in out Object;
      Client_Id      : Client_Key;
      Socket         : Net.Socket_Type'Class;
      Environment    : Client_Environment;
      Content_Type   : String             := "";
      Kind           : Mode               := Plain;
      Duplicated_Age : Duration           := Duration'Last;
      Groups         : Group_Set          := Empty_Group;
      Timeout        : Duration           := Default.Send_Timeout)
   is
      Holder : Client_Holder_Access :=
                 To_Holder
                   (new Socket_Type'Class'(Socket), Environment, Kind, Groups,
                    Timeout);
   begin
      Register
        (Server, Client_Id, Holder, (1 .. 0 => 0), Content_Type,
         Duplicated_Age, Ext_Sock_Alloc => False);
   end Register;

   -------------
   -- Release --
   -------------

   procedure Release
     (Server         : in out Object;
      Close_Sockets  : Boolean;
      Left_Open      : Boolean;
      Get_Final_Data : access function
                         (Holder : not null access Client_Holder)
                          return Stream_Element_Array := null)
   is
      Queue  : Tables.Map;
      Holder : Client_Holder_Access;
   begin
      Server.Unregister_Clients (Queue, Open => Left_Open);

      for Cursor in Queue.Iterate loop
         Holder := Tables.Element (Cursor);

         if Holder.Phase /= Available then
            Waiter_Command (Server, Holder, Remove);
         end if;
      end loop;

      if not Wait_Send_Completion (10.0) then
         Internal_Error_Handler ("Could not clear server push waiter");
      end if;

      for Cursor in Queue.Iterate loop
         Holder := Tables.Element (Cursor);

         if Get_Final_Data /= null then
            declare
               Data : Stream_Element_Array (1 .. 8196);
               Last : Stream_Element_Offset;
            begin
               loop
                  Get_Data (Holder.all, Data, Last);
                  exit when Last < 1;
                  Holder.Socket.Send (Data (1 .. Last));
               end loop;

               Holder.Socket.Send (Get_Final_Data (Holder));
            exception
               when Net.Socket_Error =>
                  null;
            end;
         end if;

         if Close_Sockets then
            Holder.Socket.Shutdown;
         end if;

         Free (Holder);
      end loop;
   end Release;

   -------------
   -- Restart --
   -------------

   procedure Restart (Server : in out Object) is
   begin
      Server.Restart;
   end Restart;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server       : in out Object;
      Data         : Client_Output_Type;
      Group_Id     : String             := "";
      Content_Type : String             := "";
      Thin_Id      : String             := "";
      Client_Gone  : access procedure (Client_Id : String) := null)
   is
      Holder : Client_Holder_Access;
      Queue  : Tables.Map;
      WQ     : Waiter_Queues.List;
   begin
      Server.Send (Data, Group_Id, Content_Type, Thin_Id, Queue);

      for Cursor in Queue.Iterate loop
         Holder := Tables.Element (Cursor);

         if Holder.Errmsg = Null_Unbounded_String then
            WQ.Append ((Server'Unrestricted_Access, Holder, Add));

         else
            declare
               Client_Id : constant String := Tables.Key (Cursor);
            begin
               if Client_Gone /= null then
                  Client_Gone (Client_Id);
               end if;

               Holder.Socket.Shutdown;

               if Holder.Phase /= Available then
                  raise Program_Error with
                    Holder.Phase'Img & ' ' & To_String (Holder.Errmsg);
               end if;

               Free (Holder);
            end;
         end if;
      end loop;

      if not WQ.Is_Empty then
         Waiter_Queue.Add (WQ);
         Waiter_Signal;
      end if;
   end Send;

   ------------
   -- Send_G --
   ------------

   procedure Send_G
     (Server       : in out Object;
      Data         : Client_Output_Type;
      Group_Id     : String             := "";
      Content_Type : String             := "";
      Thin_Id      : String             := "")
   is
      procedure Gone (Client_Id : String);

      ----------
      -- Gone --
      ----------

      procedure Gone (Client_Id : String) is
      begin
         Client_Gone (Client_Id);
      end Gone;

   begin
      Send (Server, Data, Group_Id, Content_Type, Thin_Id, Gone'Access);
   end Send_G;

   -------------
   -- Send_To --
   -------------

   procedure Send_To
     (Server       : in out Object;
      Client_Id    : Client_Key;
      Data         : Client_Output_Type;
      Content_Type : String             := "";
      Thin_Id      : String             := "")
   is
      Holder : Client_Holder_Access;
   begin
      Server.Send_To (Client_Id, Data, Content_Type, Thin_Id, Holder);

      if Holder /= null then
         if Holder.Errmsg /= Null_Unbounded_String then
            declare
               Errmsg : constant String := To_String (Holder.Errmsg);
            begin
               Holder.Socket.Shutdown;

               if Holder.Phase /= Available then
                  raise Program_Error with Holder.Phase'Img & ' ' & Errmsg;
               end if;

               Free (Holder);
               raise Client_Gone with Errmsg;
            end;
         end if;

         Waiter_Command (Server, Holder, Add);
      end if;
   end Send_To;

   --------------------------------
   -- Set_Internal_Error_Handler --
   --------------------------------

   procedure Set_Internal_Error_Handler (Handler : Error_Handler) is
   begin
      Internal_Error_Handler := Handler;
   end Set_Internal_Error_Handler;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Server : in out Object; Close_Sockets : Boolean := True) is
   begin
      Release (Server, Close_Sockets, Left_Open => False);
   end Shutdown;

   procedure Shutdown
     (Server             : in out Object;
      Final_Data         : Client_Output_Type;
      Final_Content_Type : String             := "")
   is
      function Get_Final_Data
        (Holder : not null access Client_Holder) return Stream_Element_Array;

      --------------------
      -- Get_Final_Data --
      --------------------

      function Get_Final_Data
        (Holder : not null access Client_Holder) return Stream_Element_Array is
      begin
         return Data_Chunk (Holder, Final_Data, Final_Content_Type);
      end Get_Final_Data;

   begin
      Release
        (Server,
         Close_Sockets  => True,
         Left_Open      => False,
         Get_Final_Data => Get_Final_Data'Access);
   end Shutdown;

   -----------------------
   -- Shutdown_If_Empty --
   -----------------------

   procedure Shutdown_If_Empty (Server : in out Object; Open : out Boolean) is
   begin
      Server.Shutdown_If_Empty (Open);
   end Shutdown_If_Empty;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (Server : in out Object; Client_Id : Client_Key; Group_Id : String) is
   begin
      Server.Subscribe (Client_Id, Group_Id);
   end Subscribe;

   --------------------
   -- Subscribe_Copy --
   --------------------

   procedure Subscribe_Copy
     (Server : in out Object; Source : String; Target : String) is
   begin
      Server.Subscribe_Copy (Source => Source, Target => Target);
   end Subscribe_Copy;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder
     (Socket      : Net.Socket_Access;
      Environment : Client_Environment;
      Kind        : Mode;
      Groups      : Group_Set;
      Timeout     : Duration) return Client_Holder_Access
   is
      Holder_Groups : Group_Sets.Set;
   begin
      for J in Groups'Range loop
         Holder_Groups.Insert (To_String (Groups (J)));
      end loop;

      return new Client_Holder'
        (Kind        => Kind,
         Environment => Environment,
         Created     => Real_Time.Clock,
         Socket      => Socket,
         Groups      => Holder_Groups,
         Chunks      => <>,
         Chunk_Sent  => 0,
         Thin        => <>,
         Phase       => Available,
         Timeout     => Real_Time.To_Time_Span (Timeout),
         Errmsg      => <>);
   end To_Holder;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Server       : in out Object;
      Client_Id    : Client_Key;
      Close_Socket : Boolean    := True)
   is
      Holder : Client_Holder_Access;
   begin
      Server.Unregister (Client_Id, Holder);

      if Holder = null then
         return;
      end if;

      if Holder.Phase = Available then
         if Close_Socket then
            Holder.Socket.Shutdown;
         end if;

         Free (Holder);

      elsif Close_Socket then
         Waiter_Command (Server, Holder, Shutdown);
      else
         Waiter_Command (Server, Holder, Deallocate);
      end if;

   end Unregister;

   ------------------------
   -- Unregister_Clients --
   ------------------------

   procedure Unregister_Clients
     (Server : in out Object; Close_Sockets : Boolean := True) is
   begin
      Release (Server, Close_Sockets, Left_Open => True);
   end Unregister_Clients;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe
     (Server : in out Object; Client_Id : Client_Key; Group_Id : String) is
   begin
      Server.Unsubscribe (Client_Id, Group_Id);
   end Unsubscribe;

   ----------------------
   -- Unsubscribe_Copy --
   ----------------------

   procedure Unsubscribe_Copy
     (Server : in out Object; Source : String; Target : String) is
   begin
      Server.Unsubscribe_Copy (Source => Source, Target => Target);
   end Unsubscribe_Copy;

   --------------------------
   -- Wait_Send_Completion --
   --------------------------

   function Wait_Send_Completion (Timeout : Duration) return Boolean is
   begin
      select
         Waiter_Information.Empty;
         return True;
      or
         delay Timeout;
         return False;
      end select;
   end Wait_Send_Completion;

   ------------
   -- Waiter --
   ------------

   task body Waiter is

      type Client_In_Wait is record
         SP  : Object_Access;
         CH  : Client_Holder_Access;
         Exp : Real_Time.Time;
      end record;

      package Write_Sets is new AWS.Net.Generic_Sets (Client_In_Wait);

      Write_Set : Write_Sets.Socket_Set_Type;

      use Real_Time;
      use Write_Sets;

      R_Signal : aliased Net.Socket_Type'Class := Socket (Security => False);
      Bytes    : Stream_Element_Array (1 .. 32);
      B_Last   : Stream_Element_Offset;

      Queue      : Waiter_Queues.List;
      Queue_Item : Waiter_Queue_Element;
      Counter    : Wait_Counter_Type := 0;

      procedure Add_Item
        (Server : Object_Access; Holder : Client_Holder_Access);

      procedure Remove_Processing;

      procedure Wait_On_Sockets;

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item
        (Server : Object_Access; Holder : Client_Holder_Access) is
      begin
         if Holder.Phase /= Going then
            raise Program_Error with Phase_Type'Image (Holder.Phase);
         end if;

         Holder.Phase := Waiting;

         Add
           (Set    => Write_Set,
            Socket => Holder.Socket,
            Data   => (Server, Holder, Clock + Holder.Timeout),
            Mode   => Write_Sets.Both);

         Counter := Counter + 1;
      end Add_Item;

      -----------------------
      -- Remove_Processing --
      -----------------------

      procedure Remove_Processing is
      begin
         case Queue_Item.Action is
            when Add        => raise Program_Error;
            when Remove     => null;
            when Deallocate => Free (Queue_Item.Holder);
            when Shutdown   =>
               Queue_Item.Holder.Socket.Shutdown;
               Free (Queue_Item.Holder);
         end case;
      end Remove_Processing;

      ---------------------
      -- Wait_On_Sockets --
      ---------------------

      procedure Wait_On_Sockets is
      begin
         Wait (Write_Set, Timeout => Duration'Last);
      exception
         when E : Socket_Error =>
            --  !!! Most possible it is ENOBUFS or ENOMEM error
            --  because of too many open sockets. We should see the exact
            --  error in the socket error log file.
            --  We should close sockets in waiter because server has a
            --  lack of socket resources and clients in other peer of
            --  those socket is slow receiving the socket data.

            declare
               Message : constant String :=
                           "Clear waiter because of: " & Exception_Message (E);

               procedure Process
                 (Socket : in out Socket_Type'Class;
                  Client : in out Client_In_Wait);

               -------------
               -- Process --
               -------------

               procedure Process
                 (Socket : in out Socket_Type'Class;
                  Client : in out Client_In_Wait) is
               begin
                  Client.SP.Waiter_Error (Client.CH, Message);
                  Net.Log.Error (Socket, Message);
               end Process;

            begin
               for J in reverse 2 .. Count (Write_Set) loop
                  Update_Socket (Write_Set, J, Process'Access);
                  Remove_Socket (Write_Set, J);
               end loop;
            end;
      end Wait_On_Sockets;

   begin -- Waiter
      Net.Socket_Pair (R_Signal, W_Signal);
      Add (Write_Set, R_Signal'Unchecked_Access, Mode => Write_Sets.Input);

      Waiter_Pause;

      loop
         Wait_On_Sockets;

         if Is_Read_Ready (Write_Set, 1) then
            Net.Receive (R_Signal, Bytes, B_Last);
            Waiter_Queue.Get (Queue);
         end if;

         while not Queue.Is_Empty loop
            Queue_Item := Queue.First_Element;
            Queue.Delete_First;

            if Queue_Item.Action = Add then
               if Queue_Item.Holder = null
                 and then Queue_Item.Server = null
               then
                  select
                     accept Resume;
                  or
                     terminate;
                  end select;

               else
                  Add_Item (Queue_Item.Server, Queue_Item.Holder);
               end if;

            else -- Remove | Shutdown | Deallocate
               case Queue_Item.Holder.Phase is
                  when Available =>
                     --  Socket already gone, remove processing only

                     Remove_Processing;

                  when Going =>
                     --  Socket on the way to waiter, move back to waiter queue
                     Waiter_Queue.Add (Queue_Item);

                  when Waiting =>
                     for J in reverse 2 .. Count (Write_Set) loop
                        if Get_Socket (Write_Set, J).Get_FD
                          = Queue_Item.Holder.Socket.Get_FD
                        then
                           declare
                              Socket : Net.Socket_Access;

                              procedure Process
                                (Socket : in out Socket_Type'Class;
                                 Client : in out Client_In_Wait);

                              -------------
                              -- Process --
                              -------------

                              procedure Process
                                (Socket : in out Socket_Type'Class;
                                 Client : in out Client_In_Wait)
                              is
                                 pragma Unreferenced (Socket);
                              begin
                                 if Client.SP /= Queue_Item.Server
                                   or else Client.CH /= Queue_Item.Holder
                                 then
                                    raise Program_Error with
                                      "Broken data in waiter";
                                 end if;

                                 --  We could write to phase directly because
                                 --  Holder have to be out of protected object.

                                 Queue_Item.Holder.Phase := Available;
                              end Process;

                           begin
                              Update_Socket (Write_Set, J, Process'Access);
                              Remove_Socket (Write_Set, J, Socket);

                              if Socket /= Queue_Item.Holder.Socket then
                                 raise Program_Error with
                                   "Broken socket in waiter";
                              end if;

                              Remove_Processing;

                              Queue_Item.Holder := null; -- To check integrity

                              exit;
                           end;

                        elsif
                          Get_Data (Write_Set, J).CH = Queue_Item.Holder
                        then
                           raise Program_Error with "Broken holder";
                        end if;
                     end loop;

                     if Queue_Item.Holder /= null then
                        Internal_Error_Handler
                          ("Error: removing server push socket not found");
                     end if;
               end case;
            end if; -- end Remove | Shutdown | Deallocate
         end loop;

         for J in reverse 2 .. Count (Write_Set) loop
            declare
               Remove : Boolean := False;
               Error  : Unbounded_String;
               Err_SP : Object_Access;
               Err_CH : Client_Holder_Access;

               procedure Process
                 (Socket : in out Socket_Type'Class;
                  Client : in out Client_In_Wait);

               -------------
               -- Process --
               -------------

               procedure Process
                 (Socket : in out Socket_Type'Class;
                  Client : in out Client_In_Wait)
               is
                  Data : Stream_Element_Array (1 .. 8192);
                  Last : Stream_Element_Offset;

                  procedure Socket_Error (Message : String);
                  procedure Socket_Error_Log (Message : String);

                  ------------------
                  -- Socket_Error --
                  ------------------

                  procedure Socket_Error (Message : String) is
                  begin
                     Error  := To_Unbounded_String (Message);
                     Err_SP := Client.SP;
                     Err_CH := Client.CH;
                     Remove := True;
                  end Socket_Error;

                  ----------------------
                  -- Socket_Error_Log --
                  ----------------------

                  procedure Socket_Error_Log (Message : String) is
                  begin
                     Net.Log.Error (Socket, Message);
                     Socket_Error (Message);
                  end Socket_Error_Log;

               begin
                  if Client.CH.Phase /= Waiting then
                     raise Program_Error with Client.CH.Phase'Img;
                  end if;

                  if Is_Error (Write_Set, J) then
                     Socket_Error_Log
                       ("Waiter socket error " & Utils.Image (Socket.Errno));

                  elsif Is_Read_Ready (Write_Set, J) then
                     begin
                        Socket.Receive (Data, Last);
                        Socket_Error_Log
                          ("Unexpected input SP data: "
                           & Translator.To_String (Data (1 .. Last)));
                     exception
                        when E : Net.Socket_Error =>
                           Socket_Error (Exception_Message (E));
                     end;

                  elsif Is_Write_Ready (Write_Set, J) then
                     Client.SP.Get_Data (Client.CH, Data, Last);

                     if Last >= Data'First then
                        begin
                           Socket.Send (Data (1 .. Last));
                           Client.Exp := Clock + Client.CH.Timeout;
                        exception
                           when E : Net.Socket_Error =>
                              Socket_Error (Exception_Message (E));
                        end;
                     else
                        Remove := True;
                     end if;

                  elsif Client.Exp < Clock then
                     Socket_Error_Log
                       ("Wait for write availability timeout "
                        & Utils.Significant_Image
                            (Real_Time.To_Duration (Client.CH.Timeout), 3));
                  end if;
               end Process;

            begin
               Update_Socket (Write_Set, J, Process'Access);

               if Remove then
                  if Err_SP /= null then
                     Err_SP.Waiter_Error (Err_CH, To_String (Error));
                  end if;

                  Remove_Socket (Write_Set, J);
               end if;
            end;
         end loop;

         --  Set size after receiving loop because most important to know about
         --  empty waiter than maximum size.

         Waiter_Information.Set_Size (Integer (Count (Write_Set)), Counter);
      end loop;

   exception
      when E : others =>
         Internal_Error_Handler
           ("Server push broken, " & Exception_Information (E));
   end Waiter;

   --------------------
   -- Waiter_Command --
   --------------------

   procedure Waiter_Command
     (Server : in out Object;
      Holder : Client_Holder_Access;
      Action : Action_Type) is
   begin
      Waiter_Queue.Add
        (Waiter_Queue_Element'(Server'Unrestricted_Access, Holder, Action));
      Waiter_Signal;
   end Waiter_Command;

   ------------------
   -- Waiter_Queue --
   ------------------

   protected body Waiter_Queue is

      ---------
      -- Add --
      ---------

      procedure Add (Item : Waiter_Queue_Element) is
      begin
         Queue.Append (Item);
      end Add;

      procedure Add (Queue : Waiter_Queues.List) is
      begin
         for Item of Add.Queue loop
            Waiter_Queue.Queue.Append (Item);
         end loop;
      end Add;

      ---------
      -- Get --
      ---------

      procedure Get (Queue : out Waiter_Queues.List) is
      begin
         Get.Queue := Waiter_Queue.Queue;

         Waiter_Queue.Queue.Clear;
      end Get;

   end Waiter_Queue;

   ------------------------
   -- Waiter_Information --
   ------------------------

   protected body Waiter_Information is

      ----------
      -- Info --
      ----------

      procedure Info
        (Size        : out Natural;
         Max_Size    : out Natural;
         Max_Size_DT : out Calendar.Time;
         Counter     : out Wait_Counter_Type) is
      begin
         Info.Size        := Waiter_Information.Size - 1;
         Info.Max_Size    := Waiter_Information.Max_Size - 1;
         Info.Max_Size_DT := Waiter_Information.Max_Size_DT;
         Info.Counter     := Waiter_Information.Counter;
      end Info;

      -----------
      -- Empty --
      -----------

      entry Empty when Size <= 1 is
      begin
         null;
      end Empty;

      --------------
      -- Set_Size --
      --------------

      procedure Set_Size (Size : Positive; Counter : Wait_Counter_Type) is
      begin
         Waiter_Information.Counter := Set_Size.Counter;
         Waiter_Information.Size    := Set_Size.Size;

         if Size > Max_Size then
            Max_Size := Size;
            Max_Size_DT := Calendar.Clock;
         end if;
      end Set_Size;

   end Waiter_Information;

   ------------------
   -- Waiter_Pause --
   ------------------

   procedure Waiter_Pause is
   begin
      Push.No_Servers.all;

      --  Waiter_Queue_Element' is necessary to workaround GNAT-2010 bug

      Waiter_Queue.Add (Waiter_Queue_Element'(null, null, Add));
      Waiter_Signal;
   end Waiter_Pause;

   -------------------
   -- Waiter_Resume --
   -------------------

   procedure Waiter_Resume is
   begin
      Push.First_Server.all;

      select
         Waiter.Resume;
      or delay 10.0;
         raise Program_Error with "Could not resume server push waiter";
      end select;
   end Waiter_Resume;

   -------------------
   -- Waiter_Signal --
   -------------------

   procedure Waiter_Signal is
   begin
      W_Signal.Send (Byte0);
   end Waiter_Signal;

begin
   Push.No_Servers   := Server.No_Servers;
   Push.First_Server := Server.First_Server;

   Server.No_Servers   := Waiter_Pause'Access;
   Server.First_Server := Waiter_Resume'Access;
end AWS.Server.Push;
