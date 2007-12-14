------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2007                          --
--                                 AdaCore                                  --
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

with Ada.Calendar;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.Generic_Sets;
with AWS.Net.Log;
with AWS.Translator;
with AWS.Utils;

with GNAT.Calendar.Time_IO;

package body AWS.Server.Push is

   use AWS.Net;

   type Phase_Type is (Available, Going, Waiting);
   --  Available when the socket is not in the waiting poll.
   --  Going when the socket is in process to be placed into waiting poll.
   --  Waiting when the socket is in the waiting poll.

   type Client_Holder is record
      Socket      : Net.Socket_Access;
      Kind        : Mode;
      Created     : Ada.Calendar.Time;
      Environment : Client_Environment;
      Groups      : Group_Sets.Set;
      Chunks      : Chunk_Lists.List;
      Thin        : Thin_Indexes.Map;
      Phase       : Phase_Type;
      Timeout     : Ada.Real_Time.Time_Span;
      Errmsg      : Unbounded_String; -- Filled on socket error in waiter
   end record;

   function To_Holder
     (Socket      : in Net.Socket_Type'Class;
      Environment : in Client_Environment;
      Kind        : in Mode;
      Groups      : in Group_Set;
      Timeout     : in Duration) return Client_Holder_Access;

   procedure Free (Holder : in out Client_Holder_Access);

   procedure Free is new Ada.Unchecked_Deallocation (Tables.Map, Map_Access);

   procedure Release
     (Server         : in out Object;
      Close_Sockets  : in     Boolean;
      Left_Open      : in     Boolean;
      Get_Final_Data : access function
                                (Holder : in Client_Holder)
                                 return Stream_Element_Array := null);

   procedure Add_To_Groups
     (Groups     : in out Group_Map;
      Group_Name : in     String;
      Client_Id  : in     String;
      Holder     : in     Client_Holder_Access);

   procedure Register
     (Server            : in out Object;
      Client_Id         : in     Client_Key;
      Holder            : in out Client_Holder_Access;
      Init_Data         : in     Stream_Element_Array;
      Duplicated_Age    : in     Duration);
   --  Internal register routine

   function Data_Chunk
     (Holder       : in Client_Holder;
      Data         : in Client_Output_Type;
      Content_Type : in String) return Stream_Element_Array;

   procedure Get_Data
     (Holder : in out Client_Holder;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   New_Line : constant String := ASCII.CR & ASCII.LF;
   --  HTTP new line

   Byte0 : constant Stream_Element_Array := (1 => 0);

   Boundary  : constant String := "AWS.Push.Boundary_"
     & GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%s");
   --  This is the multi-part boundary string used by AWS push server

   Delimiter : constant String := "--" & Boundary & New_Line;
   --  And the corresponding delimiter

   W_Signal : aliased Net.Socket_Type'Class := Net.Socket (Security => False);
   --  Socket to signal waiter to do to select task entries

   type Object_Access is access all Object;

   task Waiter is
      entry Add (Server : in Object_Access; Holder : in Client_Holder_Access);

      entry Add (Server : in Object_Access; Queue : in Tables.Map);

      entry Remove
        (Server : in Object_Access; Holder : in Client_Holder_Access);
      --  Socket should be appropriate and only for error control

      entry Info (Size : out Natural; Counter : out Wait_Counter_Type);

      entry Empty;
   end Waiter;

   -------------------
   -- Add_To_Groups --
   -------------------

   procedure Add_To_Groups
     (Groups     : in out Group_Map;
      Group_Name : in     String;
      Client_Id  : in     String;
      Holder     : in     Client_Holder_Access)
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

   function Count (Server : in Object) return Natural is
   begin
      return Server.Count;
   end Count;

   ----------------
   -- Data_Chunk --
   ----------------

   function Data_Chunk
     (Holder       : in Client_Holder;
      Data         : in Client_Output_Type;
      Content_Type : in String) return Stream_Element_Array
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
               return Delimiter & Messages.Content_Type (Content_Type)
                 & New_Line & New_Line;

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
               return New_Line & New_Line;

            when Chunked =>
               return New_Line;

            when Plain =>
               return "";
         end case;
      end Suffix;

   begin
      return Translator.To_Stream_Element_Array (Prefix) & Data_To_Send
            & Translator.To_Stream_Element_Array (Suffix);
   end Data_Chunk;

   ----------
   -- Free --
   ----------

   procedure Free (Holder : in out Client_Holder_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Client_Holder, Client_Holder_Access);
   begin
      Net.Free (Holder.Socket);
      Deallocate (Holder);
   end Free;

   --------------
   -- Get_Data --
   --------------

   procedure Get_Data
     (Holder : in out Client_Holder;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
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
            exit when Next > Data'Last;

            Data (Last + 1 .. Next) := Message.Data;
            Last := Next;

            if Message.Thin /= "" then
               Holder.Thin.Delete (Message.Thin);
            end if;

            Holder.Chunks.Delete (C);
            C := Holder.Chunks.First;
         end;
      end loop;

      if Last < Data'First and then Chunk_Lists.Has_Element (C) then
         raise Constraint_Error with "Too big message.";
      end if;
   end Get_Data;

   ----------
   -- Info --
   ----------

   procedure Info (Size : out Natural; Counter : out Wait_Counter_Type) is
   begin
      W_Signal.Send (Byte0);
      Waiter.Info (Size, Counter => Counter);
   end Info;

   procedure Info
     (Server  : in out Object;
      Clients : out    Natural;
      Groups  : out    Natural;
      Process : access procedure
                  (Client_Id   : in Client_Key;
                   Address     : in String;
                   State       : in String;
                   Environment : in Client_Environment;
                   Kind        : in Mode;
                   Groups      : in Group_Set) := null) is
   begin
      Server.Info (Clients, Group_Count => Groups, Process => Process);
   end Info;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Server : in Object) return Boolean is
   begin
      return Server.Is_Open;
   end Is_Open;

   ------------
   -- Object --
   ------------

   protected body Object is

      procedure Send_Data
        (Holder       : in out Client_Holder_Access;
         Data         : in     Client_Output_Type;
         Content_Type : in     String;
         Thin_Id      : in     String);
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
        (Holder : in     Client_Holder_Access;
         Data   :    out Stream_Element_Array;
         Last   :    out Stream_Element_Offset) is
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
                          (Client_Id   : in Client_Key;
                           Address     : in String;
                           State       : in String;
                           Environment : in Client_Environment;
                           Kind        : in Mode;
                           Groups      : in Group_Set))
      is
         procedure Action (C : Tables.Cursor);

         procedure Action (C : Tables.Cursor) is
            CA     : constant Client_Holder_Access := Tables.Element (C);
            Groups : Group_Set (1 .. Integer (CA.Groups.Length));
            CG     : Group_Sets.Cursor := CA.Groups.First;
            G      : Group_Maps.Cursor;

            function Get_Peer_Addr return String;

            function Get_Peer_Addr return String is
            begin
               return Peer_Addr (CA.Socket.all);
            exception
               when E : Net.Socket_Error =>
                  return Ada.Exceptions.Exception_Message (E);
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
                    & " for client.";
               end if;

               if Group_Maps.Element (G).Element (Tables.Key (C)) /= CA then
                  raise Program_Error with "loose client in group.";
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

         C  : Tables.Cursor := Container.First;
         G  : Group_Maps.Cursor;

      begin
         Container.Iterate (Action'Access);

         G := Groups.First;

         while Group_Maps.Has_Element (G) loop
            C := Group_Maps.Element (G).First;

            while Tables.Has_Element (C) loop
               if not Tables.Element (C).Groups.Contains
                        (Group_Maps.Key (G))
               then
                  raise Program_Error with "loose group in client.";
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
        (Client_Id      : in     Client_Key;
         Holder         : in out Client_Holder_Access;
         Duplicated     :    out Client_Holder_Access;
         Duplicated_Age : in     Duration)
      is
         use Ada.Calendar;

         Cursor  : Tables.Cursor;
         Success : Boolean;

         procedure Add_To_Groups (J : in Group_Sets.Cursor);

         -------------------
         -- Add_To_Groups --
         -------------------

         procedure Add_To_Groups (J : in Group_Sets.Cursor) is
         begin
            Add_To_Groups
              (Groups, Group_Sets.Element (J), Client_Id, Holder);
         end Add_To_Groups;

      begin
         if not Open then
            Free (Holder);
            raise Closed;
         end if;

         Container.Insert (Client_Id, Holder, Cursor, Success);

         if Success then
            Duplicated := null;
         else
            Duplicated := Tables.Element (Cursor);

            if Duplicated_Age < Clock - Duplicated.Created then
               Unregister (Cursor);
               Container.Insert (Client_Id, Holder);
            else
               Free (Holder);
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
        (Data         : in     Client_Output_Type;
         Group_Id     : in     String;
         Content_Type : in     String;
         Thin_Id      : in     String;
         Queue        :    out Tables.Map)
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
         Data         : in     Client_Output_Type;
         Content_Type : in     String;
         Thin_Id      : in     String)
      is
         Events : Net.Event_Set;

         procedure To_Buffer;

         procedure To_Buffer is
            CT : Thin_Indexes.Cursor;
            Chunk : constant Stream_Element_Array :=
                      Data_Chunk (Holder.all, Data, Content_Type);
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

            --  Net.Check is not blocking operation.

            Events :=
              Net.Check (Holder.Socket.all, (Output => True, Input => False));

            if Events (Error) then
               Holder.Errmsg :=
                 To_Unbounded_String
                   ("Check server push output error "
                    & Integer'Image (Net.Errno (Holder.Socket.all)));
               Net.Log.Error (Holder.Socket.all, To_String (Holder.Errmsg));

            elsif Events (Output) then
               declare
                  Chunk : constant Stream_Element_Array :=
                            Data_Chunk (Holder.all, Data, Content_Type);
                  Last : Stream_Element_Offset;
               begin
                  --  It is not blocking Net.Send operation.

                  Net.Send (Holder.Socket.all, Chunk, Last);

                  if Last = Chunk'Last then
                     --  Data sent completely

                     Holder := null;

                  elsif Last in Chunk'Range then
                     --  Data sent partionaly

                     Holder.Chunks.Append
                       ((Size      => Chunk'Last - Last,
                         Thin_Size => 0,
                         Data      => Chunk (Last + 1 .. Chunk'Last),
                         Thin      => ""));

                     Holder.Phase := Going;

                  else
                     --  Data not send. Strange, because Check routine did
                     --  show output availability.

                     To_Buffer;
                     Holder.Phase := Going;
                  end if;

               exception
                  when E : Net.Socket_Error =>
                     Holder.Errmsg :=
                       To_Unbounded_String
                         (Ada.Exceptions.Exception_Message (E));
               end;

            else
               To_Buffer;

               --  We would return Holder not null to move it to Waiter.

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
        (Client_Id    : in     Client_Key;
         Data         : in     Client_Output_Type;
         Content_Type : in     String;
         Thin_Id      : in     String;
         Holder       :    out Client_Holder_Access)
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
        (Final_Data         : in     Client_Output_Type;
         Final_Content_Type : in     String;
         Queue              :    out Tables.Map) is
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

      procedure Subscribe (Client_Id : in Client_Key; Group_Id : in String) is

         Cursor : constant Tables.Cursor := Container.Find (Client_Id);

         procedure Modify
           (Key : in String; Element : in out Client_Holder_Access);

         ------------
         -- Modify --
         ------------

         procedure Modify
           (Key : in String; Element : in out Client_Holder_Access)
         is
            CI      : Group_Sets.Cursor;
            Success : Boolean;
         begin
            Element.Groups.Insert (Group_Id, CI, Success);

            if Success then
               Add_To_Groups (Groups, Group_Id, Key, Element);
            end if;
         end Modify;

      begin
         if Tables.Has_Element (Cursor) then
            Container.Update_Element (Cursor, Modify'Access);
         else
            raise Client_Gone with "No such client id.";
         end if;
      end Subscribe;

      --------------------
      -- Subscribe_Copy --
      --------------------

      procedure Subscribe_Copy (Source : in String; Target : in String) is
         CG : Group_Maps.Cursor;

         procedure Process (C : in Tables.Cursor);

         -------------
         -- Process --
         -------------

         procedure Process (C : in Tables.Cursor) is
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
               Free (Map);
            end if;
         end Delete_Group;

      begin
         Container.Delete (Cursor);
         Holder.Groups.Iterate (Delete_Group'Access);
      end Unregister;

      procedure Unregister
        (Client_Id : in Client_Key; Holder : out Client_Holder_Access)
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

      procedure Unregister_Clients
        (Queue : out Tables.Map; Open : in Boolean)
      is
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

      procedure Unsubscribe
        (Client_Id : in Client_Key; Group_Id : in String)
      is
         Cursor : constant Tables.Cursor := Container.Find (Client_Id);

         procedure Modify
           (Key : in String; Element : in out Client_Holder_Access);

         ------------
         -- Modify --
         ------------

         procedure Modify
           (Key : in String; Element : in out Client_Holder_Access)
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
                  Free (Group);
               end if;
            end if;
         end Modify;

      begin
         if Tables.Has_Element (Cursor) then
            Container.Update_Element (Cursor, Modify'Access);
         else
            raise Client_Gone with "No such client id.";
         end if;
      end Unsubscribe;

      ----------------------
      -- Unsubscribe_Copy --
      ----------------------

      procedure Unsubscribe_Copy (Source : in String; Target : in String) is
         use type Ada.Containers.Count_Type;

         CG : Group_Maps.Cursor := Groups.Find (Target);
         CF : Tables.Cursor;

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
            Free (Group);
         end if;

      end Unsubscribe_Copy;

      ------------------
      -- Waiter_Error --
      ------------------

      procedure Waiter_Error
        (Holder : in Client_Holder_Access; Message : in String) is
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
      Client_Id      : in     Client_Key;
      Holder         : in out Client_Holder_Access;
      Init_Data      : in     Stream_Element_Array;
      Duplicated_Age : in     Duration)
   is
      Duplicated : Client_Holder_Access;
   begin
      Server.Register (Client_Id, Holder, Duplicated, Duplicated_Age);

      if Duplicated /= null then
         if Duplicated.Phase /= Available then
            W_Signal.Send (Byte0);
            Waiter.Remove (Server'Unrestricted_Access, Duplicated);
         end if;

         Duplicated.Socket.Shutdown;
         Free (Duplicated);
      end if;

      begin
         Net.Buffered.Put_Line
            (Holder.Socket.all,
             "HTTP/1.1 200 OK" & New_Line
               & "Server: AWS (Ada Web Server) v" & Version & New_Line
               & Messages.Connection ("Close"));

         if Holder.Kind = Chunked then
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               Messages.Transfer_Encoding ("chunked") & New_Line);

         elsif Holder.Kind = Multipart then
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               Messages.Content_Type
                 (MIME.Multipart_X_Mixed_Replace, Boundary) & New_Line);

         else
            Net.Buffered.New_Line (Holder.Socket.all);
         end if;

         Net.Buffered.Write (Holder.Socket.all, Init_Data);
         Net.Buffered.Flush (Holder.Socket.all);

      exception
         when others =>
            Server.Unregister (Client_Id, Holder);
            Holder.Socket.Shutdown;
            Free (Holder);
            raise;
      end;

      W_Signal.Send (Byte0);

      Waiter.Add (Server'Unrestricted_Access, Holder);

      Socket_Taken;
   end Register;

   procedure Register
     (Server            : in out Object;
      Client_Id         : in     Client_Key;
      Socket            : in     Net.Socket_Type'Class;
      Environment       : in     Client_Environment;
      Init_Data         : in     Client_Output_Type;
      Init_Content_Type : in     String             := "";
      Kind              : in     Mode               := Plain;
      Duplicated_Age    : in     Duration           := Duration'Last;
      Groups            : in     Group_Set          := Empty_Group;
      Timeout           : in     Duration           := Default.Send_Timeout)
   is
      Holder : Client_Holder_Access :=
                 To_Holder (Socket, Environment, Kind, Groups, Timeout);
   begin
      Register
        (Server,
         Client_Id,
         Holder,
         Data_Chunk (Holder.all, Init_Data, Init_Content_Type),
         Duplicated_Age);
   end Register;

   procedure Register
     (Server         : in out Object;
      Client_Id      : in     Client_Key;
      Socket         : in     Net.Socket_Type'Class;
      Environment    : in     Client_Environment;
      Kind           : in     Mode               := Plain;
      Duplicated_Age : in     Duration           := Duration'Last;
      Groups         : in     Group_Set          := Empty_Group;
      Timeout        : in     Duration           := Default.Send_Timeout)
   is
      Holder : Client_Holder_Access :=
                 To_Holder (Socket, Environment, Kind, Groups, Timeout);
   begin
      Register (Server, Client_Id, Holder, (1 .. 0 => 0), Duplicated_Age);
   end Register;

   -------------
   -- Release --
   -------------

   procedure Release
     (Server         : in out Object;
      Close_Sockets  : in     Boolean;
      Left_Open      : in     Boolean;
      Get_Final_Data : access function
                                (Holder : in Client_Holder)
                                 return Stream_Element_Array := null)
   is
      Queue  : Tables.Map;
      C      : Tables.Cursor;
      Holder : Client_Holder_Access;
   begin
      Server.Unregister_Clients (Queue, Open => Left_Open);

      C := Queue.First;

      while Tables.Has_Element (C) loop
         Holder := Tables.Element (C);

         if Holder.Phase /= Available then
            W_Signal.Send (Byte0);
            Waiter.Remove (Server'Unrestricted_Access, Holder);
         end if;

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

               Holder.Socket.Send (Get_Final_Data (Holder.all));
            exception
               when Net.Socket_Error =>
                  null;
            end;
         end if;

         if Close_Sockets then
            Holder.Socket.Shutdown;
         end if;

         Free (Holder);

         C := Tables.Next (C);
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
      Data         : in     Client_Output_Type;
      Group_Id     : in     String             := "";
      Content_Type : in     String             := "";
      Thin_Id      : in     String             := "";
      Client_Gone  : access procedure (Client_Id : in String) := null)
   is
      use type Ada.Containers.Count_Type;
      Cursor : Tables.Cursor;
      C      : Tables.Cursor;
      Holder : Client_Holder_Access;
      Queue  : Tables.Map;

   begin
      Server.Send (Data, Group_Id, Content_Type, Thin_Id, Queue);

      Cursor := Queue.First;

      while Tables.Has_Element (Cursor) loop
         C      := Cursor;
         Cursor := Tables.Next (Cursor);
         Holder := Tables.Element (C);

         if Holder.Errmsg /= Null_Unbounded_String then
            declare
               Client_Id : constant String := Tables.Key (C);
            begin
               if Client_Gone /= null then
                  Client_Gone (Client_Id);
               end if;

               Holder.Socket.Shutdown;
               Queue.Delete (C);

               if Holder.Phase /= Available then
                  raise Program_Error with
                    Holder.Phase'Img & ' ' & To_String (Holder.Errmsg);
               end if;

               Free (Holder);
            end;
         end if;
      end loop;

      if Queue.Length > 0 then
         W_Signal.Send (Byte0);
         Waiter.Add (Server'Unrestricted_Access, Queue);
      end if;

   end Send;

   ------------
   -- Send_G --
   ------------

   procedure Send_G
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Group_Id     : in     String             := "";
      Content_Type : in     String             := "";
      Thin_Id      : in     String             := "")
   is
      procedure Gone (Client_Id : in String);

      ----------
      -- Gone --
      ----------

      procedure Gone (Client_Id : in String) is
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
      Client_Id    : in     Client_Key;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "";
      Thin_Id      : in     String             := "")
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

         W_Signal.Send (Byte0);

         Waiter.Add (Server'Unrestricted_Access, Holder);
      end if;
   end Send_To;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Server : in out Object; Close_Sockets : in Boolean := True) is
   begin
      Release (Server, Close_Sockets, Left_Open => False);
   end Shutdown;

   procedure Shutdown
     (Server             : in out Object;
      Final_Data         : in     Client_Output_Type;
      Final_Content_Type : in     String             := "")
   is
      function Get_Final_Data
        (Holder : in Client_Holder) return Stream_Element_Array;

      --------------------
      -- Get_Final_Data --
      --------------------

      function Get_Final_Data
        (Holder : in Client_Holder) return Stream_Element_Array is
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
     (Server    : in out Object;
      Client_Id : in     Client_Key;
      Group_Id  : in     String) is
   begin
      Server.Subscribe (Client_Id, Group_Id);
   end Subscribe;

   --------------------
   -- Subscribe_Copy --
   --------------------

   procedure Subscribe_Copy
     (Server : in out Object; Source : in String; Target : in String) is
   begin
      Server.Subscribe_Copy (Source => Source, Target => Target);
   end Subscribe_Copy;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder
     (Socket      : in Net.Socket_Type'Class;
      Environment : in Client_Environment;
      Kind        : in Mode;
      Groups      : in Group_Set;
      Timeout     : in Duration) return Client_Holder_Access
   is
      Holder_Groups : Group_Sets.Set;
   begin
      for J in Groups'Range loop
         Holder_Groups.Insert (To_String (Groups (J)));
      end loop;

      return new Client_Holder'
        (Kind        => Kind,
         Environment => Environment,
         Created     => Ada.Calendar.Clock,
         Socket      => new Socket_Type'Class'(Socket),
         Groups      => Holder_Groups,
         Chunks      => <>,
         Thin        => <>,
         Phase       => Available,
         Timeout     => Ada.Real_Time.To_Time_Span (Timeout),
         Errmsg      => <>);
   end To_Holder;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Server       : in out Object;
      Client_Id    : in     Client_Key;
      Close_Socket : in     Boolean    := True)
   is
      Holder : Client_Holder_Access;
   begin
      Server.Unregister (Client_Id, Holder);

      if Holder = null then
         return;
      end if;

      if Holder.Phase /= Available then
         W_Signal.Send (Byte0);
         Waiter.Remove (Server'Unrestricted_Access, Holder);
      end if;

      if Close_Socket then
         Holder.Socket.Shutdown;
      end if;

      Free (Holder);
   end Unregister;

   ------------------------
   -- Unregister_Clients --
   ------------------------

   procedure Unregister_Clients
     (Server : in out Object; Close_Sockets : in Boolean := True) is
   begin
      Release (Server, Close_Sockets, Left_Open => True);
   end Unregister_Clients;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe
     (Server    : in out Object;
      Client_Id : in     Client_Key;
      Group_Id  : in     String) is
   begin
      Server.Unsubscribe (Client_Id, Group_Id);
   end Unsubscribe;

   ----------------------
   -- Unsubscribe_Copy --
   ----------------------

   procedure Unsubscribe_Copy
     (Server : in out Object; Source : in String; Target : in String) is
   begin
      Server.Unsubscribe_Copy (Source => Source, Target => Target);
   end Unsubscribe_Copy;

   ------------
   -- Waiter --
   ------------

   task body Waiter is

      type Client_In_Wait is record
         SP  : Object_Access;
         CH  : Client_Holder_Access;
         Exp : Ada.Real_Time.Time;
      end record;

      package Write_Sets is new AWS.Net.Generic_Sets (Client_In_Wait);

      Write_Set : Write_Sets.Socket_Set_Type;

      use Write_Sets;
      use Ada.Real_Time;

      R_Signal : aliased Net.Socket_Type'Class :=
                   Net.Socket (Security => False);
      Byte    : Stream_Element_Array (1 .. 1);
      pragma Warnings (Off, Byte);
      Counter : Wait_Counter_Type := 0;

      Receive_Signal : Boolean;

      procedure Add_Item
        (Server : in Object_Access; Holder : in Client_Holder_Access);

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item
        (Server : in Object_Access; Holder : in Client_Holder_Access) is
      begin
         if Holder.Phase /= Going then
            raise Program_Error with Phase_Type'Image (Holder.Phase);
         end if;

         Holder.Phase := Waiting;

         Add
           (Set    => Write_Set,
            Socket => Holder.Socket,
            Data   => (Server, Holder, Clock + Holder.Timeout),
            Mode   => Write_Sets.Output);

         Counter := Counter + 1;
      end Add_Item;

   begin
      Net.Socket_Pair (R_Signal, W_Signal);
      Add (Write_Set, R_Signal'Unchecked_Access, Mode => Write_Sets.Input);

      loop
         if Count (Write_Set) > 1 then
            Wait (Write_Set, Timeout => Duration'Last);
         end if;

         if Count (Write_Set) = 1 or else Is_Read_Ready (Write_Set, 1) then
            Receive_Signal := True;

            select
               accept Add
                 (Server : in Object_Access; Holder : in Client_Holder_Access)
               do
                  Add_Item (Server, Holder);
               end Add;

            or
               accept Add (Server : in Object_Access; Queue : Tables.Map) do
                  declare
                     C : Tables.Cursor := Queue.First;
                  begin
                     while Tables.Has_Element (C) loop
                        Add_Item (Server, Tables.Element (C));
                        C := Tables.Next (C);
                     end loop;
                  end;
               end Add;

            or
               accept Remove
                 (Server : in Object_Access; Holder : in Client_Holder_Access)
               do
                  case Holder.Phase is
                     when Going     => requeue Remove;
                     when Available => return;
                     when Waiting   => null;
                  end case;

                  for J in reverse 2 .. Count (Write_Set) loop
                     if Get_Socket (Write_Set, J).Get_FD
                        = Holder.Socket.Get_FD
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
                              pragma Unmodified (Client);
                           begin
                              if Client.SP /= Server
                                or else Client.CH /= Holder
                              then
                                 raise Program_Error
                                   with "Broken data in waiter.";
                              end if;

                              --  We could write to phase directly becuse
                              --  Holder have to be out of protected object.

                              Holder.Phase := Available;
                           end Process;

                        begin
                           Update_Socket (Write_Set, J, Process'Access);
                           Remove_Socket (Write_Set, J, Socket);

                           if Socket /= Holder.Socket then
                              raise Program_Error
                                with "Broken socket in waiter.";
                           end if;

                           exit;
                        end;

                     elsif Get_Data (Write_Set, J).CH = Holder then
                        raise Program_Error with "Broken holder.";
                     end if;
                  end loop;
               end Remove;

            or
               accept Info
                 (Size : out Natural; Counter : out Wait_Counter_Type)
               do
                  Size := Integer (Count (Write_Set) - 1);
                  Info.Counter := Waiter.Counter;
               end Info;

            or
               when Count (Write_Set) <= 1 => accept Empty;
               Receive_Signal := False;
            or
               terminate;
            end select;

            if Receive_Signal then
               Byte := Net.Receive (R_Signal, 1);
            end if;
         end if;

         for J in reverse 2 .. Count (Write_Set) loop
            declare
               Remove : Boolean := False;

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

                  procedure Socket_Error (Message : in String);
                  procedure Socket_Error_Log (Message : in String);

                  ------------------
                  -- Socket_Error --
                  ------------------

                  procedure Socket_Error (Message : in String) is
                  begin
                     Client.SP.Waiter_Error (Client.CH, Message);
                     Remove := True;
                  end Socket_Error;

                  procedure Socket_Error_Log (Message : in String) is
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

                  elsif Is_Write_Ready (Write_Set, J) then
                     Client.SP.Get_Data (Client.CH, Data, Last);

                     if Last >= Data'First then
                        begin
                           Socket.Send (Data (1 .. Last));
                           Client.Exp := Clock + Client.CH.Timeout;
                        exception
                           when E : Net.Socket_Error =>
                              Socket_Error
                                (Ada.Exceptions.Exception_Message (E));
                        end;
                     else
                        Remove := True;
                     end if;

                  elsif Client.Exp < Clock then
                     Socket_Error_Log
                       ("Wait for write availability timeout "
                        & Utils.Significant_Image
                            (Ada.Real_Time.To_Duration (Client.CH.Timeout),
                             3));
                  end if;
               end Process;

            begin
               Update_Socket (Write_Set, J, Process'Access);

               if Remove then
                  Remove_Socket (Write_Set, J);
               end if;
            end;
         end loop;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Server push broken, " & Ada.Exceptions.Exception_Information (E));
   end Waiter;

   --------------------------
   -- Wait_Send_Completion --
   --------------------------

   function Wait_Send_Completion (Timeout : in Duration) return Boolean is
   begin
      select
         Waiter.Empty;
         return True;
      or
         delay Timeout;
         return False;
      end select;
   end Wait_Send_Completion;

end AWS.Server.Push;
