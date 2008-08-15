------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;        use Ada.Streams;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Default;
with AWS.Containers.Key_Value;
with AWS.Utils;

package body AWS.Session is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   SID_Prefix             : constant String := "SID-";

   Session_Check_Interval : Duration
     := Default.Session_Cleanup_Interval;
   --  Check for obsolete section every 10 minutes

   Session_Lifetime       : Duration := Default.Session_Lifetime;
   --  A session is obsolete if not used after Session_Lifetime seconds

   package Key_Value renames Containers.Key_Value;
   type Key_Value_Set_Access is access Key_Value.Map;

   procedure Free is new Ada.Unchecked_Deallocation
     (Key_Value.Map, Key_Value_Set_Access);

   --  table of session ID

   type Session_Node is record
      Time_Stamp : Calendar.Time;
      Root       : Key_Value_Set_Access;
   end record;

   package Session_Set is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Session_Node, Ada.Strings.Hash, "=", "=");

   procedure Get_Node
     (Sessions : in out Session_Set.Map;
      SID      : in     Id;
      Node     :    out Session_Node;
      Found    :    out Boolean);
   --  Returns Node for specified SID, if found update the timestamp for
   --  this node and set Found to True, otherwise set Found to False.

   --------------------
   --  String stream --
   --------------------

   type String_Stream_Type is new Root_Stream_Type with record
      Str        : Unbounded_String;
      Read_Index : Natural := 1;
   end record;
   --  A stream that reads and writes to a string

   overriding procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out String_Stream_Type;
      Item   : in     Stream_Element_Array);
   --  See inherited documentation

   procedure Open (Stream : in out String_Stream_Type'Class; Str : in String);
   --  Open a new string. Str is the initial value of the string, to which will
   --  be appended the result of 'Output.

   -----------------
   -- Expired Set --
   -----------------

   --  This is used by the task cleaner, all SID to delete will be placed here
   --  temporarily. Note that these global value are thread safe. The data are
   --  initialized by Database.Clean. The set is used just after and the
   --  clean-up is done.

   Max_Expired : constant := 50;

   Expired_SID : array (1 .. Max_Expired) of Id;
   E_Index     : Natural := 0;

   ----------------------
   -- Session Callback --
   ----------------------

   Session_Callback : Callback := null;

   --------------
   -- Database --
   --------------

   protected Database is

      entry Add_Session (SID : in Id);
      --  Add a new session ID into the database

      entry New_Session (SID : out Id);
      --  Add a new session SID into the database

      entry Delete_Session (SID : in Id);
      --  Removes session SID from the Tree

      function Session_Exist (SID : in Id) return Boolean;
      --  Return True if session SID exist in the database

      function Session_Has_Expired (SID : in Id) return Boolean;
      --  Return True if session SID has exceeded its lifetime

      procedure Touch_Session (SID : in Id);
      --  Updates the session Time_Stamp to current time. Does nothing if SID
      --  does not exist.

      procedure Key_Exist
        (SID    : in     Id;
         Key    : in     String;
         Result :    out Boolean);
      --  Result is set to True if Key_Name exist in session SID

      procedure Get_Value
        (SID   : in     Id;
         Key   : in     String;
         Value :    out Unbounded_String);
      --  Value is set with the value associated with the key Key_Name in
      --  session SID.

      entry Set_Value (SID : in Id; Key, Value : in String);
      --  Add the pair key/value into the session SID

      entry Remove_Key (SID : in Id;  Key : in String);
      --  Removes Key from the session SID

      --
      --  Unsafe routines. These are only to be used by iterators
      --

      procedure Destroy;
      --  Release all memory associated with the database

      procedure Lock_And_Clean;
      --  Increment Lock by 1, all entries modifying data are locked. Routines
      --  reading values from the database can still be called (Key_Exist,
      --  Get_Value, Session_Exist). Checks for expired data and put them into
      --  the global Expired_SID set. The data will be removed later by the
      --  cleaner task. This is used only in the cleaner task.

      procedure Lock_And_Get_Sessions (Sessions : out Session_Set.Map);
      --  Increment Lock by 1, all entries modifying data are locked. Routines
      --  reading values from the database can still be called (Key_Exist,
      --  Get_Value, Session_Exist). Returns the Sessions tree, not that the
      --  database should be locked before calling this routine. It is not safe
      --  to use the returned value on an unlocked database.

      procedure Unsafe_Delete_Session (SID : in Id);
      --  Remove this session ID, the database should be locked before calling
      --  this routine.

      procedure Unlock;
      --  Decrement Lock by 1, unlock all entries when Lock return to 0

   private

      Lock_Counter : Natural := 0;
      Sessions     : aliased Session_Set.Map;

   end Database;

   -------------
   -- Cleaner --
   -------------

   task body Cleaner is
      use type Calendar.Time;

      Next_Run : Calendar.Time := Calendar.Clock + Session_Check_Interval;

      L_SC     : Callback;
      pragma Atomic (L_SC);
      --  Local pointer to the session callback procedure. This is to ensure
      --  that there is no race condition and that the code below will not
      --  crash if SC pointer is changed.

   begin
      Clean_Dead_Sessions : loop
         select
            accept Stop;
            exit Clean_Dead_Sessions;
         or
            delay until Next_Run;
         end select;

         Database.Lock_And_Clean;

         L_SC := Session_Callback;
         --  Use Session_Callback copy as we don't want the value to change
         --  between the test and the call to the session callback routine.

         for K in 1 .. E_Index loop

            if L_SC /= null then
               --  Run the session's callback routine, we catch all exceptions
               --  here as we do not want to fail.

               begin
                  L_SC.all (Expired_SID (K));
               exception
                  when E : others =>
                     Text_IO.Put_Line
                       (Text_IO.Current_Error,
                        "Delete session callback error : "
                          & Exception_Information (E));
               end;
            end if;

            --  Now we can delete the session data

            Database.Unsafe_Delete_Session (Expired_SID (K));
         end loop;

         E_Index := 0;

         Database.Unlock;

         Next_Run := Next_Run + Session_Check_Interval;
      end loop Clean_Dead_Sessions;

      Database.Destroy;

   exception
      when E : others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Unrecoverable Error: Cleaner Task bug detected "
            & Exception_Information (E));
   end Cleaner;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Database.Destroy;
   end Clear;

   ------------
   -- Create --
   ------------

   function Create return Id is
      New_Id : Id;
   begin
      Database.New_Session (New_Id);
      return New_Id;
   end Create;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected body Cleaner_Control is

      -----------
      -- Start --
      -----------

      procedure Start
        (Session_Check_Interval : in Duration;
         Session_Lifetime       : in Duration) is
      begin
         Server_Count := Server_Count + 1;

         if Server_Count = 1 then
            Session.Session_Check_Interval := Start.Session_Check_Interval;
            Session.Session_Lifetime       := Start.Session_Lifetime;
            Cleaner_Task := new Cleaner;
         end if;
      end Start;

      ----------
      -- Stop --
      ----------

      procedure Stop (Need_Release : out Boolean)  is
      begin
         Server_Count := Server_Count - 1;

         if Server_Count = 0 then
            Need_Release := True;
         else
            Need_Release := False;
         end if;
      end Stop;

   end Cleaner_Control;

   --------------
   -- Database --
   --------------

   protected body Database is

      -----------------
      -- Add_Session --
      -----------------

      entry Add_Session (SID : in Id) when Lock_Counter = 0 is
         New_Node : Session_Node;
         Cursor   : Session_Set.Cursor;
         Success  : Boolean;
      begin
         New_Node := (Time_Stamp => Calendar.Clock,
                      Root       => new Key_Value.Map);

         Session_Set.Insert
           (Sessions, String (SID), New_Node, Cursor, Success);

         if not Success then
            Free (New_Node.Root);
         end if;
      end Add_Session;

      --------------------
      -- Delete_Session --
      --------------------

      entry Delete_Session (SID : in Id) when Lock_Counter = 0 is
      begin
         Unsafe_Delete_Session (SID);
      end Delete_Session;

      -------------
      -- Destroy --
      -------------

      procedure Destroy is

         procedure Destroy (Cursor : in Session_Set.Cursor);

         -------------
         -- Destroy --
         -------------

         procedure Destroy (Cursor : in Session_Set.Cursor) is
            Item : Session_Node := Session_Set.Element (Cursor);
         begin
            Free (Item.Root);
         end Destroy;

      begin
         Session_Set.Iterate (Sessions, Destroy'Access);
         Session_Set.Clear (Sessions);
      end Destroy;

      ---------------
      -- Get_Value --
      ---------------

      procedure Get_Value
        (SID   : in     Id;
         Key   : in     String;
         Value :    out Unbounded_String)
      is
         Node   : Session_Node;
         Found  : Boolean;
      begin
         Value := Null_Unbounded_String;

         Get_Node (Sessions, SID, Node, Found);

         if Found then
            declare
               Cursor : constant Key_Value.Cursor
                 := Key_Value.Find (Node.Root.all, Key);
            begin
               if Key_Value.Has_Element (Cursor) then
                  Value := To_Unbounded_String (Key_Value.Element (Cursor));
               end if;
            end;
         end if;
      end Get_Value;

      ---------------
      -- Key_Exist --
      ---------------

      procedure Key_Exist
        (SID    : in     Id;
         Key    : in     String;
         Result :    out Boolean)
      is
         Node   : Session_Node;
      begin
         Get_Node (Sessions, SID, Node, Result);

         if Result then
            Result := Key_Value.Contains (Node.Root.all, Key);
         end if;
      end Key_Exist;

      --------------------
      -- Lock_And_Clean --
      --------------------

      procedure Lock_And_Clean is

         use type Calendar.Time;

         Now : constant Calendar.Time := Calendar.Clock;

         Cursor : Session_Set.Cursor;
         Node   : Session_Node;

      begin
         Lock_Counter := Lock_Counter + 1;
         E_Index := 0;

         Cursor := Session_Set.First (Sessions);

         while Session_Set.Has_Element (Cursor) loop
            Node := Session_Set.Element (Cursor);

            if Node.Time_Stamp + Session_Lifetime < Now then
               E_Index := E_Index + 1;
               Expired_SID (E_Index) := Id (Session_Set.Key (Cursor));

               exit when E_Index = Max_Expired;
               --  No more space in the expired mailbox, quit now
            end if;

            Session_Set.Next (Cursor);
         end loop;
      end Lock_And_Clean;

      ---------------------------
      -- Lock_And_Get_Sessions --
      ---------------------------

      procedure Lock_And_Get_Sessions (Sessions : out Session_Set.Map) is
      begin
         Lock_Counter := Lock_Counter + 1;
         Sessions := Database.Sessions;
      end Lock_And_Get_Sessions;

      -----------------
      -- New_Session --
      -----------------

      entry New_Session (SID : out Id) when Lock_Counter = 0 is
         New_Node : constant Session_Node
           := (Time_Stamp => Calendar.Clock,
               Root       => new Key_Value.Map);

         Cursor   : Session_Set.Cursor;
         Success  : Boolean;

      begin
         Generate_UID : loop
            Utils.Random_String (String (SID));

            Session_Set.Insert
              (Sessions, String (SID), New_Node, Cursor, Success);

            exit Generate_UID when Success;
         end loop Generate_UID;
      end New_Session;

      ------------
      -- Remove --
      ------------

      entry Remove_Key
        (SID : in Id;
         Key : in String) when Lock_Counter = 0
      is
         Node   : Session_Node;
         Found  : Boolean;
      begin
         Get_Node (Sessions, SID, Node, Found);

         if Found then
            Key_Value.Exclude (Node.Root.all, Key);
         end if;
      end Remove_Key;

      -------------------
      -- Session_Exist --
      -------------------

      function Session_Exist (SID : in Id) return Boolean is
      begin
         return Session_Set.Contains (Sessions, String (SID));
      end Session_Exist;

      -------------------------
      -- Session_Has_Expired --
      -------------------------

      function Session_Has_Expired (SID : in Id) return Boolean is
         use type Calendar.Time;
         Now    : constant Calendar.Time := Calendar.Clock;
         Cursor : constant Session_Set.Cursor :=
                    Session_Set.Find (Sessions, String (SID));
         Node   : Session_Node;
      begin
         --  Do not use Get_Node, since that would update the timestamp

         if Session_Set.Has_Element (Cursor) then
            Node := Session_Set.Element (Cursor);
            return Node.Time_Stamp + Session_Lifetime < Now;
         end if;
         return False;
      end Session_Has_Expired;

      ---------------
      -- Set_Value --
      ---------------

      entry Set_Value
        (SID        : in Id;
         Key, Value : in String) when Lock_Counter = 0
      is
         Node   : Session_Node;
         Found  : Boolean;
      begin
         Get_Node (Sessions, SID, Node, Found);

         if Found then
            Key_Value.Include (Node.Root.all, Key, Value);
         end if;
      end Set_Value;

      -------------------
      -- Touch_Session --
      -------------------

      procedure Touch_Session (SID : in Id) is
         Node   : Session_Node;
         Found  : Boolean;
      begin
         Get_Node (Sessions, SID, Node, Found);
      end Touch_Session;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         Lock_Counter := Lock_Counter - 1;
      end Unlock;

      ---------------------------
      -- Unsafe_Delete_Session --
      ---------------------------

      procedure Unsafe_Delete_Session (SID : in Id) is
         Cursor : Session_Set.Cursor
           := Session_Set.Find (Sessions, String (SID));
         Node   : Session_Node := Session_Set.Element (Cursor);
      begin
         Free (Node.Root);
         Session_Set.Delete (Sessions, Cursor);
      end Unsafe_Delete_Session;

   end Database;

   ------------
   -- Delete --
   ------------

   procedure Delete (SID : in Id) is
   begin
      Database.Delete_Session (SID);
   end Delete;

   -----------
   -- Exist --
   -----------

   function Exist (SID : in Id) return Boolean is
   begin
      return Database.Session_Exist (SID);
   end Exist;

   function Exist
     (SID : in Id;
      Key : in String)
      return Boolean
   is
      Result : Boolean;
   begin
      Database.Key_Exist (SID, Key, Result);
      return Result;
   end Exist;

   -----------------------
   -- For_Every_Session --
   -----------------------

   procedure For_Every_Session is

      use type Session_Set.Cursor;

      Sessions : Session_Set.Map;
      Cursor   : Session_Set.Cursor;
      Order    : Positive := 1;
      Quit     : Boolean  := False;

   begin
      Database.Lock_And_Get_Sessions (Sessions);

      Cursor := Session_Set.First (Sessions);

      while Session_Set.Has_Element (Cursor) loop
         Action
           (Order,
            Id (Session_Set.Key (Cursor)),
            Session_Set.Element (Cursor).Time_Stamp,
            Quit);
         exit when Quit;

         Order := Order + 1;
         Session_Set.Next (Cursor);
      end loop;

      Database.Unlock;
   exception
      when others =>
         Database.Unlock;
         raise;
   end For_Every_Session;

   ----------------------------
   -- For_Every_Session_Data --
   ----------------------------

   procedure For_Every_Session_Data (SID : in Id) is

      procedure For_Every_Data (Node : in Session_Node);
      --  Iterate through all Key/Value pairs

      Sessions : Session_Set.Map;
      Node     : Session_Node;
      Order    : Positive := 1;
      Quit     : Boolean  := False;
      Found    : Boolean;

      --------------------
      -- For_Every_Data --
      --------------------

      procedure For_Every_Data (Node : in Session_Node) is
         Cursor : Key_Value.Cursor;
      begin
         Cursor := Key_Value.First (Node.Root.all);

         while Key_Value.Has_Element (Cursor) loop
            Action
              (Order,
               Key_Value.Key (Cursor),
               Key_Value.Element (Cursor),
               Quit);
            exit when Quit;

            Order := Order + 1;
            Key_Value.Next (Cursor);
         end loop;
      end For_Every_Data;

   begin
      Database.Lock_And_Get_Sessions (Sessions);

      Get_Node (Sessions, SID, Node, Found);

      if Found then
         For_Every_Data (Node);
      end if;

      Database.Unlock;
   exception
      when others =>
         Database.Unlock;
         raise;
   end For_Every_Session_Data;

   ------------------
   -- Generic_Data --
   ------------------

   package body Generic_Data is

      ---------
      -- Get --
      ---------

      function Get (SID : in Id; Key : in String) return Data is
         Result : constant String := Get (SID, Key);
         Str    : aliased String_Stream_Type;
         Value  : Data;
      begin
         if Result = "" then
            return Null_Data;

         else
            Open (Str, Result);
            Data'Read (Str'Access, Value);
            return Value;
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set
        (SID   : in Id;
         Key   : in String;
         Value : in Data)
      is
         Str : aliased String_Stream_Type;
      begin
         Data'Write (Str'Access, Value);
         Set (SID, Key, To_String (Str.Str));
      end Set;

   end Generic_Data;

   ---------
   -- Get --
   ---------

   function Get (SID : in Id; Key : in String) return String is
      Value : Unbounded_String;
   begin
      Database.Get_Value (SID, Key, Value);
      return To_String (Value);
   end Get;

   function Get (SID : in Id; Key : in String) return Integer is
      Value : constant String := Get (SID, Key);
   begin
      return Integer'Value (Value);
   exception
      when Constraint_Error =>
         return 0;
   end Get;

   function Get (SID : in Id; Key : in String) return Float is
      Value : constant String := Get (SID, Key);
   begin
      return Float'Value (Value);
   exception
      when Constraint_Error =>
         return 0.0;
   end Get;

   function Get (SID : in Id; Key : in String) return Boolean is
   begin
      return Get (SID, Key) = "T";
   end Get;

   ------------------
   -- Get_Lifetime --
   ------------------

   function Get_Lifetime return Duration is
   begin
      return Session_Lifetime;
   end Get_Lifetime;

   --------------
   -- Get_Node --
   --------------

   procedure Get_Node
     (Sessions : in out Session_Set.Map;
      SID      : in     Id;
      Node     :    out Session_Node;
      Found    :    out Boolean)
   is
      Cursor : constant Session_Set.Cursor
        := Session_Set.Find (Sessions, String (SID));

      procedure Process
        (Key  : in String;
         Item : in out Session_Node);

      -------------
      -- Process --
      -------------

      procedure Process
        (Key  : in String;
         Item : in out Session_Node)
      is
         pragma Unreferenced (Key);
      begin
         Item.Time_Stamp := Calendar.Clock;
         Node := Item;
      end Process;

   begin
      Found := Session_Set.Has_Element (Cursor);

      if Found then
         Session_Set.Update_Element (Sessions, Cursor, Process'Access);
      end if;
   end Get_Node;

   -----------------
   -- Has_Expired --
   -----------------

   function Has_Expired (SID : in Id) return Boolean is
   begin
      return Database.Session_Has_Expired (SID);
   end Has_Expired;

   -----------
   -- Image --
   -----------

   function Image (SID : in Id) return String is
   begin
      return SID_Prefix & String (SID);
   end Image;

   ----------
   -- Load --
   ----------

   procedure Load (File_Name : in String) is
      use Ada.Streams.Stream_IO;
      File       : File_Type;
      Stream_Ptr : Stream_Access;

   begin
      Open (File, Name => File_Name, Mode => In_File);

      Stream_Ptr := Stream (File);

      while not End_Of_File (File) loop
         declare
            SID : constant Id := Id'Input (Stream_Ptr);
            Key_Value_Size : Natural;
         begin
            Database.Add_Session (SID);

            Key_Value_Size := Natural'Input (Stream_Ptr);

            for I in 1 .. Key_Value_Size loop
               declare
                  Key   : constant String := String'Input (Stream_Ptr);
                  Value : constant String := String'Input (Stream_Ptr);
               begin
                  Set (SID, Key, Value);
               end;
            end loop;

         end;
      end loop;

      Close (File);
   end Load;

   ----------
   -- Open --
   ----------

   procedure Open
     (Stream : in out String_Stream_Type'Class;
      Str    : in String) is
   begin
      Stream.Str        := To_Unbounded_String (Str);
      Stream.Read_Index := 1;
   end Open;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      Str : constant String := Slice
        (Stream.Str, Stream.Read_Index, Stream.Read_Index + Item'Length - 1);
      J   : Stream_Element_Offset := Item'First;
   begin
      for S in Str'Range loop
         Item (J) := Stream_Element (Character'Pos (Str (S)));
         J := J + 1;
      end loop;
      Last := Item'First + Str'Length - 1;
      Stream.Read_Index := Stream.Read_Index + Item'Length;
   end Read;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (SID : in Id;
      Key : in String) is
   begin
      Database.Remove_Key (SID, Key);
   end Remove;

   ----------
   -- Save --
   ----------

   procedure Save (File_Name : in String) is
      use Ada.Streams.Stream_IO;

      File       : File_Type;
      Stream_Ptr : Stream_Access;
      Sessions   : Session_Set.Map;

      procedure Process
        (N          : in     Positive;
         SID        : in     Id;
         Time_Stamp : in     Ada.Calendar.Time;
         Quit       : in out Boolean);
      --  Callback for each session node in the table

      -------------
      -- Process --
      -------------

      procedure Process
        (N          : in     Positive;
         SID        : in     Id;
         Time_Stamp : in     Ada.Calendar.Time;
         Quit       : in out Boolean)
      is
         pragma Unreferenced (N);
         pragma Unreferenced (Time_Stamp);
         pragma Unreferenced (Quit);

         Node     : Session_Node;
         Found    : Boolean;

         procedure Process
           (N          : in     Positive;
            Key, Value : in     String;
            Quit       : in out Boolean);
         --  Callback for each key/value pair for a specific session

         -------------
         -- Process --
         -------------

         procedure Process
           (N          : in     Positive;
            Key, Value : in     String;
            Quit       : in out Boolean)
         is
            pragma Unreferenced (N);
            pragma Unreferenced (Quit);
         begin
            String'Output (Stream_Ptr, Key);
            String'Output (Stream_Ptr, Value);
         end Process;

         --------------------
         -- Each_Key_Value --
         --------------------

         procedure Each_Key_Value is
           new For_Every_Session_Data (Process);

         Key_Value_Size : Natural;

      begin
         Get_Node (Sessions, SID, Node, Found);

         Key_Value_Size := Natural (Key_Value.Length (Node.Root.all));

         if Key_Value_Size > 0 then
            Id'Output (Stream_Ptr, SID);
            Natural'Output (Stream_Ptr, Key_Value_Size);
            Each_Key_Value (SID);
         end if;
      end Process;

      ------------------
      -- Each_Session --
      ------------------

      procedure Each_Session is new For_Every_Session (Process);

   begin
      Create (File, Name => File_Name);

      Database.Lock_And_Get_Sessions (Sessions);

      begin
         Stream_Ptr := Stream (File);
         Each_Session;
      exception
         when others =>
            --  Never leave this block without releasing the database lock
            Database.Unlock;
            raise;
      end;

      Database.Unlock;
      Close (File);
   end Save;

   ---------
   -- Set --
   ---------

   procedure Set
     (SID   : in Id;
      Key   : in String;
      Value : in String) is
   begin
      Database.Set_Value (SID, Key, Value);
   end Set;

   procedure Set
     (SID   : in Id;
      Key   : in String;
      Value : in Integer)
   is
      V : constant String := Integer'Image (Value);
   begin
      if V (1) = ' ' then
         Database.Set_Value (SID, Key, V (2 .. V'Last));
      else
         Database.Set_Value (SID, Key, V);
      end if;
   end Set;

   procedure Set
     (SID   : in Id;
      Key   : in String;
      Value : in Float)
   is
      V : constant String := Float'Image (Value);
   begin
      if V (1) = ' ' then
         Database.Set_Value (SID, Key, V (2 .. V'Last));
      else
         Database.Set_Value (SID, Key, V);
      end if;
   end Set;

   procedure Set
     (SID   : in Id;
      Key   : in String;
      Value : in Boolean)
   is
      V : String (1 .. 1);
   begin
      if Value then
         V := "T";
      else
         V := "F";
      end if;

      Database.Set_Value (SID, Key, V);
   end Set;

   -------------------
   -- Set__Callback --
   -------------------

   procedure Set_Callback (Callback : in Session.Callback) is
   begin
      Session_Callback := Callback;
   end Set_Callback;

   ------------------
   -- Set_Lifetime --
   ------------------

   procedure Set_Lifetime (Seconds : in Duration) is
   begin
      Session_Lifetime := Seconds;
   end Set_Lifetime;

   -----------
   -- Touch --
   -----------

   procedure Touch (SID : in Id) is
   begin
      Database.Touch_Session (SID);
   end Touch;

   -----------
   -- Value --
   -----------

   function Value (SID : in String) return Id is
   begin
      if SID'Length /= Id'Length + SID_Prefix'Length
        or else
        (SID'Length > SID_Prefix'Length
           and then
         SID (SID'First .. SID'First + SID_Prefix'Length - 1) /= SID_Prefix)
      then
         return No_Session;
      else
         return Id (SID (SID'First + SID_Prefix'Length .. SID'Last));
      end if;
   end Value;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out String_Stream_Type;
      Item   : in     Stream_Element_Array)
   is
      Str : String (1 .. Integer (Item'Length));
      S   : Integer := Str'First;
   begin
      for J in Item'Range loop
         Str (S) := Character'Val (Item (J));
         S := S + 1;
      end loop;

      Append (Stream.Str, Str);
   end Write;

end AWS.Session;
