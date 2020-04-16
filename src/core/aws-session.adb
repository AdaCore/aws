------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Containers.Key_Value;
with AWS.Default;
with AWS.Utils.Streams;

package body AWS.Session is

   use Ada.Exceptions;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   SID_Prefix     : constant String := "SID-";

   Private_Key_Length : constant := 10;
   --  Length of the string used for the private key

   Check_Interval : Duration := Default.Session_Cleanup_Interval;
   --  Check for obsolete section interval

   Lifetime       : Real_Time.Time_Span :=
                      Real_Time.To_Time_Span (Default.Session_Lifetime);
   --  A session is obsolete if not used after Session_Lifetime seconds

   Kind_Code      : constant array (Value_Kind) of Character :=
                      (Int  => 'I',
                       Real => 'R',
                       Bool => 'B',
                       Str  => 'S',
                       User => 'U');

   package Key_Value renames Containers.Key_Value;

   type Key_Value_Set_Access is access Key_Value.Map;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Key_Value.Map, Key_Value_Set_Access);

   --  table of session ID

   type Session_Node is record
      Created_Stamp : Calendar.Time;
      Private_Key   : String (1 .. Private_Key_Length);
      Time_Stamp    : Real_Time.Time;
      Root          : Key_Value_Set_Access;
   end record;

   package Session_Set is new Ada.Containers.Ordered_Maps (Id, Session_Node);

   procedure Get_Node
     (Sessions : in out Session_Set.Map;
      SID      : Id;
      Node     : out Session_Node;
      Found    : out Boolean);
   --  Returns Node for specified SID, if found update the timestamp for
   --  this node and set Found to True, otherwise set Found to False.

   Max_Expired : constant := 50;

   type Expired_SID_Array is array (1 .. Max_Expired) of Id;
   --  Used by the task cleaner to get expired session from database

   function V_Kind (K : Character) return Value_Kind;
   --  Return the value kind for K (encoding in the string)

   ----------------------
   -- Session Callback --
   ----------------------

   Session_Callback : Callback := null;

   --------------
   -- Database --
   --------------

   protected Database is

      entry Add_Session (SID : Id);
      --  Add a new session ID into the database

      function Creation_Stamp (SID : Id) return Calendar.Time;
      --  Returns the creation date for this session

      function Private_Key (SID : Id) return String;
      --  Returns the session's private key

      entry New_Session (SID : out Id);
      --  Add a new session SID into the database

      entry Delete_Session (SID : Id);
      --  Removes session SID from the database

      entry Delete_If_Empty (SID : Id; Removed : out Boolean);
      --  Removes session SID only if there is no key/value pairs

      function Session_Exist (SID : Id) return Boolean;
      --  Returns True if session SID exist in the database

      function Session_Has_Expired (SID : Id) return Boolean;
      --  Returns True if session SID has exceeded its lifetime

      function Length return Natural;
      --  Returns number of sessions in database

      function Length (SID : Id) return Natural;
      --  Returns number of key/value pairs in session SID

      procedure Touch_Session (SID : Id);
      --  Updates the session Time_Stamp to current time. Does nothing if SID
      --  does not exist.

      procedure Key_Exist (SID : Id; Key : String; Result : out Boolean);
      --  Result is set to True if Key_Name exist in session SID

      procedure Get_Value
        (SID : Id; Key : String; Value : out Unbounded_String);
      --  Value is set with the value associated with the key Key_Name in
      --  session SID.

      entry Set_Value (SID : Id; Key, Value : String);
      --  Add the pair key/value into the session SID

      entry Remove_Key (SID : Id;  Key : String);
      --  Removes Key from the session SID

      --
      --  Unsafe routines. These are only to be used by iterators
      --

      procedure Destroy;
      --  Release all memory associated with the database

      procedure Prepare_Expired_SID
        (Expired_SID : out Expired_SID_Array; Last : out Natural);
      --  Checks for expired data and put them into the Expired_SID set.
      --  The data will be removed later by the cleaner task.
      --  This is used only in the cleaner task.

      procedure Lock_And_Get_Sessions (First : out Session_Set.Cursor);
      --  Increment Lock by 1, all entries modifying data are locked. Routines
      --  reading values from the database can still be called (Key_Exist,
      --  Get_Value, Session_Exist). Returns the Sessions tree first position.

      procedure Lock_And_Get_Session
        (SID : Id; Node : out Session_Node; Found : out Boolean);
      --  Increment Lock by 1, all entries modifying data are locked. Routines
      --  reading values from the database can still be called (Key_Exist,
      --  Get_Value, Session_Exist). Returns the Session node.

      procedure Unlock;
      --  Decrement Lock by 1, unlock all entries when Lock return to 0

   private

      Lock_Counter : Natural := 0;
      Sessions     : Session_Set.Map;
      Remove_Mark  : Id := No_Session;

   end Database;

   -------------
   -- Cleaner --
   -------------

   task body Cleaner is
      use type Calendar.Time;

      Next_Run : Calendar.Time := Calendar.Clock + Check_Interval;

      L_SC     : Callback with Atomic;
      --  Local pointer to the session callback procedure. This is to ensure
      --  that there is no race condition and that the code below will not
      --  crash if SC pointer is changed.

      Expired_SID : Expired_SID_Array;
      E_Index     : Natural;

   begin
      Clean_Dead_Sessions : loop
         select
            accept Stop;
            exit Clean_Dead_Sessions;
         or
            accept Force;
         or
            delay until Next_Run;
         end select;

         Database.Prepare_Expired_SID (Expired_SID, E_Index);

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

            Database.Delete_Session (Expired_SID (K));
         end loop;

         if E_Index = Max_Expired and then Check_Interval > 1.0 then
            --  Too many expired session, we should run next expiration check
            --  faster.
            Next_Run := Next_Run + 1.0;
         else
            Next_Run := Next_Run + Check_Interval;
         end if;
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

   --------------------
   -- Creation_Stamp --
   --------------------

   function Creation_Stamp (SID : Id) return Calendar.Time is
   begin
      return Database.Creation_Stamp (SID);
   end Creation_Stamp;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected body Cleaner_Control is

      ------------------
      -- Server_Count --
      ------------------

      function Server_Count return Natural is
      begin
         return S_Count;
      end Server_Count;

      -----------
      -- Start --
      -----------

      procedure Start
        (Check_Interval : Duration;
         Lifetime       : Duration;
         Init_Cleaner   : out Boolean) is
      begin
         S_Count := S_Count + 1;
         Init_Cleaner := False;

         if S_Count = 1 then
            Session.Check_Interval := Start.Check_Interval;
            Session.Lifetime       := Real_Time.To_Time_Span (Start.Lifetime);
            Init_Cleaner := True;
         end if;
      end Start;

      ----------
      -- Stop --
      ----------

      procedure Stop (Need_Release : out Boolean)  is
      begin
         S_Count := S_Count - 1;

         if S_Count = 0 then
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

      entry Add_Session (SID : Id) when Lock_Counter = 0 is
         New_Node : Session_Node;
         Cursor   : Session_Set.Cursor;
         Success  : Boolean;
      begin
         New_Node :=
           (Created_Stamp => Calendar.Clock,
            Time_Stamp    => Real_Time.Clock,
            Private_Key   => Utils.Random_String (Private_Key_Length),
            Root          => new Key_Value.Map);

         Sessions.Insert (SID, New_Node, Cursor, Success);

         if not Success then
            Unchecked_Free (New_Node.Root);
         end if;
      end Add_Session;

      --------------------
      -- Creation_Stamp --
      --------------------

      function Creation_Stamp (SID : Id) return Calendar.Time is
         Cursor : constant Session_Set.Cursor := Sessions.Find (SID);
      begin
         if Session_Set.Has_Element (Cursor) then
            return Session_Set.Element (Cursor).Created_Stamp;
         else
            return Calendar.Clock;
         end if;
      end Creation_Stamp;

      ---------------------
      -- Delete_If_Empty --
      ---------------------

      entry Delete_If_Empty
        (SID : Id; Removed : out Boolean) when Lock_Counter = 0
      is
         use type Ada.Containers.Count_Type;
         Cursor : Session_Set.Cursor := Sessions.Find (SID);
         Node   : Session_Node;
      begin
         if Session_Set.Has_Element (Cursor) then
            Node := Session_Set.Element (Cursor);

            Removed := Node.Root.Length = 0;

            if Removed then
               Unchecked_Free (Node.Root);
               Sessions.Delete (Cursor);
            end if;

         else
            Removed := False;
         end if;
      end Delete_If_Empty;

      --------------------
      -- Delete_Session --
      --------------------

      entry Delete_Session (SID : Id) when Lock_Counter = 0 is
         Cursor : Session_Set.Cursor := Sessions.Find (SID);
         Node   : Session_Node;
      begin
         if Session_Set.Has_Element (Cursor) then
            Node := Session_Set.Element (Cursor);
            Unchecked_Free (Node.Root);
            Sessions.Delete (Cursor);
         end if;
      end Delete_Session;

      -------------
      -- Destroy --
      -------------

      procedure Destroy is
      begin
         for Item of Sessions loop
            Unchecked_Free (Item.Root);
         end loop;

         Session_Set.Clear (Sessions);
      end Destroy;

      ---------------
      -- Get_Value --
      ---------------

      procedure Get_Value
        (SID : Id; Key : String; Value : out Unbounded_String)
      is
         Node  : Session_Node;
         Found : Boolean;
      begin
         Value := Null_Unbounded_String;

         Get_Node (Sessions, SID, Node, Found);

         if Found then
            declare
               Cursor : constant Key_Value.Cursor :=
                          Key_Value.Find (Node.Root.all, Key);
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
        (SID : Id; Key : String; Result : out Boolean)
      is
         Node : Session_Node;
      begin
         Get_Node (Sessions, SID, Node, Result);

         if Result then
            Result := Key_Value.Contains (Node.Root.all, Key);
         end if;
      end Key_Exist;

      ------------
      -- Length --
      ------------

      function Length return Natural is
      begin
         return Natural (Sessions.Length);
      end Length;

      function Length (SID : Id) return Natural is
         C : constant Session_Set.Cursor := Sessions.Find (SID);
      begin
         if Session_Set.Has_Element (C) then
            return Natural (Session_Set.Element (C).Root.Length);
         else
            return 0;
         end if;
      end Length;

      --------------------------
      -- Lock_And_Get_Session --
      --------------------------

      procedure Lock_And_Get_Session
        (SID : Id; Node : out Session_Node; Found : out Boolean)
      is
         C : constant Session_Set.Cursor := Sessions.Find (SID);
      begin
         Lock_Counter := Lock_Counter + 1;

         Found := Session_Set.Has_Element (C);

         if Found then
            Node := Session_Set.Element (C);
         end if;
      end Lock_And_Get_Session;

      ---------------------------
      -- Lock_And_Get_Sessions --
      ---------------------------

      procedure Lock_And_Get_Sessions (First : out Session_Set.Cursor) is
      begin
         Lock_Counter := Lock_Counter + 1;
         First := Database.Sessions.First;
      end Lock_And_Get_Sessions;

      -----------------
      -- New_Session --
      -----------------

      entry New_Session (SID : out Id) when Lock_Counter = 0 is
         New_Node : constant Session_Node :=
                      (Created_Stamp => Calendar.Clock,
                       Time_Stamp    => Real_Time.Clock,
                       Private_Key   =>
                         Utils.Random_String (Private_Key_Length),
                       Root          => new Key_Value.Map);

         Cursor   : Session_Set.Cursor;
         Success  : Boolean;

      begin
         Generate_UID : loop
            Utils.Random_String (String (SID));

            Sessions.Insert (SID, New_Node, Cursor, Success);

            exit Generate_UID when Success;
         end loop Generate_UID;
      end New_Session;

      -------------------------
      -- Prepare_Expired_SID --
      -------------------------

      procedure Prepare_Expired_SID
        (Expired_SID : out Expired_SID_Array; Last : out Natural)
      is

         use type Real_Time.Time;

         Now : constant Real_Time.Time := Real_Time.Clock;

         Cursor : Session_Set.Cursor;
         Node   : Session_Node;

      begin
         Last := 0;

         if Remove_Mark = No_Session then
            Cursor := Sessions.First;
         else
            --  Try to continue iteration over container

            Cursor := Sessions.Find (Remove_Mark);

            Remove_Mark := No_Session;

            if not Session_Set.Has_Element (Cursor) then
               Cursor := Sessions.First;
            end if;
         end if;

         while Session_Set.Has_Element (Cursor) loop
            Node := Session_Set.Element (Cursor);

            if Node.Time_Stamp + Lifetime < Now then
               Last := Last + 1;
               Expired_SID (Last) := Session_Set.Key (Cursor);

               if Last = Expired_SID'Last then
                  --  No more space in the expired mailbox, quit now
                  Session_Set.Next (Cursor);

                  if Session_Set.Has_Element (Cursor) then
                     Remove_Mark := Session_Set.Key (Cursor);
                  end if;

                  exit;
               end if;
            end if;

            Session_Set.Next (Cursor);
         end loop;
      end Prepare_Expired_SID;

      -----------------
      -- Private_Key --
      -----------------

      function Private_Key (SID : Id) return String is
         Cursor : constant Session_Set.Cursor := Sessions.Find (SID);
      begin
         if Session_Set.Has_Element (Cursor) then
            return Session_Set.Element (Cursor).Private_Key;
         else
            --  Must not be null as used as a key for an HMAC
            return ".!.";
         end if;
      end Private_Key;

      ------------
      -- Remove --
      ------------

      entry Remove_Key (SID : Id; Key : String) when Lock_Counter = 0 is
         Node  : Session_Node;
         Found : Boolean;
      begin
         Get_Node (Sessions, SID, Node, Found);

         if Found then
            Key_Value.Exclude (Node.Root.all, Key);
         end if;
      end Remove_Key;

      -------------------
      -- Session_Exist --
      -------------------

      function Session_Exist (SID : Id) return Boolean is
      begin
         return Sessions.Contains (SID);
      end Session_Exist;

      -------------------------
      -- Session_Has_Expired --
      -------------------------

      function Session_Has_Expired (SID : Id) return Boolean is
         use type Real_Time.Time;
         Cursor : constant Session_Set.Cursor := Sessions.Find (SID);
      begin
         --  Do not use Get_Node, since that would update the timestamp

         if Session_Set.Has_Element (Cursor) then
            return Session_Set.Element (Cursor).Time_Stamp + Lifetime
                   < Real_Time.Clock;
         end if;

         return False;
      end Session_Has_Expired;

      ---------------
      -- Set_Value --
      ---------------

      entry Set_Value
        (SID : Id; Key, Value : String) when Lock_Counter = 0
      is
         Node  : Session_Node;
         Found : Boolean;
      begin
         Get_Node (Sessions, SID, Node, Found);

         if Found then
            Key_Value.Include (Node.Root.all, Key, Value);
         end if;
      end Set_Value;

      -------------------
      -- Touch_Session --
      -------------------

      procedure Touch_Session (SID : Id) is
         Node  : Session_Node;
         Found : Boolean;
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

   end Database;

   ------------
   -- Delete --
   ------------

   procedure Delete (SID : Id) is
   begin
      Database.Delete_Session (SID);
   end Delete;

   ---------------------
   -- Delete_If_Empty --
   ---------------------

   function Delete_If_Empty (SID : Id) return Boolean is
      Removed : Boolean;
   begin
      Database.Delete_If_Empty (SID, Removed);
      return Removed;
   end Delete_If_Empty;

   -----------
   -- Exist --
   -----------

   function Exist (SID : Id) return Boolean is
   begin
      return Database.Session_Exist (SID);
   end Exist;

   function Exist (SID : Id; Key : String) return Boolean is
      Result : Boolean;
   begin
      Database.Key_Exist (SID, Key, Result);
      return Result;
   end Exist;

   -----------------------
   -- For_Every_Session --
   -----------------------

   procedure For_Every_Session is

      use type Calendar.Time;
      use type Real_Time.Time;

      Now_Monoton  : constant Real_Time.Time := Real_Time.Clock;
      Now_Calendar : constant Calendar.Time  := Calendar.Clock;

      Cursor : Session_Set.Cursor;
      Order  : Positive := 1;
      Quit   : Boolean  := False;

   begin
      Database.Lock_And_Get_Sessions (Cursor);

      while Session_Set.Has_Element (Cursor) loop
         Action
           (Order,
            Session_Set.Key (Cursor),
            Now_Calendar
            - Real_Time.To_Duration
                (Now_Monoton - Session_Set.Element (Cursor).Time_Stamp),
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

   procedure For_Every_Session_Data (SID : Id) is

      procedure For_Every_Data (Node : Session_Node);
      --  Iterate through all Key/Value pairs

      Node  : Session_Node;
      Order : Positive := 1;
      Quit  : Boolean  := False;
      Found : Boolean;

      --------------------
      -- For_Every_Data --
      --------------------

      procedure For_Every_Data (Node : Session_Node) is
      begin
         for Cursor in Node.Root.Iterate loop
            declare
               Value : constant String := Key_Value.Element (Cursor);
            begin
               Action
                 (Order,
                  Key_Value.Key (Cursor),
                  Value (Value'First + 1 .. Value'Last),
                  V_Kind (Value (Value'First)),
                  Quit);
            end;
            exit when Quit;

            Order := Order + 1;
         end loop;
      end For_Every_Data;

   begin
      Database.Lock_And_Get_Session (SID, Node, Found);

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

      function Get (SID : Id; Key : String) return Data is
         Result : constant String := Get (SID, Key);
         Str    : aliased Utils.Streams.Strings;
         Value  : Data;
      begin
         if Result = "" then
            return Null_Data;

         else
            Utils.Streams.Open (Str, Result);
            Data'Read (Str'Access, Value);
            return Value;
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (SID : Id; Key : String; Value : Data) is
         Str : aliased Utils.Streams.Strings;
      begin
         Data'Write (Str'Access, Value);
         Database.Set_Value
           (SID, Key, Kind_Code (User) & Utils.Streams.Value (Str'Access));
      end Set;

   end Generic_Data;

   ---------
   -- Get --
   ---------

   function Get (SID : Id; Key : String) return String is
      Value : Unbounded_String;
   begin
      Database.Get_Value (SID, Key, Value);

      if Length (Value) > 1 then
         return Slice (Value, 2, Length (Value));
      else
         return "";
      end if;
   end Get;

   function Get (SID : Id; Key : String) return Integer is
      Value : constant String := Get (SID, Key);
   begin
      return Integer'Value (Value);
   exception
      when Constraint_Error =>
         return 0;
   end Get;

   function Get (SID : Id; Key : String) return Float is
      Value : constant String := Get (SID, Key);
   begin
      return Float'Value (Value);
   exception
      when Constraint_Error =>
         return 0.0;
   end Get;

   function Get (SID : Id; Key : String) return Boolean is
   begin
      return Get (SID, Key) = "T";
   end Get;

   ------------------
   -- Get_Lifetime --
   ------------------

   function Get_Lifetime return Duration is
   begin
      return Real_Time.To_Duration (Lifetime);
   end Get_Lifetime;

   --------------
   -- Get_Node --
   --------------

   procedure Get_Node
     (Sessions : in out Session_Set.Map;
      SID      : Id;
      Node     : out Session_Node;
      Found    : out Boolean)
   is
      Cursor : constant Session_Set.Cursor := Sessions.Find (SID);

      procedure Process (Key : Id; Item : in out Session_Node);

      -------------
      -- Process --
      -------------

      procedure Process (Key : Id; Item : in out Session_Node) is
         pragma Unreferenced (Key);
      begin
         Item.Time_Stamp := Real_Time.Clock;
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

   function Has_Expired (SID : Id) return Boolean is
   begin
      return Database.Session_Has_Expired (SID);
   end Has_Expired;

   -----------
   -- Image --
   -----------

   function Image (SID : Id) return String is
   begin
      return SID_Prefix & String (SID);
   end Image;

   ------------
   -- Length --
   ------------

   function Length return Natural is
   begin
      return Database.Length;
   end Length;

   function Length (SID : Id) return Natural is
   begin
      return Database.Length (SID);
   end Length;

   ----------
   -- Load --
   ----------

   procedure Load (File_Name : String) is
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
                  Database.Set_Value (SID, Key, Value);
               end;
            end loop;

         end;
      end loop;

      Close (File);
   end Load;

   -----------------
   -- Private_Key --
   -----------------

   function Private_Key (SID : Id) return String is
   begin
      return Database.Private_Key (SID);
   end Private_Key;

   ------------
   -- Remove --
   ------------

   procedure Remove (SID : Id; Key : String) is
   begin
      Database.Remove_Key (SID, Key);
   end Remove;

   ----------
   -- Save --
   ----------

   procedure Save (File_Name : String) is
      use Ada.Streams.Stream_IO;

      File       : File_Type;
      Stream_Ptr : Stream_Access;
      Position   : Session_Set.Cursor;

      procedure Process_Session;

      -------------
      -- Process --
      -------------

      procedure Process_Session  is
         Node           : constant Session_Node :=
                            Session_Set.Element (Position);
         Key_Value_Size : constant Natural :=
                            Natural (Key_Value.Length (Node.Root.all));
      begin
         if Key_Value_Size > 0 then
            Id'Output (Stream_Ptr, Session_Set.Key (Position));
            Natural'Output (Stream_Ptr, Key_Value_Size);

            for Cursor in Node.Root.Iterate loop
               String'Output (Stream_Ptr, Key_Value.Key (Cursor));
               String'Output (Stream_Ptr, Key_Value.Element (Cursor));
            end loop;
         end if;
      end Process_Session;

   begin
      Create (File, Name => File_Name);

      Database.Lock_And_Get_Sessions (First => Position);

      begin
         Stream_Ptr := Stream (File);

         while Session_Set.Has_Element (Position) loop
            Process_Session;
            Session_Set.Next (Position);
         end loop;

      exception
         when others =>
            --  Never leave this block without releasing the database lock
            Database.Unlock;
            raise;
      end;

      Database.Unlock;
      Close (File);
   end Save;

   ------------------
   -- Server_Count --
   ------------------

   function Server_Count return Natural is
   begin
      return Cleaner_Control.Server_Count;
   end Server_Count;

   ---------
   -- Set --
   ---------

   procedure Set (SID : Id; Key : String; Value : String) is
   begin
      Database.Set_Value (SID, Key, Kind_Code (Str) & Value);
   end Set;

   procedure Set (SID : Id; Key : String; Value : Integer) is
      V : constant String := Integer'Image (Value);
   begin
      if V (1) = ' ' then
         Database.Set_Value (SID, Key, Kind_Code (Int) & V (2 .. V'Last));
      else
         Database.Set_Value (SID, Key, Kind_Code (Int) & V);
      end if;
   end Set;

   procedure Set (SID : Id; Key : String; Value : Float) is
      V : constant String := Float'Image (Value);
   begin
      if V (1) = ' ' then
         Database.Set_Value (SID, Key, Kind_Code (Real) & V (2 .. V'Last));
      else
         Database.Set_Value (SID, Key, Kind_Code (Real) & V);
      end if;
   end Set;

   procedure Set (SID : Id; Key : String; Value : Boolean) is
      V : Character;
   begin
      if Value then
         V := 'T';
      else
         V := 'F';
      end if;

      Database.Set_Value (SID, Key, Kind_Code (Bool) & V);
   end Set;

   ------------------
   -- Set_Callback --
   ------------------

   procedure Set_Callback (Callback : Session.Callback) is
   begin
      Session_Callback := Callback;
   end Set_Callback;

   ------------------
   -- Set_Lifetime --
   ------------------

   procedure Set_Lifetime (Seconds : Duration) is
   begin
      Lifetime := Real_Time.To_Time_Span (Seconds);
   end Set_Lifetime;

   -----------
   -- Touch --
   -----------

   procedure Touch (SID : Id) is
   begin
      Database.Touch_Session (SID);
   end Touch;

   ------------
   -- V_Kind --
   ------------

   function V_Kind (K : Character) return Value_Kind is
   begin
      case K is
         when 'I' => return Int;
         when 'S' => return Str;
         when 'R' => return Real;
         when 'B' => return Bool;
         when 'U' => return User;

         when others =>
            raise Constraint_Error;
      end case;
   end V_Kind;

   -----------
   -- Value --
   -----------

   function Value (SID : String) return Id is
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

end AWS.Session;
