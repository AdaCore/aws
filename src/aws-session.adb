------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

--  $Id$

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with System;

with Table_Of_Static_Keys_And_Dynamic_Values_G;

with AWS.Default;
with AWS.Key_Value;

package body AWS.Session is

   type NID is range 0 .. System.Max_Int;

   package SID_Random is new Ada.Numerics.Discrete_Random (NID);

   use Ada;
   use Ada.Strings.Unbounded;

   SID_Generator          : SID_Random.Generator;
   SID_Prefix             : constant String := "SID-";

   Session_Check_Interval : Duration
     := Default.Session_Cleanup_Interval;
   --  Check for obsolete section every 10 minutes.

   Session_Lifetime       : Duration := Default.Session_Lifetime;
   --  A session is obsolete if not used after Session_Lifetime seconds.

   --  table of session ID

   type Session_Node is record
      Time_Stamp : Calendar.Time;
      Root       : AWS.Key_Value.Set;
   end record;

   procedure Assign
     (Destination : in out Session_Node;
      Source      : in     Session_Node);

   procedure Destroy (Value : in out Session_Node);

   procedure Assign
     (Destination : in out Session_Node;
      Source      : in     Session_Node) is
   begin
      Destination.Time_Stamp := Source.Time_Stamp;
      Key_Value.Assign (Destination.Root, Source.Root);
   end Assign;

   procedure Destroy (Value : in out Session_Node) is
   begin
      Key_Value.Destroy (Value.Root);
   end Destroy;

   package Session_Set is new Table_Of_Static_Keys_And_Dynamic_Values_G
     (ID, "<", "=", Session_Node, Assign, Destroy);

   type Session_Set_Access is access all Session_Set.Table_Type;

   --------------
   -- Database --
   --------------

   protected Database is

      entry Add_Session (SID : in ID);
      --  Acdd a new session ID into the database.

      entry New_Session (SID : out ID);
      --  Add a new session SID into the database.

      entry Delete_Session (SID : in ID);
      --  Removes session SID from the Tree.

      function Session_Exist (SID : in ID) return Boolean;
      --  Return True if session SID exist in the database.

      entry Key_Exist
        (SID    : in     ID;
         Key    : in     String;
         Result :    out Boolean);
      --  Result is set to True if Key_Name exist in session SID.

      entry Get_Value
        (SID   : in     ID;
         Key   : in     String;
         Value :    out Unbounded_String);
      --  Value is set with the value associated with the key Key_Name in
      --  session SID.

      entry Set_Value
        (SID        : in ID;
         Key, Value : in String);
      --  Add the pair key/value into the session SID.

      entry Remove_Key
        (SID : in ID;
         Key : in String);
      --  Removes Key from the session SID.

      entry Clean;
      --  Removes old session data that are older than Session_Lifetime
      --  seconds.

      --
      --  Not safe routines. These are only to be used by iterators and the
      --  task cleaner.
      --

      procedure Get_Sessions_And_Lock (Sessions : out Session_Set_Access);
      --  Increment Lock by 1, all entries are lockedand return the Sessions
      --  tree

      procedure Unlock;
      --  Decrement Lock by 1, unlock all entries when Lock return to 0.

   private

      Lock     : Natural := 0;

      Sessions : aliased Session_Set.Table_Type;

      function Generate_ID return ID;
      --  Retruns a session ID. This ID is not certified to be uniq in the
      --  system. It is required that the caller check for uniqness if
      --  necessary.

   end Database;

   -------------
   -- Cleaner --
   -------------

   task body Cleaner is
      use Ada;
      use type Calendar.Time;

      Next_Run : Calendar.Time := Calendar.Clock + Session_Check_Interval;
   begin
      Clean_Dead_Sessions: loop
         select
            accept Stop;
            exit Clean_Dead_Sessions;
         or
            delay until Next_Run;
         end select;

         Database.Clean;
         Next_Run := Next_Run + Session_Check_Interval;
      end loop Clean_Dead_Sessions;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error,
            "Unrecoverable Error : Cleaner Task bug detected"
            & Exceptions.Exception_Information (E));
   end Cleaner;

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

   ------------
   -- Create --
   ------------

   function Create return ID is
      New_ID : ID;
   begin
      Database.New_Session (New_ID);
      return New_ID;
   end Create;

   --------------
   -- Database --
   --------------

   type Session_ID_Array is array (Integer range <>) of ID;

   protected body Database is

      -----------------
      -- Add_Session --
      -----------------

      entry Add_Session (SID : in ID) when Lock = 0 is
         New_Node : Session_Node;

      begin
         New_Node.Time_Stamp := Calendar.Clock;
         Session_Set.Insert (Sessions, SID, New_Node);
      end Add_Session;

      -----------
      -- Clean --
      -----------

      entry Clean when Lock = 0 is

         Max_Remove : constant := 200;
         --  Maximum number of items that will get removed at a time. Other
         --  items will be check for removal during next run.

         Remove : Session_ID_Array (1 .. Max_Remove);
         --  ??? can't use anonymous array here, GNAT bug TN 9023-002
         --  Fixed in GNAT 3.15w (20010625).

         Index : Natural := 0;

         Now : constant Calendar.Time := Calendar.Clock;

         procedure Process
           (SID      : in     ID;
            Session  : in     Session_Node;
            Order    : in     Positive;
            Continue : in out Boolean);
         --  Iterator callback

         procedure Process
           (SID      : in     ID;
            Session  : in     Session_Node;
            Order    : in     Positive;
            Continue : in out Boolean)
         is
            use type Calendar.Time;
         begin
            if Session.Time_Stamp + Session_Lifetime < Now then
               Index := Index + 1;
               Remove (Index) := SID;

               if Index = Max_Remove then
                  --  No more space in the removal buffer, quit now.
                  Continue := False;

               end if;
            end if;

         end Process;

         procedure In_Order is
            new Session_Set.Traverse_Asc_G (Process);

      begin
         In_Order (Sessions);

         --  delete nodes

         for K in 1 .. Index loop
            Session_Set.Remove (Sessions, Remove (K));
         end loop;
      end Clean;

      --------------------
      -- Delete_Session --
      --------------------

      entry Delete_Session (SID : in ID) when Lock = 0 is
      begin
         Session_Set.Remove (Sessions, SID);
      exception
         when Key_Value.Table.Missing_Item_Error =>
            raise Internal_Error;
      end Delete_Session;

      ------------------
      -- Generate_UID --
      ------------------

      function Generate_ID return ID is
         use SID_Random;

         Chars : constant String := "0123456789"
            & "abcdefghijklmnopqrstuvwxyz"
            & "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
         Rand   : NID := 0;
         Result : ID;

      begin
         for I in ID'Range loop
            if Rand = 0 then
               Rand := Random (SID_Generator);
            end if;

            Result (I) := Chars (Integer (Rand rem Chars'Length) + 1);
            Rand := Rand / Chars'Length;
         end loop;

         return Result;
      end Generate_ID;

      ---------------
      -- Get_Value --
      ---------------

      entry Get_Value
        (SID   : in     ID;
         Key   : in     String;
         Value : out Unbounded_String)
      when Lock = 0 is

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node);
         --  Adjust time stamp and retreive the value associated to key.

         Found : Boolean;

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node) is
         begin
            Node.Time_Stamp := Calendar.Clock;
            Key_Value.Get_Value (Node.Root, Key, Value);
         end Modify;

         procedure Update_Value is
            new Session_Set.Update_Value_Or_Status_G (Modify);

      begin
         Update_Value (Sessions, SID, Found);

         if not Found then
            Value := Null_Unbounded_String;
         end if;

      exception
         when Key_Value.Table.Missing_Item_Error =>
            Value := Null_Unbounded_String;
      end Get_Value;

      ---------------
      -- Key_Exist --
      ---------------

      entry Key_Exist
        (SID    : in     ID;
         Key    : in     String;
         Result :    out Boolean)
      when Lock = 0 is

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node);
         --  Adjust time stamp and check if Key is present.

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node) is
         begin
            Node.Time_Stamp := Calendar.Clock;
            Result := Key_Value.Is_Present (Node.Root, Key);
         end Modify;

         procedure Update_Value is
            new Session_Set.Update_Value_Or_Exception_G (Modify);

      begin
         Result := False;
         Update_Value (Sessions, SID);

      exception
         when Key_Value.Table.Missing_Item_Error =>
            Result := False;
      end Key_Exist;

      -----------------
      -- New_Session --
      -----------------

      entry New_Session (SID : out ID) when Lock = 0 is
         New_Node : Session_Node;

      begin
         Generate_UID : loop
            SID := Generate_ID;

            New_Node.Time_Stamp := Calendar.Clock;

            begin
               Session_Set.Insert (Sessions, SID, New_Node);
               exit Generate_UID;
            exception
               when Session_Set.Duplicate_Item_Error =>
                  --  very low probability but we should catch it
                  --  and try to generate unique key again.
                  null;
            end;
         end loop Generate_UID;
      end New_Session;

      ------------
      -- Remove --
      ------------

      entry Remove_Key
        (SID : in ID;
         Key : in String) when Lock = 0
      is

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node);
         --  Adjust time stamp and removes key.

         Found : Boolean;

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node)
         is
            Was_Present : Boolean;
         begin
            Node.Time_Stamp := Calendar.Clock;
            Key_Value.Remove (Node.Root, Key, Was_Present);
         end Modify;

         procedure Update_Value is
            new Session_Set.Update_Value_Or_Status_G (Modify);

      begin
         Update_Value (Sessions, SID, Found);
      end Remove_Key;

      -------------------
      -- Session_Exist --
      -------------------

      function Session_Exist (SID : in ID) return Boolean is
      begin
         return Session_Set.Is_Present (Sessions, SID);
      end Session_Exist;

      ---------------
      -- Set_Value --
      ---------------

      entry Set_Value
        (SID        : in ID;
         Key, Value : in String)
      when Lock = 0 is

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node);
         --  Adjust time stamp and set key's value.

         Found : Boolean;

         procedure Modify
           (SID  : in     ID;
            Node : in out Session_Node)
         is
            V : constant Unbounded_String := To_Unbounded_String (Value);
         begin
            Node.Time_Stamp := Calendar.Clock;
            Key_Value.Insert_Or_Replace_Value (Node.Root, Key, V);
         end Modify;

         procedure Update_Value is
            new Session_Set.Update_Value_Or_Status_G (Modify);

      begin
         Update_Value (Sessions, SID, Found);
      end Set_Value;

      ---------------------------
      -- Get_Sessions_And_Lock --
      ---------------------------

      procedure Get_Sessions_And_Lock (Sessions : out Session_Set_Access) is
      begin
         Lock     := Lock + 1;
         Sessions := Database.Sessions'Access;
      end Get_Sessions_And_Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         Lock := Lock - 1;
      end Unlock;

   end Database;

   ------------
   -- Delete --
   ------------

   procedure Delete (SID : in ID) is
   begin
      Database.Delete_Session (SID);
   end Delete;

   -----------
   -- Exist --
   -----------

   function Exist (SID : in ID) return Boolean is
   begin
      return Database.Session_Exist (SID);
   end Exist;

   function Exist
     (SID : in ID;
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

      procedure Process
        (Key      : in     ID;
         Session  : in     Session_Node;
         Order    : in     Positive;
         Continue : in out Boolean);
      --  iterator callback

      -------------
      -- Process --
      -------------

      procedure Process
        (Key      : in     ID;
         Session  : in     Session_Node;
         Order    : in     Positive;
         Continue : in out Boolean)
      is
         Quit : Boolean := False;
      begin
         Action (Order,
                 Key,
                 Session.Time_Stamp,
                 Quit);

         Continue := not Quit;
      end Process;

      --------------
      -- In_Order --
      --------------

      procedure In_Order is
         new Session_Set.Traverse_Asc_G (Process);

      Sessions : Session_Set_Access;

   begin
      Database.Get_Sessions_And_Lock (Sessions);
      In_Order (Sessions.all);
      Database.Unlock;
   exception
      when others =>
         Database.Unlock;
   end For_Every_Session;

   ----------------------------
   -- For_Every_Session_Data --
   ----------------------------

   procedure For_Every_Session_Data (SID : in ID) is

      procedure Process
        (Key      : in     String;
         Value    : in     Unbounded_String;
         Order    : in     Positive;
         Continue : in out Boolean);
      --  Key/Value iterator callback.

      procedure Start
        (SID  : in     ID;
         Node : in out Session_Node);
      --  Session iterator callback.

      Sessions : Session_Set_Access;

      Found : Boolean;

      -------------
      -- Process --
      -------------

      procedure Process
        (Key      : in     String;
         Value    : in     Unbounded_String;
         Order    : in     Positive;
         Continue : in out Boolean)
      is
         Quit : Boolean := False;
      begin
         Action (Order,
                 Key,
                 To_String (Value),
                 Quit);

         Continue := not Quit;
      end Process;

      --------------
      -- In_Order --
      --------------

      procedure In_Order is
         new Key_Value.Table.Traverse_Asc_G (Process);

      -----------
      -- Start --
      -----------

      procedure Start
        (SID  : in     ID;
         Node : in out Session_Node) is
      begin
         In_Order (Key_Value.Table.Table_Type (Node.Root));
      end Start;

      -----------------
      -- For_Session --
      -----------------

      procedure For_Session is
         new Session_Set.Update_Value_Or_Status_G (Start);

   begin
      Database.Get_Sessions_And_Lock (Sessions);

      For_Session (Sessions.all, SID, Found);

      Database.Unlock;
   exception
      when others =>
         Database.Unlock;
         raise;
   end For_Every_Session_Data;

   ---------
   -- Get --
   ---------

   function Get
     (SID : in ID;
      Key : in String)
      return String
   is
      Value : Unbounded_String;
   begin
      Database.Get_Value (SID, Key, Value);
      return To_String (Value);
   end Get;

   function Get
     (SID : in ID;
      Key : in String)
      return Integer
   is
      Value : Unbounded_String;
   begin
      Database.Get_Value (SID, Key, Value);
      return Integer'Value (To_String (Value));
   exception
      when others =>
         return 0;
   end Get;

   function Get
     (SID : in ID;
      Key : in String)
      return Float
   is
      Value : Unbounded_String;
   begin
      Database.Get_Value (SID, Key, Value);
      return Float'Value (To_String (Value));
   exception
      when others =>
         return 0.0;
   end Get;

   ------------------
   -- Get_Lifetime --
   ------------------

   function Get_Lifetime return Duration is
   begin
      return Session_Lifetime;
   end Get_Lifetime;

   -----------
   -- Image --
   -----------

   function Image (SID : in ID) return String is
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
            SID : constant ID := ID'Input (Stream_Ptr);
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

   ------------
   -- Remove --
   ------------

   procedure Remove
     (SID : in ID;
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
      Sessions   : Session_Set_Access;
      Stream_Ptr : Stream_Access;

      procedure Process
        (Key      : in     ID;
         Value    : in     Session_Node;
         Order    : in     Positive;
         Continue : in out Boolean);
      --  Callback for each session node in the table.

      -------------
      -- Process --
      -------------

      procedure Process
        (Key      : in     ID;
         Value    : in     Session_Node;
         Order    : in     Positive;
         Continue : in out Boolean)
      is

         procedure Process
           (Key      : in     String;
            Value    : in     Unbounded_String;
            Order    : in     Positive;
            Continue : in out Boolean);
         --  Callback for each key/value pair for a specific session.

         -------------
         -- Process --
         -------------

         procedure Process
           (Key      : in     String;
            Value    : in     Unbounded_String;
            Order    : in     Positive;
            Continue : in out Boolean) is
         begin
            String'Output (Stream_Ptr, Key);
            String'Output (Stream_Ptr, To_String (Value));
         end Process;

         --------------------
         -- Each_Key_Value --
         --------------------

         procedure Each_Key_Value is
            new Key_Value.Table.Disorder_Traverse_G (Process);

         Key_Value_Size : constant Natural := Key_Value.Size (Value.Root);

      begin
         if Key_Value_Size > 0 then
            ID'Output (Stream_Ptr, Key);
            Natural'Output (Stream_Ptr, Key_Value_Size);
            Each_Key_Value (Key_Value.Table.Table_Type (Value.Root));
         end if;
      end Process;

      ------------------
      -- Each_Session --
      ------------------

      procedure Each_Session is
         new Session_Set.Disorder_Traverse_G (Process);

   begin
      Create (File, Name => File_Name);

      Database.Get_Sessions_And_Lock (Sessions);

      begin
         Stream_Ptr := Stream (File);
         Each_Session (Sessions.all);
      exception
         when others =>
            --  Never leave this block without releasing the database lock.
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
     (SID   : in ID;
      Key   : in String;
      Value : in String) is
   begin
      Database.Set_Value (SID, Key, Value);
   end Set;

   procedure Set
     (SID   : in ID;
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
     (SID   : in ID;
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

   ------------------
   -- Set_Lifetime --
   ------------------

   procedure Set_Lifetime (Seconds : in Duration) is
   begin
      Session_Lifetime := Seconds;
   end Set_Lifetime;

   -----------
   -- Value --
   -----------

   function Value (SID : in String) return ID is
   begin
      if SID'Length /= ID'Length + SID_Prefix'Length
        or else
        (SID'Length > SID_Prefix'Length
           and then
         SID (SID'First .. SID'First + SID_Prefix'Length - 1) /= SID_Prefix)
      then
         return No_Session;
      else
         return ID (SID (SID'First + SID_Prefix'Length .. SID'Last));
      end if;
   end Value;

begin
   SID_Random.Reset (SID_Generator);
end AWS.Session;
