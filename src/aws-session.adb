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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config;
with AWS.Key_Value;

with Avl_Tree_Generic;

package body AWS.Session is

   package SID_Random is new Ada.Numerics.Discrete_Random (ID);

   use SID_Random;
   use Ada;
   use Ada.Strings.Unbounded;

   SID_Generator          : Generator;
   SID_Prefix             : constant String := "SID-";

   Session_Check_Interval : constant Duration
     := Config.Session_Cleanup_Interval;
   --  Check for obsolete section every 10 minutes.

   Session_Lifetime       : Duration := Config.Session_Lifetime;
   --  A session is obsolete if not used after Session_Lifetime seconds.

   --  table of session ID

   type Session_Node is record
      SID        : ID;
      Time_Stamp : Calendar.Time;
      Root       : AWS.Key_Value.Set;
   end record;

   function Key_For (Item : in Session_Node) return ID;
   --  returns the Key for Session_Node

   function Key_For (Item : in Session_Node) return ID is
   begin
      return Item.SID;
   end Key_For;

   package Session_Set is new Avl_Tree_Generic (ID, Session_Node, Key_For);

   --------------
   -- Database --
   --------------

   protected Database is

      entry New_Session (SID : out ID);
      --  Add a new session SID into the database.

      entry Delete_Session (SID : in ID);
      --  Removes session SID from the Tree.

      function Session_Exist (SID : in ID) return Boolean;
      --  Return True if session SID exist in the database.

      entry Key_Exist (SID    : in     ID;
                       Key    : in     String;
                       Result :    out Boolean);
      --  Result is set to True if Key_Name exist in session SID.

      entry Get_Value (SID   : in     ID;
                       Key   : in     String;
                       Value :    out Unbounded_String);
      --  Value is set with the value associated with the key Key_Name in
      --  session SID.

      entry Set_Value (SID        : in ID;
                       Key, Value : in String);
      --  Add the pair key/value into the session SID.

      entry Remove_Key (SID : in ID;
                        Key : in String);
      --  Removes Key from the session SID.

      entry Clean;
      --  Removes old session data that are older than Session_Lifetime
      --  seconds.

      --
      --  Not safe routines. These are only to be used by iterators and the
      --  task cleaner.
      --

      procedure Get_Sessions_And_Lock (Sessions : out Session_Set.Avl_Tree);
      --  Increment Lock by 1, all entries are lockedand return the Sessions
      --  tree

      procedure Unlock;
      --  Decrement Lock by 1, unlock all entries when Lock return to 0.

   private

      Lock     : Natural := 0;

      Sessions : Session_Set.Avl_Tree;

   end Database;

   task type Cleaner;
   --  Call Database.Clean every Session_Lifetime seconds.

   type Cleaner_Access is access Cleaner;

   Cleaner_Task : Cleaner_Access;

   -------------
   -- Cleaner --
   -------------

   task body Cleaner is
      use Ada;
      use type Calendar.Time;

      Next_Run : Calendar.Time := Calendar.Clock + Session_Check_Interval;
   begin
      loop
         delay until Next_Run;
         Database.Clean;
         Next_Run := Next_Run + Session_Check_Interval;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Unrecoverable Error : Cleaner Task bug detected"
            & Exceptions.Exception_Message (E));
   end Cleaner;

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

   type Session_Node_Array is array (Integer range <>) of Session_Node;

   protected body Database is

      -----------
      -- Clean --
      -----------

      entry Clean when Lock = 0 is

         Max_Remove : constant := 200;
         --  Maximum number of items that will get removed at a time. Other
         --  items will be check for removal during next run.

         Remove : Session_Node_Array (1 .. Max_Remove);
         --  ??? can't use anonymous array here, GNAT bug TN 9023-002
         --  Fixed in GNAT 3.15w (20010625).

         Index : Natural := 0;

         Now : constant Calendar.Time := Calendar.Clock;

         procedure Process (Session  : in out Session_Node;
                            Continue :    out Boolean);
         --  Iterator callback

         procedure Process (Session  : in out Session_Node;
                            Continue :    out Boolean)
         is
            use type Calendar.Time;
         begin
            if Session.Time_Stamp + Session_Lifetime < Now then
               Index := Index + 1;
               Remove (Index) := Session;

               if Index = Max_Remove then
                  --  No more space in the removal buffer, quit now.
                  Continue := False;
               end if;
            end if;

            Continue := True;
         end Process;

         procedure In_Order is
            new Session_Set.In_Order_Tree_Traversal (Process);

      begin
         In_Order (Sessions);

         --  delete nodes

         for K in 1 .. Index loop
            Key_Value.Delete_Tree (Remove (K).Root);
            Session_Set.Delete_Node (Key_For (Remove (K)), Sessions);
         end loop;
      end Clean;

      --------------------
      -- Delete_Session --
      --------------------

      entry Delete_Session (SID : in ID) when Lock = 0 is

         N  : Session_Node;

      begin
         Session_Set.Inquire (SID, Sessions, N);

         Key_Value.Delete_Tree (N.Root);

         Session_Set.Delete_Node (SID, Sessions);
      exception
         when Key_Value.Tree.Node_Not_Found =>
            raise Internal_Error;
      end Delete_Session;

      ---------------
      -- Get_Value --
      ---------------

      entry Get_Value (SID   : in     ID;
                       Key   : in     String;
                       Value :    out Unbounded_String)
        when Lock = 0
      is
         N  : Session_Node;
         KV : Key_Value.Data;
      begin
         Session_Set.Inquire (SID, Sessions, N);

         N.Time_Stamp := Calendar.Clock;

         Session_Set.Update_Node (N, Sessions);

         Key_Value.Inquire (Key, N.Root, KV);
         Value := KV.Value;
      exception
         when others =>
            Value := Null_Unbounded_String;
      end Get_Value;

      ---------------
      -- Key_Exist --
      ---------------

      entry Key_Exist (SID    : in     ID;
                       Key    : in     String;
                       Result :    out Boolean)
        when Lock = 0
      is
         N  : Session_Node;
         KV : Key_Value.Data;
      begin
         Session_Set.Inquire (SID, Sessions, N);

         N.Time_Stamp := Calendar.Clock;

         Session_Set.Update_Node (N, Sessions);

         Key_Value.Inquire (Key, N.Root, KV);
         Result := True;
      exception
         when Key_Value.Tree.Node_Not_Found =>
            Result := False;
      end Key_Exist;

      -----------------
      -- New_Session --
      -----------------

      entry New_Session (SID : out ID) when Lock = 0 is

         New_Node : Session_Node;

      begin

         New_Node.Time_Stamp := Calendar.Clock;

         loop
            New_Node.SID := Random (SID_Generator);
            SID := New_Node.SID;

            begin
               Session_Set.Insert_Node (New_Node, Sessions);
               exit;
            exception
               when Session_Set.Duplicate_Key =>
                  --  very low probability but we should catch it
                  --  and try to generate unique key again.
                  null;
            end;
         end loop;
      end New_Session;

      ------------
      -- Remove --
      ------------

      entry Remove_Key (SID : in ID;
                        Key : in String) when Lock = 0
      is
         N : Session_Node;
      begin
         Session_Set.Inquire (SID, Sessions, N);
         N.Time_Stamp := Calendar.Clock;

         begin
            Key_Value.Delete_Node (Key, N.Root);
         exception
            when Key_Value.Tree.Node_Not_Found =>
               null;
         end;

         --  set back the node

         Session_Set.Update_Node (N, Sessions);
      exception
         when others =>
            null;
      end Remove_Key;

      -------------------
      -- Session_Exist --
      -------------------

      function Session_Exist (SID : in ID) return Boolean is
         N  : Session_Node;
      begin
         Session_Set.Inquire (SID, Sessions, N);
         return True;
      exception
         when Session_Set.Node_Not_Found =>
            return False;
      end Session_Exist;

      ---------------
      -- Set_Value --
      ---------------

      entry Set_Value (SID        : in ID;
                       Key, Value : in String) when Lock = 0
      is
         N  : Session_Node;
         KV : Key_Value.Data := (To_Unbounded_String (Key),
                                 To_Unbounded_String (Value));
      begin
         Session_Set.Inquire (SID, Sessions, N);
         N.Time_Stamp := Calendar.Clock;

         begin
            Key_Value.Insert_Node (KV, N.Root);
         exception
            when Key_Value.Tree.Duplicate_Key =>
               Key_Value.Update_Node (KV, N.Root);
         end;

         --  set back the node

         Session_Set.Update_Node (N, Sessions);
      exception
         when others =>
            null;
      end Set_Value;

      ---------------------------
      -- Get_Sessions_And_Lock --
      ---------------------------

      procedure Get_Sessions_And_Lock (Sessions : out Session_Set.Avl_Tree) is
      begin
         Lock     := Lock + 1;
         Sessions := Database.Sessions;
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
   exception
      when others =>
         return False;
   end Exist;

   function Exist (SID : in ID;
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

      procedure Process (Session  : in out Session_Node;
                         Continue :    out Boolean);
      --  iterator callback

      Index    : Positive := 1;

      -------------
      -- Process --
      -------------

      procedure Process (Session  : in out Session_Node;
                         Continue :    out Boolean)
      is
         Quit : Boolean := False;
      begin
         Action (Index,
                 Session.SID,
                 Session.Time_Stamp,
                 Quit);

         Continue := not Quit;
         Index := Index + 1;
      end Process;

      --------------
      -- In_Order --
      --------------

      procedure In_Order is
         new Session_Set.In_Order_Tree_Traversal (Process);

      Sessions : Session_Set.Avl_Tree;

   begin
      Database.Get_Sessions_And_Lock (Sessions);
      In_Order (Sessions);
      Database.Unlock;
   exception
      when others =>
         Database.Unlock;
   end For_Every_Session;

   ----------------------------
   -- For_Every_Session_Data --
   ----------------------------

   procedure For_Every_Session_Data (SID : in ID) is

      procedure Process (KV       : in out Key_Value.Data;
                         Continue :    out Boolean);
      --  iterator callback

      Index    : Positive := 1;

      -------------
      -- Process --
      -------------

      procedure Process (KV       : in out Key_Value.Data;
                         Continue :    out Boolean)
      is
         Quit : Boolean := False;
      begin
         Action (Index,
                 To_String (KV.Key),
                 To_String (KV.Value),
                 Quit);

         Continue := not Quit;
         Index := Index + 1;
      end Process;

      --------------
      -- In_Order --
      --------------

      procedure In_Order is
         new Key_Value.Tree.In_Order_Tree_Traversal (Process);

      N        : Session_Node;
      Sessions : Session_Set.Avl_Tree;

   begin
      Database.Get_Sessions_And_Lock (Sessions);

      Session_Set.Inquire (SID, Sessions, N);

      In_Order (Key_Value.Tree.Avl_Tree (N.Root));

      Database.Unlock;
   exception
      when others =>
         Database.Unlock;
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
      IID : constant String := ID'Image (SID);
   begin
      return SID_Prefix & IID (2 .. IID'Last);
   end Image;

   ------------
   -- Remove --
   ------------

   procedure Remove (SID : in ID;
                     Key : in String) is
   begin
      Database.Remove_Key (SID, Key);
   end Remove;

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
   -- Start --
   -----------

   procedure Start is
   begin
      Cleaner_Task := new Cleaner;
   end Start;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      abort Cleaner_Task.all;
   end Shutdown;

   -----------
   -- Value --
   -----------

   function Value (SID : in String) return ID is
   begin
      return ID'Value (SID (SID'First + SID_Prefix'Length .. SID'Last));
   end Value;

begin
   Reset (SID_Generator);
end AWS.Session;
