------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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

with Ada.Strings.Unbounded;

with AWS.Key_Value;

with Avl_Tree_Generic;

package body AWS.Session is

   use Ada;
   use Ada.Strings.Unbounded;

   Session_Lifetime : Duration := Default_Session_Lifetime;

   --  table of session ID

   type Session_Node is record
      SID        : Unbounded_String;
      Time_Stamp : Calendar.Time;
      Root       : AWS.Key_Value.Set;
   end record;

   function Key_For (Item : in Session_Node) return String;
   --  returns the Key for Session_Node

   function Key_For (Item : in Session_Node) return String is
   begin
      return To_String (Item.SID);
   end Key_For;

   package Session_Set is new Avl_Tree_Generic (String, Session_Node, Key_For);

   --------------
   -- Database --
   --------------

   protected Database is

      entry New_Session (SID : out Session.ID);
      --  add a new session named Session_Name into the database

      function Session_Exist (Session_Name : in String) return Boolean;
      --  return True if session named Session_Name exist in the database

      entry Key_Exist (Session_Name, Key : in    String;
                       Result            :    out Boolean);
      --  Result is set to True if Key_Name exist in session named
      --  Session_Name.

      entry Get_Value (Session_Name, Key : in     String;
                       Value             :    out Unbounded_String);
      --  Value is set with the value associated with the key Key_Name in
      --  session Session_Name.

      entry Set_Value (Session_Name, Key, Value : in String);
      --  Add the pair key/value into the session named Session_Name

      entry Clean;
      --  Removes old session data that are older than Session_Lifetime
      --  seconds.

      --
      --  Not safe routine. These are only to be used by iterators and the
      --  task cleaner.
      --

      procedure Get_Sessions_And_Lock (Sessions : out Session_Set.Avl_Tree);
      --  Increment Lock by 1, all entries are lockedand return the Sessions
      --  tree

      procedure Unlock;
      --  Decrement Lock by 1, unlock all entries when Lock return to 0.

   private

      Lock     : Natural := 0;

      ID       : Session.ID := 0;

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
      use type Calendar.Time;

      Next_Run : Calendar.Time := Calendar.Clock + Session_Lifetime;
   begin
      loop
         delay until Next_Run;
         Database.Clean;
         Next_Run := Next_Run + Session_Lifetime;
      end loop;
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

   protected body Database is

      -----------
      -- Clean --
      -----------

      entry Clean when Lock = 0 is

         Now : Calendar.Time := Calendar.Clock;

         procedure Process (Session  : in out Session_Node;
                            Continue :    out Boolean);
         --  Iterator callback

         procedure Process (Session  : in out Session_Node;
                            Continue :    out Boolean)
         is
            use type Calendar.Time;
         begin
            if Session.Time_Stamp + Session_Lifetime < Now then

               Key_Value.Delete_Tree (Session.Root);
               Session_Set.Delete_Node (Key_For (Session), Sessions);

               --  after deleting a Session it is not safe to continue the
               --  iterator.
               Continue := False;
            end if;

            Continue := True;
         end Process;

         procedure In_Order is
            new Session_Set.In_Order_Tree_Traversal (Process);

      begin
         In_Order (Sessions);
      end Clean;

      ---------------
      -- Get_Value --
      ---------------

      entry Get_Value (Session_Name, Key : in     String;
                       Value             :    out Unbounded_String)
        when Lock = 0
      is
         N  : Session_Node;
         KV : Key_Value.Data;
      begin
         Session_Set.Inquire (Session_Name, Sessions, N);

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

      entry Key_Exist (Session_Name, Key : in    String;
                       Result            :    out Boolean)
        when Lock = 0
      is
         N  : Session_Node;
         KV : Key_Value.Data;
      begin
         Session_Set.Inquire (Session_Name, Sessions, N);

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

      entry New_Session (SID : out Session.ID) when Lock = 0 is
         New_Node : Session_Node;
      begin
         SID := ID;
         New_Node.SID        := To_Unbounded_String (Image (ID));
         New_Node.Time_Stamp := Calendar.Clock;

         ID := ID + 1;

         Session_Set.Insert_Node (New_Node, Sessions);
      end New_Session;

      -------------------
      -- Session_Exist --
      -------------------

      function Session_Exist (Session_Name : in String) return Boolean is
         N  : Session_Node;
      begin
         Session_Set.Inquire (Session_Name, Sessions, N);
         return True;
      exception
         when Session_Set.Node_Not_Found =>
            return False;
      end Session_Exist;

      ---------------
      -- Set_Value --
      ---------------

      entry Set_Value (Session_Name, Key, Value : in String) when Lock = 0 is
         N  : Session_Node;
         KV : Key_Value.Data := (To_Unbounded_String (Key),
                                 To_Unbounded_String (Value));
      begin
         Session_Set.Inquire (Session_Name, Sessions, N);
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

   -----------
   -- Exist --
   -----------

   function Exist (SID : in ID) return Boolean is
   begin
      return Database.Session_Exist (Image (SID));
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
      Database.Key_Exist (Image (SID), Key, Result);
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
                 Value (To_String (Session.SID)),
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

      Session_Set.Inquire (Image (SID), Sessions, N);

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
      Database.Get_Value (Image (SID), Key, Value);
      return To_String (Value);
   end Get;

   function Get
     (SID : in ID;
      Key : in String)
      return Integer
   is
      Value : Unbounded_String;
   begin
      Database.Get_Value (Image (SID), Key, Value);
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
      Database.Get_Value (Image (SID), Key, Value);
      return Float'Value (To_String (Value));
   exception
      when others =>
         return 0.0;
   end Get;

   -----------
   -- Image --
   -----------

   function Image (SID : in ID) return String is
      IID : constant String := ID'Image (SID);
   begin
      return "sid-" & IID (2 .. IID'Last);
   end Image;

   ---------
   -- Set --
   ---------

   procedure Set
     (SID   : in ID;
      Key   : in String;
      Value : in String)
   is
   begin
      Database.Set_Value (Image (SID), Key, Value);
   end Set;

   procedure Set
     (SID   : in ID;
      Key   : in String;
      Value : in Integer)
   is
      V : constant String := Integer'Image (Value);
   begin
      if V (1) = ' ' then
         Database.Set_Value (Image (SID), Key, V (2 .. V'Last));
      else
         Database.Set_Value (Image (SID), Key, V);
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
         Database.Set_Value (Image (SID), Key, V (2 .. V'Last));
      else
         Database.Set_Value (Image (SID), Key, V);
      end if;
   end Set;

   --------------------------
   -- Set_Session_Lifetime --
   --------------------------

   procedure Set_Session_Lifetime (Seconds : in Duration) is
   begin
      Session_Lifetime := Seconds;
   end Set_Session_Lifetime;

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
      return ID'Value (SID (5 .. SID'Last));
   end Value;

end AWS.Session;
