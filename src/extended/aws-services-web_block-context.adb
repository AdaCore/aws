------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2015, AdaCore                     --
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

with Ada.Calendar;
with Ada.Containers.Hashed_Maps;

with AWS.Config;
with AWS.Utils.Streams;

package body AWS.Services.Web_Block.Context is

   Max_Id_Deleted : constant := 100;
   --  Maximum number of contexts deleted in one pass

   function Hash (Key : Id) return Containers.Hash_Type;

   type Context_Stamp is record
      C       : Object;
      Created : Calendar.Time; -- used to delete when expired
   end record;

   package Contexts is new Containers.Hashed_Maps
     (Id, Context_Stamp, Hash, "=");

   --  Concurrent Database

   protected Database is

      procedure Clean;
      --  Removes expired contexts

      function Contains (CID : Id) return Boolean;
      --  Returns True if context CID is in the database

      function Get (CID : Id) return Object;
      --  Retruns the context object

      procedure Include (Context : Object; CID : Id)
        with Post => Contains (CID);
      --  Add or update context into the database

   private
      DB : Contexts.Map;
   end Database;

   --------------
   -- Database --
   --------------

   protected body Database is

      -----------
      -- Clean --
      -----------

      procedure Clean is
         use type Ada.Calendar.Time;
         Now      : constant Calendar.Time := Calendar.Clock;
         Elapsed  : constant Duration := Config.Context_Lifetime;
         CIDS     : array (1 .. Max_Id_Deleted) of Id;
         Last     : Natural := 0;
         Position : Contexts.Cursor := DB.First;
      begin
         --  First check for obsolete context

         while Contexts.Has_Element (Position) and then Last < CIDS'Last loop
            if Now - Contexts.Element (Position).Created > Elapsed then
               Last := Last + 1;
               CIDS (Last) := Contexts.Key (Position);
            end if;

            Contexts.Next (Position);
         end loop;

         for K in CIDS'First .. Last loop
            DB.Delete (CIDS (K));
         end loop;
      end Clean;

      --------------
      -- Contains --
      --------------

      function Contains (CID : Id) return Boolean is
      begin
         return DB.Contains (CID);
      end Contains;

      ---------
      -- Get --
      ---------

      function Get (CID : Id) return Object is
      begin
         if DB.Contains (CID) then
            return DB.Element (CID).C;
         else
            return Empty;
         end if;
      end Get;

      -------------
      -- Include --
      -------------

      procedure Include (Context : Object; CID : Id) is
      begin
         Clean;

         --  Register new context

         DB.Include (CID, Context_Stamp'(Context, Calendar.Clock));
      end Include;

   end Database;

   -----------
   -- Exist --
   -----------

   function Exist (Context : Object; Name : String) return Boolean is
   begin
      return Context.Contains (Name);
   end Exist;

   function Exist (CID : Id) return Boolean is
   begin
      return Database.Contains (CID);
   end Exist;

   ---------
   -- Get --
   ---------

   function Get (CID : Id) return Object is
   begin
      return Database.Get (CID);
   end Get;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Context : Object; Name : String) return String is
   begin
      if Context.Contains (Name) then
         return Context.Element (Name);
      else
         return "";
      end if;
   end Get_Value;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Id) return Containers.Hash_Type is
   begin
      return Strings.Hash (String (Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (CID : Id) return String is
   begin
      return String (CID);
   end Image;

   --------------
   -- Register --
   --------------

   function Register (Context : Object) return Id is
      Stream : aliased Utils.Streams.SHA1;
      CID    : Id;
   begin
      Object'Output (Stream'Access, Context);
      CID := Id (Utils.Streams.Value (Stream'Access));

      Database.Include (Context, CID);
      return CID;
   end Register;

   ------------
   -- Remove --
   ------------

   procedure Remove (Context : in out Object; Name : String) is
   begin
      Context.Exclude (Name);
   end Remove;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Context : in out Object; Name, Value : String) is
   begin
      Context.Include (Name, Value);
   end Set_Value;

   -----------
   -- Value --
   -----------

   function Value (CID : String) return Id is
   begin
      if CID'Length = Id'Length then
         return Id (CID);
      else
         return Id'(others => 'x');
      end if;
   end Value;

   ------------------
   -- Generic_Data --
   ------------------

   package body Generic_Data is

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Context : Object; Name : String) return Data is
         Position : constant KV.Cursor := Context.Find (Name);
      begin
         if KV.Has_Element (Position) then
            declare
               Result : constant String := KV.Element (Position);
               Str    : aliased Utils.Streams.Strings;
               Value  : Data;
            begin
               Utils.Streams.Open (Str, Result);
               Data'Read (Str'Access, Value);
               return Value;
            end;

         else
            return Null_Data;
         end if;
      end Get_Value;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Context : in out Object;
         Name    : String;
         Value   : Data)
      is
         Str : aliased Utils.Streams.Strings;
      begin
         Data'Write (Str'Access, Value);
         Context.Include (Name, Utils.Streams.Value (Str'Access));
      end Set_Value;

   end Generic_Data;

end AWS.Services.Web_Block.Context;
