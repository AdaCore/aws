------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Strings.Hash;
with Ada.Text_IO;

with AWS.Utils;

package body AWS.Services.Transient_Pages is

   use Ada;

   type Item is record
      Stream      : Resources.Streams.Stream_Access;
      Delete_Time : Real_Time.Time;
   end record;

   package Table is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Item, Ada.Strings.Hash, "=", "=");

   subtype ID is String (1 .. 25);
   --  Random ID generated as transient page identity, the five first
   --  characters are a number with a set of '$' character as prefix the 20
   --  next characters are completely random.

   Clean_Interval : Real_Time.Time_Span;
   --  Interval between each run of the cleaner task

   --  Concurrent access for the transient pages

   protected Database is

      procedure Generate_ID (URI : out ID);
      --  Generate a unique ID used to create a transient URI

      procedure Register
        (URI      : String;
         Resource : Item);
      --  Register URI into the database

      procedure Get_Value
        (URI    : String;
         Result : out Item;
         Found  : out Boolean);
      --  Returns URI's information or set Found to False if not found

      procedure Clean;
      --  Removes obsolete items from the table

   private
      K         : Natural := 0;
      Resources : Table.Map;
   end Database;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected body Cleaner_Control is

      --------------
      -- Register --
      --------------

      procedure Register
        (Transient_Check_Interval : Duration;
         Need_Start               : out Boolean) is
      begin
         Need_Start     := Server_Count = 0 and then Cleaner_Task = null;
         Server_Count   := Server_Count + 1;
         Clean_Interval := Real_Time.To_Time_Span (Transient_Check_Interval);
      end Register;

      ----------
      -- Stop --
      ----------

      procedure Stop (Need_Release : out Boolean) is
      begin
         Server_Count := Server_Count - 1;
         Need_Release := (Server_Count = 0 and then Cleaner_Task /= null);
      end Stop;

   end Cleaner_Control;

   -------------
   -- Cleaner --
   -------------

   task body Cleaner is
      use type Real_Time.Time;
      Next : Real_Time.Time := Real_Time.Clock + Clean_Interval;
   begin
      Clean : loop
         select
            accept Stop;
            exit Clean;
         or
            delay until Next;
         end select;

         Database.Clean;

         Next := Next + Clean_Interval;
      end loop Clean;

   exception
      when E : others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Transient_Pages cleaner error: "
            & Exceptions.Exception_Information (E));
   end Cleaner;

   --------------
   -- Database --
   --------------

   protected body Database is

      -----------
      -- Clean --
      -----------

      procedure Clean is
         use type Real_Time.Time;

         Now    : constant Real_Time.Time := Real_Time.Clock;
         Cursor : Table.Cursor;
      begin
         Cursor := Resources.First;

         while Table.Has_Element (Cursor) loop
            declare
               Value : constant Item := Table.Element (Cursor);
            begin
               if Now > Value.Delete_Time then
                  declare
                     Current : Table.Cursor := Cursor;
                  begin
                     Table.Next (Cursor);
                     Resources.Delete (Current);

                     declare
                        Resource : AWS.Resources.File_Type;
                     begin
                        AWS.Resources.Streams.Create (Resource, Value.Stream);
                        AWS.Resources.Close (Resource);
                     end;
                  end;

               else
                  Table.Next (Cursor);
               end if;
            end;
         end loop;
      end Clean;

      -----------------
      -- Generate_ID --
      -----------------

      procedure Generate_ID (URI : out ID) is
         K_Img : constant String := Utils.Image (K);
      begin
         --  Fill with '$'

         URI (1 .. 5 - K_Img'Length) := (others => '$');

         --  Add the number

         URI (5 - K_Img'Length + 1 .. 5) := K_Img;

         --  Fill the next 20 characters with random characters

         Utils.Random_String (URI (6 .. URI'Last));

         K := K + 1;

         if K >= 100_000 then
            K := 0;
         end if;
      end Generate_ID;

      ---------------
      -- Get_Value --
      ---------------

      procedure Get_Value
        (URI    : String;
         Result : out Item;
         Found  : out Boolean)
      is
         Cursor : Table.Cursor;
      begin
         Cursor := Resources.Find (URI);

         if Table.Has_Element (Cursor) then
            Found  := True;
            Result := Table.Element (Cursor);
         else
            Found  := False;
         end if;
      end Get_Value;

      --------------
      -- Register --
      --------------

      procedure Register
        (URI      : String;
         Resource : Item)
      is
         Cursor  : Table.Cursor;
         Success : Boolean;
      begin
         Resources.Insert (URI, Resource, Cursor, Success);
      end Register;

   end Database;

   ---------
   -- Get --
   ---------

   function Get (URI : String) return Resources.Streams.Stream_Access is
      Result : Item;
      Found  : Boolean;
   begin
      Database.Get_Value (URI, Result, Found);

      if Found then
         --  Reset the stream pointer to the stream's first byte
         Resources.Streams.Reset (Result.Stream.all);
         return Result.Stream;
      else
         return null;
      end if;
   end Get;

   -------------
   -- Get_URI --
   -------------

   function Get_URI return String is
      URI : ID;
   begin
      Database.Generate_ID (URI);
      return "/transient/" & URI;
   end Get_URI;

   --------------
   -- Register --
   --------------

   procedure Register
     (URI      : String;
      Resource : Resources.Streams.Stream_Access;
      Lifetime : Duration := Config.Transient_Lifetime)
   is
      use Real_Time;
   begin
      Database.Register (URI, (Resource, Clock + To_Time_Span (Lifetime)));
   end Register;

end AWS.Services.Transient_Pages;
