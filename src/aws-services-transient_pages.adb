------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2006                          --
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
with Ada.Exceptions;
with Ada.Text_IO;

with Strings_Maps;

with AWS.Utils;

package body AWS.Services.Transient_Pages is

   use Ada;

   type Item is record
      Stream      : AWS.Resources.Streams.Stream_Access;
      Delete_Time : Calendar.Time;
   end record;

   package Table_Container is new Strings_Maps (Item, "=");
   package Table renames Table_Container.Containers;

   subtype ID is String (1 .. 25);
   --  Random ID generated as transient page identity, the five first
   --  characters are a number with a set of '$' character as prefix the 20
   --  next characters are completely random.

   Clean_Interval : Duration;
   --  Interval between each run of the cleaner task

   --  Concurrent access for the transient pages

   protected Database is

      procedure Generate_ID (URI : out ID);
      --  Generate a unique ID used to create a transient URI

      procedure Register
        (URI      : in String;
         Resource : in Item);
      --  Register URI into the database

      procedure Get_Value
        (URI    : in     String;
         Result :    out Item;
         Found  :    out Boolean);
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
        (Transient_Check_Interval : in     Duration;
         Need_Start               :    out Boolean) is
      begin
         Need_Start     := Server_Count = 0 and then Cleaner_Task = null;
         Server_Count   := Server_Count + 1;
         Clean_Interval := Transient_Check_Interval;
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
      use type Calendar.Time;
      Next : Calendar.Time := Calendar.Clock + Clean_Interval;
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
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error,
            "Transient_Pages cleaner error: "
            & Ada.Exceptions.Exception_Information (E));
   end Cleaner;

   --------------
   -- Database --
   --------------

   protected body Database is

      -----------
      -- Clean --
      -----------

      procedure Clean is
         use type Calendar.Time;

         Now    : constant Calendar.Time := Calendar.Clock;
         Cursor : Table.Cursor;
      begin
         Cursor := Table.First (Resources);

         while Table.Has_Element (Cursor) loop
            declare
               Value : constant Item := Table.Element (Cursor);
            begin
               if Now > Value.Delete_Time then
                  declare
                     Current : Table.Cursor := Cursor;
                  begin
                     Table.Next (Cursor);
                     Table.Delete (Resources, Current);

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

         type NID is new AWS.Utils.Random_Integer;

         Chars : constant String
           := "0123456789"
                & "abcdefghijklmnopqrstuvwxyz"
                & "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

         Rand   : NID := 0;
         Result : ID;

         K_Img  : constant String := Natural'Image (K);
         J      : Positive := K_Img'First + 1;
      begin
         --  Fill with '$'

         for I in 1 .. 6 - K_Img'Length loop
            Result (I) := '$';
         end loop;

         --  Add the number

         for I in 6 - K_Img'Length + 1 .. 5 loop
            Result (I) := K_Img (J);
            J := J + 1;
         end loop;

         --  Fill the next 20 characters with random characters

         for I in 6 .. ID'Last loop
            if Rand = 0 then
               Rand := Random;
            end if;

            Result (I) := Chars (Integer (Rand rem Chars'Length) + 1);
            Rand := Rand / Chars'Length;
         end loop;

         K := K + 1;

         if K >= 100_000 then
            K := 0;
         end if;

         URI := Result;
      end Generate_ID;

      ---------------
      -- Get_Value --
      ---------------

      procedure Get_Value
        (URI    : in     String;
         Result :    out Item;
         Found  :    out Boolean)
      is
         Cursor : Table.Cursor;
      begin
         Cursor := Table.Find (Resources, URI);

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
        (URI      : in     String;
         Resource : in     Item)
      is
         Cursor  : Table.Cursor;
         Success : Boolean;
      begin
         Table.Insert (Resources, URI, Resource, Cursor, Success);
      end Register;

   end Database;

   ---------
   -- Get --
   ---------

   function Get (URI : in String) return AWS.Resources.Streams.Stream_Access is
      Result : Item;
      Found  : Boolean;
   begin
      Database.Get_Value (URI, Result, Found);

      if Found then
         --  Reset the stream pointer to the stream's first byte
         AWS.Resources.Streams.Reset (Result.Stream.all);
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
     (URI      : in String;
      Resource : in AWS.Resources.Streams.Stream_Access;
      Lifetime : in Duration := Default.Transient_Lifetime)
   is
      use type Calendar.Time;
   begin
      Database.Register (URI, (Resource, Calendar.Clock + Lifetime));
   end Register;

end AWS.Services.Transient_Pages;
