------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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
with Ada.Strings.Unbounded;

with Table_Of_Strings_And_Static_Values_G;

with AWS.Resources.Release;
with AWS.Utils;

package body AWS.Services.Transient_Pages is

   use Ada;
   use Ada.Strings.Unbounded;

   Max_Obsolete : constant := 30;

   type Item is record
      Stream      : AWS.Resources.Streams.Stream_Access;
      Delete_Time : Calendar.Time;
   end record;

   package Table is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", Item);

   subtype ID is String (1 .. 25);
   --  Random ID generated as transient page identity

   Obsolete : array (1 .. Max_Obsolete) of Unbounded_String;
   O_Index  : Natural := 0;

   --  Concurrent access for the transient pages

   protected Database is

      procedure Generate_ID (URI : out ID);
      --  Generate a unique ID used to create a transient URI

      procedure Register
        (URI      : in String;
         Resource : in Item);
      --  Register URI into the database

      procedure Release (URI : in String);
      --  Release all memory associated with URI (the entry in the table and
      --  the memory stream).

      procedure Get_Value
        (URI    : in     String;
         Result :    out Item;
         Found  :    out Boolean);
      --  Returns URI's information or set Found to False if not found

      procedure Fill_Obsolete_Table;
      --  Add a set of obsolete objects into the obsolete table

   private
      K         : Natural := 0;
      Resources : Table.Table_Type;
   end Database;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected body Cleaner_Control is

      --------------
      -- Register --
      --------------

      procedure Register (Transient_Check_Interval : in Duration) is
         pragma Unreferenced (Transient_Check_Interval);
      begin
         Server_Count := Server_Count + 1;
         null;
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

      C_Interval : constant Duration := 60.0;
      --  ??? This should be a configuration option

      Next : Calendar.Time := Calendar.Clock + C_Interval;
   begin
      Clean : loop
         select
            accept Stop;
            exit Clean;
         or
            delay until Next;
         end select;

         Database.Fill_Obsolete_Table;

         for K in 1 .. O_Index loop
            Database.Release (To_String (Obsolete (K)));
            Obsolete (K) := Null_Unbounded_String;
         end loop;

         Next := Next + C_Interval;
      end loop Clean;
   end Cleaner;

   --------------
   -- Database --
   --------------

   protected body Database is

      -------------------------
      -- Fill_Obsolete_Table --
      -------------------------

      procedure Fill_Obsolete_Table is

         Now : constant Calendar.Time := Calendar.Clock;

         procedure Action
           (Key          : in     String;
            Value        : in     Item;
            Order_Number : in     Positive;
            Continue     : in out Boolean);
         --  Iterator callback

         procedure Action
           (Key          : in     String;
            Value        : in     Item;
            Order_Number : in     Positive;
            Continue     : in out Boolean)
         is
            pragma Unreferenced (Order_Number);

            use type Calendar.Time;
         begin
            if Now > Value.Delete_Time then
               O_Index := O_Index + 1;
               Obsolete (O_Index) := To_Unbounded_String (Key);

               if O_Index = Obsolete'Last then
                  Continue := False;
               end if;
            end if;
         end Action;

         procedure Check_Delete_Time is new Table.Traverse_Asc_G;

      begin
         O_Index := 0;

         Check_Delete_Time (Resources);
      end Fill_Obsolete_Table;

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
         for I in 1 .. 6 - K_Img'Length loop
            Result (I) := '$';
         end loop;

         for I in 6 - K_Img'Length + 1 .. 5 loop
            Result (I) := K_Img (J);
            J := J + 1;
         end loop;

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
         Found  :    out Boolean) is
      begin
         Table.Get_Value (Resources, URI, Result, Found);
      end Get_Value;

      --------------
      -- Register --
      --------------

      procedure Register
        (URI      : in String;
         Resource : in Item) is
      begin
         if Cleaner_Task = null then
            Cleaner_Task := new Cleaner;
         end if;

         Table.Insert (Resources, URI, Resource);
      end Register;

      -------------
      -- Release --
      -------------

      procedure Release (URI : in String) is
         Found  : Boolean;
         Result : Item;
      begin
         Table.Get_Value (Resources, URI, Result, Found);
         Table.Remove (Resources, URI);

         declare
            Resource : AWS.Resources.File_Type;
         begin
            AWS.Resources.Streams.Create (Resource, Result.Stream);

            --  Close the stream

            AWS.Resources.Streams.Memory.Close
              (AWS.Resources.Streams.Memory.Stream_Type (Result.Stream.all));

            --  Release the memory associated with the stream handle

            AWS.Resources.Release (Resource);
         end;
      end Release;

   end Database;

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out Stream_Type) is
      pragma Unreferenced (Resource);
   begin
      null;
   end Close;

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
         Reset (Stream_Type (Result.Stream.all));
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
      Lifetime : in Duration := Default_Lifetime)
   is
      use type Calendar.Time;
   begin
      Database.Register (URI, (Resource, Calendar.Clock + Lifetime));
   end Register;

   -------------
   -- Release --
   -------------

   procedure Release
     (Resource : in     Stream_Type;
      File     : in out AWS.Resources.File_Type)
   is
      pragma Unreferenced (Resource, File);
   begin
      null;
   end Release;

end AWS.Services.Transient_Pages;
