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

with Table_Of_Strings_And_Static_Values_G;

with AWS.Utils;

package body AWS.Services.Transient_Pages is

   --  ??? This is the first implementation mostly as a proof-of concept. The
   --  real implementation will have a task to release the stream after theirs
   --  life-time and eventually move them to disk after a certain period...

   use Ada;

   type Item is record
      Stream      : AWS.Resources.Streams.Stream_Access;
      Delete_Time : Calendar.Time;
   end record;

   package Table is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", Item);

   Resources : Table.Table_Type;

   K : Natural := 0;

   ---------
   -- Get --
   ---------

   function Get (URI : in String) return AWS.Resources.Streams.Stream_Access is
      Result : Item;
      Found  : Boolean;
   begin
      Table.Get_Value (Resources, URI, Result, Found);

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
   begin
      K := K + 1;
      return "uri" & Utils.Image (K);
   end Get_URI;

   --------------
   -- Register --
   --------------

   procedure Register
     (URI       : in String;
      Resource  : in AWS.Resources.Streams.Stream_Access;
      Life_Time : in Duration := Default_Life_Time)
   is
      use type Calendar.Time;
   begin
      Table.Insert (Resources, URI, (Resource, Calendar.Clock + Life_Time));
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
