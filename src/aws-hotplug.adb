------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
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

with AWS.Client;
with AWS.Parameters;

package body AWS.Hotplug is

   use Ada.Strings.Unbounded;

   procedure Adjust (Filters : in out Filter_Set);
   --  Check that the filter set is large enough to receive a new value. If it
   --  is not, filter set will be ajusted.

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Filters : in out Filter_Set) is
      Old_Set : Filter_Array_Access;
   begin
      if Filters.Set = null then
         Filters.Set := new Filter_Array (1 .. 10);

      elsif Filters.Set'Length <= Filters.Count then
         Old_Set := Filters.Set;
         Filters.Set := new Filter_Array (1 .. Filters.Count + 5);
         Filters.Set.all (Old_Set'Range) := Old_Set.all;
      end if;
   end Adjust;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Filters : in     Filter_Set;
      Status  : in     AWS.Status.Data;
      Found   :    out Boolean;
      Data    :    out Response.Data)
   is
      URI : constant String := AWS.Status.URI (Status);
      P   : constant AWS.Parameters.List := AWS.Status.Parameters (Status);

      function Parameters return String;
      --  Returns the list of parameters suitable to send to a GET HTTP
      --  command "?name1=value1&name2=value2...".

      function Parameters return String is
         Result : Unbounded_String;
      begin
         for K in 1 .. AWS.Parameters.Count (P) loop
            if K = 1 then
               Append (Result, '?');
            else
               Append (Result, '&');
            end if;

            Append (Result, AWS.Parameters.Get_Name (P, K));
            Append (Result, '=' & AWS.Parameters.Get_Value (P, K));
         end loop;

         return To_String (Result);
      end Parameters;

      use type AWS.Status.Request_Method;

   begin
      Found := False;

      Look_For_Filters : for K in 1 .. Filters.Count loop

         if GNAT.Regexp.Match (URI, Filters.Set (K).Regexp) then

            Found := True;

            --  we must call the registered server to get the Data.

            if AWS.Status.Method (Status) = AWS.Status.GET then
               Data := Client.Get
                 (To_String (Filters.Set (K).URL)
                  & URI (URI'First + 1 .. URI'Last)
                  & Parameters);
            else
               Data := Client.Post
                 (To_String (Filters.Set (K).URL)
                  & URI (URI'First + 1 .. URI'Last),
                  AWS.Status.Binary_Data (Status));
            end if;

            exit Look_For_Filters;
         end if;

      end loop Look_For_Filters;
   end Apply;

   -------------
   -- Move_Up --
   -------------

   procedure Move_Up (Filters : in Filter_Set;
                      N       : in Positive)
   is
      Tmp : Filter_Data;
   begin
      if Filters.Count >= N and then N > 1 then
         Tmp := Filters.Set (N - 1);
         Filters.Set (N - 1) := Filters.Set (N);
         Filters.Set (N) := Tmp;
      end if;
   end Move_Up;

   ---------------
   -- Move_Down --
   ---------------

   procedure Move_Down (Filters : in Filter_Set;
                        N       : in Positive)
   is
      Tmp : Filter_Data;
   begin
      if Filters.Count > N then
         Tmp := Filters.Set (N);
         Filters.Set (N) := Filters.Set (N + 1);
         Filters.Set (N + 1) := Tmp;
      end if;
   end Move_Down;

   --------------
   -- Register --
   --------------

   procedure Register
     (Filters : in out Filter_Set;
      Regexp  : in     String;
      URL     : in     String) is
   begin
      Adjust (Filters);
      Filters.Count := Filters.Count + 1;
      Filters.Set (Filters.Count) := (To_Unbounded_String (Regexp),
                                      GNAT.Regexp.Compile (Regexp),
                                      To_Unbounded_String (URL));
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Filters : in out Filter_Set;
      Regexp  : in String) is
   begin
      for K in 1 .. Filters.Count loop
         if To_String (Filters.Set (K).Regexp_Str) = Regexp then
            Filters.Set (K .. Filters.Count - 1) :=
              Filters.Set (K + 1 .. Filters.Count);
            Filters.Count := Filters.Count - 1;
            exit;
         end if;
      end loop;
   end Unregister;

end AWS.Hotplug;

