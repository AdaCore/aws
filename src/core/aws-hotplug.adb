------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with AWS.Client;
with AWS.Parameters;

package body AWS.Hotplug is

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Filters : Filter_Set;
      Status  : AWS.Status.Data;
      Found   : out Boolean;
      Data    : out Response.Data)
   is
      use type AWS.Status.Request_Method;

      URI : constant String              := AWS.Status.URI (Status);
      P   : constant AWS.Parameters.List := AWS.Status.Parameters (Status);

      function Parameters return String;
      --  Returns the list of parameters suitable to send to a GET HTTP
      --  command "?name1=value1&name2=value2...".

      ----------------
      -- Parameters --
      ----------------

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

   begin
      Found := False;

      Look_For_Filters :
      for K in 1 .. Natural (Filter_Table.Length (Filters.Set)) loop
         declare
            Item : constant Filter_Data
              := Filter_Table.Element (Filters.Set, K);
         begin
            if GNAT.Regexp.Match (URI, Item.Regexp) then
               Found := True;

               --  We must call the registered server to get the Data

               if AWS.Status.Method (Status) = AWS.Status.GET then
                  Data := Client.Get
                    (To_String (Item.URL)
                     & URI (URI'First + 1 .. URI'Last)
                     & Parameters);
               else
                  --  This is a POST, check if it is a SOAP request
                  declare
                     Resource : constant String
                       := URI (URI'First + 1 .. URI'Last);
                  begin
                     if AWS.Status.Is_SOAP (Status) then
                        Data := Client.SOAP_Post
                          (To_String (Item.URL) & Resource,
                           AWS.Status.Payload (Status),
                           AWS.Status.SOAPAction (Status));
                     else
                        Data := Client.Post
                          (To_String (Item.URL) & Resource,
                           AWS.Status.Binary_Data (Status));
                     end if;
                  end;
               end if;

               exit Look_For_Filters;
            end if;
         end;
      end loop Look_For_Filters;
   end Apply;

   ----------------
   -- Equal_Data --
   ----------------

   function Equal_Data (Left, Right : Filter_Data) return Boolean is
   begin
      return Left.Regexp_Str = Right.Regexp_Str;
   end Equal_Data;

   ---------------
   -- Move_Down --
   ---------------

   procedure Move_Down
     (Filters : in out Filter_Set;
      N       : Positive) is
   begin
      if Positive (Filter_Table.Length (Filters.Set)) > N then
         Filter_Table.Swap (Filters.Set, N, N + 1);
      end if;
   end Move_Down;

   -------------
   -- Move_Up --
   -------------

   procedure Move_Up
     (Filters : in out Filter_Set;
      N       : Positive) is
   begin
      if Positive (Filter_Table.Length (Filters.Set)) >= N
        and then N > 1
      then
         Filter_Table.Swap (Filters.Set, N, N - 1);
      end if;
   end Move_Up;

   --------------
   -- Register --
   --------------

   procedure Register
     (Filters : in out Filter_Set;
      Regexp  : String;
      URL     : String)
   is
      Item   : Filter_Data;
      Cursor : Filter_Table.Cursor;
      U      : Unbounded_String;
   begin
      --  Add '/' at the end of the URL if needed

      if URL'Length > 0 and then URL (URL'Last) /= '/' then
         U := To_Unbounded_String (URL & '/');
      else
         U := To_Unbounded_String (URL);
      end if;

      Item := (To_Unbounded_String (Regexp), GNAT.Regexp.Compile (Regexp), U);
      Cursor := Filter_Table.Find (Filters.Set, Item);

      case Filters.Mode is
         when Add =>
            if Filter_Table.Has_Element (Cursor) then
               raise Register_Error;
            else
               Filter_Table.Append (Filters.Set, Item);
            end if;

         when Replace =>
            if Filter_Table.Has_Element (Cursor) then
               Filter_Table.Replace_Element (Filters.Set, Cursor, Item);
            else
               Filter_Table.Append (Filters.Set, Item);
            end if;
      end case;
   end Register;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Filters : in out Filter_Set; Mode : Register_Mode) is
   begin
      Filters.Mode := Mode;
   end Set_Mode;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Filters : in out Filter_Set;
      Regexp  : String) is
   begin
      for K in 1 .. Natural (Filter_Table.Length (Filters.Set)) loop
         if To_String (Filter_Table.Element (Filters.Set, K).Regexp_Str)
           = Regexp
         then
            Filter_Table.Delete (Filters.Set, K);
            exit;
         end if;
      end loop;
   end Unregister;

end AWS.Hotplug;
