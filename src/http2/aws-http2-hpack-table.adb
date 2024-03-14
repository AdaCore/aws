------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

with Ada.Characters.Handling;
with Ada.Text_IO;

with AWS.HTTP2.Connection;

package body AWS.HTTP2.HPACK.Table is

   use Ada.Containers;

   Static_Table_Size : constant := 61;
   --  The static table size

   Static : Static_Table;

   procedure Insert_Static (Name, Value : String);

   function D_Index (T : Object; Index : Positive) return Positive is
     (T.Dynamic.Length - Index + Static_Table_Size + 1);

   function NV_Size (NV : Name_Value) return Positive is
     (NV.Name'Length + NV.Value'Length + 32);

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self := Object'(others => <>);
   end Clear;

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : Object) is
      D : Dynamic_Table renames Self.Dynamic;
   begin
      for C in reverse D.T_IN.Iterate loop
         declare
            K : constant Positive := Index_NV.To_Index (C);
            I : constant Name_Value := D.T_IN (K);
         begin
            Text_IO.Put_Line
              ('['
               & Positive'Image (Positive (Self.Dynamic.T_IN.Length) - K + 1)
               & "] (s = " & NV_Size (I)'Img & ") "
               & I.Name & " " & I.Value);
         end;
      end loop;
   end Dump;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Object; Index : Positive) return String is
   begin
      if Index <= Static_Table_Size then
         return Static.T_IN (Index).Name;

      elsif Index - Static_Table_Size <= Self.Dynamic.Length then
         return Self.Dynamic.T_IN (D_Index (Self, Index)).Name;

      else
         raise Protocol_Error with Exception_Message
           (C_Protocol_Error,
            "wrong name index" & Index'Img & " >"
            & Natural'Image (Static_Table_Size + Self.Dynamic.Length));
      end if;
   end Get_Name;

   --------------------
   -- Get_Name_Value --
   --------------------

   function Get_Name_Value
     (Self : Object; Index : Positive) return Name_Value is
   begin
      if Index <= Static_Table_Size then
         return Static.T_IN (Index);

      elsif Index - Static_Table_Size <= Self.Dynamic.Length then
         return Self.Dynamic.T_IN (D_Index (Self, Index));

      else
         raise Protocol_Error with Exception_Message
           (C_Protocol_Error,
            "wrong name-value index" & Index'Img & " >"
            & Natural'Image (Static_Table_Size + Self.Dynamic.Length));
      end if;
   end Get_Name_Value;

   --------------------------
   -- Get_Name_Value_Index --
   --------------------------

   function Get_Name_Value_Index
     (Self     : in out Object;
      Settings : not null access HTTP2.Connection.Object;
      Name     : String;
      Value    : String := "";
      Both     : out Boolean) return Natural
   is
      Has_Value : constant Boolean := Value /= "";
      N         : constant String := Characters.Handling.To_Lower (Name);
      NV        : constant String := N & '=' & Value;
      CNI       : NV_Index.Cursor;

   begin
      for With_Value in reverse False .. Has_Value loop
         CNI := Static.T_NI.Find (if With_Value then NV else N);

         if NV_Index.Has_Element (CNI) then
            Both := With_Value;
            return NV_Index.Element (CNI);
         end if;
      end loop;

      for With_Value in reverse False .. Has_Value loop
         CNI := Self.Dynamic.T_NI.Find (if With_Value then NV else N);

         if NV_Index.Has_Element (CNI) then
            Both := With_Value;

            return Self.Dynamic.Rank - NV_Index.Element (CNI)
                   + Static_Table_Size + 1;
         end if;
      end loop;

      Self.Insert (Settings, Name, Value);

      Both := False;

      return 0;
   end Get_Name_Value_Index;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self        : in out Object;
      Settings    : not null access HTTP2.Connection.Object;
      Name, Value : String)
   is
      N    : constant String := Characters.Handling.To_Lower (Name);
      V    : String renames Value;
      Item : constant Name_Value := (N'Length, V'Length, N, V);
      Size : constant Positive := NV_Size (Item);
   begin
      Self.Dynamic.Size   := Self.Dynamic.Size + Size;
      Self.Dynamic.Length := Self.Dynamic.Length + 1;
      Self.Dynamic.Rank   := Self.Dynamic.Rank + 1;

      declare
         Idx : constant Positive := Self.Dynamic.Rank;
      begin
         Self.Dynamic.T_NI.Include (N, Idx);

         if V /= "" then
            Self.Dynamic.T_NI.Include (N & "=" & V, Idx);
         end if;

         Self.Dynamic.T_IN.Append (Item);
      end;

      --  Clean up table if too large. Remove oldest elements until the table
      --  size is under the maximum allowed by the server.

      Clean_Up : declare
         Max_Header_List_Size : constant Integer :=
                                  Settings.Dynamic_Header_Table_Size;
      begin
         while Self.Dynamic.Size > Max_Header_List_Size loop
            declare
               Item : constant Name_Value := Self.Dynamic.T_IN.First_Element;
            begin
               Self.Dynamic.T_IN.Delete_First;
               Self.Dynamic.Size := Self.Dynamic.Size - NV_Size (Item);
               Self.Dynamic.Length := Self.Dynamic.Length - 1;

               Self.Dynamic.T_NI.Exclude (Item.Name);
               Self.Dynamic.T_NI.Exclude (Item.Name & '=' & Item.Value);
            end;
         end loop;
      end Clean_Up;
   end Insert;

   -------------------
   -- Insert_Static --
   -------------------

   procedure Insert_Static (Name, Value : String) is
      Item : constant Name_Value := (Name'Length, Value'Length, Name, Value);
   begin
      Static.T_IN.Append (Item);

      declare
         Id : constant Positive := Positive (Static.T_IN.Length);
      begin
         Static.T_NI.Include (Name, Id);

         if Value /= "" then
            Static.T_NI.Include (Name & "=" & Value, Id);
         end if;
      end;
   end Insert_Static;

begin
   Insert_Static (":authority", "");
   Insert_Static (":method", "GET");
   Insert_Static (":method", "POST");
   Insert_Static (":path", "/");
   Insert_Static (":path", "/index.html");
   Insert_Static (":scheme", "http");
   Insert_Static (":scheme", "https");
   Insert_Static (":status", "200");
   Insert_Static (":status", "204");
   Insert_Static (":status", "206");
   Insert_Static (":status", "304");
   Insert_Static (":status", "400");
   Insert_Static (":status", "404");
   Insert_Static (":status", "500");
   Insert_Static ("accept-charset", "");
   Insert_Static ("accept-encoding", "gzip, deflate");
   Insert_Static ("accept-language", "");
   Insert_Static ("accept-ranges", "");
   Insert_Static ("accept", "");
   Insert_Static ("access-control-allow-origin", "");
   Insert_Static ("age", "");
   Insert_Static ("allow", "");
   Insert_Static ("authorization", "");
   Insert_Static ("cache-control", "");
   Insert_Static ("content-disposition", "");
   Insert_Static ("content-encoding", "");
   Insert_Static ("content-language", "");
   Insert_Static ("content-length", "");
   Insert_Static ("content-location", "");
   Insert_Static ("content-range", "");
   Insert_Static ("content-type", "");
   Insert_Static ("cookie", "");
   Insert_Static ("date", "");
   Insert_Static ("etag", "");
   Insert_Static ("expect", "");
   Insert_Static ("expires", "");
   Insert_Static ("from", "");
   Insert_Static ("host", "");
   Insert_Static ("if-match", "");
   Insert_Static ("if-modified-since", "");
   Insert_Static ("if-none-match", "");
   Insert_Static ("if-range", "");
   Insert_Static ("if-unmodified-since", "");
   Insert_Static ("last-modified", "");
   Insert_Static ("link", "");
   Insert_Static ("location", "");
   Insert_Static ("max-forwards", "");
   Insert_Static ("proxy-authenticate", "");
   Insert_Static ("proxy-authorization", "");
   Insert_Static ("range", "");
   Insert_Static ("referer", "");
   Insert_Static ("refresh", "");
   Insert_Static ("retry-after", "");
   Insert_Static ("server", "");
   Insert_Static ("set-cookie", "");
   Insert_Static ("strict-transport-security", "");
   Insert_Static ("transfer-encoding", "");
   Insert_Static ("user-agent", "");
   Insert_Static ("vary", "");
   Insert_Static ("via", "");
   Insert_Static ("www-authenticate", "");

   --  61 values, indexed from 1 .. 61

   pragma Assert (Static.T_IN.Length = Static_Table_Size);
end AWS.HTTP2.HPACK.Table;
