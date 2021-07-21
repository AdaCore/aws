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

   procedure Insert_Static (Self : in out Object; Name, Value : String);

   function D_Index (T : Object; Index : Positive) return Positive
     is (Positive (T.Dynamic.T_IN.Length) - Index + 62);

   function NV_Size (NV : Name_Value) return Positive
     is (NV.Name'Length + NV.Value'Length + 32);

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
         return Self.Static.T_IN (Index).Name;

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
         return Self.Static.T_IN (Index);

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
     (Self  : Object;
      Name  : String;
      Value : String := "";
      Both  : out Boolean) return Positive
   is
      Has_Value : constant Boolean := Value /= "";
      N         : constant String := Characters.Handling.To_Lower (Name);
      V         : constant String := Value;
      NV        : constant String := N & '=' & V;
   begin
      Both := False;

      if Has_Value and then Self.Static.T_NI.Contains (NV) then
         Both := True;
         return Self.Static.T_NI (NV).Index;

      elsif Self.Static.T_NI.Contains (N) then
         return Self.Static.T_NI (N).Index;

      elsif Has_Value and then Self.Dynamic.T_NI.Contains (NV) then
         Both := True;

         declare
            Id : constant Ids := Self.Dynamic.T_NI (NV);
         begin
            return Id.Index + (Self.Dynamic.Rank - Id.Rank);
         end;

      elsif Self.Dynamic.T_NI.Contains (N) then
         declare
            Id : constant Ids := Self.Dynamic.T_NI (N);
         begin
            return Id.Index + (Self.Dynamic.Rank - Id.Rank);
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Name_Value_Index;

   ----------
   -- Init --
   ----------

   procedure Init (Self : in out Object) is
   begin
      Insert_Static (Self, ":authority", "");
      Insert_Static (Self, ":method", "GET");
      Insert_Static (Self, ":method", "POST");
      Insert_Static (Self, ":path", "/");
      Insert_Static (Self, ":path", "/index.html");
      Insert_Static (Self, ":scheme", "http");
      Insert_Static (Self, ":scheme", "https");
      Insert_Static (Self, ":status", "200");
      Insert_Static (Self, ":status", "204");
      Insert_Static (Self, ":status", "206");
      Insert_Static (Self, ":status", "304");
      Insert_Static (Self, ":status", "400");
      Insert_Static (Self, ":status", "404");
      Insert_Static (Self, ":status", "500");
      Insert_Static (Self, "accept-charset", "");
      Insert_Static (Self, "accept-encoding", "gzip, deflate");
      Insert_Static (Self, "accept-language", "");
      Insert_Static (Self, "accept-ranges", "");
      Insert_Static (Self, "accept", "");
      Insert_Static (Self, "access-control-allow-origin", "");
      Insert_Static (Self, "age", "");
      Insert_Static (Self, "allow", "");
      Insert_Static (Self, "authorization", "");
      Insert_Static (Self, "cache-control", "");
      Insert_Static (Self, "content-disposition", "");
      Insert_Static (Self, "content-encoding", "");
      Insert_Static (Self, "content-language", "");
      Insert_Static (Self, "content-length", "");
      Insert_Static (Self, "content-location", "");
      Insert_Static (Self, "content-range", "");
      Insert_Static (Self, "content-type", "");
      Insert_Static (Self, "cookie", "");
      Insert_Static (Self, "date", "");
      Insert_Static (Self, "etag", "");
      Insert_Static (Self, "expect", "");
      Insert_Static (Self, "expires", "");
      Insert_Static (Self, "from", "");
      Insert_Static (Self, "host", "");
      Insert_Static (Self, "if-match", "");
      Insert_Static (Self, "if-modified-since", "");
      Insert_Static (Self, "if-none-match", "");
      Insert_Static (Self, "if-range", "");
      Insert_Static (Self, "if-unmodified-since", "");
      Insert_Static (Self, "last-modified", "");
      Insert_Static (Self, "link", "");
      Insert_Static (Self, "location", "");
      Insert_Static (Self, "max-forwards", "");
      Insert_Static (Self, "proxy-authenticate", "");
      Insert_Static (Self, "proxy-authorization", "");
      Insert_Static (Self, "range", "");
      Insert_Static (Self, "referer", "");
      Insert_Static (Self, "refresh", "");
      Insert_Static (Self, "retry-after", "");
      Insert_Static (Self, "server", "");
      Insert_Static (Self, "set-cookie", "");
      Insert_Static (Self, "strict-transport-security", "");
      Insert_Static (Self, "transfer-encoding", "");
      Insert_Static (Self, "user-agent", "");
      Insert_Static (Self, "vary", "");
      Insert_Static (Self, "via", "");
      Insert_Static (Self, "www-authenticate", "");
      --  61 values, indexed from 1 .. 61
   end Init;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self        : in out Object;
      Settings    : not null access HTTP2.Connection.Object;
      Name, Value : String)
   is
      N    : constant String := Characters.Handling.To_Lower (Name);
      V    : constant String := Value;
      Item : constant Name_Value := (N'Length, V'Length, N, V);
      Size : constant Positive := NV_Size (Item);
   begin
      Self.Dynamic.Size := Self.Dynamic.Size + Size;
      Self.Dynamic.Length := Self.Dynamic.Length + 1;
      Self.Dynamic.Rank := Self.Dynamic.Rank + 1;

      declare
         Index : constant Positive := Positive (Self.Dynamic.Length);
         Id    : constant Ids := (Index, Self.Dynamic.Rank);
      begin
         Self.Dynamic.T_NI.Include (N, Id);
         Self.Dynamic.T_NI.Include (N & "=" & V, Id);
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
               Item  : constant Name_Value := Self.Dynamic.T_IN.First_Element;
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

   procedure Insert_Static (Self : in out Object; Name, Value : String) is
      Item : constant Name_Value := (Name'Length, Value'Length, Name, Value);
   begin
      Self.Static.T_IN.Append (Item);

      declare
         Index : constant Positive := Positive (Self.Static.T_IN.Length);
         Id    : constant Ids := (Index, 1);
      begin
         Self.Static.T_NI.Include (Name, Id);

         if Value /= "" then
            Self.Static.T_NI.Include (Name & "=" & Value, Id);
         end if;
      end;
   end Insert_Static;

end AWS.HTTP2.HPACK.Table;
