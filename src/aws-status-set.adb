------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2000-2001                         --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Messages;
with AWS.Translater;

package body AWS.Status.Set is

   use Ada.Strings;

   -------------------
   -- Authorization --
   -------------------

   procedure Authorization (D             : in out Data;
                            Authorization : in     String)
   is
      Basic_Token : constant String := "Basic ";
   begin
      if Messages.Is_Match (Authorization, Basic_Token) then

         declare
            use Ada.Streams;

            Auth_Bin : Stream_Element_Array :=
              Translater.Base64_Decode
              (Authorization (Basic_Token'Length + Authorization'First
                              .. Authorization'Last));
            Auth_Str : String (1 .. Auth_Bin'Length);
            K        : Positive := Auth_Str'First;
            Delimit  : Natural;
         begin

            for i in Auth_Bin'Range loop
               Auth_Str (K) :=
                 Character'Val (Stream_Element'Pos (Auth_Bin (i)));
               K := K + 1;
            end loop;

            Delimit := Fixed.Index (Auth_Str, ":");

            if Delimit = 0 then
               D.Auth_Name := To_Unbounded_String (Auth_Str);
            else
               D.Auth_Name     :=
                 To_Unbounded_String (Auth_Str (1 .. Delimit - 1));
               D.Auth_Password :=
                 To_Unbounded_String (Auth_Str (Delimit + 1 .. Auth_Str'Last));
            end if;
         end;
      end if;
   end Authorization;

   ----------------
   -- Connection --
   ----------------

   procedure Connection (D : in out Data; Connection : in String) is
   begin
      D.Connection := To_Unbounded_String (Connection);
   end Connection;

   --------------------
   -- Content_Length --
   --------------------

   procedure Content_Length (D              : in out Data;
                             Content_Length : in     Natural) is
   begin
      D.Content_Length := Content_Length;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   procedure Content_Type (D            : in out Data;
                           Content_Type : in     String) is
   begin
      D.Content_Type := To_Unbounded_String (Content_Type);
   end Content_Type;

   ---------------------
   -- File_Up_To_Date --
   ---------------------

   procedure File_Up_To_Date (D               : in out Data;
                              File_Up_To_Date : in     Boolean) is
   begin
      D.File_Up_To_Date := File_Up_To_Date;
   end File_Up_To_Date;

   ----------
   -- Host --
   ----------

   procedure Host (D : in out Data; Host : in String) is
   begin
      D.Host := To_Unbounded_String (Host);
   end Host;

   -----------------------
   -- If_Modified_Since --
   -----------------------

   procedure If_Modified_Since (D                 : in out Data;
                                If_Modified_Since : in     String) is
   begin
      D.If_Modified_Since := To_Unbounded_String (If_Modified_Since);
   end If_Modified_Since;

   ------------------------
   -- Multipart_Boundary --
   ------------------------

   procedure Multipart_Boundary (D        : in out Data;
                                 Boundary : in     String) is
   begin
      D.Boundary := To_Unbounded_String (Boundary);
   end Multipart_Boundary;

   ----------------
   -- Parameters --
   ----------------

   procedure Parameters (D : in out Data; Name, Value : in String) is

   begin
      AWS.Parameters.Add
        (D.Parameters,
         Normalize_Name (Name, not D.Parameters_Case_Sensitive),
         Translater.Decode_URL (Value));
   end Parameters;

   procedure Parameters (D : in out Data; Parameters : in String) is
      P : String renames Parameters;
      C : Positive := P'First;
      I : Natural;
      S : Positive := P'First;
      E : Natural;
   begin
      loop
         I := Fixed.Index (P (C .. P'Last), "=");

         exit when I = 0;

         S := I + 1;

         E := Fixed.Index (P (S .. P'Last), "&");

         if E = 0 then
            --  last parameter

            Set.Parameters (D, P (C .. I - 1), P (S .. P'Last));
            exit;

         else
            Set.Parameters (D, P (C .. I - 1), P (S .. E - 1));
            C := E + 1;
         end if;
      end loop;
   end Parameters;

   procedure Parameters (D         : in out Data;
                         Parameter : in     Stream_Element_Array) is
   begin
      D.Binary_Data := new Stream_Element_Array'(Parameter);
   end Parameters;

   --------------
   -- Peername --
   --------------

   procedure Peername (D        : in out Data;
                       Peername : in     String) is
   begin
      D.Peername := To_Unbounded_String (Peername);
   end Peername;

   -------------
   -- Request --
   -------------

   procedure Request (D            : in out Data;
                      Method       : in     Request_Method;
                      URI          : in     String;
                      HTTP_Version : in     String;
                      Parameters   : in     String := "") is
   begin
      D.Method       := Method;
      D.URI          := To_Unbounded_String (URI);
      D.HTTP_Version := To_Unbounded_String (HTTP_Version);

      Set.Parameters (D, Parameters);
   end Request;

   -----------
   -- Reset --
   -----------

   procedure Reset (D : in out Data) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Stream_Element_Array, Stream_Element_Array_Access);

   begin
      AWS.Parameters.Release (D.Parameters);
      Free (D.Binary_Data);

      D.Connection        := Null_Unbounded_String;
      D.Host              := Null_Unbounded_String;
      D.Method            := GET;
      D.URI               := Null_Unbounded_String;
      D.HTTP_Version      := Null_Unbounded_String;
      D.Content_Type      := Null_Unbounded_String;
      D.Boundary          := Null_Unbounded_String;
      D.Content_Length    := 0;
      D.If_Modified_Since := Null_Unbounded_String;
      D.File_Up_To_Date   := False;
      D.Auth_Name         := Null_Unbounded_String;
      D.Auth_Password     := Null_Unbounded_String;
      D.Session_ID        := Null_Unbounded_String;
   end Reset;

   -------------
   -- Session --
   -------------

   procedure Session  (D  : in out Data;
                       ID : in     String) is
   begin
      D.Session_ID := To_Unbounded_String (ID);
   end Session;

end AWS.Status.Set;
