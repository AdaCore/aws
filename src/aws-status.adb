------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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
with Ada.Characters.Handling;

with AWS.Messages;
with AWS.Translater;

package body AWS.Status is

   use Ada.Strings;

   -----------------
   -- Binary_Data --
   -----------------

   function Binary_Data (D : in Data) return Stream_Element_Array is
   begin
      return D.Binary_Data.all;
   end Binary_Data;

   ----------------
   -- Connection --
   ----------------

   function Connection (D : in Data) return String is
   begin
      return To_String (D.Connection);
   end Connection;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (D : in Data) return Natural is
   begin
      return D.Content_Length;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (D : in Data) return String is
   begin
      return To_String (D.Content_Type);
   end Content_Type;

   -----------
   -- Count --
   -----------

   function Count (D : in Data; Name : in String) return Natural is
   begin
      return AWS.Parameters.Count (D.Parameters, Name);
   end Count;

   ---------------------
   -- File_Up_To_Date --
   ---------------------

   function File_Up_To_Date (D : in Data) return Boolean is
   begin
      return D.File_Up_To_Date;
   end File_Up_To_Date;

   ----------
   -- Host --
   ----------

   function Host (D : in Data) return String is
   begin
      return To_String (D.Host);
   end Host;

   ------------------
   -- HTTP_Version --
   ------------------

   function HTTP_Version (D : in Data) return String is
   begin
      return To_String (D.HTTP_Version);
   end HTTP_Version;

   -----------------------
   -- If_Modified_Since --
   -----------------------

   function If_Modified_Since (D : in Data) return String is
   begin
      return To_String (D.If_Modified_Since);
   end If_Modified_Since;

   ------------
   -- Method --
   ------------

   function Method (D : in Data) return Request_Method is
   begin
      return D.Method;
   end Method;

   ------------------------
   -- Multipart_Boundary --
   ------------------------

   function Multipart_Boundary (D : in Data) return String is
   begin
      return To_String (D.Boundary);
   end Multipart_Boundary;

   -------------
   -- Session --
   -------------

   function Session (D : in Data) return String is
   begin
      return To_String (D.Session_ID);
   end Session;

   function Session (D : in Data) return AWS.Session.ID is
   begin
      return AWS.Session.Value (Session (D));
   end Session;

   ---------------
   -- Parameter --
   ---------------

   function Parameter (D : in Data; N : in Positive) return String is
   begin
      return Parameters.Get_Value (D.Parameters, N);
   end Parameter;

   function Parameter (D              : in Data;
                       Name           : in String;
                       N              : in Positive := 1;
                       Case_Sensitive : in Boolean  := True) return String
   is

      function Equal (S1, S2 : in String) return Boolean;
      --  Returns true if S1 and S2 are equal. The check is not
      --  Case_Sensitive.

      function Equal (S1, S2 : in String) return Boolean is
         use Ada;
      begin
         return Characters.Handling.To_Upper (S1)
           = Characters.Handling.To_Upper (S2);
      end Equal;

      Index : Natural := 0;

   begin
      if Case_Sensitive then
         return Parameters.Get (D.Parameters, Name, N);
      else
         for K in 1 .. Parameters.Count (D.Parameters) loop
            if Equal (Parameters.Get_Key (D.Parameters, K), Name) then
               Index := Index + 1;

               if Index = N then
                  return Parameters.Get_Value (D.Parameters, K);
               end if;
            end if;
         end loop;

         --  Key not found.

         return "";
      end if;
   end Parameter;

   --------------------
   -- Parameter_Name --
   --------------------

   function Parameter_Name (D : in Data; N : in Positive) return String is
   begin
      return Parameters.Get_Key (D.Parameters, N);
   end Parameter_Name;

   -----------
   -- Reset --
   -----------

   procedure Reset (D : in out Data) is
   begin
      Parameters.Release (D.Parameters);
      D := No_Data;
   end Reset;

   --------------------
   -- Set_Connection --
   --------------------

   procedure Set_Connection (D : in out Data; Connection : in String) is
   begin
      D.Connection := To_Unbounded_String (Connection);
   end Set_Connection;

   ------------------------
   -- Set_Content_Length --
   ------------------------

   procedure Set_Content_Length (D              : in out Data;
                                 Content_Length : in     Natural) is
   begin
      D.Content_Length := Content_Length;
   end Set_Content_Length;

   ----------------------
   -- Set_Content_Type --
   ----------------------

   procedure Set_Content_Type (D            : in out Data;
                               Content_Type : in     String) is
   begin
      D.Content_Type := To_Unbounded_String (Content_Type);
   end Set_Content_Type;

   -------------------------
   -- Set_File_Up_To_Date --
   -------------------------

   procedure Set_File_Up_To_Date (D               : in out Data;
                                  File_Up_To_Date : in     Boolean) is
   begin
      D.File_Up_To_Date := File_Up_To_Date;
   end Set_File_Up_To_Date;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host (D : in out Data; Host : in String) is
   begin
      D.Host := To_Unbounded_String (Host);
   end Set_Host;

   ---------------------------
   -- Set_If_Modified_Since --
   ---------------------------

   procedure Set_If_Modified_Since (D                 : in out Data;
                                    If_Modified_Since : in     String) is
   begin
      D.If_Modified_Since := To_Unbounded_String (If_Modified_Since);
   end Set_If_Modified_Since;

   ----------------------------
   -- Set_Multipart_Boundary --
   ----------------------------

   procedure Set_Multipart_Boundary (D        : in out Data;
                                     Boundary : in     String) is
   begin
      D.Boundary := To_Unbounded_String (Boundary);
   end Set_Multipart_Boundary;

   --------------------
   -- Set_Parameters --
   --------------------

   procedure Set_Parameters (D : in out Data; Name, Value : in String) is
   begin
      AWS.Parameters.Add (D.Parameters, Name, Translater.Decode_URL (Value));
   end Set_Parameters;

   procedure Set_Parameters (D : in out Data; Parameters : in String) is
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

            Set_Parameters (D, P (C .. I - 1), P (S .. P'Last));
            exit;

         else
            Set_Parameters (D, P (C .. I - 1), P (S .. E - 1));
            C := E + 1;
         end if;
      end loop;
   end Set_Parameters;

   procedure Set_Parameters (D         : in out Data;
                             Parameter : in     Stream_Element_Array) is
   begin
      D.Binary_Data := new Stream_Element_Array'(Parameter);
   end Set_Parameters;

   -----------------
   -- Set_Request --
   -----------------

   procedure Set_Request (D            : in out Data;
                          Method       : in     Request_Method;
                          URI          : in     String;
                          HTTP_Version : in     String;
                          Parameters   : in     String := "") is
   begin
      D.Method       := Method;
      D.URI          := To_Unbounded_String (URI);
      D.HTTP_Version := To_Unbounded_String (HTTP_Version);

      Set_Parameters (D, Parameters);
   end Set_Request;

   -----------------
   -- Set_Session --
   -----------------

   procedure Set_Session  (D  : in out Data;
                           ID : in     String) is
   begin
      D.Session_ID := To_Unbounded_String (ID);
   end Set_Session;

   ---------
   -- URI --
   ---------

   function URI (D : in Data) return String is
   begin
      return To_String (D.URI);
   end URI;

   -----------------------
   -- Set_Authorization --
   -----------------------

   procedure Set_Authorization (D             : in out Data;
                                Authorization : in     String) is
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
   end Set_Authorization;

   ------------------------
   -- Authorization_Name --
   ------------------------

   function Authorization_Name (D : in Data) return String is
   begin
      return To_String (D.Auth_Name);
   end Authorization_Name;

   ----------------------------
   -- Authorization_Password --
   ----------------------------

   function Authorization_Password (D : in Data) return String is
   begin
      return To_String (D.Auth_Password);
   end Authorization_Password;

end AWS.Status;
