------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

with AWS.Messages;
with AWS.Translator;
with AWS.Parameters.Set;
with AWS.Utils;

package body AWS.Status.Set is

   use Ada.Strings;

   -------------------
   -- Authorization --
   -------------------

   procedure Authorization
     (D             : in out Data;
      Authorization : in     String)
   is
      Basic_Token : constant String := "Basic ";
      Digest_Token : constant String := "Digest ";
   begin
      if Messages.Is_Match (Authorization, Basic_Token) then

         D.Auth_Mode := Basic;

         declare
            use Ada.Streams;

            Auth_Bin : Stream_Element_Array :=
              Translator.Base64_Decode
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
      elsif Messages.Is_Match (Authorization, Digest_Token) then

         D.Auth_Mode := Digest;

         declare

            type Digest_Attribute is
              (Username, Realm, Nonce, NC, CNonce, QOP,
                 URI, Response, Algorithm);
            type Result_Set is array (Digest_Attribute) of Unbounded_String;

            Result : Result_Set;

            procedure Parse_Auth_Line is new
               AWS.Utils.Parse_HTTP_Header_Line (Digest_Attribute, Result_Set);

         begin
            Parse_Auth_Line (
               Data => Authorization
                  (Authorization'First + Digest_Token'Length
                   .. Authorization'Last),
               Result => Result);

            D.URI           := Result (URI);
            D.Auth_Name     := Result (Username);
            D.Auth_Realm    := Result (Realm);
            D.Auth_Nonce    := Result (Nonce);
            D.Auth_Response := Result (Response);
            D.Auth_NC       := Result (NC);
            D.Auth_CNonce   := Result (CNonce);
            D.Auth_QOP      := Result (QOP);

            if Result (Algorithm) /= Null_Unbounded_String
            and then To_String (Result (Algorithm)) /= "MD5" then
               Ada.Exceptions.Raise_Exception
                  (Constraint_Error'Identity,
                   "Only MD5 algorithm is supported.");
            end if;

         end;

      end if;
   end Authorization;

   ------------
   -- Binary --
   ------------

   procedure Binary
     (D         : in out Data;
      Parameter : in     Stream_Element_Array) is
   begin
      D.Binary_Data := new Stream_Element_Array'(Parameter);
   end Binary;

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

   procedure Content_Length
     (D              : in out Data;
      Content_Length : in     Natural) is
   begin
      D.Content_Length := Content_Length;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   procedure Content_Type
     (D            : in out Data;
      Content_Type : in     String) is
   begin
      D.Content_Type := To_Unbounded_String (Content_Type);
   end Content_Type;

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

   procedure If_Modified_Since
     (D                 : in out Data;
      If_Modified_Since : in     String) is
   begin
      D.If_Modified_Since := To_Unbounded_String (If_Modified_Since);
   end If_Modified_Since;

   ------------------------
   -- Multipart_Boundary --
   ------------------------

   procedure Multipart_Boundary
     (D        : in out Data;
      Boundary : in     String) is
   begin
      D.Boundary := To_Unbounded_String (Boundary);
   end Multipart_Boundary;

   ----------------
   -- Parameters --
   ----------------

   procedure Parameters (D : in out Data; Set : in AWS.Parameters.List) is
   begin
      D.Parameters := Set;
   end Parameters;

   -------------
   -- Payload --
   -------------

   procedure Payload
     (D       : in out Data;
      Payload : in     String) is
   begin
      D.Payload := To_Unbounded_String (Payload);
   end Payload;

   --------------
   -- Peername --
   --------------

   procedure Peername
     (D        : in out Data;
      Peername : in     String) is
   begin
      D.Peername := To_Unbounded_String (Peername);
   end Peername;

   -------------
   -- Referer --
   -------------

   procedure Referer
     (D       : in out Data;
      Referer : in     String) is
   begin
      D.Referer := To_Unbounded_String (Referer);
   end Referer;

   -------------
   -- Request --
   -------------

   procedure Request
     (D            : in out Data;
      Method       : in     Request_Method;
      URI          : in     String;
      HTTP_Version : in     String) is
   begin
      D.Method       := Method;
      D.URI          := To_Unbounded_String (URI);
      D.HTTP_Version := To_Unbounded_String (HTTP_Version);
   end Request;

   -----------
   -- Reset --
   -----------

   procedure Reset (D : in out Data) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Stream_Element_Array, Stream_Element_Array_Access);

   begin
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
      D.Auth_Mode         := None;
      D.Auth_Name         := Null_Unbounded_String;
      D.Auth_Password     := Null_Unbounded_String;
      D.Auth_Realm        := Null_Unbounded_String;
      D.Auth_Nonce        := Null_Unbounded_String;
      D.Auth_NC           := Null_Unbounded_String;
      D.Auth_CNonce       := Null_Unbounded_String;
      D.Auth_QOP          := Null_Unbounded_String;
      D.Auth_Response     := Null_Unbounded_String;
      D.Session_ID        := AWS.Session.No_Session;

      AWS.Parameters.Set.Reset (D.Parameters);
   end Reset;

   -------------
   -- Session --
   -------------

   procedure Session
     (D  : in out Data;
      ID : in     String) is
   begin
      D.Session_ID := AWS.Session.Value (ID);
   end Session;

   ----------------
   -- SOAPAction --
   ----------------

   procedure SOAPAction
     (D          : in out Data;
      SOAPAction : in     String) is
   begin
      D.SOAPAction := To_Unbounded_String (SOAPAction);
   end SOAPAction;

   ------------
   -- Socket --
   ------------

   procedure Socket
     (D    : in out Data;
      Sock : in     Socket_Access) is
   begin
      D.Socket := Sock;
   end Socket;

   ----------------
   -- User_Agent --
   ----------------

   procedure User_Agent
     (D          : in out Data;
      User_Agent : in     String) is
   begin
      D.User_Agent := To_Unbounded_String (User_Agent);
   end User_Agent;

end AWS.Status.Set;
