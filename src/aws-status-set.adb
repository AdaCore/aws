------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Headers.Set;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.Translator;
with AWS.Parameters.Set;

package body AWS.Status.Set is

   use Ada.Strings;

   procedure Authorization (D : in out Data);
   --  Parse the Authorization parameters from the Authorization header value.

   procedure Update_Data_From_Header (D : in out Data);
   --  Update some Data fields from the internal Data header container.
   --  The Update_Data_From_Header should be called after the complete
   --  header parsing.

   -------------------
   -- Authorization --
   -------------------

   procedure Authorization (D : in out Data) is

      Header_Value : constant String :=
         AWS.Headers.Get (D.Header, Messages.Authorization_Token);

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean);

      procedure Value (Item : in String; Quit : in out Boolean);

      -----------------
      -- Named_Value --
      -----------------

      procedure Named_Value
        (Name, Value : in String;
         Quit        : in out Boolean)
      is

         type Digest_Attribute is
           (Username, Realm, Nonce, NC, CNonce,
            QOP, URI, Response, Algorithm);
         --  The enumeration type is using to be able to
         --  use the name in the case statement.
         --  The case statement has usially faster implementation.

         Attribute : Digest_Attribute;

         function "+"
           (Item : in String)
            return Unbounded_String
           renames To_Unbounded_String;

      begin
         begin
            Attribute := Digest_Attribute'Value (Name);
         exception
            when Constraint_Error =>
               --  Ignoring unrecognized attribute
               return;
         end;

         --  Check if the attributes is for the Digest authenticatio schema.
         --  AWS does not support othe authentication schemas with attributes
         --  now.
         if D.Auth_Mode /= Digest then
            Quit := True;
         end if;

         case Attribute is
            when Username  => D.Auth_Name     := +Value;
            when Realm     => D.Auth_Realm    := +Value;
            when NC        => D.Auth_NC       := +Value;
            when CNonce    => D.Auth_CNonce   := +Value;
            when QOP       => D.Auth_QOP      := +Value;
            when Nonce     => D.Auth_Nonce    := +Value;
            when Response  => D.Auth_Response := +Value;
            when URI       => D.URI
              := URL.Parse (Value, False, False);
            when Algorithm =>
               if Value /= "MD5" then
                  Ada.Exceptions.Raise_Exception
                     (Constraint_Error'Identity,
                      "Only MD5 algorithm is supported.");
               end if;
         end case;
      end Named_Value;

      -----------
      -- Value --
      -----------

      procedure Value (Item : in String; Quit : in out Boolean) is
         Upper_Item : String := Ada.Characters.Handling.To_Upper (Item);
      begin
         if Upper_Item = "BASIC" then

            D.Auth_Mode := Basic;

            Quit := True;

            --  We could not continue to parse Basic authentication
            --  by the regular way, becouse next value is Base64
            --  encoded username:password, it is possibe to be
            --  symbol '=' there, our parser could
            --  think that it is name/value delimiter.
            declare
               use Ada.Streams;

               Auth_Str : constant String
                 := Translator.To_String (Translator.Base64_Decode
                   (Header_Value (Item'Length + 2 .. Header_Value'Last)));

               Delimit  : Natural := Fixed.Index (Auth_Str, ":");
            begin

               if Delimit = 0 then
                  D.Auth_Name := To_Unbounded_String (Auth_Str);
               else
                  D.Auth_Name
                    := To_Unbounded_String (Auth_Str (1 .. Delimit - 1));
                  D.Auth_Password
                    := To_Unbounded_String (Auth_Str
                                              (Delimit + 1 .. Auth_Str'Last));
               end if;
            end;

         elsif Upper_Item = "DIGEST" then
            D.Auth_Mode := Digest;

         end if;
      end Value;

      procedure Parse is new Headers.Values.Parse (Value, Named_Value);

   begin
      Parse (Header_Value);
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

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Stream_Element_Array_Access);

   procedure Free (D : in out Data) is
   begin
      Free (D.Binary_Data);

      AWS.Parameters.Set.Free (D.Parameters);
      AWS.Headers.Set.Free (D.Header);

   end Free;

   ----------------
   -- Keep_Alive --
   ----------------

   procedure Keep_Alive
     (D    : in out Data;
      Flag : in     Boolean) is
   begin
      D.Keep_Alive := Flag;
   end Keep_Alive;

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

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header
     (Socket : in Net.Socket_Type'Class;
      D : in out Data) is
   begin
      Headers.Set.Read (D.Header, Socket);
      Update_Data_From_Header (D);
   end Read_Header;

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
      D.URI          := URL.Parse (URI, False, False);
      D.HTTP_Version := To_Unbounded_String (HTTP_Version);
   end Request;

   -----------
   -- Reset --
   -----------

   procedure Reset (D : in out Data) is
   begin
      Free (D.Binary_Data);

      D.Method            := GET;
      D.HTTP_Version      := Null_Unbounded_String;
      D.Content_Length    := 0;
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
      D.Session_Created   := False;

      AWS.Parameters.Set.Reset (D.Parameters);
      AWS.Headers.Set.Reset (D.Header);
   end Reset;

   -------------
   -- Session --
   -------------

   procedure Session
     (D : in out Data) is
   begin
      D.Session_ID      := AWS.Session.Create;
      D.Session_Created := True;
   end Session;

   ------------
   -- Socket --
   ------------

   procedure Socket
     (D    : in out Data;
      Sock : in     Net.Socket_Access) is
   begin
      D.Socket := Sock;
   end Socket;

   -----------------------------
   -- Update_Data_From_Header --
   -----------------------------

   procedure Update_Data_From_Header (D : in out Data) is
   begin
      Authorization (D);
      declare
         Content_Length : String := AWS.Headers.Get
           (D.Header, Messages.Content_Length_Token);
      begin
         if Content_Length /= "" then
            D.Content_Length := Integer'Value (Content_Length);
         end if;
      end;

      D.SOAP_Action := AWS.Headers.Exist
         (D.Header, Messages.SOAPAction_Token);

      declare
         use AWS.Headers;
         Cookies_Set : VString_Array := Get_Values
            (D.Header,
             Messages.Cookie_Token);

      begin
         for Idx in Cookies_Set'Range loop
            declare

               --  The expected Cookie line is:
               --  Cookie: ... AWS=<cookieID>[,;] ...

               use type AWS.Session.ID;

               procedure Value
                 (Item : in     String;
                  Quit : in out Boolean);
               --  Called for every un-named value read from the header value

               procedure Named_Value
                 (Name  : in     String;
                  Value : in     String;
                  Quit  : in out Boolean);
               --  Called for every named value read from the header value

               -----------------
               -- Named_Value --
               -----------------

               procedure Named_Value
                 (Name  : in     String;
                  Value : in     String;
                  Quit  : in out Boolean) is
               begin

                  --  Check if it is AWS Cookie.
                  if Name /= "AWS" then
                     return;
                  end if;

                  D.Session_ID := AWS.Session.Value (Value);

                  --  Check if the cookie value was correct.
                  if D.Session_ID = AWS.Session.No_Session then
                     return;
                  end if;

                  --  Check if cookie exists in the server.
                  if not AWS.Session.Exist (D.Session_ID) then

                     --  reset to empty cookie if not exists.
                     --  We don't interesting in not existing values.

                     D.Session_ID := AWS.Session.No_Session;

                     return;
                  end if;

                  --  If all filters done, we found our cookie !
                  Quit := True;
               end Named_Value;

               -----------
               -- Value --
               -----------

               procedure Value
                 (Item : in     String;
                  Quit : in out Boolean)
               is
                  pragma Warnings (Off, Item);
                  pragma Warnings (Off, Quit);
               begin
                  --  Just ignore for now.
                  --  We are looking only for AWS=<cookieID>
                  null;
               end Value;

               -----------
               -- Parse --
               -----------

               procedure Parse is new AWS.Headers.Values.Parse
                                        (Value, Named_Value);

            begin
               Parse (To_String (Cookies_Set (Idx)));

               --  exit when we found existing cookie.
               exit when D.Session_ID /= AWS.Session.No_Session;
            end;
         end loop;
      end;
   end Update_Data_From_Header;

end AWS.Status.Set;
