------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

with Ada.Strings;

with AWS.Digest;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.Translator;

package body AWS.Status is

   use Ada.Strings;

   --------------------------
   -- Authorization_CNonce --
   --------------------------

   function Authorization_CNonce (D : in Data) return String is
   begin
      return To_String (D.Auth_CNonce);
   end Authorization_CNonce;

   ------------------------
   -- Authorization_Mode --
   ------------------------

   function Authorization_Mode (D : in Data) return Authorization_Type is
   begin
      return D.Auth_Mode;
   end Authorization_Mode;

   ------------------------
   -- Authorization_Name --
   ------------------------

   function Authorization_Name (D : in Data) return String is
   begin
      return To_String (D.Auth_Name);
   end Authorization_Name;

   ----------------------
   -- Authorization_NC --
   ----------------------

   function Authorization_NC (D : in Data) return String is
   begin
      return To_String (D.Auth_NC);
   end Authorization_NC;

   -------------------------
   -- Authorization_Nonce --
   -------------------------

   function Authorization_Nonce (D : in Data) return String is
   begin
      return To_String (D.Auth_Nonce);
   end Authorization_Nonce;

   ----------------------------
   -- Authorization_Password --
   ----------------------------

   function Authorization_Password (D : in Data) return String is
   begin
      return To_String (D.Auth_Password);
   end Authorization_Password;

   -----------------------
   -- Authorization_QOP --
   -----------------------

   function Authorization_QOP (D : in Data) return String is
   begin
      return To_String (D.Auth_QOP);
   end Authorization_QOP;

   -------------------------
   -- Authorization_Realm --
   -------------------------

   function Authorization_Realm (D : in Data) return String is
   begin
      return To_String (D.Auth_Realm);
   end Authorization_Realm;

   ----------------------------
   -- Authorization_Response --
   ----------------------------

   function Authorization_Response (D : in Data) return String is
   begin
      return To_String (D.Auth_Response);
   end Authorization_Response;

   -----------------
   -- Binary_Data --
   -----------------

   function Binary_Data (D : in Data) return Stream_Element_Array is
   begin
      return D.Binary_Data.all;
   end Binary_Data;

   ------------------
   -- Check_Digest --
   ------------------

   function Check_Digest (D : in Data; Password : in String) return Boolean is

      function Get_Nonce return String;
      --  returns Nonce for the Digest authentication without "qop"
      --  parameter, or [nonce]:[nc]:[cnonce]:[qop] for the Digest
      --  authentication with qop parameter.
      --  It is just for convinience to implement RFC 2617 3.2.2.1.

      ---------------
      -- Get_Nonce --
      ---------------

      function Get_Nonce return String is
         Nonce : constant String := Authorization_Nonce (D);
         QOP   : constant String := Authorization_QOP (D);
      begin
         if QOP = "" then
            return Nonce;
         else
            return Nonce
              & ':' & Authorization_NC (D)
              & ':' & Authorization_CNonce (D)
              & ':' & QOP;
         end if;
      end Get_Nonce;

   begin
      return
        Authorization_Response (D)
        =
        AWS.Digest.Create_Digest
          (Username => Authorization_Name (D),
           Realm    => Authorization_Realm (D),
           Password => Password,
           Nonce    => Get_Nonce,
           Method   => Request_Method'Image (D.Method),
           URI      => URI (D));
   end Check_Digest;

   ----------------
   -- Connection --
   ----------------

   function Connection (D : in Data) return String is
   begin
      return Headers.Get (D.Header, Messages.Connection_Token);
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
      return Headers.Get (D.Header, Messages.Content_Type_Token);
   end Content_Type;

   -----------------
   -- Has_Session --
   -----------------

   function Has_Session (D : in Data) return Boolean is
      use type AWS.Session.ID;
   begin
      return D.Session_ID /= AWS.Session.No_Session;
   end Has_Session;

   ------------
   -- Header --
   ------------

   function Header (D : in Data) return Headers.List is
   begin
      return D.Header;
   end Header;

   ----------
   -- Host --
   ----------

   function Host (D : in Data) return String is
   begin
      return Headers.Get (D.Header, Messages.Host_Token);
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
      return Headers.Get (D.Header, Messages.If_Modified_Since_Token);
   end If_Modified_Since;

   -------------
   -- Is_SOAP --
   -------------

   function Is_SOAP (D : in Data) return Boolean is
   begin
      return D.SOAP_Action;
   end Is_SOAP;

   ----------------
   -- Keep_Alive --
   ----------------

   function Keep_Alive (D : in Data) return Boolean is
   begin
      return D.Keep_Alive;
   end Keep_Alive;

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
      use Headers;
   begin
      --  Get the Boundary value from the Contant_Type header value.
      --  We do not need to have the boundary in the Status.Data preparsed,
      --  becouse the AWS is not using function Multipart_Boundary internally.

      return Values.Search
        (Get (D.Header, Messages.Content_Type_Token),
         "Boundary", Case_Sensitive => False);
   end Multipart_Boundary;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (D : in Data) return AWS.Parameters.List is
   begin
      return D.Parameters;
   end Parameters;

   -------------
   -- Payload --
   -------------

   function Payload (D : in Data) return String is
   begin
      if D.SOAP_Action then
         return Translator.To_String (D.Binary_Data.all);
      else
         return "";
      end if;
   end Payload;

   --------------
   -- Peername --
   --------------

   function Peername (D : in Data) return String is
   begin
      return To_String (D.Peername);
   end Peername;

   -------------
   -- Referer --
   -------------

   function Referer (D : in Data) return String is
   begin
      return Headers.Get (D.Header, Messages.Referer_Token);
   end Referer;

   -------------
   -- Session --
   -------------

   function Session (D : in Data) return AWS.Session.ID is
   begin
      return D.Session_ID;
   end Session;

   ---------------------
   -- Session_Created --
   ---------------------

   function Session_Created (D : in Data) return Boolean is
   begin
      return D.Session_Created;
   end Session_Created;

   ----------------
   -- SOAPAction --
   ----------------

   function SOAPAction (D : in Data) return String is
      Result : constant String
        := Headers.Get (D.Header, Messages.SOAPAction_Token);
   begin
      if Result'First < Result'Last
        and then Result (Result'First) = '"'
        and then Result (Result'Last) = '"'
      then
         return Result (Result'First + 1 .. Result'Last - 1);
      else
         return Result;
      end if;
   end SOAPAction;

   ------------
   -- Socket --
   ------------

   function Socket (D : in Data) return Net.Socket_Type'Class is
   begin
      return D.Socket.all;
   end Socket;

   ---------
   -- URI --
   ---------

   function URI (D : in Data) return String is
   begin
      return URL.URL (D.URI);
   end URI;

   function URI (D : in Data) return URL.Object
   is
   begin
      return D.URI;
   end URI;

   ----------------
   -- User_Agent --
   ----------------

   function User_Agent (D : in Data) return String is
   begin
      return Headers.Get (D.Header, Messages.User_Agent_Token);
   end User_Agent;

end AWS.Status;
