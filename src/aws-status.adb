------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
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
with Ada.Strings;

with AWS.Digest;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.Translator;

package body AWS.Status is

   use Ada.Strings;

   ---------------------
   -- Accept_Encoding --
   ---------------------

   function Accept_Encoding (D : in Data) return String is
   begin
      return Headers.Get (D.Header, Messages.Accept_Encoding_Token);
   end Accept_Encoding;

   -----------------
   -- Attachments --
   -----------------

   function Attachments (D : in Data) return AWS.Attachments.List is
   begin
      return D.Attachments;
   end Attachments;

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
      use type Messages.Status_Code;
   begin
      return Check_Digest (D, Password) = Messages.S200;
   end Check_Digest;

   function Check_Digest
     (D        : in Data;
      Password : in String)
      return Messages.Status_Code
   is
      Nonce    : constant String := Authorization_Nonce (D);
      Auth_URI : constant String := To_String (D.Auth_URI);
      Auth_URL : constant URL.Object
        := URL.Parse (Auth_URI, Check_Validity => False, Normalize => True);

      Data_URL : URL.Object := D.URI;

      function Get_Nonce return String;
      --  Returns Nonce for the Digest authentication without "qop"
      --  parameter, or [nonce]:[nc]:[cnonce]:[qop] for the Digest
      --  authentication with qop parameter.
      --  It is just for convenience to implement RFC 2617 3.2.2.1.

      ---------------
      -- Get_Nonce --
      ---------------

      function Get_Nonce return String is
         QOP : constant String := Authorization_QOP (D);
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
      URL.Normalize (Data_URL);

      if URL.Abs_Path (Data_URL) /= URL.Abs_Path (Auth_URL)
        or else Nonce = ""
      then
         --  Bad request.

         return Messages.S400;

      elsif Authorization_Response (D)
            /=
            AWS.Digest.Create
              (Username => Authorization_Name (D),
               Realm    => Authorization_Realm (D),
               Password => Password,
               Nonce    => Get_Nonce,
               Method   => Request_Method'Image (D.Method),
               URI      => Auth_URI)
      then
         --  Unauthorized.

         return Messages.S401;

      else
         --  Successful.

         return Messages.S200;
      end if;
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

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported
     (D        : in Data;
      Encoding : in Messages.Content_Encoding)
      return Boolean
   is
      function To_Lower (Item : String) return String
         renames Ada.Characters.Handling.To_Lower;

      Encoding_Image : constant String
        := To_Lower (Messages.Content_Encoding'Image (Encoding));

      Found_Encoding  : Boolean := False;
      Enable_Encoding : Boolean := True;
      --  QValue is 1 by default, i.e enought.

      Just_Found_Others : Boolean := False;

      Found_Others  : Boolean := False;
      --  Found others - "*" symbol.

      Enable_Others : Boolean := True;
      --  All others encoding enabled by default.

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean);

      procedure Value (Item : in String; Quit : in out Boolean);

      -----------------
      -- Named_Value --
      -----------------

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean) is
      begin
         if (Name = "q" or Name = "Q") and Float'Value (Value) = 0.0 then
            if Found_Encoding then
               --  Encoding is disabled by encoding;q=0;

               Enable_Encoding := False;
               Quit            := True;
            elsif Just_Found_Others then
               Enable_Others := False;
            end if;
         end if;
      end Named_Value;

      -----------
      -- Value --
      -----------

      procedure Value (Item : in String; Quit : in out Boolean) is
      begin
         if Found_Encoding then
            if Enable_Encoding then
               Quit := True;

               return;
            else
               --  Encoding is enable if qvalue is not specified.

               Enable_Encoding := True;
            end if;
         end if;

         Found_Encoding    := To_Lower (Item) = Encoding_Image;
         Just_Found_Others := Item = "*";

         if Just_Found_Others and not Found_Others then
            Found_Others := True;
         end if;
      end Value;

      procedure Parse is new Headers.Values.Parse (Value, Named_Value);

      use type Messages.Content_Encoding;

   begin
      for
        K in 1 .. Headers.Count (D.Header, Messages.Accept_Encoding_Token)
      loop
         Parse (Headers.Get (D.Header, Messages.Accept_Encoding_Token, K));
      end loop;

      return (Found_Encoding and Enable_Encoding)
        or else
        (not Found_Encoding and Found_Others and Enable_Others)
        or else
        (Encoding = Messages.Identity and Enable_Others);
   end Is_Supported;

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
      --  We do not need to have the boundary in the Status.Data pre-parsed,
      --  because AWS is not using function Multipart_Boundary internally.

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

   ----------------------
   -- Preferred_Coding --
   ----------------------

   function Preferred_Coding (D : in Data) return Messages.Content_Encoding is

      Best_Encoding : Messages.Content_Encoding := Messages.Identity;
      Next_Encoding : Messages.Content_Encoding;

      Supported     : Boolean := False;
      --  Next coding supported by AWS.

      Best_QValue : Float := 0.0;
      Next_QValue : Float;

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean);

      procedure Value (Item : in String; Quit : in out Boolean);

      -----------------
      -- Named_Value --
      -----------------

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean) is
         pragma Unreferenced (Quit);
      begin
         if Supported and (Name = "Q" or Name = "q") then
            Next_QValue := Float'Value (Value);
         end if;
      end Named_Value;

      -----------
      -- Value --
      -----------

      procedure Value (Item : in String; Quit : in out Boolean) is
      begin
         if Supported and Next_QValue > Best_QValue then
            Best_Encoding := Next_Encoding;
            Best_QValue   := Next_QValue;

            if Best_QValue = 1.0 then
               --  Could not be more then 1.

               Quit := True;
               return;
            end if;
         end if;

         begin
            Next_Encoding := Messages.Content_Encoding'Value (Item);
            Next_QValue   := 1.0; --  Default qvalue.
            Supported     := True;
         exception
            when Constraint_Error =>
               Supported := False;
         end;
      end Value;

      procedure Parse is new Headers.Values.Parse (Value, Named_Value);

   begin
      for
        K in 1 .. Headers.Count (D.Header, Messages.Accept_Encoding_Token)
      loop
         Parse (Headers.Get (D.Header, Messages.Accept_Encoding_Token, K));
      end loop;

      if Supported and Next_QValue > Best_QValue then
         Best_Encoding := Next_Encoding;
         Best_QValue   := Next_QValue;
      end if;

      return Best_Encoding;
   end Preferred_Coding;

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
      use Ada.Exceptions;
   begin
      if Has_Session (D) then
         return D.Session_ID;

      else
         Raise_Exception
           (Constraint_Error'Identity,
            Message => "Can't use AWS session feature "
              & "if session support not activated.");
      end if;
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
