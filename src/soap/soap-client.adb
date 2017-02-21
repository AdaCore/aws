------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.Response;
with AWS.URL;

with SOAP.Message.Response.Error;
with SOAP.Message.XML;

package body SOAP.Client is

   use Ada.Strings.Unbounded;

   ----------
   -- Call --
   ----------

   function Call
     (URL          : String;
      P            : Message.Payload.Object;
      SOAPAction   : String                     := No_SOAPAction;
      User         : String                     := Not_Specified;
      Pwd          : String                     := Not_Specified;
      Proxy        : String                     := Not_Specified;
      Proxy_User   : String                     := Not_Specified;
      Proxy_Pwd    : String                     := Not_Specified;
      Timeouts     : AWS.Client.Timeouts_Values := AWS.Client.No_Timeout;
      Asynchronous : Boolean                    := False;
      Schema       : WSDL.Schema.Definition     := WSDL.Schema.Empty)
      return Message.Response.Object'Class
   is
      Connection : AWS.Client.HTTP_Connection;
   begin
      AWS.Client.Create
        (Connection,
         URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
         Persistent => False,
         Timeouts   => Timeouts);

      declare
         Result : constant Message.Response.Object'Class :=
                    Call (Connection, SOAPAction, P, Asynchronous, Schema);
      begin
         AWS.Client.Close (Connection);
         return Result;
      end;
   exception
      when others =>
         AWS.Client.Close (Connection);
         raise;
   end Call;

   ----------
   -- Call --
   ----------

   function Call
     (Connection   : AWS.Client.HTTP_Connection;
      SOAPAction   : String;
      P            : Message.Payload.Object;
      Asynchronous : Boolean := False;
      Schema       : WSDL.Schema.Definition := WSDL.Schema.Empty)
      return Message.Response.Object'Class
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type AWS.Messages.Status_Code;

      function SOAP_Action return String;
      --  Returns the proper SOAPAction string for this call

      -----------------
      -- SOAP_Action --
      -----------------

      function SOAP_Action return String is
      begin
         if SOAPAction = No_SOAPAction then
            declare
               URL        : constant String := AWS.Client.Host (Connection);
               URL_Object : constant AWS.URL.Object := AWS.URL.Parse (URL);
            begin
               return AWS.URL.URL (URL_Object) & '#'
                      & SOAP.Message.Payload.Procedure_Name (P);
            end;

         elsif SOAPAction = "" then
            --  Empty SOAP Action
            return """""";

         else
            return SOAPAction;
         end if;
      end SOAP_Action;

      Response : AWS.Response.Data;
      XML_Data : constant Unbounded_String :=
                   SOAP.Message.XML.Image (P, Schema);
      XML_Str  : String_Access := new String (1 .. Length (XML_Data));
   begin
      for I in 1 .. Length (XML_Data) loop
         XML_Str (I) := Element (XML_Data, I);
      end loop;

      AWS.Client.SOAP_Post
        (Connection, Response, SOAP_Action, XML_Str.all, True);
      Free (XML_Str);

      --  Valid responses code included S500 which is the code returned for
      --  SOAP fault-messages.
      --  See SOAP HTTP Response section 6.2 of W3C Note from 08 May 2000.

      if AWS.Response.Status_Code (Response)
        in AWS.Messages.Success | AWS.Messages.S500
      then
         --  In Asynchronous mode allow an empty message body. In this specific
         --  case there is nothing to read from the connection (socket).

         if Asynchronous
           and then AWS.Response.Content_Length (Response) = 0
         then
            return S : Message.Response.Object do
               null;
            end return;

         else
            --  All other cases, read the response from the connection

            return Message.XML.Load_Response (Connection, Schema => Schema);
         end if;

      else
         return Message.Response.Error.Build
           (Faultcode   => Message.Response.Error.Faultcode
              (AWS.Messages.Status_Code'Image
                 (AWS.Response.Status_Code (Response))),
            Faultstring => AWS.Response.Message_Body (Response));
      end if;

   exception
      when others =>
         Free (XML_Str);
         raise;
   end Call;

end SOAP.Client;
