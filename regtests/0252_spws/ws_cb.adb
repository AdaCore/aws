------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2014-2015, AdaCore                      --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Net;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Parameters;
with AWS.Server.Push;
with AWS.Status;
with AWS.Translator;

package body WS_CB is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   use AWS;

   type WS_type is new AWS.Net.WebSocket.Object with null record;
   type WS_Access is access WS_Type;

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data) return AWS.Net.WebSocket.Object'Class;

   procedure On_Open    (Socket : in out WS_type; Message : String);
   procedure On_Message (Socket : in out WS_type; Message : String);
   procedure On_Message (Socket : in out WS_type; Message : Unbounded_String);
   procedure On_Close   (Socket : in out WS_type; Message : String);
   procedure On_Error   (Socket : in out WS_type; Message : String);

   type Client_Env is record
      Num : Integer;
      Str : Unbounded_String;
   end record;

   function To_Array
     (Num : Integer;
      Env : Client_Env) return Stream_Element_Array;

   package Int_Push is new AWS.Server.Push
     (Client_Output_Type => Integer,
      Client_Environment => Client_Env,
      To_Stream_Array    => To_Array);

   SP : Int_Push.Object;

   -------------------------
   -- protected body Wait --
   -------------------------

   protected body Wait is

      entry Ready when Started is
      begin
         null;
      end Ready;

      entry Close when not Started is
      begin
         null;
      end Close;

      procedure Set (Value : Boolean) is
      begin
         Started := Value;
      end Set;

   end Wait;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data) return AWS.Net.WebSocket.Object'Class
   is
      use  AWS.Status, AWS.Net;
   begin
      return WS_Type'(AWS.Net.WebSocket.Object
                        (AWS.Net.WebSocket.Create (Socket, Request))
                        with null record);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Request : AWS.Status.Data) return AWS.Response.Data is
   begin
      return Response.Build
        ("text/html",
         "<html><head></head><body>ERROR:HTML-Response</body></html>");
   end Get;

   --------------
   -- On_Close --
   --------------

   procedure On_Close (Socket : in out WS_type; Message : String) is
   begin
      Text_IO.Put_Line ("On_Close:" & Message);
      Wait.Set (False);
   end On_Close;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Socket : in out WS_type; Message : String) is
   begin
      Text_IO.Put_Line ("On_Error:" & Message);
   end On_Error;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Socket : in out WS_type; Message : String) is
   begin
      Text_IO.Put_Line ("On_Message:" & Message);
   end On_Message;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message
     (Socket : in out WS_type; Message : Unbounded_String) is
   begin
      Text_IO.Put_Line ("On_Message (Unb):" & To_String (Message));
   end On_Message;

   -------------
   -- On_Open --
   -------------

   procedure On_Open (Socket : in out WS_type; Message : String) is
      use Ada.Calendar;
      use AWS.Net.WebSocket;

      URI : String := AWS.Net.WebSocket.Object'Class (Socket).URI;
   begin
      Text_IO.Put_Line ("On_Open:" & URI);

      Int_Push.Register
        (Server            => SP,
         Client_Id         => "qwerty",
         Socket            => AWS.Net.Socket_Access'
           (new WS_Type'(Socket)),
         Environment       => (Num => 999,
                               Str => To_Unbounded_String ("abcdef")),
         Init_Content_Type => "text/plain",
         Init_Data         => 0,
         Kind              => Int_Push.Plain);

      Wait.Set (True);
   end On_Open;

   ----------------------
   -- Server_Push_Send --
   ----------------------

   procedure Server_Push_Send (Num : Integer) is
   begin
      Int_Push.Send (SP, Num, Content_Type => "text/plain");
   end Server_Push_Send;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Num : Integer;
      Env : Client_Env) return Ada.Streams.Stream_Element_Array is
   begin
      return Translator.To_Stream_Element_Array
        ("Client " & To_String (Env.Str) & Env.Num'Img
           & " Message " & Num'Img);
   end To_Array;

   -------------------
   -- Websock_Start --
   -------------------

   procedure Websock_Start is
   begin
      AWS.Net.WebSocket.Registry.Control.Start;

      AWS.Net.WebSocket.Registry.Register
        (URI => "/server_push", Factory => Create'Access);
   end Websock_Start;

end WS_CB;
