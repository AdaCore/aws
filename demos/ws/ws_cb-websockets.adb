------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2014-2017, AdaCore                      --
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

with Ada.Text_IO;

with AWS.Net.WebSocket.Registry.Control;
with Aws.Parameters;

package body WS_CB.WebSockets is

   use Ada;

   type WS_type is new AWS.Net.WebSocket.Object with record
      Client_Tag : Unbounded_String;
   end record;

   procedure On_Open (Socket : in out WS_type; Message : String);
   procedure On_Message (Socket : in out WS_type; Message : String);
   procedure On_Message (Socket : in out WS_type; Message : Unbounded_String);
   procedure On_Close (Socket : in out WS_type; Message : String);
   procedure On_Error (Socket : in out WS_type; Message : String);

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data) return AWS.Net.WebSocket.Object'Class;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data) return AWS.Net.WebSocket.Object'Class
   is
      use AWS.Status;

      Client_Tag : constant String :=
                     AWS.Status.Parameter (Request, "Client_Id");
   begin
      if Client_Tag'Length > 32 then
         raise Program_Error with "Client_Tag parameter too large";
      end if;

      Text_IO.Put_Line
        ("WS_Constructor, URI:" & URI (Request)
           & AWS.Parameters.URI_Format (AWS.Status.Parameters (Request)));

      return WS_Type'(AWS.Net.WebSocket.Object
                        (AWS.Net.WebSocket.Create (Socket, Request))
                        with Client_Tag => To_Unbounded_String (Client_Tag));
   end Create;

   --------------
   -- On_Close --
   --------------

   procedure On_Close (Socket : in out WS_type; Message : String) is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("On_Close:" & Message);
   end On_Close;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Socket : in out WS_type; Message : String) is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("On_Error:" & Message);
   end On_Error;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Socket : in out WS_type; Message : String) is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("On_Message:" & Message);
   end On_Message;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message
     (Socket : in out WS_type; Message : Unbounded_String)
   is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("On_Message (Unb):" & To_String (Message));
   end On_Message;

   -------------
   -- On_Open --
   -------------

   procedure On_Open (Socket : in out WS_type; Message : String) is
      pragma Unreferenced (Message);
      URI       : constant String :=
                    AWS.Net.WebSocket.Object'Class (Socket).URI;
      Client_Id : String (1 .. 32);
   begin
      Ada.Text_IO.Put_Line ("On_Open:" & URI);

      WS_CB.New_Client_Id.Get (Client_Id);

      Time_Push.Register
        (Server            => SP,
         Client_Id         => To_String (Socket.Client_Tag) & "-" & Client_Id,
         Socket            => AWS.Net.Socket_Access'(new WS_Type'(Socket)),
         Environment       => (Clock, To_Unbounded_String ("%D - %T")),
         Init_Content_Type => "text/plain",
         Init_Data         => Ada.Calendar.Clock,
         Kind              => Time_Push.Plain);
   end On_Open;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      AWS.Net.WebSocket.Registry.Control.Start;

      AWS.Net.WebSocket.Registry.Register
        (URI => "/server_push", Factory => Create'Access);
   end Start;

end WS_CB.WebSockets;
