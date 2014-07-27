------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

package body WSUID_CB is

   use Ada;
   use type AWS.Net.WebSocket.Kind_Type;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : Net.Socket_Access;
      Request : Status.Data) return Net.WebSocket.Object'Class is
   begin
      return Object'(Net.WebSocket.Object
                      (Net.WebSocket.Create (Socket, Request)) with C => 0);
   end Create;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      return Response.Build
        ("text/html", "<html><head></head><body>HTML Response</body></html>");
   end HW_CB;

   --------------
   -- On_Close --
   --------------

   overriding procedure On_Close (Socket : in out Object; Message : String) is
      use type Net.WebSocket.UID;
   begin
      Text_IO.Put_Line
        ("Received : Connection_Close "
         & Net.WebSocket.Error_Type'Image (Socket.Error) & ", " & Message);
      Created := False;

      if Socket.Get_UID = Uid_Store then
         Text_IO.Put_Line ("closed socket known");
      else
         Text_IO.Put_Line
           ("unexpected socket closed "
             & Net.WebSocket.UID'Image (Socket.Get_UID));
      end if;
   end On_Close;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (Socket : in out Object; Message : String) is
   begin
      Text_IO.Put_Line
        ("Error : "
         & Net.WebSocket.Error_Type'Image (Socket.Error) & ", " & Message);
   end On_Error;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Socket : in out Object; Message : String) is
   begin
      Text_IO.Put_Line ("Received : " & Message);
      Socket.Send ("Echo " & Message);
   end On_Message;

   -------------
   -- On_Open --
   -------------

   overriding procedure On_Open (Socket : in out Object; Message : String) is
   begin
      Created := True;
      UID_Store := Socket.Get_UID;
      Text_IO.Put_Line
        ("On_Open : " & Message & Net.WebSocket.UID'Image (UID_Store));
      Socket.Send ("Server open connect");
   end On_Open;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : in out Object; Message : String; Is_Binary : Boolean := False)
   is
      pragma Unreferenced (Is_Binary);
   begin
      --  This implementation just adds a counter after the message
      Socket.C := Socket.C + 1;
      Text_IO.Put_Line
        ("Send : " & Message & " [" & Natural'Image (Socket.C) & "]");
      Net.WebSocket.Object (Socket).Send
        (Message & " [" & Natural'Image (Socket.C) & "]");
   end Send;

   overriding procedure Send
     (Socket    : in out Object;
      Message   : Unbounded_String;
      Is_Binary : Boolean := False)
   is
      pragma Unreferenced (Is_Binary);
   begin
      --  This implementation just adds a counter after the message
      Socket.C := Socket.C + 1;
      Text_IO.Put_Line
        ("Send : "
         & To_String (Message) & " [" & Natural'Image (Socket.C) & "]");
      Net.WebSocket.Object (Socket).Send
        (Message & " [" & Natural'Image (Socket.C) & "]");
   end Send;

end WSUID_CB;
