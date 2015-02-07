------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
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

package body WebSock_CB is

   use Ada;
   use type AWS.Net.WebSocket.Kind_Type;

   Opened : Natural := 0;
   --  Number of opened WebSocket

   ---------
   -- Wait --
   ----------

   protected body Wait is
      entry Start when Step > 0 is
      begin
         null;
      end Start;

      entry Stop when Step > 99 is
      begin
         null;
      end Stop;

      procedure Set (Value : Integer) is
      begin
         Step := Value;
      end Set;
   end Wait;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : Net.Socket_Access;
      Request : Status.Data) return Net.WebSocket.Object'Class is
   begin
      --  Wait for previous socket to be closed, this is to ensure all log
      --  from previous WebSocket are displayed.

      while Opened /= 0 loop
         delay 0.1;
      end loop;

      Opened := Opened + 1;

      return Object'(Net.WebSocket.Object
                      (Net.WebSocket.Create (Socket, Request)) with C => 0);
   end Create;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.Build
        ("text/html", "<html><head></head><body>HTML Response</body></html>");
   end HW_CB;

   --------------
   -- On_Close --
   --------------

   overriding procedure On_Close (Socket : in out Object; Message : String) is
   begin
      Text_IO.Put_Line
        ("On_Close : "
         & Net.WebSocket.Error_Type'Image (Socket.Error) & ", " & Message);
      Opened := Opened - 1;
   end On_Close;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (Socket : in out Object; Message : String) is
   begin
      Text_IO.Put_Line
        ("On_Error : "
         & Net.WebSocket.Error_Type'Image (Socket.Error) & ", " & Message);
   end On_Error;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Socket : in out Object; Message : String) is
   begin
      Text_IO.Put_Line ("Received : " & Message);

      if Message = "END_TEST" then
         Wait.Set (100);
      else
         Socket.Send ("Echo " & Message);
      end if;
   end On_Message;

   -------------
   -- On_Open --
   -------------

   overriding procedure On_Open (Socket : in out Object; Message : String) is
   begin
      Text_IO.Put_Line ("On_Open : " & Message);
      Socket.Send ("Server open connect");
      Wait.Set (1);
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

end WebSock_CB;
