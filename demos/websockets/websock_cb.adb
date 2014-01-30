------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

with AWS.Messages;
with AWS.MIME;
with AWS.Templates;
with AWS.Translator;

package body WebSock_CB is

   use Ada;
   use type AWS.Net.WebSocket.Kind_Type;

   WWW_Root : constant String := "../../web_elements";

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
      if URI'Length > 6
        and then URI (URI'First .. URI'First + 6) = "/we_js/"
      then
         return AWS.Response.Build
           (MIME.Text_Javascript,
            Message_Body => Templates.Parse
              (WWW_Root & "/javascripts" & URI (URI'First + 6 .. URI'Last)));

      elsif URI'Length = 12
        and then URI (URI'First .. URI'First + 11) = "/favicon.ico"
      then
         return AWS.Response.Acknowledge (Messages.S404);

      else
         return Response.Build
           ("text/html", String'(Templates.Parse ("page.thtml")));
      end if;
   end HW_CB;

   --------------
   -- On_Close --
   --------------

   overriding procedure On_Close (Socket : in out Object; Message : String) is
   begin
      Text_IO.Put_Line
        ("Received : Connection_Close "
         & Net.WebSocket.Error_Type'Image (Socket.Error) & ", " & Message);
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

      --  We have received a message from the client, echo this message

      Socket.Send
        (String'(Templates.Parse
                   ("echo.xml", (1 => Templates.ASSOC ("MESSAGE", Message)))));
   end On_Message;

   -------------
   -- On_Open --
   -------------

   overriding procedure On_Open (Socket : in out Object; Message : String) is
   begin
      Created := True;
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

      if Message (Message'First) = '<' then
         Net.WebSocket.Object (Socket).Send (Message);
      else
         Net.WebSocket.Object (Socket).Send
           (Message & Natural'Image (Socket.C));
      end if;
   end Send;

   -----------
   -- W_Log --
   -----------

   procedure W_log
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset)
   is
      Max : constant := 6;
      Str : String (1 .. Max);
      I   : Natural := Str'First - 1;
   begin
      Text_IO.Put_Line (Net.Log.Data_Direction'Image (Direction));
      Text_IO.Put_Line ("[");

      for K in Data'First .. Last loop
         I := I + 1;
         if Characters.Handling.Is_Graphic (Character'Val (Data (K))) then
            Str (I) := Character'Val (Data (K));
         else
            Str (I) := '.';
         end if;

         Text_IO.Put (Str (I));

         Text_IO.Put ('|');
         Integer_Text_IO.Put (Integer (Data (K)), Base => 16, Width => 6);
         Text_IO.Put ("   ");

         if K mod Max = 0 then
            Text_IO.Put_Line (" " & Str (Str'First .. I));
            I := Str'First - 1;
         end if;
      end loop;

      if I > Str'First then
         Text_IO.Set_Col (67);
         Text_IO.Put_Line (" " & Str (Str'First .. I));
      end if;

      Text_IO.Put_Line ("]");
   end W_Log;

end WebSock_CB;
