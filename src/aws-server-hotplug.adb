------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2005                          --
--                                 AdaCore                                  --
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AI302.Containers.Indefinite_Hashed_Maps;
with AI302.Strings.Hash;
with Strings_Cutter;

with AWS.Communication;
with AWS.Communication.Server;
with AWS.Digest;
with AWS.Messages;
with AWS.MIME;
with AWS.URL;

package body AWS.Server.Hotplug is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   Authorization_Error : exception;

   function Message
     (Server     : in String;
      Name       : in String;
      Web_Server : in HTTP_Access;
      Parameters : in Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
      return Response.Data;
   --  Handle incoming message to register/unregister a module

   package Hotplug_Server is
     new Communication.Server (HTTP, HTTP_Access, Message);

   type Client_Data is record
      Password : Unbounded_String;
      Host     : Unbounded_String;
      Port     : Positive;
      Nonce    : Digest.Nonce;
   end record;

   package Client_Table is new AI302.Containers.Indefinite_Hashed_Maps
     (String, Client_Data, AI302.Strings.Hash);

   Null_Nonce : constant Digest.Nonce := (others => ' ');

   protected Client_Handler is

      procedure Add
        (Client : in String;
         Data   : in Client_Data);
      --  Add this client to the list of trusted clients

      procedure Get_Nonce
        (Client  : in     String;
         Nonce   :    out Digest.Nonce);
      --  Returns a new Nonce string

      function Get (Client : in String) return Client_Data;
      --  Returns data for specified client

      procedure Delete (Client : in String);
      pragma Unreferenced (Delete);
      --  Removes this client from the handler

      procedure Delete_All;
      --  Removes all client from the handler

   private
      Clients : Client_Table.Map;
   end Client_Handler;

   ----------------------
   -- Activate_Hotplug --
   ----------------------

   procedure Activate
     (Web_Server         : in HTTP_Access;
      Port               : in Positive;
      Authorization_File : in String;
      Register_Mode      : in AWS.Hotplug.Register_Mode := AWS.Hotplug.Add)
   is
      use Ada;
      use Ada.Characters.Handling;

      function "+"
        (Str : in String)
         return Unbounded_String renames To_Unbounded_String;

      File   : Text_IO.File_Type;
      Buffer : String (1 .. 1_024);
      Last   : Natural;
      Line   : Strings_Cutter.Cut_String;
      N      : Natural := 0;
   begin
      Hotplug_Server.Start (Port, Web_Server);
      AWS.Hotplug.Set_Mode (Web_Server.Filters, Register_Mode);

      Text_IO.Open (File, Text_IO.In_File, Authorization_File);

      while not Text_IO.End_Of_File (File) loop
         Text_IO.Get_Line (File, Buffer, Last);
         N := N + 1;
         Strings_Cutter.Create (Line, Buffer (1 .. Last), Separators => ":");

         if Strings_Cutter.Field_Count (Line) /= 4 then
            declare
               Error_Message : constant String
                 := Authorization_File & ": format error in line "
                    & Natural'Image (N);
            begin
               Log.Write (Web_Server.Error_Log, Error_Message);
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity, Error_Message);
            end;
         end if;

         Client_Handler.Add
           (Client => Strings_Cutter.Field (Line, 1),
            Data   =>
              (Password => +Strings_Cutter.Field (Line, 2),
               Host     => +To_Lower (Strings_Cutter.Field (Line, 3)),
               Port     => Positive'Value (Strings_Cutter.Field (Line, 4)),
               Nonce    => Null_Nonce));
      end loop;

      Text_IO.Close (File);
   exception
      when Text_IO.Name_Error =>
         Log.Write
           (Web_Server.Error_Log,
            "Can't open authorization file " & Authorization_File);
   end Activate;

   -------------
   -- Message --
   -------------

   function Message
     (Server     : in String;
      Name       : in String;
      Web_Server : in HTTP_Access;
      Parameters : in Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
      return Response.Data
   is
      pragma Unreferenced (Server);
      use Ada.Characters.Handling;

      function Get_Nonce (Client_Name : in String) return String;
      --  Returns a Nonce string

      function Check_Auth
        (Client_Name, Digest, Regexp : in String;
         URL                         : in String := "") return Boolean;
      --  Returns True if the Digest string is ok. If URL is specified, checks
      --  also that this redirection is authorized.

      ----------------
      -- Check_Auth --
      ----------------

      function Check_Auth
        (Client_Name, Digest, Regexp : in String;
         URL                         : in String := "") return Boolean
      is
         procedure Log_Error;
         --  Log an error message into the server error log file

         CD     : constant Client_Data := Client_Handler.Get (Client_Name);
         D      : AWS.Digest.Digest_String;
         Result : Boolean;

         ---------------
         -- Log_Error --
         ---------------

         procedure Log_Error is
         begin
            Log.Write
              (Web_Server.Error_Log,
               "Wrong authorization "
               & Client_Name & '|' & Regexp & '|' & URL);
         end Log_Error;

      begin
         D := AWS.Digest.Create
           (Client_Name, "hotplug",
            To_String (CD.Password), String (CD.Nonce), "hotplug", Regexp);

         if URL = "" then
            Result := D = Digest;
            return D = Digest;
         else
            declare
               U : AWS.URL.Object;
            begin
               U := AWS.URL.Parse (URL);
               Result := D = Digest
                 and then To_Lower (AWS.URL.Host (U)) = To_String (CD.Host)
                 and then AWS.URL.Port (U) = CD.Port;
            exception
               when AWS.URL.URL_Error =>
                  Result := False;
            end;
         end if;

         if not Result then
            Log_Error;
         end if;

         return Result;
      end Check_Auth;

      ---------------
      -- Get_Nonce --
      ---------------

      function Get_Nonce (Client_Name : in String) return String is
         Nonce : Digest.Nonce;
      begin
         Client_Handler.Get_Nonce (Client_Name, Nonce);
         return String (Nonce);
      end Get_Nonce;

   begin
      --  There is two kind of message REGISTER and UNREGISTER. The formats
      --  are (parameters are between <>):
      --
      --  REGISTER <name> <digest> <regexp> <URL>
      --  UNREGISTER <name> <digest> <regexp>
      --  REQUEST_NONCE <name>

      if Name = Register_Message and then Parameters'Length = 4 then
         if Check_Auth
           (Client_Name => To_String (Parameters (1)),
            Digest      => To_String (Parameters (2)),
            Regexp      => To_String (Parameters (3)),
            URL         => To_String (Parameters (4)))
         then
            --  Now check that the URL host for this client is authorized
            AWS.Hotplug.Register
              (Web_Server.Filters,
               To_String (Parameters (3)),
               To_String (Parameters (4)));
            return Response.Acknowledge (Messages.S200, "OK");
         else
            return Response.Acknowledge (Messages.S401, "Wrong authorization");
         end if;

      elsif Name = Unregister_Message and then Parameters'Length = 3 then
         if Check_Auth
           (Client_Name => To_String (Parameters (1)),
            Digest      => To_String (Parameters (2)),
            Regexp      => To_String (Parameters (3)))
         then
            AWS.Hotplug.Unregister
              (Web_Server.Filters,
               To_String (Parameters (3)));
            return Response.Acknowledge (Messages.S200, "OK");
         else
            return Response.Acknowledge (Messages.S401, "Wrong authorization");
         end if;

      elsif Name = Request_Nonce_Message and then Parameters'Length = 1 then
         return Response.Build
           (MIME.Text_Plain,
            Get_Nonce (To_String (Parameters (1))));

      else
         return Response.Acknowledge (Messages.S400, "Unknown message");
      end if;

   exception
      when AWS.Hotplug.Register_Error =>
         --  Exception sent because a duplicate client name has been found
         --  on this server.
         return Response.Acknowledge (Messages.S409, "Cannot register");

      when Authorization_Error =>
         return Response.Acknowledge (Messages.S401, "Cannot register");
   end Message;

   -------------------
   -- Nonce_Handler --
   -------------------

   protected body Client_Handler is

      ---------
      -- Add --
      ---------

      procedure Add
        (Client  : in     String;
         Data    : in     Client_Data)
      is
         Cursor  : Client_Table.Cursor;
         Success : Boolean;
      begin
         Client_Table.Insert (Clients, Client, Data, Cursor, Success);

         if not Success then
            raise Authorization_Error;
         end if;
      end Add;

      ------------
      -- Delete --
      ------------

      procedure Delete (Client : in String) is
      begin
         Client_Table.Delete (Clients, Client);
      end Delete;

      ----------------
      -- Delete_All --
      ----------------

      procedure Delete_All is
      begin
         Client_Table.Clear (Clients);
      end Delete_All;

      ---------
      -- Get --
      ---------

      function Get (Client : in String) return Client_Data is
         Cursor : Client_Table.Cursor;
      begin
         Cursor := Client_Table.Find (Clients, Client);

         if Client_Table.Has_Element (Cursor) then
            return Client_Table.Element (Cursor);
         else
            raise Authorization_Error;
         end if;
      end Get;

      ---------------
      -- Get_Nonce --
      ---------------

      procedure Get_Nonce
        (Client : in     String;
         Nonce  :    out Digest.Nonce)
      is
         Cursor : Client_Table.Cursor;
      begin
         Cursor := Client_Table.Find (Clients, Client);

         if Client_Table.Has_Element (Cursor) then
            declare
               CD : Client_Data := Client_Table.Element (Cursor);
            begin
               CD.Nonce := Digest.Create_Nonce;
               Nonce    := CD.Nonce;
               Client_Table.Replace_Element (Cursor, CD);
            end;

         else
            raise Authorization_Error;
         end if;
      end Get_Nonce;

   end Client_Handler;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Hotplug_Server.Shutdown;
      Client_Handler.Delete_All;
   end Shutdown;

end AWS.Server.Hotplug;
