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

with Ada.Calendar;
with AI302.Containers;

with AWS.Messages;
with AWS.MIME;
with AWS.Utils;

with GNAT.Calendar.Time_IO;
with System;

package body AWS.Server.Push is

   use AWS.Net;

   function To_Holder
     (Socket      : in Net.Socket_Type'Class;
      Environment : in Client_Environment;
      Kind        : in Mode)
      return Client_Holder;

   function To_Stream (Socket : in Net.Socket_Type'Class) return Stream_Access
     renames AWS.Net.Stream_IO.Stream;

   New_Line : constant String := ASCII.CR & ASCII.LF;
   --  HTTP new line.

   Boundary : constant String := "--AWS.Push.Boundary_"
     & GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%s")
     & New_Line;
   --  This is the multi-part boundary string used by AWS push server.

   -----------
   -- Count --
   -----------

   function Count (Server : in Object) return Natural is
   begin
      return Server.Count;
   end Count;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Server : in Object) return Boolean is
   begin
      return Server.Is_Open;
   end Is_Open;

   ------------
   -- Object --
   ------------

   protected body Object is

      ---------------
      -- Send_Data --
      ---------------

      procedure Send_Data
        (Holder       : in Client_Holder;
         Data         : in Client_Output_Type;
         Content_Type : in String);
      --  Send Data to a client identified by Holder.

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Natural (Table.Length (Container));
      end Count;

      -------------
      -- Is_Open --
      -------------

      function Is_Open return Boolean is
      begin
         return Open;
      end Is_Open;

      --------------
      -- Register --
      --------------

      procedure Register
        (Client_ID       : in     Client_Key;
         Holder          : in out Client_Holder;
         Close_Duplicate : in     Boolean)
      is
         Cursor  : Table.Cursor;
         Success : Boolean;
      begin

         if not Open then
            Net.Stream_IO.Free (Holder.Stream, False);
            raise Closed;
         end if;

         Table.Insert (Container, Client_ID, Holder, Cursor, Success);

         if not Success then
            if Close_Duplicate then
               Unregister (Client_ID, True);
               Table.Containers.Replace_Element (Cursor, Holder);
            else
               Net.Stream_IO.Free (Holder.Stream, False);
               raise Duplicate_Client_ID;
            end if;
         end if;

         begin
            String'Write
              (Holder.Stream,
               "HTTP/1.1 200 OK" & New_Line
                 & "Server: AWS (Ada Web Server) v"
                 & Version & New_Line
                 & Messages.Connection ("Close") & New_Line);

            if Holder.Kind = Chunked then
               String'Write
                 (Holder.Stream,
                  Messages.Transfer_Encoding ("chunked")
                    & New_Line & New_Line);

            elsif Holder.Kind = Multipart then
               String'Write
                 (Holder.Stream,
                  Messages.Content_Type
                    (MIME.Multipart_X_Mixed_Replace, Boundary)
                    & New_Line);

            else
               String'Write (Holder.Stream, New_Line);
            end if;

            Net.Stream_IO.Flush (Holder.Stream);

         exception
            when others =>
               Unregister (Client_ID, Close_Socket => False);
               raise;
         end;
      end Register;

      procedure Register
        (Client_ID         : in     Client_Key;
         Holder            : in out Client_Holder;
         Init_Data         : in     Client_Output_Type;
         Init_Content_Type : in     String;
         Close_Duplicate   : in     Boolean) is
      begin
         Register (Client_ID, Holder, Close_Duplicate);

         begin
            Send_Data (Holder, Init_Data, Init_Content_Type);
         exception
            when others =>
               Unregister (Client_ID, Close_Socket => False);
               raise;
         end;
      end Register;

      -------------
      -- Restart --
      -------------

      procedure Restart is
      begin
         Open := True;
      end Restart;

      ----------
      -- Send --
      ----------

      procedure Send
        (Data         : in     Client_Output_Type;
         Content_Type : in     String;
         Unregistered : in out Table.Map)
      is
         Cursor : Table.Cursor;
      begin
         Cursor := Table.First (Container);

         while Table.Has_Element (Cursor) loop
            declare
               Holder : constant Client_Holder
                 := Table.Containers.Element (Cursor);
            begin
               declare
                  Success : Boolean;
               begin
                  Send_Data (Holder, Data, Content_Type);

                  Table.Containers.Next (Cursor);
               exception
                  when Net.Socket_Error =>
                     declare
                        C   : Table.Cursor;
                        Key : constant Client_Key
                          := Table.Containers.Key (Cursor);
                     begin
                        Table.Insert (Unregistered, Key, Holder, C, Success);

                        --  We have to move cursor to the next position before
                        --  delete element from current position.

                        Table.Containers.Next (Cursor);

                        Unregister (Key, True);
                     end;
               end;
            end;
         end loop;
      end Send;

      ---------------
      -- Send_Data --
      ---------------

      procedure Send_Data
        (Holder       : in Client_Holder;
         Data         : in Client_Output_Type;
         Content_Type : in String)
      is
         Data_To_Send : constant Stream_Output_Type
           := To_Stream_Output (Data, Holder.Environment);

      begin
         if Holder.Kind = Multipart then
            String'Write
              (Holder.Stream,
               Boundary
                 & Messages.Content_Type (Content_Type) & New_Line & New_Line);

         elsif Holder.Kind = Chunked then
            String'Write
              (Holder.Stream,
               Utils.Hex (Data_To_Send'Size / System.Storage_Unit) & New_Line);
         end if;

         Stream_Output_Type'Write (Holder.Stream, Data_To_Send);

         if Holder.Kind = Multipart then
            String'Write (Holder.Stream, New_Line & New_Line);

         elsif Holder.Kind = Chunked then
            String'Write (Holder.Stream, New_Line);
         end if;

         Net.Stream_IO.Flush (Holder.Stream);
      end Send_Data;

      -------------
      -- Send_To --
      -------------

      procedure Send_To
        (Client_ID    : in Client_Key;
         Data         : in Client_Output_Type;
         Content_Type : in String)
      is
         Cursor : Table.Cursor;
      begin
         Cursor := Table.Find (Container, Client_ID);

         if Table.Has_Element (Cursor) then
            Send_Data (Table.Containers.Element (Cursor), Data, Content_Type);
         else
            raise Client_Gone;
         end if;

      exception
         when Net.Socket_Error =>
            Unregister (Client_ID, True);
            raise Client_Gone;
      end Send_To;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown (Close_Sockets : in Boolean) is
      begin
         Open := False;
         Unregister_Clients (Close_Sockets => Close_Sockets);
      end Shutdown;


      procedure Shutdown
        (Final_Data         : in Client_Output_Type;
         Final_Content_Type : in String)
      is
         Gone : Table.Map;
      begin
         Send (Final_Data, Final_Content_Type, Gone);
         Table.Clear (Gone);
         Shutdown (Close_Sockets => True);
      end Shutdown;

      -----------------------
      -- Shutdown_If_Empty --
      -----------------------

      procedure Shutdown_If_Empty (Open : out Boolean) is
         use type AI302.Containers.Size_Type;
      begin
         if Table.Length (Container) = 0 then
            Object.Open := False;
         end if;
         Shutdown_If_Empty.Open := Object.Open;
      end Shutdown_If_Empty;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister
        (Client_ID    : in Client_Key;
         Close_Socket : in Boolean)
      is
         Cursor : Table.Cursor;
         Value  : Client_Holder;
      begin
         Cursor := Table.Find (Container, Client_ID);

         if Table.Has_Element (Cursor) then
            Value := Table.Containers.Element (Cursor);

            if Close_Socket then
               Net.Stream_IO.Shutdown (Value.Stream);
            end if;

            Net.Stream_IO.Free (Value.Stream, Close_Socket);

            Table.Delete (Container, Cursor);
         end if;
      end Unregister;

      ------------------------
      -- Unregister_Clients --
      ------------------------

      procedure Unregister_Clients (Close_Sockets : in Boolean) is
         Cursor : Table.Cursor;
      begin
         loop
            Cursor := Table.First (Container);

            exit when not Table.Has_Element (Cursor);

            Unregister (Table.Containers.Key (Cursor), Close_Sockets);
         end loop;
      end Unregister_Clients;

   end Object;

   --------------
   -- Register --
   --------------

   procedure Register
     (Server            : in out Object;
      Client_ID         : in     Client_Key;
      Socket            : in     Net.Socket_Type'Class;
      Environment       : in     Client_Environment;
      Init_Data         : in     Client_Output_Type;
      Init_Content_Type : in     String := "";
      Kind              : in     Mode := Plain;
      Close_Duplicate   : in     Boolean := False)
   is
      Holder : Client_Holder := To_Holder (Socket, Environment, Kind);
   begin
      Server.Register
        (Client_ID,
         Holder,
         Init_Data,
         Init_Content_Type,
         Close_Duplicate);
   end Register;

   procedure Register
     (Server          : in out Object;
      Client_ID       : in     Client_Key;
      Socket          : in     Net.Socket_Type'Class;
      Environment     : in     Client_Environment;
      Kind            : in     Mode               := Plain;
      Close_Duplicate : in     Boolean := False)
   is
      Holder : Client_Holder := To_Holder (Socket, Environment, Kind);
   begin
      Server.Register (Client_ID, Holder, Close_Duplicate);
   end Register;

   -------------
   -- Restart --
   -------------

   procedure Restart (Server : in out Object) is
   begin
      Server.Restart;
   end Restart;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "")
   is
      Gone : Table.Map;
   begin
      Server.Send (Data, Content_Type, Gone);
      Table.Clear (Gone);
   end Send;

   ------------
   -- Send_G --
   ------------

   procedure Send_G
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "")
   is
      Cursor : Table.Cursor;
      Gone   : Table.Map;
   begin
      Server.Send (Data, Content_Type, Gone);

      Cursor := Table.First (Gone);

      while Table.Has_Element (Cursor) loop
         Client_Gone (Table.Containers.Key (Cursor));
         Table.Containers.Next (Cursor);
      end loop;

      Table.Clear (Gone);
   end Send_G;

   -------------
   -- Send_To --
   -------------

   procedure Send_To
     (Server       : in out Object;
      Client_ID    : in     Client_Key;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "") is
   begin
      Server.Send_To (Client_ID, Data, Content_Type);
   end Send_To;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Server        : in out Object;
      Close_Sockets : in     Boolean := True) is
   begin
      Server.Shutdown (Close_Sockets => Close_Sockets);
   end Shutdown;

   procedure Shutdown
     (Server             : in out Object;
      Final_Data         : in     Client_Output_Type;
      Final_Content_Type : in     String             := "") is
   begin
      Server.Shutdown (Final_Data, Final_Content_Type);
   end Shutdown;

   -----------------------
   -- Shutdown_If_Empty --
   -----------------------

   procedure Shutdown_If_Empty (Server : in out Object; Open : out Boolean) is
   begin
      Server.Shutdown_If_Empty (Open);
   end Shutdown_If_Empty;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder
     (Socket      : in Net.Socket_Type'Class;
      Environment : in Client_Environment;
      Kind        : in Mode)
      return Client_Holder is
   begin
      return (Kind        => Kind,
              Environment => Environment,
              Stream      => To_Stream (Socket));
   end To_Holder;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Server       : in out Object;
      Client_ID    : in     Client_Key;
      Close_Socket : in     Boolean    := True) is
   begin
      Server.Unregister (Client_ID, Close_Socket);
   end Unregister;

   ------------------------
   -- Unregister_Clients --
   ------------------------

   procedure Unregister_Clients
     (Server        : in out Object;
      Close_Sockets : in     Boolean := True) is
   begin
      Server.Unregister_Clients (Close_Sockets => Close_Sockets);
   end Unregister_Clients;

end AWS.Server.Push;
