------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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
with System;
with GNAT.Calendar.Time_IO;

with AWS.Messages;
with AWS.MIME;
with AWS.Utils;

package body AWS.Server.Push is

   use AWS.Net;

   function To_Holder
     (Socket      : in Socket_Type;
      Environment : in Client_Environment;
      Kind        : in Mode)
     return Client_Holder;

   function To_Stream (Socket : Socket_Type) return Stream_Access
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
         return Table.Size (Container);
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
        (Client_ID       : in Client_Key;
         Holder          : in Client_Holder;
         Close_Duplicate : in Boolean)
      is
         Duplicate : Boolean;
      begin

         if not Open then
            raise Closed;
         end if;

         Table.Insert (Container, Client_ID, Holder, Duplicate);

         if Duplicate then
            if Close_Duplicate then
               Unregister (Client_ID, True);
               Table.Insert (Container, Client_ID, Holder);
            else
               raise Duplicate_Client_ID;
            end if;
         end if;

         begin

            String'Write (Holder.Stream,
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
                  Messages.Content_Type (MIME.Multipart_Mixed_Replace,
                     Boundary)
                  & New_Line);

            else
               String'Write (Holder.Stream, New_Line);
            end if;

            Stream_IO.Flush (Holder.Stream);

         exception
         when others =>
            Unregister (Client_ID, Close_Socket => False);
            raise;
         end;

      end Register;

      procedure Register
        (Client_ID         : in Client_Key;
         Holder            : in Client_Holder;
         Init_Data         : in Client_Output_Type;
         Init_Content_Type : in String;
         Close_Duplicate   : in Boolean) is
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
         Unregistered : in out Table.Table_Type)
      is

         procedure Action
           (Key          : in     Client_Key;
            Value        : in     Client_Holder;
            Order_Number : in     Positive;
            Continue     : in out Boolean);

         procedure Free
           (Key          : in     Client_Key;
            Value        : in     Client_Holder;
            Order_Number : in     Positive;
            Continue     : in out Boolean);

         ------------
         -- Action --
         ------------

         procedure Action
           (Key          : in     Client_Key;
            Value        : in     Client_Holder;
            Order_Number : in     Positive;
            Continue     : in out Boolean)
         is
            pragma Unreferenced (Order_Number);
            pragma Unreferenced (Continue);
         begin
            Send_Data (Value, Data, Content_Type);
         exception
            when others =>
               Table.Insert (Unregistered, Key, Value);
         end Action;

         ----------
         -- Free --
         ----------

         procedure Free
           (Key          : in     Client_Key;
            Value        : in     Client_Holder;
            Order_Number : in     Positive;
            Continue     : in out Boolean)
         is
            pragma Unreferenced (Value);
            pragma Unreferenced (Order_Number);
            pragma Unreferenced (Continue);
         begin
            Unregister (Key, True);
         end Free;

         procedure For_Each is new Table.Disorder_Traverse_G (Action);

         procedure Remove_Each is new Table.Disorder_Traverse_G (Free);

      begin
         For_Each (Container);
         Remove_Each (Unregistered);
      end Send;

      ---------------
      -- Send_Data --
      ---------------

      procedure Send_Data
        (Holder       : in Client_Holder;
         Data         : in Client_Output_Type;
         Content_Type : in String)
      is

         Data_To_Send : constant Stream_Output_Type :=
           To_Stream_Output (Data, Holder.Environment);

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

         Stream_IO.Flush (Holder.Stream);
      end Send_Data;

      -------------
      -- Send_To --
      -------------

      procedure Send_To
        (Client_ID    : in Client_Key;
         Data         : in Client_Output_Type;
         Content_Type : in String)
      is
         Holder : Client_Holder;
         use Sockets;
      begin
         Holder := Table.Value (Container, Client_ID);
         Send_Data (Holder, Data, Content_Type);
      exception
         when Table.Missing_Item_Error =>
            raise Client_Gone;
         when others =>
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
         Gone : Table.Table_Type;
      begin
         Send (Final_Data, Final_Content_Type, Gone);
         Table.Destroy (Gone);
         Shutdown (Close_Sockets => True);
      end Shutdown;

      -----------------------
      -- Shutdown_If_Empty --
      -----------------------

      procedure Shutdown_If_Empty (Open : out Boolean) is
      begin
         if Table.Size (Container) = 0 then
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
         Value : Client_Holder;
      begin
         Table.Remove (Container, Client_ID, Value);
         if Close_Socket then
            Stream_IO.Shutdown (Value.Stream);
         end if;
         Stream_IO.Free (Value.Stream);
      exception
         when Table.Missing_Item_Error =>
            null;
      end Unregister;

      ------------------------
      -- Unregister_Clients --
      ------------------------

      procedure Unregister_Clients (Close_Sockets : in Boolean) is
      begin
         while Table.Size (Container) > 0 loop
            Unregister (Table.Min_Key (Container), Close_Sockets);
         end loop;
      end Unregister_Clients;

   end Object;

   --------------
   -- Register --
   --------------

   procedure Register
     (Server            : in out Object;
      Client_ID         : in     Client_Key;
      Socket            : in     Socket_Type;
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
   exception
   when Closed | Duplicate_Client_ID =>
      Stream_IO.Free (Holder.Stream);
      raise;
   end Register;

   procedure Register
     (Server          : in out Object;
      Client_ID       : in     Client_Key;
      Socket          : in     Socket_Type;
      Environment     : in     Client_Environment;
      Kind            : in     Mode               := Plain;
      Close_Duplicate : in     Boolean := False)
   is
      Holder : Client_Holder := To_Holder (Socket, Environment, Kind);
   begin
      Server.Register
        (Client_ID,
         Holder, Close_Duplicate);
   exception
   when Closed | Duplicate_Client_ID =>
      Stream_IO.Free (Holder.Stream);
      raise;
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
      Gone : Table.Table_Type;
   begin
      Server.Send (Data, Content_Type, Gone);
      Table.Destroy (Gone);
   end Send;

   ------------
   -- Send_G --
   ------------

   procedure Send_G
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "")
   is
      procedure Action
        (Key          : in     Client_Key;
         Value        : in     Client_Holder;
         Order_Number : in     Positive;
         Continue     : in out Boolean);

      Gone : Table.Table_Type;

      ------------
      -- Action --
      ------------

      procedure Action
        (Key          : in     Client_Key;
         Value        : in     Client_Holder;
         Order_Number : in     Positive;
         Continue     : in out Boolean)
      is
         pragma Unreferenced (Value);
         pragma Unreferenced (Order_Number);
         pragma Unreferenced (Continue);
      begin
         Client_Gone (Key);
      end Action;

      procedure For_Each is new Table.Disorder_Traverse_G (Action);

   begin
      Server.Send (Data, Content_Type, Gone);
      For_Each (Gone);
      Table.Destroy (Gone);
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
     (Socket      : in Socket_Type;
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

