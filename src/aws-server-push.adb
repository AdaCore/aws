------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with Ada.Unchecked_Deallocation;
with Ada.Calendar;
with System;
with GNAT.Calendar.Time_IO;

with Sockets.Stream_IO;

with AWS.Messages;
with AWS.MIME;
with AWS.Utils;

package body AWS.Server.Push is

   function To_Holder
     (Socket      : in Socket_Type;
      Environment : in Client_Environment;
      Kind        : in Mode)
     return Client_Holder;

   procedure Close (Socket : in out Client_Holder);

   New_Line : constant String := ASCII.CR & ASCII.LF;
   --  HTTP new line.

   Boundary : constant String := "--AWS.Server.Push.Boundary_"
     & GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%s")
     & New_Line;
   --  This is the multi-part boundary string used by AWS push server.

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder
     (Socket      : in Socket_Type;
      Environment : in Client_Environment;
      Kind        : in Mode)
     return Client_Holder
   is
      use Sockets.Stream_IO;
      Stream : constant Stream_Access := new Socket_Stream_Type;
   begin
      Initialize (Socket_Stream_Type'Class (Stream.all),
                  Sockets.Socket_FD (Socket));

      return (Kind        => Kind,
              Environment => Environment,
              Socket      => new Socket_Type'(Socket),
              Stream      => Stream);
   end To_Holder;

   -----------
   -- Close --
   -----------

   procedure Close (Socket : in out Client_Holder) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Ada.Streams.Root_Stream_Type'Class, Stream_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Socket_Type, Socket_Access);

   begin
      Sockets.Shutdown (Socket.Socket.all);
      Free (Socket.Stream);
      Free (Socket.Socket);
   end Close;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "") is
   begin
      Server.Send (Data, Content_Type);
   end Send;

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

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Server    : in out Object;
      Client_ID : in     Client_Key) is
   begin
      Server.Unregister (Client_ID);
   end Unregister;

   --------------
   -- Register --
   --------------

   procedure Register
     (Server      : in out Object;
      Client_ID   : in     Client_Key;
      Socket      : in     Socket_Type;
      Environment : in     Client_Environment;
      Kind        : in     Mode               := Plain) is
   begin
      Server.Register (Client_ID,
                       To_Holder (Socket, Environment, Kind));
   end Register;

   -----------------
   -- Object --
   -----------------

   protected body Object is

      procedure Send_Data
        (Holder       : in Client_Holder;
         Data         : in Client_Output_Type;
         Content_Type : in String);
      --  Send Data to a client identified by Holder.

      ----------
      -- Send --
      ----------

      procedure Send
        (Data         : in Client_Output_Type;
         Content_Type : in String)
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

         For_Remove : Table.Table_Type;

         ------------
         -- Action --
         ------------

         procedure Action
           (Key          : in     Client_Key;
            Value        : in     Client_Holder;
            Order_Number : in     Positive;
            Continue     : in out Boolean) is
         begin
            Send_Data (Value, Data, Content_Type);
         exception
            when others =>
               Table.Insert (For_Remove, Key, Value);
         end Action;

         ----------
         -- Free --
         ----------

         procedure Free
           (Key          : in     Client_Key;
            Value        : in     Client_Holder;
            Order_Number : in     Positive;
            Continue     : in out Boolean) is
         begin
            Unregister (Key);
         end Free;

         procedure For_Each is new Table.Disorder_Traverse_G (Action);

         procedure Remove_Each is new Table.Disorder_Traverse_G (Free);

      begin
         For_Each (Container);
         Remove_Each (For_Remove);
         Table.Destroy (For_Remove);
      end Send;

      -------------
      -- Send_To --
      -------------

      procedure Send_To
        (Client_ID    : in Client_Key;
         Data         : in Client_Output_Type;
         Content_Type : in String)
      is
         Value : constant Client_Holder := Table.Value (Container, Client_ID);
      begin
         Send_Data (Value, Data, Content_Type);
      exception
         when others =>
            Unregister (Client_ID);
            raise Client_Gone;
      end Send_To;

      --------------
      -- Register --
      --------------

      procedure Register
        (Client_ID : in Client_Key;
         Holder    : in Client_Holder) is
      begin
         Table.Insert (Container, Client_ID, Holder);

         String'Write (Holder.Stream,
                       "HTTP/1.1 200 OK" & New_Line
                       & "Server: AWS (Ada Web Server) v" & Version & New_Line
                       & Messages.Connection ("Close") & New_Line);

         if Holder.Kind = Chunked then
            String'Write
              (Holder.Stream,
               Messages.Transfer_Encoding ("chunked") & New_Line & New_Line);

         elsif Holder.Kind = Multipart then
            String'Write
              (Holder.Stream,
               Messages.Content_Type (MIME.Multipart_Mixed_Replace, Boundary)
               & New_Line);
         end if;
      end Register;

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

      end Send_Data;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (Client_ID : in Client_Key) is
         Value : Client_Holder;
      begin
         Table.Remove (Container, Client_ID, Value);
         Close (Value);
      end Unregister;

   end Object;

end AWS.Server.Push;

