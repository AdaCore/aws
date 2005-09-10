------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                ACT-Europe                                --
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

with Ada.Streams;

package body AWS.Net.Acceptors is

   Server_Index : constant := 1;
   Signal_Index : constant := 2;
   First_Index  : constant := 3;

   Shutdown_Command : constant := 0;
   Socket_Command   : constant := 1;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Acceptor : in out Acceptor_Type) is
   begin
      Std.Free (Acceptor.Server);
      Std.Free (Acceptor.R_Signal);
      Std.Free (Acceptor.W_Signal);
   end Finalize;

   ---------
   -- Get --
   ---------

   procedure Get
     (Acceptor : in out Acceptor_Type;
      Socket   :    out Socket_Access)
   is
      use type Sets.Socket_Count;
      Ready, Error : Boolean;
   begin
      if Sets.Count (Acceptor.Set) = 0 then
         --  After shutdown of the server socket.
         raise Socket_Error;
      end if;

      loop
         Read_Ready : loop
            exit Read_Ready when Acceptor.Index > Acceptor.Last;

            Sets.Is_Read_Ready (Acceptor.Set, Acceptor.Index, Ready, Error);

            if Error or else Ready then
               Sets.Remove_Socket (Acceptor.Set, Acceptor.Index, Socket);

               Acceptor.Last := Acceptor.Last - 1;

               if Error then
                  Shutdown (Socket.all);
                  Free (Socket);

               elsif Ready then
                  return;
               else
                  raise Program_Error;
               end if;

            else
               Acceptor.Index := Acceptor.Index + 1;
            end if;
         end loop Read_Ready;

         Sets.Wait (Acceptor.Set, Forever);

         Acceptor.Last := Sets.Count (Acceptor.Set);

         Sets.Is_Read_Ready (Acceptor.Set, Server_Index, Ready, Error);

         if Error then
            raise Socket_Error;

         elsif Ready then
            declare
               New_Socket : Std.Socket_Type;
            begin
               --  We could not accept SSL socket because SSL handshake could
               --  take a long time inside Accept_Socket. We would make socket
               --  SSL later outside acceptor if necessary.

               Std.Accept_Socket (Acceptor.Server, New_Socket);
               Sets.Add (Acceptor.Set, New_Socket, Sets.Input);
            end;
         end if;

         if Sets.Is_Read_Ready (Acceptor.Set, Signal_Index) then
            declare
               Socket : Socket_Access;
               Bytes  : Ada.Streams.Stream_Element_Array (1 .. 16);
               Last   : Ada.Streams.Stream_Element_Offset;
            begin
               --  Read bytes from signalling socket and take sockets from
               --  mailbox.

               Std.Receive (Acceptor.R_Signal, Bytes, Last);

               for J in 1 .. Last loop
                  case Bytes (J) is
                     when Socket_Command =>
                        Acceptor.Box.Get (Socket);
                        Sets.Add (Acceptor.Set, Socket, Sets.Input);

                     when Shutdown_Command =>
                        Std.Shutdown (Acceptor.Server);
                        Std.Shutdown (Acceptor.R_Signal);
                        Std.Shutdown (Acceptor.W_Signal);

                        --  Remove R_Signal and Server sockets from socket set
                        --  Remove signal first because
                        --  Signal_Index > Server_Index

                        Sets.Remove_Socket (Acceptor.Set, Signal_Index);
                        Sets.Remove_Socket (Acceptor.Set, Server_Index);

                        while Sets.Count (Acceptor.Set) > 0 loop
                           Sets.Remove_Socket (Acceptor.Set, 1, Socket);
                           Shutdown (Socket.all);

                           --  We could free other sockets, becuse it is not
                           --  using anywhere else whan it is in socket set

                           Free (Socket);
                        end loop;

                        raise Socket_Error;

                     when others => raise Constraint_Error;
                  end case;
               end loop;
            end;
         end if;

         Acceptor.Index := First_Index;
      end loop;
   end Get;

   ---------------
   -- Give_Back --
   ---------------

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket   : in     Socket_Access) is
   begin
      Send (Acceptor.W_Signal, (1 => Socket_Command));
      Acceptor.Box.Add (Socket);
   end Give_Back;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Acceptor   : in out Acceptor_Type;
      Host       : in     String;
      Port       : in     Positive;
      Queue_Size : in     Positive)
   is
      use type Sets.Socket_Count;
   begin
      Std.Bind (Acceptor.Server, Host => Host, Port => Port);
      Std.Listen (Acceptor.Server, Queue_Size => Queue_Size);

      Std.Socket_Pair (Acceptor.W_Signal, Acceptor.R_Signal);

      Sets.Reset (Acceptor.Set);
      Sets.Add (Acceptor.Set, Acceptor.Server, Sets.Input);
      Sets.Add (Acceptor.Set, Acceptor.R_Signal, Sets.Input);

      Acceptor.Index := First_Index;
      Acceptor.Last  := Sets.Count (Acceptor.Set);
   end Listen;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Acceptor : in out Acceptor_Type) is
   begin
      Send (Acceptor.W_Signal, (1 => Shutdown_Command));
   end Shutdown;

end AWS.Net.Acceptors;
