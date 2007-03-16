------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2006                          --
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

with Ada.Calendar;
with Ada.Unchecked_Deallocation;

with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Utils;

with GNAT.Calendar.Time_IO;
with System;

package body AWS.Server.Push is

   use AWS.Net;

   function To_Holder
     (Socket      : in Net.Socket_Type'Class;
      Environment : in Client_Environment;
      Kind        : in Mode;
      Groups      : in Group_Set) return Client_Holder_Access;

   procedure Free (Holder : in out Client_Holder_Access);

   procedure Add_To_Groups
     (Groups     : in out Group_Maps.Map;
      Group_Name : in     String;
      Client_Id  : in     String;
      Holder     : in     Client_Holder_Access);

   New_Line : constant String := ASCII.CR & ASCII.LF;
   --  HTTP new line.

   Boundary : constant String := "--AWS.Push.Boundary_"
     & GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%s")
     & New_Line;
   --  This is the multi-part boundary string used by AWS push server.

   -------------------
   -- Add_To_Groups --
   -------------------

   procedure Add_To_Groups
     (Groups     : in out Group_Maps.Map;
      Group_Name : in     String;
      Client_Id  : in     String;
      Holder     : in     Client_Holder_Access)
   is
      Cursor : constant Group_Maps.Cursor := Groups.Find (Group_Name);
      Map    : Map_Access;
   begin
      if Group_Maps.Has_Element (Cursor) then
         Map := Group_Maps.Element (Cursor);
      else
         Map := new Tables.Map;
         Groups.Insert (Group_Name, Map);
      end if;

      Map.Insert (Client_Id, Holder);
   end Add_To_Groups;

   -----------
   -- Count --
   -----------

   function Count (Server : in Object) return Natural is
   begin
      return Server.Count;
   end Count;

   ----------
   -- Free --
   ----------

   procedure Free (Holder : in out Client_Holder_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Client_Holder, Client_Holder_Access);
   begin
      Net.Free (Holder.Socket);
      Deallocate (Holder);
   end Free;

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
         return Natural (Container.Length);
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
        (Client_Id       : in     Client_Key;
         Holder          : in out Client_Holder_Access;
         Duplicated_Age  : in     Duration)
      is
         use Ada.Calendar;

         Cursor  : Tables.Cursor;
         Success : Boolean;

         procedure Add_To_Groups (J : Group_Vectors.Cursor);

         procedure Add_To_Groups (J : Group_Vectors.Cursor) is
         begin
            Add_To_Groups
              (Groups, Group_Vectors.Element (J), Client_Id, Holder);
         end Add_To_Groups;

      begin
         if not Open then
            Free (Holder);
            raise Closed;
         end if;

         Container.Insert (Client_Id, Holder, Cursor, Success);

         if not Success then
            if Duplicated_Age < Clock - Tables.Element (Cursor).Created then
               Unregister (Client_Id, Close_Socket => True);
               Container.Insert (Client_Id, Holder);
            else
               Free (Holder);
               raise Duplicate_Client_Id;
            end if;
         end if;

         Holder.Groups.Iterate (Add_To_Groups'Access);

         begin
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               "HTTP/1.1 200 OK" & New_Line
                 & "Server: AWS (Ada Web Server) v" & Version & New_Line
                 & Messages.Connection ("Close"));

            if Holder.Kind = Chunked then
               Net.Buffered.Put_Line
                 (Holder.Socket.all,
                  Messages.Transfer_Encoding ("chunked") & New_Line);

            elsif Holder.Kind = Multipart then
               Net.Buffered.Put_Line
                 (Holder.Socket.all,
                  Messages.Content_Type
                    (MIME.Multipart_X_Mixed_Replace, Boundary));

            else
               Net.Buffered.New_Line (Holder.Socket.all);
            end if;

            Net.Buffered.Flush (Holder.Socket.all);

            Socket_Taken (True);
         exception
            when others =>
               Unregister (Client_Id, Close_Socket => False);
               raise;
         end;
      end Register;

      procedure Register
        (Client_Id         : in     Client_Key;
         Holder            : in out Client_Holder_Access;
         Init_Data         : in     Client_Output_Type;
         Init_Content_Type : in     String;
         Duplicated_Age    : in     Duration) is
      begin
         Register (Client_Id, Holder, Duplicated_Age);

         begin
            Send_Data (Holder.all, Init_Data, Init_Content_Type);
         exception
            when others =>
               Unregister (Client_Id, Close_Socket => False);
               Socket_Taken (False);
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
         Group_Id     : in     String;
         Content_Type : in     String;
         Unregistered : in out Tables.Map)
      is
         Cursor : Tables.Cursor;
      begin
         if Group_Id = "" then
            Cursor := Container.First;
         else
            declare
               use Group_Maps;
               C : constant Group_Maps.Cursor := Groups.Find (Group_Id);
            begin
               if not Has_Element (C) then
                  return;
               end if;

               Cursor := Element (C).First;
            end;
         end if;

         while Tables.Has_Element (Cursor) loop
            declare
               Holder : constant Client_Holder_Access
                 := Tables.Element (Cursor);
            begin
               Send_Data (Holder.all, Data, Content_Type);

               Tables.Next (Cursor);
            exception
               when Net.Socket_Error =>
                  declare
                     Key : constant Client_Key := Tables.Key (Cursor);
                  begin
                     Unregistered.Insert (Key, Holder);

                     --  We have to move cursor to the next position before
                     --  delete element from current position.

                     Tables.Next (Cursor);

                     Unregister (Key, True);
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
         Data_To_Send : constant Ada.Streams.Stream_Element_Array
           := To_Stream_Array (Data, Holder.Environment);

      begin
         if Holder.Kind = Multipart then
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               Boundary & Messages.Content_Type (Content_Type) & New_Line);

         elsif Holder.Kind = Chunked then
            Net.Buffered.Put_Line
              (Holder.Socket.all,
               Utils.Hex (Data_To_Send'Size / System.Storage_Unit));
         end if;

         Net.Buffered.Write (Holder.Socket.all, Data_To_Send);

         if Holder.Kind = Multipart then
            Net.Buffered.Put_Line (Holder.Socket.all, New_Line);

         elsif Holder.Kind = Chunked then
            Net.Buffered.New_Line (Holder.Socket.all);
         end if;

         Net.Buffered.Flush (Holder.Socket.all);
      end Send_Data;

      -------------
      -- Send_To --
      -------------

      procedure Send_To
        (Client_Id    : in Client_Key;
         Data         : in Client_Output_Type;
         Content_Type : in String)
      is
         Cursor : Tables.Cursor;
      begin
         Cursor := Container.Find (Client_Id);

         if Tables.Has_Element (Cursor) then
            Send_Data (Tables.Element (Cursor).all, Data, Content_Type);
         else
            Ada.Exceptions.Raise_Exception
              (Client_Gone'Identity, "No such client id.");
         end if;

      exception
         when E : Net.Socket_Error =>
            Unregister (Client_Id, True);

            Ada.Exceptions.Raise_Exception
              (Client_Gone'Identity, Ada.Exceptions.Exception_Message (E));
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
         Gone : Tables.Map;
      begin
         Send (Final_Data, "", Final_Content_Type, Gone);
         Gone.Clear;
         Shutdown (Close_Sockets => True);
      end Shutdown;

      -----------------------
      -- Shutdown_If_Empty --
      -----------------------

      procedure Shutdown_If_Empty (Open : out Boolean) is
         use type Ada.Containers.Count_Type;
      begin
         if Container.Length = 0 then
            Object.Open := False;
         end if;
         Shutdown_If_Empty.Open := Object.Open;
      end Shutdown_If_Empty;

      ---------------
      -- Subscribe --
      ---------------

      procedure Subscribe (Client_Id : in Client_Key; Group_Id : in String) is

         Cursor : constant Tables.Cursor := Container.Find (Client_Id);

         procedure Modify (Key     : in     String;
                           Element : in out Client_Holder_Access);

         procedure Modify (Key     : in     String;
                           Element : in out Client_Holder_Access)
         is
            pragma Unreferenced (Key);
         begin
            if not Element.Groups.Contains (Group_Id) then
               Element.Groups.Append (Group_Id);
               Add_To_Groups (Groups, Group_Id, Client_Id, Element);
            end if;
         end Modify;

      begin
         if Tables.Has_Element (Cursor) then
            Tables.Update_Element (Container, Cursor, Modify'Access);
         else
            Ada.Exceptions.Raise_Exception
              (Client_Gone'Identity, "No such client id.");
         end if;
      end Subscribe;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister
        (Client_Id : in Client_Key; Close_Socket : in Boolean)
      is
         Cursor : Tables.Cursor;
         Value  : Client_Holder_Access;

         procedure Delete_Group (J : Group_Vectors.Cursor);

         procedure Delete_Group (J : Group_Vectors.Cursor) is
            use type Ada.Containers.Count_Type;
            C   : Group_Maps.Cursor := Groups.Find (Group_Vectors.Element (J));
            Map : Map_Access := Group_Maps.Element (C);

            procedure Free is
              new Ada.Unchecked_Deallocation (Tables.Map, Map_Access);
         begin
            Tables.Delete (Map.all, Client_Id);

            if Map.Length = 0 then
               Groups.Delete (C);
               Free (Map);
            end if;
         end Delete_Group;

      begin
         Cursor := Container.Find (Client_Id);

         if Tables.Has_Element (Cursor) then
            Value := Tables.Element (Cursor);

            Value.Groups.Iterate (Delete_Group'Access);

            if Close_Socket then
               Net.Buffered.Shutdown (Value.Socket.all);
            end if;

            Free (Value);

            Container.Delete (Cursor);
         end if;
      end Unregister;

      ------------------------
      -- Unregister_Clients --
      ------------------------

      procedure Unregister_Clients (Close_Sockets : in Boolean) is
         Cursor : Tables.Cursor;
      begin
         loop
            Cursor := Container.First;

            exit when not Tables.Has_Element (Cursor);

            Unregister (Tables.Key (Cursor), Close_Sockets);
         end loop;
      end Unregister_Clients;

      -----------------
      -- Unsubscribe --
      -----------------

      procedure Unsubscribe
        (Client_Id : in Client_Key; Group_Id : in String)
      is
         Cursor : constant Tables.Cursor := Container.Find (Client_Id);

         procedure Modify (Key     : in     String;
                           Element : in out Client_Holder_Access);

         procedure Modify (Key     : in     String;
                           Element : in out Client_Holder_Access)
         is
            pragma Unreferenced (Key);
            Cursor : Group_Vectors.Cursor := Element.Groups.Find (Group_Id);
         begin
            if Group_Vectors.Has_Element (Cursor) then
               Element.Groups.Delete (Cursor);
               Tables.Delete (Groups.Element (Group_Id).all, Client_Id);
            end if;
         end Modify;

      begin
         if Tables.Has_Element (Cursor) then
            Container.Update_Element (Cursor, Modify'Access);
         else
            raise Client_Gone with "No such client id.";
         end if;
      end Unsubscribe;

   end Object;

   --------------
   -- Register --
   --------------

   procedure Register
     (Server            : in out Object;
      Client_Id         : in     Client_Key;
      Socket            : in     Net.Socket_Type'Class;
      Environment       : in     Client_Environment;
      Init_Data         : in     Client_Output_Type;
      Init_Content_Type : in     String             := "";
      Kind              : in     Mode               := Plain;
      Duplicated_Age    : in     Duration           := Duration'Last;
      Groups            : in     Group_Set          := Empty_Group)
   is
      Holder : Client_Holder_Access
        := To_Holder (Socket, Environment, Kind, Groups);
   begin
      Server.Register
        (Client_Id,
         Holder,
         Init_Data,
         Init_Content_Type,
         Duplicated_Age);
   end Register;

   procedure Register
     (Server          : in out Object;
      Client_Id       : in     Client_Key;
      Socket          : in     Net.Socket_Type'Class;
      Environment     : in     Client_Environment;
      Kind            : in     Mode               := Plain;
      Duplicated_Age  : in     Duration           := Duration'Last;
      Groups          : in     Group_Set          := Empty_Group)
   is
      Holder : Client_Holder_Access
        := To_Holder (Socket, Environment, Kind, Groups);
   begin
      Server.Register (Client_Id, Holder, Duplicated_Age);
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
      Group_Id     : in     String             := "";
      Content_Type : in     String             := "")
   is
      Gone : Tables.Map;
   begin
      Server.Send (Data, Group_Id, Content_Type, Gone);
      Gone.Clear;
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Client_Gone  : access procedure (Client_Id : in String);
      Group_Id     : in     String             := "";
      Content_Type : in     String             := "")
   is
      Cursor : Tables.Cursor;
      Gone   : Tables.Map;
   begin
      Server.Send (Data, Group_Id, Content_Type, Gone);

      Cursor := Gone.First;

      while Tables.Has_Element (Cursor) loop
         Client_Gone (Tables.Key (Cursor));
         Tables.Next (Cursor);
      end loop;

      Gone.Clear;
   end Send;

   ------------
   -- Send_G --
   ------------

   procedure Send_G
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Group_Id     : in     String             := "";
      Content_Type : in     String             := "")
   is
      Cursor : Tables.Cursor;
      Gone   : Tables.Map;
   begin
      Server.Send (Data, Group_Id, Content_Type, Gone);

      --  Cursor := Gone.First; would cause GNAT compiler hungup here, at least
      --  in version 5.04a1.

      Cursor := Tables.First (Gone);

      while Tables.Has_Element (Cursor) loop
         Client_Gone (Tables.Key (Cursor));
         Tables.Next (Cursor);
      end loop;

      Gone.Clear;
   end Send_G;

   -------------
   -- Send_To --
   -------------

   procedure Send_To
     (Server       : in out Object;
      Client_Id    : in     Client_Key;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "") is
   begin
      Server.Send_To (Client_Id, Data, Content_Type);
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
   -- Subscribe --
   ---------------

   procedure Subscribe
     (Server          : in out Object;
      Client_Id       : in     Client_Key;
      Group_Id        : in     String) is
   begin
      Server.Subscribe (Client_Id, Group_Id);
   end Subscribe;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder
     (Socket      : in Net.Socket_Type'Class;
      Environment : in Client_Environment;
      Kind        : in Mode;
      Groups      : in Group_Set) return Client_Holder_Access
   is
      Holder_Groups : Group_Vectors.Vector
        := Group_Vectors.To_Vector (Groups'Length);
   begin
      for J in Groups'Range loop
         Holder_Groups.Replace_Element (J, To_String (Groups (J)));
      end loop;

      return new Client_Holder'(Kind        => Kind,
                                Environment => Environment,
                                Created     => Ada.Calendar.Clock,
                                Socket      => new Socket_Type'Class'(Socket),
                                Groups      => Holder_Groups);
   end To_Holder;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Server       : in out Object;
      Client_Id    : in     Client_Key;
      Close_Socket : in     Boolean    := True) is
   begin
      Server.Unregister (Client_Id, Close_Socket);
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

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe
     (Server          : in out Object;
      Client_Id       : in     Client_Key;
      Group_Id        : in     String) is
   begin
      Server.Unsubscribe (Client_Id, Group_Id);
   end Unsubscribe;

end AWS.Server.Push;
