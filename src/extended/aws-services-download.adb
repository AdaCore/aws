------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Vectors;

with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.Generic_Sets;
with AWS.Parameters;
with AWS.Services.Dispatchers.URI;
with AWS.Templates;
with AWS.Utils;

package body AWS.Services.Download is

   use Ada;
   use Ada.Containers;
   use Ada.Exceptions;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   URI_Prefix : constant String := "$dm_prefix$";
   --  The URI prefix used for all the download manager request

   W_Tmplt    : constant String := "aws_download_manager_waiting.thtml";
   S_Tmplt    : constant String := "aws_download_manager_start.thtml";

   type Waiting_Position is new Natural;
   --  The position in the waiting line, 0 means that the download can be
   --  started. 1 means in the first position in the waiting line, the download
   --  will start as soon as a download terminates.

   type Download_Information is record
      URI        : Unbounded_String; -- download manager unique key URI
      Name       : Unbounded_String; -- the resource name (filename)
      R_URI      : Unbounded_String; -- the resource URI
      Started    : Boolean;          -- True if download can start
      Header     : Boolean;          -- True if HTTP header sent
      Stream     : Resources.Streams.Stream_Access; -- data stream (input)
      Socket     : Net.Socket_Access;               -- client socket (output)
      Position   : Waiting_Position; -- position in the waiting line
      Index      : Positive;         -- item vector index (for fast update)
      Time_Stamp : Calendar.Time;    -- when the download was created
   end record;

   No_Information : constant Download_Information :=
                      (Null_Unbounded_String, Null_Unbounded_String,
                       Null_Unbounded_String, False, False, null, null,
                       0, 1, Calendar.Clock);

   package Download_Vectors is
     new Containers.Vectors (Positive, Download_Information);
   use Download_Vectors;

   --  The task that handles the downloads

   task type Download_Manager with Priority => Config.Service_Priority is
   end Download_Manager;

   type Download_Manager_Access is access Download_Manager;

   DM : Download_Manager_Access;
   --  The download manager created in the start routine

   --  The socket set (multiplexer)

   Max_Concurrent_Download : Positive;

   package Sock_Set is new Net.Generic_Sets (Download_Information);

   --  Data manager

   protected Data_Manager is

      procedure Insert (Download : Download_Information);
      --  Add a new download data information

      procedure Update (Download : Download_Information);
      --  Update the download information (Download.URI is the unique key)

      procedure Remove (Download : Download_Information);
      --  Remove the download information, this is either because the download
      --  is terminated or an error occurs during the download (interrupted by
      --  the user).

      entry Ready;
      --  Returns if there is at least one download to do

      procedure Create_Set (Socket_Set : in out Sock_Set.Socket_Set_Type);
      --  Returns in Socket_Set the socket to look at for output availability

      procedure Get (URI : String; Download : out Download_Information);
      --  Returns the Download_Information for the given URI or No_Information
      --  if this URI is not part of the download data. Note that this routine
      --  also set the Index and Position fields according to the position in
      --  the vector.

      procedure Get_UID (UID : out Positive);
      --  Returns a unique ID

      procedure Set_Shutdown;
      --  Download manager shutdown requested

      function Shutdown return Boolean;
      --  Returns the shutdown status

      procedure Release;
      --  Release memory associated with the data manager

   private

      procedure Check_Queue;
      --  Check all sockets in the queue and set Socket_Present if sockets are
      --  present so waiting for data.

      Count     : Natural := 0;
      --  Set to true when there is nothing to do

      Downloads : Vector;

      function Index (Download : Download_Information) return Positive;
      --  Returns the updated index for Download, 0 if not found

      UID       : Natural := 0;

      Closing        : Boolean := False;
      Socket_Present : Boolean := False;

   end Data_Manager;

   ----------------
   -- DM_Handler --
   ----------------

   DM_Handler : Dispatchers.URI.Handler;

   function CB (Request : Status.Data) return Response.Data;
   --  Download manager callback handler

   -----------
   -- Build --
   -----------

   function Build
     (Request  : Status.Data;
      Name     : String;
      Resource : not null access Resources.Streams.Stream_Type'Class)
      return Response.Data
   is
      UID : Positive;
   begin
      Data_Manager.Get_UID (UID);

      declare
         Key_URI : constant String :=
                     Status.URI (Request) & "$" & Utils.Image (UID);
      begin
         Data_Manager.Insert
           (Download =>
              (To_Unbounded_String (Key_URI), To_Unbounded_String (Name),
               To_Unbounded_String (Status.URI (Request)),
               False, False,
               Resources.Streams.Stream_Access (Resource), null,
               0, 1, Calendar.Clock));

         return Response.URL
           ("/" & URI_Prefix & "?RES_URI=" & Key_URI,
            Cache_Control => Messages.No_Cache);
      end;
   end Build;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      P_List : constant Parameters.List := Status.Parameters (Request);
      URI    : constant String := Parameters.Get (P_List, "RES_URI");
      Info   : Download_Information;
   begin
      Data_Manager.Get (URI, Info);

      if Info = No_Information then
         --  This should not happen, guard against bad URL (reload after
         --  interrupted download).
         return Response.Acknowledge
           (Messages.S404, "<p>Download manager page not registered!");
      end if;

      if Info.Position = 0 then
         --  Download can be started now

         if Info.Started then
            --  Let's go now, set the client side socket
            Info.Socket := new Net.Socket_Type'Class'(Status.Socket (Request));
            Data_Manager.Update (Info);

            return Response.Socket_Taken;

         else
            --  Display the start page before launching the download
            Info.Started := True;
            Data_Manager.Update (Info);

            return Response.Build
              (MIME.Text_HTML,
               String'(Templates.Parse
                 (S_Tmplt,
                    (1 => Templates.Assoc ("NAME", To_String (Info.Name)),
                     2 => Templates.Assoc
                       ("RES_URI", To_String (Info.R_URI))))));
         end if;

      else
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse
              (W_Tmplt,
                 (1 => Templates.Assoc ("NAME", To_String (Info.Name)),
                  2 => Templates.Assoc ("RES_URI", To_String (Info.R_URI)),
                  3 => Templates.Assoc
                    ("POSITION", Positive (Info.Position))))));
      end if;
   end CB;

   ----------
   -- Data --
   ----------

   protected body Data_Manager is

      -----------------
      -- Check_Queue --
      -----------------

      procedure Check_Queue is
         use type Net.Socket_Access;
      begin
         Socket_Present := False;

         Check_Socket_Present : for K in 1 .. Natural (Downloads.Length) loop
            if Downloads.Element (K).Socket /= null then
               Socket_Present := True;
               exit Check_Socket_Present;
            end if;
         end loop Check_Socket_Present;
      end Check_Queue;

      ----------------
      -- Create_Set --
      ----------------

      procedure Create_Set (Socket_Set : in out Sock_Set.Socket_Set_Type) is
         use type Net.Socket_Access;
         Info : Download_Information;
         N    : Positive;
      begin
         N := Positive'Min
           (Max_Concurrent_Download, Positive (Downloads.Length));

         for K in 1 .. N loop
            Info := Downloads.Element (K);

            if Info.Socket /= null then
               Sock_Set.Add (Socket_Set, Info.Socket, Sock_Set.Output);
               Sock_Set.Set_Data (Socket_Set, Sock_Set.Socket_Count (K), Info);
            end if;
         end loop;
      end Create_Set;

      ---------
      -- Get --
      ---------

      procedure Get (URI : String; Download : out Download_Information) is
         use type Calendar.Time;
         Info  : Download_Information;
         Index : Natural := 0;
      begin
         --  First remove old entries which have not been checked for at least
         --  15 seconds.

         Remove_Old_Entries : while not Downloads.Is_Empty loop
            Info := Downloads.First_Element;
            if Calendar.Clock - Info.Time_Stamp > 15.0 then
               Downloads.Delete_First;
               Count := Count - 1;
            else
               exit Remove_Old_Entries;
            end if;
         end loop Remove_Old_Entries;

         --  Look for the given URI in the vector

         for K in 1 .. Natural (Downloads.Length) loop
            Info := Downloads.Element (K);
            if URI = To_String (Info.URI) then
               Index := K;
               exit;
            end if;
         end loop;

         if Index = 0 then
            --  Not found
            Info := No_Information;

         else
            Info.Index := Index;
            --  As this download was checked, update time-stamp
            Info.Time_Stamp := Calendar.Clock;

            Downloads.Replace_Element (Index, Info);

            if Index <= Max_Concurrent_Download then
               Info.Position := 0;
            else
               Info.Position :=
                 Waiting_Position (Index - Max_Concurrent_Download);
            end if;
         end if;

         Download := Info;

         Check_Queue;
      end Get;

      -------------
      -- Get_UID --
      -------------

      procedure Get_UID (UID : out Positive) is
      begin
         Data_Manager.UID := Data_Manager.UID + 1;
         UID := Data_Manager.UID;
      end Get_UID;

      -----------
      -- Index --
      -----------

      function Index (Download : Download_Information) return Positive is
      begin
         --  Use Download.Index for fast lookup, this was the original position
         --  for this item. The new position is either at the same index or in
         --  a lower position in case some downloads have endded since we got
         --  this item.
         for K in reverse 1 .. Download.Index loop
            if Download.URI = Downloads.Element (K).URI then
               return K;
            end if;
         end loop;

         raise Constraint_Error;
      end Index;

      ------------
      -- Insert --
      ------------

      procedure Insert (Download : Download_Information) is
      begin
         Downloads.Append (Download);
         Count := Count + 1;
      end Insert;

      -----------
      -- Ready --
      -----------

      entry Ready when Socket_Present or else Closing is
      begin
         null;
      end Ready;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Downloads.Clear;
         Socket_Present := False;
         Count := 0;
      end Release;

      ------------
      -- Remove --
      ------------

      procedure Remove (Download : Download_Information) is
      begin
         Downloads.Delete (Index (Download));
         Count := Count - 1;
         Check_Queue;
      end Remove;

      ------------------
      -- Set_Shutdown --
      ------------------

      procedure Set_Shutdown is
      begin
         Closing := True;
      end Set_Shutdown;

      --------------
      -- Shutdown --
      --------------

      function Shutdown return Boolean is
      begin
         return Closing;
      end Shutdown;

      ------------
      -- Update --
      ------------

      procedure Update (Download : Download_Information) is
         use type Net.Socket_Access;
      begin
         --  Set Socket_Ready status if a socket is available

         if Download.Socket /= null then
            Socket_Present := True;
         end if;

         Downloads.Replace_Element (Index (Download), Download);
      end Update;

   end Data_Manager;

   ----------------------
   -- Download_Manager --
   ----------------------

   task body Download_Manager is

      procedure Send_Header
        (Socket_Set : in out Sock_Set.Socket_Set_Type;
         N          : Sock_Set.Socket_Count;
         Info       : in out Download_Information);
      --  Send HTTP headers

      procedure Send_Data
        (Info : Download_Information;
         Done : out Boolean);
      --  Send some data for Info. Done is set to true if the download is
      --  completed.

      ---------------
      -- Send_Data --
      ---------------

      procedure Send_Data
        (Info : Download_Information;
         Done : out Boolean)
      is
         Buffer_Size : constant := 4 * 1_024;

         Buffer : Streams.Stream_Element_Array (1 .. Buffer_Size);
         Last   : Streams.Stream_Element_Offset;
      begin
         Resources.Streams.Read (Info.Stream.all, Buffer, Last);

         Done := Last < Buffer'First;

         if not Done then
            Net.Buffered.Write (Info.Socket.all, Buffer (1 .. Last));
         end if;
      end Send_Data;

      -----------------
      -- Send_Header --
      -----------------

      procedure Send_Header
        (Socket_Set : in out Sock_Set.Socket_Set_Type;
         N          : Sock_Set.Socket_Count;
         Info       : in out Download_Information)
      is
         pragma Unreferenced (Socket_Set, N);
         Sock : constant Net.Socket_Type'Class := Info.Socket.all;
      begin
         Info.Header := True;
         Data_Manager.Update (Info);

         Net.Buffered.Put_Line (Sock, Messages.Status_Line (Messages.S200));

         Net.Buffered.Put_Line
           (Sock,
            "Date: " & Messages.To_HTTP_Date (Utils.GMT_Clock));

         --  Server

         Net.Buffered.Put_Line
           (Sock,
            "Server: AWS (Ada Web Server) v" & Version);

         Net.Buffered.Put_Line (Sock, Messages.Connection ("close"));

         Net.Buffered.Put_Line
           (Sock, Messages.Content_Length
                    (Resources.Streams.Size (Info.Stream.all)));

         Net.Buffered.Put_Line
           (Sock, Messages.Content_Type (MIME.Application_Octet_Stream));

         Net.Buffered.Put_Line
           (Sock, Messages.Content_Disposition
              ("attachment", To_String (Info.Name), To_String (Info.Name)));

         Net.Buffered.New_Line (Sock);
         Net.Buffered.Flush (Sock);
      end Send_Header;

      Socket_Set : Sock_Set.Socket_Set_Type;
      Count      : Sock_Set.Socket_Count;
      Done       : Boolean;

   begin
      Main : loop
         Data_Manager.Ready;

         exit Main when Data_Manager.Shutdown;

         --  Some data are ready to be sent

         Data_Manager.Create_Set (Socket_Set);

         Sock_Set.Wait (Socket_Set, Net.Forever, Count);

         --  For all write ready socket, send some data

         for K in 1 .. Sock_Set.Count (Socket_Set) loop
            Done := False;

            declare
               Info : Download_Information :=
                        Sock_Set.Get_Data (Socket_Set, K);
            begin
               if Sock_Set.Is_Write_Ready (Socket_Set, K) then
                  begin
                     if not Info.Header then
                        Send_Header (Socket_Set, K, Info);
                     end if;

                     Send_Data (Info, Done);

                     if Done then
                        Net.Buffered.Flush (Info.Socket.all);
                        Net.Shutdown (Info.Socket.all);
                     end if;
                  exception
                     when Net.Socket_Error =>
                        Done := True;
                  end;

                  if Done then
                     Resources.Streams.Close (Info.Stream.all);
                  end if;

               elsif Sock_Set.Is_Error (Socket_Set, K) then
                  Done := True;
               end if;

               if Done then
                  --  Remove this socket from the server
                  Data_Manager.Remove (Info);
               end if;
            end;
         end loop;

         Sock_Set.Reset (Socket_Set);
      end loop Main;
   exception
      when E : others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Download manager bug detected: " & Exception_Information (E));
   end Download_Manager;

   -----------
   -- Start --
   -----------

   procedure Start
     (Server_Dispatcher       : AWS.Dispatchers.Handler'Class;
      Main_Dispatcher         : out Services.Dispatchers.Linker.Handler;
      Max_Concurrent_Download : Positive := Config.Max_Concurrent_Download) is
   begin
      Download.Max_Concurrent_Download := Max_Concurrent_Download;

      --  Set the dispatcher

      Dispatchers.URI.Register (DM_Handler, "/" & URI_Prefix, CB'Access, True);

      Dispatchers.Linker.Register
        (Main_Dispatcher, Server_Dispatcher, DM_Handler);

      --  Start download manager task
      DM := new Download_Manager;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Download_Manager, Download_Manager_Access);
   begin
      Dispatchers.URI.Unregister (DM_Handler, "/" & URI_Prefix);
      Data_Manager.Set_Shutdown;

      while not DM'Terminated loop
         delay 0.1;
      end loop;

      Unchecked_Free (DM);

      Data_Manager.Release;
   end Stop;

end AWS.Services.Download;
