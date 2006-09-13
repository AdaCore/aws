------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2005-2006                          --
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

--  This is a download manager service, can be used to avoid polluting the main
--  server slot with long downloads. A single task is used in this
--  implementation.

with AWS.Config;
with AWS.Dispatchers;
with AWS.Resources.Streams;
with AWS.Response;
with AWS.Services.Dispatchers.Linker;
with AWS.Status;

package AWS.Services.Download is

   procedure Start
     (Server_Dispatcher       : in     AWS.Dispatchers.Handler'Class;
      Main_Dispatcher         :    out Services.Dispatchers.Linker.Handler;
      Max_Concurrent_Download : in     Positive :=
        Config.Max_Concurrent_Download);
   --  Start the download manager server. Server_Dispatcher is the dispatcher
   --  for the Web server. Main_Dispatcher is the dispatcher that must be used
   --  with the main server start routine. This dispatcher handles the standard
   --  web server resources and the download manager ones.
   --  Max_Concurrent_Download contains the number of simultaneous download
   --  that can be handled, request past this limit are queued. Note that a
   --  single task is used for this implementation. Using a download manager is
   --  useful to avoid the standard Web server to be busy with long downloads.

   procedure Stop;
   --  Stop the download server, all current download are interrupted

   function Build
     (Request  : in Status.Data;
      Name     : in String;
      Resource : not null access Resources.Streams.Stream_Type'Class)
      return Response.Data;
   --  Queue a download request. If there is room on the download manager the
   --  template page aws_download_manager_start.thtml is used to build the
   --  answer otherwise the template page aws_download_manager_waiting.thtml is
   --  used. Name is the resource name and will be the default name used on the
   --  user side to save the file on disk. Resource is a stream on which the
   --  data to be sent are read.
   --
   --  Templates tags description:
   --
   --  aws_download_manager_waiting.thtml
   --     NAME      the name of the resource as pass to build
   --     RES_URI   the resource URI unique to the download server
   --     POSITION  the position on the waiting queue
   --  aws_download_manager_start.thtml
   --     NAME      the name of the resource as pass to build
   --     RES_URI   the resource URI unique to the download server
   --
   --  Note that both template pages must contain a refresh meta-tag:
   --
   --     <meta http-equiv="refresh" content="2">

end AWS.Services.Download;
