------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

--  This package provide an easy way to handle a configuration file for
--  AWS. Each line in the aws.ini has the following format:
--
--     <option name> <option value>
--
--  This package will initialize itself by parsing aws.ini, each option are
--  descripted below.
--
--  It is then possible to use AWS.Config to initialize the HTTP settings.

package AWS.Config is

   function Server_Name return String;
   --  Format: Server_Name <string>
   --  This is the name of the server as set by AWS.Server.Start.

   function Admin_URI return String;
   --  Format: Admin_URI <string>
   --  This is the name of the admin server page as set by AWS.Server.Start.

   function Server_Port return Positive;
   --  Format: Server_Port <positive>
   --  This is the server port as set by the HTTP object declaration.

   function Max_Connection return Positive;
   --  Format: Max_Connection <positive>
   --  This is the max simultaneous connections as set by the HTTP object
   --  declaration.

   function Log_File_Directory return String;
   --  Format: Log_File_Directory <string>
   --  This point to the directory where log files will be written. The
   --  directory returned will end with a directory separator.

   function Upload_Directory return String;
   --  Format: Upload_Directory <string>
   --  This point to the directory where uploaded files will be stored. The
   --  directory returned will end with a directory separator.

end AWS.Config;
