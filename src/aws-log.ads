------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
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

--  This package handle the logging facility for AWS. The log file is named
--  'aws-Day-Y-M-D.log' and is written in the directory where the server is
--  launched. Each new run will append data to this file.

with AWS.Status;
with AWS.Messages;

package AWS.Log is

   type Split_Mode is (None, Daily, Monthly);

   procedure Start (Split : in Split_Mode := None);
   --  Activate server activity logging. Split indicate the way the log file
   --  should be.

   procedure Write (Connect_Stat : in Status.Data;
                    Answer_Stat  : in Messages.Status_Code;
                    Peername     : in String);
   --  Write log is activated.

   procedure Stop;
   --  Stop logging activity.

end AWS.Log;
