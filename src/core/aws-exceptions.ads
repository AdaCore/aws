------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

with Ada.Exceptions;

with AWS.Log;
with AWS.Response;
with AWS.Status;

package AWS.Exceptions is

   use Ada.Exceptions;

   type Data is record
      Fatal   : Boolean;
      --  If True it means that we go a fatal error. The slot will be
      --  terminated so AWS will loose one of it's simultaneous connection.
      --  This is clearly an AWS internal error that should be fixed in AWS.

      Slot    : Positive;
      --  The failing slot number

      Request : Status.Data;
      --  The complete request information that was served when the slot has
      --  failed. This variable is set only when Fatal is False.
   end record;

   type Unexpected_Exception_Handler is not null access
     procedure (E      : Exception_Occurrence;
                Log    : in out AWS.Log.Object;
                Error  : Data;
                Answer : in out Response.Data);
   --  Unexpected exception handler can be set to monitor server errors.
   --  Answer can be set with the answer to send back to the client's
   --  browser. Note that this is possible only for non fatal error
   --  (i.e. Error.Fatal is False).
   --  Log is the error log object for the failing server, it can be used
   --  to log user's information (if error log is activated for this
   --  server). Note that the server will have already logged information
   --  about the problem.

end AWS.Exceptions;
