------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
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

with Ada.Text_IO;

with AWS.Server;
with AWS.Response;
with AWS.Status;
with AWS.MIME;

procedure Simple is

   use AWS;

   WS : Server.HTTP;

   function CB (Request : in Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "should not be called");
   end CB;

begin
   Server.Start
     (WS, "simple", CB'Unrestricted_Access, Port => 1234, Max_Connection => 5);
   Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;
   delay 1.0;
   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");
end Simple;
