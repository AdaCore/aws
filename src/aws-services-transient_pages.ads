------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with AWS.Resources.Streams.Memory;

package AWS.Services.Transient_Pages is

   Default_Life_Time : constant Duration := 5 * 60.0;
   --  Default life time for a transient page

   type Stream_Type is
     new AWS.Resources.Streams.Memory.Stream_Type with null record;

   function Get_URI return String;
   --  Create a unique URI, must be used to register a transient web page

   procedure Register
     (URI       : in String;
      Resource  : in AWS.Resources.Streams.Stream_Access;
      Life_Time : in Duration := Default_Life_Time);
   --  Register a new transient page, this page will be deleted after Lifetime
   --  seconds.

   function Get (URI : in String) return AWS.Resources.Streams.Stream_Access;
   --  Returns the stream access for the URI or null if this URI has not been
   --  registered.

private

   procedure Close (Resource : in out Stream_Type);
   --  We override this routine here as we do not want the server to release
   --  the stream handle. This is handled internaly.

   procedure Release
     (Resource : in     Stream_Type;
      File     : in out AWS.Resources.File_Type);
   --  We override this routine here as we do not want the server to release
   --  the stream handle. This is handled internaly.

end AWS.Services.Transient_Pages;
