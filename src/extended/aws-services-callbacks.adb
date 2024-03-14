------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with AWS.Messages;
with AWS.MIME;

package body AWS.Services.Callbacks is

   ----------
   -- File --
   ----------

   function File (Request : Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
      K        : Natural := Directory'Last;
   begin
      if URI'Length >= Prefix'Length
        and then URI (URI'First .. URI'First + Prefix'Length - 1) = Prefix
      then
         if Directory (K) = '/' then
            K := K - 1;
         end if;

         return Response.File
           (MIME.Content_Type (Filename),
            Filename => Directory (Directory'First .. K) & '/'
                          & URI (URI'First + Prefix'Length - 1 .. URI'Last));
      else
         return Response.Build
           (MIME.Text_HTML, "Page not found: " & URI, Messages.S404);
      end if;
   end File;

end AWS.Services.Callbacks;
