------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                               ACT-Europe                                 --
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

with AWS.Client;
with AWS.Net;
with AWS.Utils;

package body AWS.Communication.Client is

   ------------------
   -- Send_Message --
   ------------------

   function Send_Message
     (Server     : in String;
      Port       : in Positive;
      Name       : in String;
      Parameters : in Parameter_Set := Null_Parameter_Set)
      return Response.Data
   is
      URL : Unbounded_String;
   begin
      URL := URL & "http://" & Server & ':' & Utils.Image (Port)
        & AWS_Com
        & "?HOST=" & Net.Host_Name
        & "&NAME=" & Name;

      for K in Parameters'Range loop
         URL := URL & "&P" & Utils.Image (K) & '=' & Parameters (K);
      end loop;

      return AWS.Client.Get (To_String (URL));
   end Send_Message;

end AWS.Communication.Client;
