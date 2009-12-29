------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                 S M T P - Simple Mail Transfer Protocol                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

package body AWS.SMTP.Messages.Set is

   -------------
   -- Headers --
   -------------

   procedure Headers
     (Message : in out Data; Headers : AWS.Headers.List) is
   begin
      Message.Headers := Headers;
   end Headers;

   --------------
   -- Set_Body --
   --------------

   procedure Set_Body (Message : in out Data; Message_Body : String) is
   begin
      Message.Message_Body := To_Unbounded_String (Message_Body);
   end Set_Body;

end AWS.SMTP.Messages.Set;
