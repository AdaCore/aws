------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                     Copyright (C) 2007-2009, AdaCore                     --
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

--  This package and children provide a number of types and subprograms for
--  creating reponse text used in ESMTP authentication.

package AWS.SMTP.Authentication is

   type Credential is abstract tagged private;
   --  Information needed by some authentication protocol

   procedure Before_Send
     (Credential : Authentication.Credential;
      Sock       : in out Net.Socket_Type'Class;
      Status     : out SMTP.Status) is null;
   --  Null default implementation

   procedure After_Send
     (Credential : Authentication.Credential;
      Sock       : in out Net.Socket_Type'Class;
      Status     : out SMTP.Status) is null;
   --  Null default implementation

   function Image (Info : Credential) return String is abstract;
   --  Response to be sent to the server

private

   type Credential is abstract tagged null record;

end AWS.SMTP.Authentication;
