------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

package body AWS.Net.Websocket.Handshake_Error is

   ------------
   -- Create --
   ------------

   function Create
     (Status_Code   : Messages.Client_Error := Messages.S403;
      Reason_Phrase : String := "") return Object is
   begin
      return Object'
        (WebSocket.Object
         with Status_Code, To_Unbounded_String (Reason_Phrase));
   end Create;

   -------------------
   -- Reason_Phrase --
   -------------------

   function Reason_Phrase (Socket : Object'Class) return String is
   begin
      return To_String (Socket.Reason_Phrase);
   end Reason_Phrase;

   -----------------
   -- Status_Code --
   -----------------

   function Status_Code (Socket : Object'Class) return Messages.Client_Error is
   begin
      return Socket.Status_Code;
   end Status_Code;

end AWS.Net.WebSocket.Handshake_Error;
