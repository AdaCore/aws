------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.Headers;

package body AWS.HTTP2.Message is

   ------------
   -- Create --
   ------------

   function Create
     (Headers : AWS.Headers.List;
      Payload : Unbounded_String)
      return Object is
   begin
      return O : Object do
         O.Headers := Headers;
         O.Payload := Payload;
      end return;
   end Create;

   ---------------
   -- To_Frames --
   ---------------

   function To_Frames
     (Self      : Object;
      Ctx       : in out Context;
      Stream_Id : HTTP2.Stream_Id)
      return AWS.HTTP2.Frame.List.Object
   is
      List : Frame.List.Object;
   begin
      List.Append (Frame.Headers.Create (Ctx.Table, Stream_Id, Self.Headers));
      List.Append (Frame.Data.Create (Stream_Id, To_String (Self.Payload)));
      return List;
   end To_Frames;

end AWS.HTTP2.Message;