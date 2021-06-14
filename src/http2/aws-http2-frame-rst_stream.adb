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

with AWS.Net.Buffered;

package body AWS.HTTP2.Frame.RST_Stream is

   ------------
   -- Create --
   ------------

   function Create (Error : Error_Codes) return Object is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Payload'Size / 8);
   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length    := Length_Type (Len);
         O.Header.H.Kind      := K_RST_Stream;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := 0;

         O.Data.P.Error_Code := Error;
      end return;
   end Create;

   ----------
   -- Read --
   ----------

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object is
   begin
      return O : Object := (Header with Data => <>) do
         Net.Buffered.Read (Sock, O.Data.S);
      end return;
   end Read;

   ------------------
   -- Send_Payload --
   ------------------

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class) is
   begin
      Net.Buffered.Write (Sock, Self.Data.S);
   end Send_Payload;

   --------------
   -- Validate --
   --------------

   overriding function Validate
     (Self     : Object;
      Settings : not null access constant Connection.Object)
      return Error_Codes is
   begin
      if Self.Header.H.Stream_Id = 0 then
         return C_Protocol_Error;
      elsif Self.Header.H.Length /= 4 then
         return C_Frame_Size_Error;
      else
         return C_No_Error;
      end if;
   end Validate;

end AWS.HTTP2.Frame.RST_Stream;
