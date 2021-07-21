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

with AWS.HTTP2.HPACK;
with AWS.Net.Buffered;

package body AWS.HTTP2.Frame.Continuation is

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length
     (Self : Object) return Stream_Element_Count is
   begin
      return Self.Data.S'Length;
   end Content_Length;

   ------------
   -- Create --
   ------------

   function Create
     (Table       : not null access HTTP2.HPACK.Table.Object;
      Settings    : not null access Connection.Object;
      Stream_Id   : HTTP2.Stream_Id;
      List        : Headers.List;
      End_Headers : Boolean := True) return Object is
   begin
      return O : Object do
         if List.Length > 0 then
            O.Data.S :=
              new Stream_Element_Array'(HPACK.Encode (Table, Settings, List));
         end if;

         O.Header.H.Stream_Id := Stream_Id;
         O.Header.H.Length    := Length_Type (O.Data.S'Length);
         O.Header.H.Kind      := K_Continuation;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := (if End_Headers then End_Headers_Flag else 0);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get
     (Self  : Object;
      Index : Stream_Element_Offset) return Stream_Element is
   begin
      return Self.Data.S (Self.Data.S'First - 1 + Index);
   end Get;

   ----------
   -- Read --
   ----------

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
   is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Header.Header.H.Length);
   begin
      return O : Object := (Header with Data => <>) do
         if Len > 0 then
            O.Data.S := new Stream_Element_Array (1 .. Len);
            Net.Buffered.Read (Sock, O.Data.S.all);
         end if;
      end return;
   end Read;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Object) is
   begin
      Utils.Unchecked_Free (Self.Data.S);
   end Release;

   ------------------
   -- Send_Payload --
   ------------------

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class) is
   begin
      Net.Buffered.Write (Sock, Self.Data.S.all);
   end Send_Payload;

   --------------
   -- Validate --
   --------------

   overriding function Validate
     (Self : Object; Settings : Connection.Object) return Error_Codes is
   begin
      if Self.Header.H.Stream_Id = 0 then
         return C_Protocol_Error;
      else
         return HTTP2.Frame.Object (Self).Validate (Settings);
      end if;
   end Validate;

end AWS.HTTP2.Frame.Continuation;
