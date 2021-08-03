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

package body AWS.HTTP2.Frame.Push_Promise is

   ------------
   -- Create --
   ------------

   function Create
     (Table             : not null access HTTP2.HPACK.Table.Object;
      Settings          : not null access Connection.Object;
      Stream_Id         : HTTP2.Stream_Id;
      Promise_Stream_Id : HTTP2.Stream_Id;
      List              : AWS.Headers.List;
      End_Headers       : Boolean := True) return Object is
   begin
      return O : Object do
         if List.Length > 0 then
            declare
               H_Fragments : constant Stream_Element_Array :=
                               HPACK.Encode (Table, Settings, List);
            begin
               O.Data.S := new Stream_Element_Array
                                 (1 .. 4 + H_Fragments'Length);
               O.Data.S (5 .. O.Data.S'Last) := H_Fragments;
            end;
         end if;

         O.Data.D_Payload.Promise_Stream_Id := Promise_Stream_Id;

         O.Header.H :=
           (Stream_Id => Stream_Id,
            Length    => Length_Type (O.Data.S'Length),
            Kind      => K_Push_Promise,
            R         => 0,
            Flags     => (if End_Headers then End_Headers_Flag else 0));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Object;
      Table    : not null access HTTP2.HPACK.Table.Object;
      Settings : not null access HTTP2.Connection.Object)
      return AWS.Headers.List
   is

      I          : Stream_Element_Offset := Self.Data.S'First;
      Pad_Length : Length_Type := 0;

      function End_Of_Stream return Boolean;

      function Next return Stream_Element;

      function End_Of_Stream return Boolean is
        (Length_Type (I) > Self.Header.H.Length - Pad_Length);

      ----------
      -- Next --
      ----------

      function Next return Stream_Element is
         E : constant Stream_Element := Self.Data.S (I);
      begin
         I := I + 1;
         return E;
      end Next;

      function Get_Headers is new AWS.HTTP2.HPACK.Decode
        (End_Of_Stream => End_Of_Stream,
         Get_Byte      => Next);

   begin
      if Self.Has_Flag (Padded_Flag) then
         --  Skip first byte which is the padding length
         I := I + Self.Data.Pad'Size / 8;
         Pad_Length := Length_Type (Self.Data.Pad);
      end if;

      --  Skip promise stream id

      I := I + 4;

      return Get_Headers (Table, Settings);
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

end AWS.HTTP2.Frame.Push_Promise;
