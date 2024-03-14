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

with Ada.Text_IO;

with AWS.Net.Buffered;
with AWS.Response.Set;
with AWS.Status.Set;
with AWS.Translator;

package body AWS.HTTP2.Frame.Data is

   ------------
   -- Append --
   ------------

   procedure Append (Self : Object; Status : in out AWS.Status.Data) is
      First : Stream_Element_Offset := Self.Data.S'First;
      Last  : Stream_Element_Offset := Self.Data.S'Last;
   begin
      --  If we have a padded flag, remove the padding from the payload

      if Self.Has_Flag (Padded_Flag) then
         First := First + Self.Data.D.Pad_Length'Size / 8;
         Last  := Last - Stream_Element_Offset (Self.Data.D.Pad_Length);
      end if;

      AWS.Status.Set.Append_Body (Status, Self.Data.S (First .. Last));
   end Append;

   procedure Append (Self : Object; Response : in out AWS.Response.Data) is
      First : Stream_Element_Offset := Self.Data.S'First;
      Last  : Stream_Element_Offset := Self.Data.S'Last;
   begin
      --  If we have a padded flag, remove the padding from the payload

      if Self.Has_Flag (Padded_Flag) then
         First := First + Self.Data.D.Pad_Length'Size / 8;
         Last  := Last - Stream_Element_Offset (Self.Data.D.Pad_Length);
      end if;

      AWS.Response.Set.Append_Body (Response, Self.Data.S (First .. Last));
   end Append;

   ------------
   -- Create --
   ------------

   function Create
     (Stream_Id  : HTTP2.Stream_Id;
      Content    : Utils.Stream_Element_Array_Access;
      End_Stream : Boolean) return Object
   is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Content'Length);
   begin
      return O : Object do
         O.Header.H.Stream_Id := Stream_Id;
         O.Header.H.Length    := Length_Type (Len);
         O.Header.H.Kind      := K_Data;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := (if End_Stream then End_Stream_Flag else 0);

         O.Data.S := Content;
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Stream_Id : HTTP2.Stream_Id;
      Content   : String) return Object is
   begin
      return Create
        (Stream_Id, Translator.To_Stream_Element_Array (Content),
         End_Stream => True);
   end Create;

   ------------------
   -- Dump_Payload --
   ------------------

   overriding procedure Dump_Payload (Self : Object) is
   begin
      if Self.Is_Defined and then Self.Has_Flag (Padded_Flag) then
         Text_IO.Put_Line ("Padding:" & Self.Data.D.Pad_Length'Img);
      end if;
   end Dump_Payload;

   -------------
   -- Payload --
   -------------

   function Payload (Self : Object) return Unbounded_String is
      First : Stream_Element_Offset := Self.Data.S'First;
      Last  : Stream_Element_Offset := Self.Data.S'Last;
   begin
      --  If we have a padded flag, remove the padding from the payload

      if Self.Has_Flag (Padded_Flag) then
         First := First + Self.Data.D.Pad_Length'Size / 8;
         Last  := Last - Stream_Element_Offset (Self.Data.D.Pad_Length);
      end if;

      return Translator.To_Unbounded_String (Self.Data.S (First .. Last));
   end Payload;

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
      if Self.Header.H.Stream_Id = 0
        or else
          (Self.Has_Flag (Padded_Flag)
           and then Length_Type (Self.Data.D.Pad_Length)
                      + Self.Data.D.Pad_Length'Size / 8
                    >= Self.Header.H.Length)
      then
         return C_Protocol_Error;
      else
         return HTTP2.Frame.Object (Self).Validate (Settings);
      end if;
   end Validate;

end AWS.HTTP2.Frame.Data;
