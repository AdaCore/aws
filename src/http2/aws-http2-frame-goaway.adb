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
with AWS.Translator;

package body AWS.HTTP2.Frame.GoAway is

   Data_Prefix : constant Stream_Element_Array :=
                   Translator.To_Stream_Element_Array ("AWS/");
   --  AWS will add this specific tag in from of the additional debug data
   --  and then the corresponding HTTP/1 code, plus the error message prefixed
   --  with a slash. It makes the additional debug data format as follow:
   --
   --    AWS/SXXX/<message>

   ----------
   -- Code --
   ----------

   function Code (Self : Object) return Messages.Status_Code is
   begin
      return Messages.Status_Code'Value
        (Translator.To_String
           (Self.Data.S
                (9 + Data_Prefix'Length .. 8 + Data_Prefix'Length + 4)));
   end Code;

   ------------
   -- Create --
   ------------

   function Create
     (Stream_Id : Stream.Id;
      Error     : Error_Codes;
      Code      : Messages.Status_Code := Messages.S200;
      Message   : String := "") return Object is

   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length    := 8 + Data_Prefix'Length + 5 + Message'Length;
         O.Header.H.Kind      := K_GoAway;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := 0;

         O.Data.S := new Stream_Element_Array
                           (1 .. Stream_Element_Offset (O.Header.H.Length));

         O.Data.P.all := (R          => 0,
                          Stream_Id  => Stream_Id,
                          Error_Code => Error_Codes'Pos (Error));

         --  Add AWS Code + Message : AWS/SXXX<message>
         --  Add data prefix

         O.Data.S (9 .. 8 + Data_Prefix'Length + 5) :=
           Data_Prefix
           & Translator.To_Stream_Element_Array
               (Messages.Status_Code'Image (Code) & '/');

         --  And the message itself if present

         if Message'Length > 0 then
            O.Data.S (9 + Data_Prefix'Length + 5 .. O.Data.S'Last) :=
              Translator.To_Stream_Element_Array (Message);
         end if;
      end return;
   end Create;

   ----------
   -- Data --
   ----------

   function Data (Self : Object) return String is
   begin
      return Translator.To_String (Self.Data.S (9 .. Self.Data.S'Last));
   end Data;

   ------------------
   -- Dump_Payload --
   ------------------

   overriding procedure Dump_Payload (Self : Object) is
   begin
      Utils.Dump_Binary (Self.Data.S.all);
   end Dump_Payload;

   ----------------------
   -- Has_Code_Message --
   ----------------------

   function Has_Code_Message (Self : Object) return Boolean is
   begin
      return Self.Data.S (9 .. 8 + Data_Prefix'Length) = Data_Prefix;
   end Has_Code_Message;

   -------------
   -- Message --
   -------------

   function Message (Self : Object) return String is
   begin
      return Translator.To_String
        (Self.Data.S
           (9 + Data_Prefix'Length + 5 .. Self.Data.S'Last));
   end Message;

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
      if Self.Header.H.Stream_Id /= 0
        or else Self.Header.H.Length < 8
      then
         return C_Protocol_Error;

      else
         return HTTP2.Frame.Object (Self).Validate (Settings);
      end if;
   end Validate;

end AWS.HTTP2.Frame.GoAway;
