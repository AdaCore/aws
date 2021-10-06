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

with Ada.Streams;

package AWS.HTTP2 is

   use Ada.Streams;

   type Bit_1 is mod 2 ** 1 with Size => 1;

   type Byte_1 is mod 2 **  8 with Size => 8;
   type Byte_2 is mod 2 ** 16 with Size => 16;
   type Byte_3 is mod 2 ** 24 with Size => 24;
   type Byte_4 is mod 2 ** 32 with Size => 32;

   type Stream_Id is new Natural range 0 .. 2 ** 31 - 1;

   Protocol_Error : exception;

   type Error_Codes is
     (C_No_Error, C_Protocol_Error, C_Internal_Error, C_Flow_Control_Error,
      C_Settings_Timeout, C_Stream_Closed, C_Frame_Size_Error,
      C_Refused_Stream, C_Cancel, C_Compression_Error, C_Connect_Error,
      C_Enhance_Your_CALM, C_Inadequate_Security, C_HTTP_1_1_Required);
   --  Error codes that are used in RST_Stream and GoAway frames

   Client_Connection_Preface_1 : constant String := "PRI * HTTP/2.0";
   --  Connection preface first part

   Client_Connection_Preface_2 : constant String := "SM";
   --  Connection preface second part

   Client_Connection_Preface   : constant  Stream_Element_Array;
   --  The full connection preface:
   --  "PRI * HTTP/2.0" & CRLF & CRLF & "SM" & CRLF & CRLF

   function Exception_Message
     (Error : Error_Codes; Message : String) return String
     with Pre => Message /= "";
   --  Build an exception message with the error code endoded at the start of
   --  the message and surrounded with square brackets.

   function Exception_Code (Exception_Message : String) return Error_Codes
     with Pre => Exception_Message'Length > 2;
   --  Extract the execption code from an exception message built with the
   --  Exception_Message routine above.

   Debug : constant Boolean := False;
   --  Activate some debug output for HTTP/2 protocol

private

   Client_Connection_Preface : constant Stream_Element_Array :=
                                 (16#50#, 16#52#, 16#49#, 16#20#, 16#2a#,
                                  16#20#, 16#48#, 16#54#, 16#54#, 16#50#,
                                  16#2f#, 16#32#, 16#2e#, 16#30#, 16#0d#,
                                  16#0a#, 16#0d#, 16#0a#, 16#53#, 16#4d#,
                                  16#0d#, 16#0a#, 16#0d#, 16#0a#);

   for Error_Codes use (C_No_Error            => 16#0#,
                        C_Protocol_Error      => 16#1#,
                        C_Internal_Error      => 16#2#,
                        C_Flow_Control_Error  => 16#3#,
                        C_Settings_Timeout    => 16#4#,
                        C_Stream_Closed       => 16#5#,
                        C_Frame_Size_Error    => 16#6#,
                        C_Refused_Stream      => 16#7#,
                        C_Cancel              => 16#8#,
                        C_Compression_Error   => 16#9#,
                        C_Connect_Error       => 16#A#,
                        C_Enhance_Your_CALM   => 16#B#,
                        C_Inadequate_Security => 16#C#,
                        C_HTTP_1_1_Required   => 16#D#);

end AWS.HTTP2;
