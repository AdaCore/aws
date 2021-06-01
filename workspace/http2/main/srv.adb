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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Headers;
with AWS.Net.Buffered;
with AWS.Net.Log;
with AWS.Net.Std;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

with AWS.HTTP2.Frame;
with AWS.HTTP2.HPACK.Huffman;

with GNAT.OS_Lib;

procedure Srv is

   use Ada;
   use Ada.Command_Line;
   use Ada.Exceptions;
   use Ada.Streams;
   use Ada.Text_IO;

   use AWS;

   Client_Activated : constant boolean := False;
   --  Wether to activate the client task for testing
   --  purpose. Not really needed now that the implementation
   --  can be used with curl & a Web browser.

   task Client is
      entry Start;
      entry Stop;
   end Client;

   procedure Get_Response (S : Net.Socket_Type'Class);
   --  Get response server until empty line

   procedure Check_Huffman (Str : String);

   function CB (Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
      H   : Headers.List := AWS.Status.Header (Request);
   begin
      Text_IO.Put_Line ("CB headers" & H.Count'Img & " :");
      for K in 1 .. H.Count loop
         Put_Line (K'Img & " "
                   & String'(H.Get_Name (K))
                   & " = " & String'(H.Get_Value (K)));
      end loop;

      if URI = "/file" then
         return Response.File (Content_Type => "image/png",
                               Filename     => "adains.png");
      elsif URI = "/" then
         return AWS.Response.Build
           ("text/html", "Hello all goo 200 !");
      else
         return AWS.Response.Build
           ("text/html", "Hello ! (" & URI & ")");
      end if;

   exception
      when E : others =>
         Text_IO.Put_Line ("exception: " & Exception_Information (E));
   end CB;

   procedure Check_Huffman (Str : String) is
      E : constant Stream_Element_Array :=
            AWS.HTTP2.HPACK.Huffman.Encode (Str);
      D : constant String :=
            AWS.HTTP2.HPACK.Huffman.Decode (E);
   begin
      Text_IO.Put ("H encode: ");
      for B of E loop
         Integer_Text_IO.Put (Positive (B), Base => 16);
      end loop;
      Text_IO.New_Line;

      Text_IO.Put_Line ("H decode : " & D);

      if D = Str then
         Text_IO.Put_Line ("OK");
      else
         Text_IO.Put_Line ("NOK !!!!!!");
      end if;
   end Check_Huffman;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response (S : Net.Socket_Type'Class) is
      Buffer   : Stream_Element_Array (1 .. 200);
      Last     : Stream_Element_Offset;
   begin
      loop
         Net.Receive (S, Buffer, Last);
         exit when Last = 0;

         Text_IO.Put_Line
           ("> " & Translator.To_String (Buffer (Buffer'First .. Last)));
      end loop;
   end Get_Response;

   Security : constant Boolean := False;

   Srv_Sock : Net.Socket_Type'Class := Net.Socket (Security);
   New_Sock : Net.Socket_Type'Class := Net.Socket (Security);

   task body Client is
      Sock   : Net.Socket_Type'Class := Net.Socket (Security);

      B1 : constant Stream_Element_Array :=
             (16#40#, 16#0a#, 16#63#, 16#75#, 16#73#, 16#74#,
              16#6f#, 16#6d#, 16#2d#, 16#6b#, 16#65#, 16#79#,
              16#0d#, 16#63#, 16#75#, 16#73#, 16#74#, 16#6f#,
              16#6d#, 16#2d#, 16#68#, 16#65#, 16#61#, 16#64#,
              16#65#, 16#72#);
      --  C.2.1.  Literal Header Field with Indexing

      B2 : constant Stream_Element_Array :=
             (16#04#, 16#0c#, 16#2f#, 16#73#, 16#61#, 16#6d#,
              16#70#, 16#6c#, 16#65#, 16#2f#, 16#70#, 16#61#,
              16#74#, 16#68#);
      --  C.2.2.  Literal Header Field without Indexing

      B3 : constant Stream_Element_Array :=
             (1 => 16#82#);
      --  C.2.4.  Indexed Header Field

      B4 : constant Stream_Element_Array :=
             (16#82#, 16#86#, 16#84#, 16#41#, 16#0f#, 16#77#,
              16#77#, 16#77#, 16#2e#, 16#65#, 16#78#, 16#61#,
              16#6d#, 16#70#, 16#6c#, 16#65#, 16#2e#, 16#63#,
              16#6f#, 16#6d#);
      --  C.3.1 (First Request)  Request Examples without Huffman Coding

      B5 : constant Stream_Element_Array :=
             (16#82#, 16#86#, 16#84#, 16#be#, 16#58#, 16#08#,
              16#6e#, 16#6f#, 16#2d#, 16#63#, 16#61#, 16#63#,
              16#68#, 16#65#);
      --  C.3.2 (Second Request)  Request Examples without Huffman Coding

      B6 : constant Stream_Element_Array :=
             (16#82#, 16#87#, 16#85#, 16#bf#, 16#40#, 16#0a#,
              16#63#, 16#75#, 16#73#, 16#74#, 16#6f#, 16#6d#,
              16#2d#, 16#6b#, 16#65#, 16#79#, 16#0c#, 16#63#,
              16#75#, 16#73#, 16#74#, 16#6f#, 16#6d#, 16#2d#,
              16#76#, 16#61#, 16#6c#, 16#75#, 16#65#);
      --  C.3.2 (Third Request)  Request Examples without Huffman Coding

      B7 : constant Stream_Element_Array :=
             (16#82#, 16#86#, 16#84#, 16#41#, 16#8c#, 16#f1#,
              16#e3#, 16#c2#, 16#e5#, 16#f2#, 16#3a#, 16#6b#,
              16#a0#, 16#ab#, 16#90#, 16#f4#, 16#ff#);
      --  C.4.1 (First Request)  Request Examples with Huffman Coding

   begin
      if Client_Activated then
         accept Start;

         Net.Connect (Sock, "127.0.0.1", 3128);

         --  3 request no Huffman
         --  Net.Send (Sock, B4);
         --  Net.Send (Sock, B5);
         --  Net.Send (Sock, B6);

         Net.Send (Sock, B7);

         accept Stop;
      end if;
   end Client;

   -----------------
   -- Local_Check --
   -----------------

   procedure Local_Check is
   begin
      Check_Huffman ("www.example.com");
      Check_Huffman ("some other string");
      Check_Huffman ("0");
   end Local_Check;

   --------------------
   -- Write_Callback --
   --------------------

   procedure Write_Callback
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset)
   is
      L : constant := 10;
      C : Stream_Element_Offset := Data'First;
      N : Stream_Element_Offset :=
            Stream_Element_Offset'Min (Last, C + L - 1);
   begin
      while N < Last loop
         Text_IO.Put ("> ");
         for K in C .. N loop
            Put
              (Utils.Hex (Integer (Data (K)), Width => 2)); -- , Base => 16);
            Text_IO.Put (' ');
         end loop;

         Text_IO.New_Line;

         Text_IO.Put ("  ");
         for K in C .. N loop
            declare
               I : Character := Character'Val (Integer (Data (K)));
            begin
               if Integer (Data (K)) not in 32 .. 127 then
                  I := '?';
               end if;
               Text_IO.Put (" " & I & ' ');
            end;
         end loop;

         Text_IO.New_Line;

         C := N + 1;
         N := Stream_Element_Offset'Min (Last, C + L - 1);
      end loop;
   end Write_Callback;

   WS     : Server.HTTP;
   Config : AWS.Config.Object;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Press Q to exit...");

   --  AWS.Net.Log.Start (Write_Callback'Unrestricted_Access);
   --
   --  Local_Check;

   --  AWS.Headers.Debug (True);

   AWS.Config.Set.Reuse_Address (Config, True);
   AWS.Config.Set.HTTP2_Activated (Config, True);

   if Argument_Count = 0 then
      AWS.Config.Set.Server_Port (Config, 1234);
      AWS.Config.Set.Security (Config, False);
   else
      AWS.Config.Set.Server_Port (Config, Natural'Value (Argument (1)));
      AWS.Config.Set.Security (Config, True);
      AWS.Config.Set.Certificate (Config, "cert.pem");
      AWS.Config.Set.Key (Config, "cert.pem");
   end if;

   AWS.Config.Set.Server_Name (Config, "Srv HTTP/2");
   AWS.Config.Set.Max_Connection (Config, 5);

   AWS.Server.Start
     (WS,
      Config   => Config,
      Callback => CB'Unrestricted_Access);

   AWS.Server.Log.Start_Error (WS);

   if AWS.Config.Server_Port (Config) = 0 then
      Put_Line ("Secure server on port" & AWS.Server.Status.Port (WS)'Img);
   end if;

   Server.Wait (AWS.Server.Q_Key_Pressed);

   AWS.Server.Shutdown (WS);

exception
   when E : others =>
      Text_IO.Put_Line ("srv crashed !!!!" & Exception_Information (E));
end Srv;
