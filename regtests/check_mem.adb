------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2003-2004                         --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

--  ~ MAIN [SOAP+SSL]

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.Resources.Streams.Disk;
with AWS.Resources.Streams.Memory.ZLib;
with AWS.Server;
with AWS.Session;
with AWS.Services.Split_Pages;
with AWS.Status;
with AWS.Templates;
with AWS.Translator;
with AWS.Utils;

with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Message.XML;
with SOAP.Parameters;
with SOAP.Types;

procedure Check_Mem is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   procedure Check (Str : in String);

   procedure Client;

   function SOAP_CB (Request : in Status.Data) return Response.Data;

   procedure Check_Zlib;

   procedure Check_Memory_Streams;

   procedure Check_Dynamic_Message (Encoding : Messages.Content_Encoding);

   task Server is
      entry Started;
      entry Stopped;
   end Server;

   HTTP      : AWS.Server.HTTP;

   S_Port    : constant String   := Command_Line.Argument (2);
   Port      : constant Positive := Positive'Value (S_Port);

   Iteration : Positive;

   -----------
   -- Check --
   -----------

   procedure Check (Str : in String) is
   begin
      Put_Line (Str);
   end Check;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      SOAP_Action : constant String          := Status.SOAPAction (Request);
      URI         : constant String          := Status.URI (Request);
      P_List      : constant Parameters.List := Status.Parameters (Request);
      SID         : constant Session.ID      := Status.Session (Request);

      N           : Natural := 0;
      Strm        : Resources.Streams.Stream_Access;
   begin
      if Session.Exist (SID, "key") then
         N := Session.Get (SID, "key");
         N := N + 1;
      end if;

      Session.Set (SID, "key", N);
      Session.Set (SID, "key" & Utils.Image (N), "a value");

      if SOAP_Action = "/soap_demo" then
         return SOAP_CB (Request);

      elsif URI = "/simple" then
         Check (Natural'Image (Parameters.Count (P_List)));
         Check (Parameters.Get (P_List, "p1"));
         Check (Parameters.Get (P_List, "p2"));

         return Response.Build (MIME.Text_HTML, "simple ok");

      elsif URI = "/complex" then
         Check (Natural'Image (Parameters.Count (P_List)));
         Check (Parameters.Get (P_List, "p1"));
         Check (Parameters.Get (P_List, "p2"));

         for K in 1 .. Parameters.Count (P_List) loop
            Check (Parameters.Get_Name (P_List, K));
            Check (Parameters.Get_Value (P_List, K));
         end loop;

         return Response.Build (MIME.Text_HTML, "complex ok");

      elsif URI = "/multiple" then
         Check (Natural'Image (Parameters.Count (P_List)));
         Check (Parameters.Get (P_List, "par", 1));
         Check (Parameters.Get (P_List, "par", 2));
         Check (Parameters.Get (P_List, "par", 3));
         Check (Parameters.Get (P_List, "par", 4));
         Check (Parameters.Get (P_List, "par", 5));

         return Response.Build (MIME.Text_HTML, "multiple ok");

      elsif URI = "/file" then
         return Response.File (MIME.Text_Plain, "check_mem.adb");

      elsif URI = "/filea.txt"
        or else URI = "/fileb.txt"
        or else URI = "/filec.txt"
      then
         return Response.File ("text/plain", URI (URI'First + 1 .. URI'Last));

      elsif URI = "/no-template" then
         declare
            Trans : constant Templates.Translate_Table
              := (1 => Templates.Assoc ("ONE", 1));

            Result : Unbounded_String;

         begin
            Result
              := Templates.Parse ("_._.tmplt", Trans, Cached => False);
         exception
            when Ada.IO_Exceptions.Name_Error =>
               null;
         end;

         return Response.Build (MIME.Text_HTML, "dummy");

      elsif URI = "/template" then

         declare
            use type Templates.Vector_Tag;

            Vect   : Templates.Vector_Tag := +"V1" & "V2" & "V3";
            Matrix : constant Templates.Matrix_Tag := +Vect & Vect;

            Trans : constant Templates.Translate_Table
              := (1 => Templates.Assoc ("ONE", 1),
                  2 => Templates.Assoc ("TWO", 2),
                  3 => Templates.Assoc ("EXIST", True),
                  4 => Templates.Assoc ("V", Vect),
                  5 => Templates.Assoc ("M", Matrix));
         begin
            Templates.Clear (Vect);
            Vect := +"V1" & "V2" & "V3";

            return Response.Build
              (MIME.Text_HTML,
               String'(Templates.Parse ("check_mem.tmplt", Trans)));
         end;

      elsif URI = "/stream" then
         Strm :=  new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all), "check_mem.adb");
         return Response.Stream (MIME.Application_Octet_Stream, Strm);

      else
         Check ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      AWS.Server.Start
        (HTTP, "check_mem",
         CB'Unrestricted_Access,
         Port           => Port,
         Max_Connection => 5,
         Session        => True);

      Put_Line ("Server started");
      New_Line;

      accept Started;

      select
         accept Stopped;
      or
         delay 4.0 * Iteration;
         Put_Line ("Too much time to do the job !");
      end select;

      AWS.Server.Shutdown (HTTP);
   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB (Request : in Status.Data) return Response.Data is
      use SOAP.Types;
      use SOAP.Parameters;

      Payload      : constant SOAP.Message.Payload.Object
        := SOAP.Message.XML.Load_Payload (AWS.Status.Payload (Request));

      SOAP_Proc    : constant String
        := SOAP.Message.Payload.Procedure_Name (Payload);

      Parameters   : constant SOAP.Parameters.List
        := SOAP.Message.Parameters (Payload);

      Response     : SOAP.Message.Response.Object;
      R_Parameters : SOAP.Parameters.List;

   begin
      Response := SOAP.Message.Response.From (Payload);

      declare
         X : constant Integer := SOAP.Parameters.Get (Parameters, "x");
         Y : constant Integer := SOAP.Parameters.Get (Parameters, "y");
      begin
         if SOAP_Proc = "multProc" then
            R_Parameters := +I (X * Y, "result");
         elsif SOAP_Proc = "addProc" then
            R_Parameters := +I (X + Y, "result");
         end if;
      end;

      SOAP.Message.Set_Parameters (Response, R_Parameters);

      return SOAP.Message.Response.Build (Response);
   end SOAP_CB;

   ------------
   -- Client --
   ------------

   procedure Client is

      procedure Request (URL : in String);

      procedure Request (Proc : in String; X, Y : in Integer);

      -------------
      -- Request --
      -------------

      procedure Request (URL : in String) is
         R : Response.Data;
      begin
         R := AWS.Client.Get ("http://localhost:" & S_Port & URL);
         Check (Response.Message_Body (R));
      end Request;

      procedure Request (Proc : in String; X, Y : in Integer) is
         use SOAP.Types;
         use type SOAP.Parameters.List;

         P_Set   : constant SOAP.Parameters.List := +I (X, "x") & I (Y, "y");
         Payload : SOAP.Message.Payload.Object;
      begin
         Payload := SOAP.Message.Payload.Build (Proc, P_Set);

         declare
            Response     : constant SOAP.Message.Response.Object'Class
              := SOAP.Client.Call
                   ("http://localhost:" & S_Port & "/soap_demo",
                    Payload,
                    "/soap_demo");

            R_Parameters : constant SOAP.Parameters.List
              := SOAP.Message.Parameters (Response);

            Result : constant Integer
              := SOAP.Parameters.Get (R_Parameters, "result");
         begin
            null;
         end;
      end Request;

   begin
      Request ("/simple");
      Request ("/simple?p1=8&p2=azerty%20qwerty");
      Request ("/simple?p2=8&p1=azerty%20qwerty");
      Request ("/doesnotexist?p=8");

      Request ("/complex?p1=1&p2=2&p3=3&p4=4&p5=5&p6=6"
                 & "&p7=7&p8=8&p9=9&p10=10&p11=11&p12=12&p13=13&p14=14&p15=15"
                 & "&very_long_name_in_a_get_form=alongvalueforthistest");

      Request ("/multiple?par=1&par=2&par=3&par=4&par=whatever");

      Request ("/simple?p1=8&p2=azerty%20qwerty");
      Request ("/file");
      Request ("/template");
      Request ("/no-template");
      Request ("/stream");

      Request ("multProc", 2, 3);
      Request ("multProc", 98, 123);
      Request ("multProc", 5, 9);
      Request ("addProc", 2, 3);
      Request ("addProc", 98, 123);
      Request ("addProc", 5, 9);
   end Client;

   ---------------------------
   -- Check_Dynamic_Message --
   ---------------------------

   procedure Check_Dynamic_Message (Encoding : in Messages.Content_Encoding) is
      Answer : Response.Data;
   begin
      Response.Set.Data_Encoding (Answer, Encoding);

      Response.Set.Message_Body
        (Answer, Streams.Stream_Element_Array'(1 .. 64 => 10));
   end Check_Dynamic_Message;

   --------------------------
   -- Check_Memory_Streams --
   --------------------------

   procedure Check_Memory_Streams is
      use AWS.Resources.Streams.Memory;

      use type Streams.Stream_Element_Array;

      Sample : Streams.Stream_Element_Array := (1 .. 64 => 20);

      Plain  : Stream_Type;
      Unpack : ZLib.Stream_Type;
      Packed : Utils.Stream_Element_Array_Access;

      procedure Test
        (Stream : in out Stream_Type'Class;
         Data   : in     Streams.Stream_Element_Array);
      --  Append dynamically allocated data, test content and close the stream.

      ----------
      -- Test --
      ----------

      procedure Test
        (Stream : in out Stream_Type'Class;
         Data   : in     Streams.Stream_Element_Array)
      is
         Test   : Streams.Stream_Element_Array (Sample'Range);
         Last   : Streams.Stream_Element_Offset;
      begin
         Append (Stream, Data);
         Read (Stream, Test, Last);

         if Test (1 .. Last) /= Sample or else not End_Of_File (Stream) then
            raise Program_Error;
         end if;

         Close (Stream);
      end Test;

   begin
      ZLib.Inflate_Initialize (Unpack);

      Packed := Translator.Compress (Sample);

      Test (Unpack, Packed.all);
      Utils.Free (Packed);

      Test (Plain, Sample);
   end Check_Memory_Streams;

   ----------------
   -- Check_Zlib --
   ----------------

   procedure Check_Zlib is

      use type Streams.Stream_Element_Array;

      procedure Test (Str : in String);

      ----------
      -- Test --
      ----------

      procedure Test (Str : in String) is
         Data   : constant Streams.Stream_Element_Array
           := Translator.To_Stream_Element_Array (Str);
         Comp   : Utils.Stream_Element_Array_Access;
         Decomp : Utils.Stream_Element_Array_Access;
      begin
         Comp   := Translator.Compress (Data);
         Decomp := Translator.Decompress (Comp.all);

         if Data = Decomp.all then
            Text_IO.Put_Line ("Ok");
         else
            Text_IO.Put_Line ("Nok: " & Translator.To_String (Decomp.all));
         end if;

         Text_IO.Put_Line
           (Integer'Image (Data'Length) & " bytes compressed to"
              & Integer'Image (Comp'Length));

         Utils.Free (Comp);
         Utils.Free (Decomp);
      end Test;

   begin
      Test ("simple");

      Test ("A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "one on which we will display real size.");
   end Check_Zlib;

   ---------------------
   -- Check_Transient --
   ---------------------

   procedure Check_Transient is

      use Templates;

      T1 : Translate_Table
        := (Assoc ("ONE", "one"), Assoc ("TWO", "2"), Assoc ("THREE", "3"));

      T2 : Translate_Table
        := (Assoc ("V1", Vector_Tag'(+"t11" & "t12" & "t13")),
            Assoc ("V2", Vector_Tag'(+"t21" & "t22" & "t23")));

      R : Response.Data;
   begin
      R := Services.Split_Pages.Parse ("split1.tmplt", T1, T2, 2);
   end Check_Transient;

   -----------------
   -- Check_ZOpen --
   -----------------

   procedure Check_ZOpen is

      use Templates;

      R : Response.Data;
   begin
      R := AWS.Client.Get ("http://localhost:" & S_Port & "/filea.txt");
      R := AWS.Client.Get ("http://localhost:" & S_Port & "/fileb.txt");
      R := AWS.Client.Get ("http://localhost:" & S_Port & "/filec.txt");
   end Check_ZOpen;

begin
   Put_Line ("Start main, wait for server to start...");

   Iteration := Integer'Value (Command_Line.Argument (1));

   Server.Started;

   --  This is the main loop. Be sure to run everything inside this
   --  loop. Check_Mem is checked between 2 runs with a different number of
   --  iterations.

   for K in 1 ..  Iteration loop
      Client;
      Check_Zlib;
      Check_Memory_Streams;
      Check_Dynamic_Message (Messages.Identity);
      Check_Dynamic_Message (Messages.Deflate);
      Check_Transient;
      Check_Zopen;
   end loop;

   delay 4.0;

   Server.Stopped;

   Command_Line.Set_Exit_Status (Command_Line.Success);
exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end Check_Mem;
