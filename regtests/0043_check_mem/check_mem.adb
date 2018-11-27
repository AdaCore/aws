------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Editing;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Net.SSL.Certificate;
with AWS.Net.Std;
with AWS.Parameters;
with AWS.Resources.Streams.Disk;
with AWS.Resources.Streams.Memory.ZLib;
with AWS.Resources.Streams.ZLib;
with AWS.Response.Set;
with AWS.Server.Push;
with AWS.Server.Status;
with AWS.Services.Split_Pages;
with AWS.Session;
with AWS.SMTP.Client;
with AWS.Status;
with AWS.Templates;
with AWS.Translator;
with AWS.Utils;

with GNAT.MD5;
with GNAT.Traceback.Symbolic;

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

   use type Ada.Calendar.Time;

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Certificate_Name : constant String := "cert.pem";
   Private_Key_Name : constant String := "cert.key";

   function CB (Request : Status.Data) return Response.Data;

   procedure Check (Str : String);

   procedure Stamp (Label : String);

   procedure Client;

   function SOAP_CB (Request : Status.Data) return Response.Data;

   procedure Check_Zlib;

   procedure Check_Memory_Streams;

   procedure Check_Dynamic_Message (Encoding : Messages.Content_Encoding);

   procedure Check_Socket;

   type Push_Data_Type is delta 0.01 digits 7;

   function To_Array
     (Data : Push_Data_Type;
      Env  : Text_IO.Editing.Picture)
      return Ada.Streams.Stream_Element_Array;

   package Server_Push is new AWS.Server.Push
     (Client_Output_Type => Push_Data_Type,
      Client_Environment => Text_IO.Editing.Picture,
      To_Stream_Array    => To_Array);

   task Server is
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;
   Push : Server_Push.Object;

   SSL_Srv : Net.SSL.Config;
   SSL_Clt : Net.SSL.Config;
   Hash    : Net.SSL.Hash_Method := Net.SSL.Hash_Method'First;

   DH_Time  : Ada.Calendar.Time := AWS.Utils.AWS_Epoch;
   RSA_Time : Ada.Calendar.Time := AWS.Utils.AWS_Epoch;

   Generation_Log : Text_IO.File_Type;

   Iteration : Positive;
   Timestamp : Ada.Calendar.Time;

   -----------
   -- Check --
   -----------

   procedure Check (Str : String) is
   begin
      Put_Line (Str);
   end Check;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAP_Action : constant String          := Status.SOAPAction (Request);
      URI         : constant String          := Status.URI (Request);
      P_List      : constant Parameters.List := Status.Parameters (Request);
      SID         : constant Session.ID      := Status.Session (Request);

      N           : Natural := 0;
      Strm        : Resources.Streams.Stream_Access;
      Answer      : Response.Data;
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
         return Response.Build
                  (MIME.Text_HTML,
                   "simple ok" & Natural'Image (Parameters.Count (P_List))
                   & ' ' & Parameters.Get (P_List, "p1")
                   & ' ' & Parameters.Get (P_List, "p2"));

      elsif URI = "/complex" then
         for K in 1 .. Parameters.Count (P_List) loop
            declare
               Name  : constant String := Parameters.Get_Name (P_List, K);
               Value : constant String := Parameters.Get_Value (P_List, K);
            begin
               if Name (Name'First) /= 'p'
                 or else Name (Name'First + 1 .. Name'Last) /= Value
                 or else K /= Natural'Value (Value)
               then
                  Response.Set.Append_Body
                    (Answer, Name & '=' & Value & K'Img);
               end if;
            end;
         end loop;

         Response.Set.Append_Body
           (Answer, Natural'Image (Parameters.Count (P_List)));

         return Answer;

      elsif URI = "/multiple" then
         return Response.Build
                  (MIME.Text_HTML,
                   "multiple ok" & Natural'Image (Parameters.Count (P_List))
                   & ' ' & Parameters.Get (P_List, "par", 1)
                   & ' ' & Parameters.Get (P_List, "par", 2)
                   & ' ' & Parameters.Get (P_List, "par", 3)
                   & ' ' & Parameters.Get (P_List, "par", 4)
                   & ' ' & Parameters.Get (P_List, "par", 5));

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

            Trans : constant Templates.Translate_Table :=
                      (1 => Templates.Assoc ("ONE", 1),
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
         declare
            use AWS.Resources.Streams;
         begin
            Strm := new Disk.Stream_Type;

            Disk.Open (Disk.Stream_Type (Strm.all), "check_mem.adb");

            return Response.Stream
                     (MIME.Text_Plain, Strm, Encoding => Messages.GZip);
         end;

      elsif URI = "/server-push" then
         Server_Push.Register
           (Server            => Push,
            Client_ID         => Session.Image (SID),
            Groups            => P_List.Get_Values ("group"),
            Socket            => Net.Socket_Access'(Status.Socket (Request)),
            Init_Data         => 76543.21,
            Init_Content_Type => "text/number",
            Environment       => Editing.To_Picture (P_List.Get ("picture")),
            Kind              => Server_Push.Plain);

         return Response.Socket_Taken;

      else
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   -----------
   -- Error --
   -----------

   procedure Error (Socket : AWS.Net.Socket_Type'Class; Message : String) is
      use GNAT.Traceback;
      Trace : Tracebacks_Array (1 .. 64);
      Last  : Natural;
   begin
      Call_Chain (Trace, Last);

      Ada.Text_IO.Put_Line
        ("# Network error: "
         & Message & Symbolic.Symbolic_Traceback (Trace (1 .. Last)));
   end Error;

   ------------------------
   -- Generation_Logging --
   ------------------------

   procedure Generation_Logging (Text : String) is
   begin
      if Text_IO.Is_Open (Generation_Log) then
         Text_IO.Put_Line (Generation_Log, Text);
         Text_IO.Flush (Generation_Log);
      end if;
   end Generation_Logging;

   ------------
   -- Server --
   ------------

   task body Server is
      Cfg : Config.Object;
   begin
      Config.Set.Server_Name    (Cfg, "check_mem");
      Config.Set.Server_Port    (Cfg, 0);
      Config.Set.Max_Connection (Cfg, 5);
      Config.Set.Security       (Cfg, True);
      Config.Set.Session        (Cfg, True);

      AWS.Net.SSL.Initialize
        (Config               => SSL_Srv,
         Certificate_Filename => Certificate_Name,
         Key_Filename         => Private_Key_Name,
         Session_Cache_Size   => 2);

      --  Session_Cache_Size  => 2 - limit session cache to avoid different
      --  cache container allocation in different execution.

      AWS.Server.Set_SSL_Config (HTTP, SSL_Srv);

      AWS.Server.Start (HTTP, CB'Unrestricted_Access, Cfg);

      Put_Line ("Server started");
      New_Line;

      accept Started;

      select
         accept Stopped;
      or
         delay 6.0 * Iteration;
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

   function SOAP_CB (Request : Status.Data) return Response.Data is
      use SOAP.Parameters;
      use SOAP.Types;

      P_Str        : aliased constant String := AWS.Status.Payload (Request);
      Payload      : constant SOAP.Message.Payload.Object
        := SOAP.Message.XML.Load_Payload (P_Str);

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
      package NSC renames Net.SSL.Certificate;
      use type NSC.Object;

      Connect : AWS.Client.HTTP_Connection;
      Session : Unbounded_String;
      Key     : Net.SSL.Private_Key := Net.SSL.Load (Private_Key_Name);
      Cert_F  : constant NSC.Object := NSC.Load (Certificate_Name);
      Cert_S  : NSC.Object;

      procedure Request (URL : String; Filename : String := "");

      procedure Request (Proc : String; X, Y : Integer);

      -------------
      -- Request --
      -------------

      procedure Request (URL : String; Filename : String := "") is
         use Ada.Streams;
         use type Net.SSL.Hash_Method;
         Answer : Response.Data;
         Result : Resources.File_Type;
         File   : Resources.File_Type;
         Data_R : Stream_Element_Array (1 .. 4096);
         Last_R : Stream_Element_Offset;
         Data_F : Stream_Element_Array (1 .. Data_R'Last);
         Last_F : Stream_Element_Offset;
      begin
         AWS.Client.Get (Connect, Answer, URL);

         Response.Message_Body (Answer, Result);

         if Filename /= "" then
            Resources.Open (File, Filename, "shared=no");
            Check (Filename);
         end if;

         loop
            Resources.Read (Result, Data_R, Last_R);

            if Filename = "" then
               if Last_R < 64 then
                  Check (Translator.To_String (Data_R (1 .. Last_R)));
               else
                  Check
                    (GNAT.MD5.Digest
                       (Net.SSL.Signature (Data_R (1 .. Last_R), Key, Hash)));

                  if Hash < Net.SSL.Hash_Method'Last then
                     Hash := Net.SSL.Hash_Method'Succ (Hash);
                  end if;
               end if;
            else
               Resources.Read (File, Data_F, Last_F);

               if Data_R (1 .. Last_R) /= Data_F (1 .. Last_F) then
                  Put_Line ("Responce error");
               end if;
            end if;

            exit when Last_R < Data_R'Last;
         end loop;

         if Filename /= "" then
            Resources.Close (File);
         end if;

         Resources.Close (Result);
      end Request;

      procedure Request (Proc : String; X, Y : Integer) is
         use SOAP.Types;
         use type SOAP.Parameters.List;

         P_Set   : constant SOAP.Parameters.List := +I (X, "x") & I (Y, "y");
         Payload : SOAP.Message.Payload.Object;
      begin
         Payload := SOAP.Message.Payload.Build (Proc, P_Set);

         declare
            Response : constant SOAP.Message.Response.Object'Class :=
                         SOAP.Client.Call (Connect, "/soap_demo", Payload);

            R_Parameters : constant SOAP.Parameters.List
              := SOAP.Message.Parameters (Response);

            Result : constant Integer
              := SOAP.Parameters.Get (R_Parameters, "result");
            Expect : Integer;
         begin
            if Proc = "multProc" then
               Expect := X * Y;
            elsif Proc = "addProc" then
               Expect := X + Y;
            else
               Expect := Integer'Last;
            end if;

            if Result /= Expect then
               Check ("!!! Error " & Proc & Result'Img & " /=" & Expect'Img);
            end if;
         end;
      end Request;

   begin
      AWS.Client.Create (Connect, AWS.Server.Status.Local_URL (HTTP));

      Cert_S := AWS.Client.Get_Certificate (Connect);
      Check (NSC.Subject (Cert_S));

      if Cert_S /= Cert_F then
         Check ("Wrong server certificate");
      end if;

      Request ("/simple");

      Session := To_Unbounded_String (AWS.Client.SSL_Session_Id (Connect));

      if Session = Null_Unbounded_String
        and then Ada.Strings.Fixed.Index
          (AWS.Client.Cipher_Description (Connect), "TLS1.3") = 0
      then
         Check ("!!! Empty SSL session.");
      end if;

      Request ("/simple?p1=8&p2=azerty%20qwerty");
      Request ("/simple?p2=8&p1=azerty%20qwerty");
      Request ("/doesnotexist?p=8");

      Request ("/complex?p1=1&p2=2&p3=3&p4=4&p5=5&p6=6"
                 & "&p7=7&p8=8&p9=9&p10=10&p11=11&p12=12&p13=13&p14=14&p15=15"
                 & "&very_long_name_in_a_get_form=alongvalueforthistest");

      Request ("/multiple?par=1&par=2&par=3&par=4&par=whatever");

      --  Make connection non persistent to check memory leak on session resume

      AWS.Client.Set_Persistent (Connect, False);

      Request ("/simple?p1=8&p2=azerty%20qwerty");
      Request ("/file", "check_mem.adb");
      Request ("/template");
      Request ("/no-template");
      Request ("/stream", "check_mem.adb");

      Request ("multProc", 2, 3);
      Request ("multProc", 98, 123);
      Request ("multProc", 5, 9);
      Request ("addProc", 2, 3);
      Request ("addProc", 98, 123);
      Request ("addProc", 5, 9);

      if To_String (Session) /= AWS.Client.SSL_Session_Id (Connect) then
         Check ("!!! SSL session changed.");
      end if;

      AWS.Client.Close (Connect);
      Net.SSL.Free (Key);
   end Client;

   ---------------------------
   -- Check_Dynamic_Message --
   ---------------------------

   procedure Check_Dynamic_Message (Encoding : Messages.Content_Encoding) is
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
         Data   : Streams.Stream_Element_Array);
      --  Append dynamically allocated data, test content and close the stream

      ----------
      -- Test --
      ----------

      procedure Test
        (Stream : in out Stream_Type'Class;
         Data   : Streams.Stream_Element_Array)
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
      Utils.Unchecked_Free (Packed);

      Test (Plain, Sample);
   end Check_Memory_Streams;

   -----------------------
   -- Check_Server_Push --
   -----------------------

   procedure Check_Server_Push is
      Connect : array (1 .. 8) of AWS.Client.HTTP_Connection;
      Answer  : AWS.Response.Data;
      Data    : Push_Data_Type;

      procedure Check_Data (Index : Positive; Sample : Push_Data_Type);

      ----------------
      -- Check_Data --
      ----------------

      procedure Check_Data (Index : Positive; Sample : Push_Data_Type) is
         SPD : constant String :=
                 AWS.Client.Read_Until (Connect (Index), CRLF);
      begin
         if Push_Data_Type'Value (SPD (SPD'First .. SPD'Last - 2))
            /= Sample
         then
            raise Constraint_Error with Sample'Img & " /= " & SPD;
         end if;
      end Check_Data;

   begin
      Data := 12345.67;

      --  Initialize all the push connections

      for J in Connect'Range loop
         AWS.Client.Create
           (Connection  => Connect (J),
            Host        => AWS.Server.Status.Local_URL (HTTP),
            Timeouts    => AWS.Client.Timeouts
              (Connect => 5.0,
               Send => 15.0, Receive => 15.0, Response => 15.0),
            Server_Push => True);

         AWS.Client.Get
           (Connect (J), Answer,
            "/server-push?picture=zzzz9.99&group=aa&group=bb&group=cc&group=id"
            & AWS.Utils.Image (J));
      end loop;

      for J in Connect'Range loop
         Check_Data (J, 76543.21);
      end loop;

      for J in Connect'Range loop
         Server_Push.Send
           (Push,
            Group_Id     => "id" & AWS.Utils.Image (J),
            Data         => Data + Push_Data_Type (J),
            Content_Type => "text/number");
      end loop;

      for J in Connect'Range loop
         Check_Data (J, Data + Push_Data_Type (J));
      end loop;

      declare
         task type Closer is
            entry Close (Index : Positive);
         end Closer;

         task body Closer is
            Index : Positive;
         begin
            accept Close (Index : Positive) do
               Closer.Index := Index;
            end Close;

            AWS.Client.Close (Connect (Index));
         end Closer;

         Closers : array (Connect'Range) of Closer;

      begin
         for J in Connect'Range loop
            Closers (J).Close (J);
         end loop;
      end;

      Server_Push.Send (Push, Data => Data, Content_Type => "text/plain");
   end Check_Server_Push;

   ----------------
   -- Check_Zlib --
   ----------------

   procedure Check_Zlib is

      use type Streams.Stream_Element_Array;

      procedure Test (Str : String);

      ----------
      -- Test --
      ----------

      procedure Test (Str : String) is
         Data   : constant Streams.Stream_Element_Array :=
                    Translator.To_Stream_Element_Array (Str);
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

         Utils.Unchecked_Free (Comp);
         Utils.Unchecked_Free (Decomp);
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

      T1 : Translate_Table :=
             (1 => Assoc ("ONE", "one"),
              2 => Assoc ("TWO", "2"),
              3 => Assoc ("THREE", "3"));

      T2 : Translate_Table :=
             (1 => Assoc ("V1", Vector_Tag'(+"t11" & "t12" & "t13")),
              2 => Assoc ("V2", Vector_Tag'(+"t21" & "t22" & "t23")));

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
      Local_URL : constant String := AWS.Server.Status.Local_URL (HTTP);
   begin
      R := AWS.Client.Get (Local_URL & "/filea.txt");
      R := AWS.Client.Get (Local_URL & "/fileb.txt");
      R := AWS.Client.Get (Local_URL & "/filec.txt");
   end Check_ZOpen;

   ---------------------
   -- Check_Reconnect --
   ---------------------

   procedure Check_Reconnect (Security : Boolean) is
      use Ada.Streams;
      use AWS.Net;
      N : constant := 2;
      Server : Std.Socket_Type;
      Peer   : Socket_Type'Class := Socket (Security);
      Buffer : Stream_Element_Array (1 .. 64);
      Last   : Stream_Element_Offset;

      task Connector is
         entry Start;
      end Connector;

      task body Connector is
         Client : Socket_Type'Class := Socket (Security);
      begin
         Set_Timeout (Client, 0.5);

         accept Start;

         if Security then
            Net.SSL.Socket_Type (Client).Set_Config (SSL_Clt);
         end if;

         for J in 1 .. N loop
            Connect (Client, Localhost (Server.Is_IPv6), Server.Get_Port);
            Send (Client, (1 .. 10 => 11));
            Receive (Client, Buffer, Last);
            Shutdown (Client);
         end loop;
      exception
         when E : others =>
            Put_Line
              ("On connect " & Ada.Exceptions.Exception_Information (E));
      end Connector;

   begin
      Std.Bind (Server, 0);
      Std.Listen (Server);

      Std.Set_Timeout (Server, 0.5);

      Connector.Start;

      if Security then
         Net.SSL.Socket_Type (Peer).Set_Config (SSL_Srv);
      end if;

      for J in 1 .. N loop
         Accept_Socket (Server, Peer);
         Receive (Peer, Buffer, Last);
         Send (Peer, Data => (1 .. 11 => 12));
         begin
            --  Wait for opposite shutdown
            Receive (Peer, Buffer, Last);
            raise Program_Error;
         exception
            when Socket_Error =>
               Shutdown (Peer);
         end;
      end loop;

   end Check_Reconnect;

   ----------------
   -- Check_SMTP --
   ----------------

   procedure Check_SMTP (Secure : Boolean) is
      From      : AWS.SMTP.E_Mail_Data;
      Recipient : AWS.SMTP.E_Mail_Data;
      Server    : AWS.SMTP.Receiver;
      Status    : AWS.SMTP.Status;

      SMTP_Host : constant String := "255.255.255.255";
   begin
      From := AWS.SMTP.E_Mail
        (Name    => "Pascal Obry",
         Address => "pascal@obry.net");

      Recipient := AWS.SMTP.E_Mail
        (Name    => "Somebody",
         Address => "somebody@obry.net");

      Server := AWS.SMTP.Client.Initialize
        (Server_Name => SMTP_Host,
         Secure      => Secure,
         Port        => 25);

      AWS.SMTP.Client.Send
        (Server  => Server,
         From    => From,
         To      => Recipient,
         Subject => "Test subject",
         Message => "Test message",
         Status  => Status);

      if AWS.SMTP.Is_Ok (Status) then
         Put_Line ("Status OK");
      end if;
   exception
      when others =>
         null;
   end Check_SMTP;

   ------------------
   -- Check_Socket --
   ------------------

   procedure Check_Socket is
   begin
      for K in 1 .. 10 loop
         declare
            S1 : Net.Std.Socket_Type;
            S2 : Net.SSL.Socket_Type;
            pragma Unreferenced (S1, S2);
         begin
            null;
         end;
      end loop;
   end Check_Socket;

   -----------
   -- Stamp --
   -----------

   procedure Stamp (Label : String) is
      use Calendar;
      Now : constant Time := Clock;
   begin
      if Ada.Command_Line.Argument_Count > 1 then
         Ada.Text_IO.Put_Line (Label & ' ' & Utils.Image (Now - Timestamp));
      end if;
      Timestamp := Now;
   end Stamp;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Data : Push_Data_Type;
      Env  : Text_IO.Editing.Picture) return Ada.Streams.Stream_Element_Array
   is
      package Format is new Text_IO.Editing.Decimal_Output (Push_Data_Type);
   begin
      return Translator.To_Stream_Element_Array
               (Format.Image (Data, Env) & CRLF);
   end To_Array;

begin
   Put_Line ("Start main, wait for server to start...");

   Iteration := Integer'Value (Command_Line.Argument (1));

   Server.Started;

   declare
      FN : constant String := "rsa-dh-generation.log";
   begin
      if AWS.Resources.Is_Regular_File (FN) then
         Text_IO.Open (Generation_Log, Text_IO.Append_File, FN);
         Text_IO.Put_Line (Generation_Log, "-- " & Net.SSL.Version & " --");
      end if;
   end;

   --  This is the main loop. Be sure to run everything inside this
   --  loop. Check_Mem is checked between 2 runs with a different number of
   --  iterations.

   for K in 1 ..  Iteration loop
      Net.SSL.Initialize (Config => SSL_Clt, Certificate_Filename => "");

      Timestamp := Calendar.Clock;
      Client;
      Stamp ("Client");
      Check_Zlib;
      Stamp ("ZLib");
      Check_Memory_Streams;
      Stamp ("Stream");
      Check_Dynamic_Message (Messages.Identity);
      Stamp ("Dymanic identity");
      Check_Dynamic_Message (Messages.Deflate);
      Stamp ("Dymanic deflate");
      Check_Transient;
      Stamp ("Transient");
      Check_Zopen;
      Stamp ("Zopen");
      Check_Socket;
      Stamp ("Socket");
      Check_Reconnect (False);
      Stamp ("Reconnect plain");
      Check_Reconnect (True);
      Stamp ("Reconnect SSL");
      Check_SMTP (False);
      Stamp ("SMTP plain");
      Check_SMTP (True);
      Stamp ("SMTP SSL");
      Check_Server_Push;
      Stamp ("Server push");

      --  Get the first generated DH and RSA parameters

      if DH_Time = AWS.Utils.AWS_Epoch then
         DH_Time := AWS.Net.SSL.Generated_Time_DH;
      end if;

      if RSA_Time = AWS.Utils.AWS_Epoch then
         RSA_Time := AWS.Net.SSL.Generated_Time_RSA;
      end if;

      AWS.Net.SSL.Start_Parameters_Generation
        (K rem 3 = 1, Generation_Logging'Access);

      Net.SSL.Release (Config => SSL_Clt);
   end loop;

   Server.Stopped;

   --  Clear session cache to normalize rest allocated memory

   AWS.Net.SSL.Clear_Session_Cache (SSL_Srv);

   for J in 1 .. 8 loop
      exit when DH_Time /= AWS.Net.SSL.Generated_Time_DH;
      delay 0.25;
   end loop;

   AWS.Net.Log.Start (Error => Error'Unrestricted_Access, Write => null);

   Net.SSL.Abort_DH_Generation;

   if RSA_Time = AWS.Net.SSL.Generated_Time_RSA then
      Check
        ("RSA only once generated at " & AWS.Messages.To_HTTP_Date (RSA_Time));
   end if;

   Generation_Logging ("-- end");

   Command_Line.Set_Exit_Status (Command_Line.Success);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));

      if not Server'Terminated then
         Server.Stopped;
      end if;

      Command_Line.Set_Exit_Status (Command_Line.Failure);
end Check_Mem;
