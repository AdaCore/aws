
--  $Id$

--  See Sock. This could be used as a regression test when bug showed by
--  sock.adb will be fixed.

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Server;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Client;
with AWS.Parameters;

procedure Hload is

   use Ada;
   use Ada.Strings.Unbounded;

   Max_Client : constant := 2;

   function Image (K : in Positive) return String;

   function Image (K : in Positive) return String is
      K_Img : constant String := Positive'Image (K);
   begin
      return K_Img (K_Img'First + 1 .. K_Img'Last);
   end Image;

   task type Client is
      entry Start (Name : in String);
      entry Stop;
   end Client;

   task body Client is
      Name : Unbounded_String;
   begin
      accept Start (Name : in String) do
         Client.Name := To_Unbounded_String (Name);
      end Start;

      for K in 1 .. 3000 loop
         begin
            declare
               K_Img : constant String := Image (K);
               R : AWS.Response.Data
                 := AWS.Client.Get
                 ("http://localhost:1234/toto?PARAM=" & K_Img);
--               Expected : constant String := "Data:" & K_Img;
               Expected : constant String := "0123456789";
            begin
               if not (Expected = String'(AWS.Response.Message_Body (R))) then
                  Text_IO.Put_Line
                    ("nok " & K_Img &
                     "expected " & Expected
                     & " -> found " & String'(AWS.Response.Message_Body (R)));
               end if;
            end;
         exception
            when E : others =>
               Text_IO.Put_Line
                 ("client " & To_String (Name)
                  & " request " & Image (K) & " aborted.");
               Text_IO.Put_Line
                 (" => " & Exceptions.Exception_Information (E));
         end;
      end loop;

      accept Stop;

      Text_IO.Put_Line ("client " & To_String (Name) & " stopped.");

   end Client;

   use AWS;

   WS : Server.HTTP;

   function CB (Request : in Status.Data) return Response.Data is
      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      return Response.Build
        (MIME.Text_HTML, "Data:" & AWS.Parameters.Get (P, "PARAM"));
   end CB;

   Clients : array (1 .. Max_Client) of Client;

begin
   Server.Start
     (WS, "Heavy Loaded",
      CB'Unrestricted_Access,
      Port           => 1234,
      Max_Connection => 1,
      Session        => True);

   Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   for K in Clients'Range loop
      Clients (K).Start ("client" & Image (K));
   end loop;

   for K in Clients'Range loop
      Clients (K).Stop;
   end loop;

   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");
end Hload;
