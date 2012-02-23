------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure TGetParam_Proc (Security : Boolean) is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP : AWS.Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/simple" then
         Put_Line ("N  = " & Natural'Image (Parameters.Count (P_List)));
         Put_Line ("p1 = " & Parameters.Get (P_List, "p1"));
         Put_Line ("p2 = " & Parameters.Get (P_List, "p2"));

         return Response.Build (MIME.Text_HTML, "simple ok");

      elsif URI = "/complex" then
         Put_Line ("N  = " & Natural'Image (Parameters.Count (P_List)));
         Put_Line ("p1 = " & Parameters.Get (P_List, "p1"));
         Put_Line ("p2 = " & Parameters.Get (P_List, "p2"));

         for K in 1 .. Parameters.Count (P_List) loop
            Put_Line (Integer'Image (K)
                        & " name  = " & Parameters.Get_Name (P_List, K));
            Put_Line (Integer'Image (K)
                        & " value = " & Parameters.Get_Value (P_List, K));
         end loop;

         return Response.Build (MIME.Text_HTML, "complex ok");

      elsif URI = "/multiple" then
         Put_Line ("N   = " & Natural'Image (Parameters.Count (P_List)));
         Put_Line ("par = " & Parameters.Get (P_List, "par", 1));
         Put_Line ("par = " & Parameters.Get (P_List, "par", 2));
         Put_Line ("par = " & Parameters.Get (P_List, "par", 3));
         Put_Line ("par = " & Parameters.Get (P_List, "par", 4));
         Put_Line ("par = " & Parameters.Get (P_List, "par", 5));

         return Response.Build (MIME.Text_HTML, "multiple ok");
      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String) is
      R : Response.Data;
   begin
      R := Client.Get (URL);
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   AWS.Server.Start
     (HTTP, "TGetParam",
      CB'Unrestricted_Access,
      Port           => 0,
      Security       => Security,
      Max_Connection => 5);

   Put_Line ("Server started");
   New_Line;

   declare
      URL_Prefix : constant String := AWS.Server.Status.Local_URL (HTTP);
   begin
      Request (URL_Prefix & "/simple");
      Request (URL_Prefix & "/simple?p1=8&p2=azerty%20qwerty");
      Request (URL_Prefix & "/simple?p2=8&p1=azerty%20qwerty");
      Request (URL_Prefix & "/doesnotexist?p=8");

      Request (URL_Prefix & "/complex?p1=1&p2=2&p3=3&p4=4&p5=5&p6=6"
               & "&p7=7&p8=8&p9=9&p10=10&p11=11&p12=12&p13=13&p14=14&p15=15"
               & "&very_long_name_in_a_get_form=alongvalueforthistest");

      Request (URL_Prefix & "/multiple?" &
               "par=1&par=2&par=3&par=4&par=whatever");

      Request (URL_Prefix & "/simple?p1=8&p2=azerty%20qwerty");
   end;

   AWS.Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
   AWS.Server.Shutdown (HTTP);
end TGetParam_Proc;
