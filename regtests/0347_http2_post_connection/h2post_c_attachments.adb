------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Attachments;
with AWS.Client;
with AWS.Config.Set;
with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure H2Post_C_Attachments is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use AWS;

   Data_Name : constant String := "big-data";

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Params  : Parameters.List;
      Attachs : Attachments.List;

      procedure Output_A_Name (E : Attachments.Element);

      -------------------
      -- Output_A_Name --
      -------------------

      procedure Output_A_Name (E : Attachments.Element) is
         use Strings.Unbounded;

         L_Filename : Unbounded_String :=
                        To_Unbounded_String (Attachments.Local_Filename (E));
         C          : constant Natural :=
                        Strings.Unbounded.Index (L_Filename, "-");
      begin
         Text_IO.Put_Line ("Filename       " & Attachments.Filename (E));

         --  Convert slashes

         Strings.Unbounded.Translate
           (L_Filename, Strings.Maps.To_Mapping ("\", "/"));

         --  Now remove the pid

         Strings.Unbounded.Replace_Slice (L_Filename, 3, C, "XpidX-");

         Text_IO.Put_Line ("Local Filename " & To_String (L_Filename));
      end Output_A_Name;

   begin
      if not Status.Is_Body_Uploaded (Request) then
         return Response.Continue;
      end if;

      Params  := Status.Parameters (Request);
      Attachs := Status.Attachments (Request);

      Text_IO.Put_Line ("*** ID =" & Params.Get ("ID"));
      Text_IO.Put_Line
        ("*** N Attachments = " & Natural'Image (Attachments.Count (Attachs)));

      Attachments.Iterate (Attachs, Output_A_Name'Access);

      if Params.Exist (Data_Name) then
         Text_IO.Put_Line
           (Data_Name & " size"
            & Length (Params.Get_Values (Data_Name) (1))'Img & " bytes");
      end if;

      return Response.Build (MIME.Text_HTML, "ok");
   exception
      when others =>
         Text_IO.Put_Line ("Exception on CB!");
         return Response.Build (MIME.Text_HTML, "nok");
   end CB;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset)
   is
      use type Net.Log.Data_Direction;
   begin
      if Direction = Net.Log.Sent then
         Text_IO.Put_Line
           ("********** " & Net.Log.Data_Direction'Image (Direction));
         Text_IO.Put_Line
           (Translator.To_String (Data (Data'First .. Last)));
         Text_IO.New_Line;
      end if;
   end Dump;

   CNF         : Config.Object;
   WS          : Server.HTTP;
   WC          : Client.HTTP_Connection;
   R           : Response.Data;
   Attachments : AWS.Attachments.List;

begin
   Config.Set.Server_Name (CNF, "H2 Post Attachments");
   Config.Set.Server_Port (CNF, 0);
   Config.Set.Max_Connection (CNF, 5);
   Config.Set.Upload_Directory (CNF, ".");
   Config.Set.HTTP2_Activated (CNF, True);

   AWS.Attachments.Add
     (Attachments => Attachments,
      Filename    => "file1.dat",
      Headers     => AWS.Headers.Empty_List);
   AWS.Attachments.Add
     (Attachments => Attachments,
      Filename    => "file2.dat",
      Headers     => AWS.Headers.Empty_List);

   Server.Start (WS, CB'Unrestricted_Access, CNF);

   Server.Log.Start_Error (WS);

   Text_IO.Put_Line ("started");
   Text_IO.Flush;
   Text_IO.New_Line;

--   AWS.Net.Log.Start (Dump'Unrestricted_Access);

   Client.Create (WC, Server.Status.Local_URL (WS), HTTP_Version => HTTPv2);

   AWS.Client.Post
     (WC, R, URI => "/Upload", Data => "ID=100");
   Text_IO.Put_Line ("R-POST1 : " & Response.Message_Body (R));
   Text_IO.New_Line;

   AWS.Client.Post
     (WC, R, URI => "/Upload", Data => "ID=101",
      Content_Type => AWS.MIME.Application_Form_Data,
      Attachments  => Attachments);
   Text_IO.Put_Line ("R-POST2 : " & Response.Message_Body (R));
   Text_IO.New_Line;

   AWS.Client.Post
     (WC, R, URI => "/Upload?ID=102",
      Data         => AWS.Client.No_Data,
      Content_Type => AWS.MIME.Application_Form_Data,
      Attachments  => Attachments);
   Text_IO.Put_Line ("R-POST3 : " & Response.Message_Body (R));
   Text_IO.New_Line;

    AWS.Client.Post
      (WC, R, URI => "/Upload?ID=103", Data => "AnyOldString",
       Content_Type => AWS.Client.No_Data,
       Attachments  => Attachments);
    Text_IO.Put_Line ("R-POST4 : " & Response.Message_Body (R));
    Text_IO.New_Line;

    declare
       Content : Unbounded_String;
       Sample  : constant String :=
                   "1234567890qwertyuioplkjhgfdszxcvbnmMNBVCXZASDFGHJKLPOIUYTW";
       Headers : AWS.Headers.List;
    begin
       for J in 10_001 .. 95_000 loop
          Append (Content, Sample & J'Img & ASCII.LF);
       end loop;

       Headers.Add
         (AWS.Messages.Content_Disposition_Token, "name=""" & Data_Name & '"');
       --  !!! No name for Data_Name parameter in callback without this header.
       --  Maybe need some fix.

       Attachments.Add
         (Name    => Data_Name,
          Data    => AWS.Attachments.Value (Content),
          Headers => Headers);
    end;

    AWS.Client.Post
      (WC, R, URI => "/Upload", Data => "ID=104",
       Content_Type => AWS.MIME.Application_Form_Data,
       Attachments  => Attachments);
    Text_IO.Put_Line ("R-POST5 : " & Response.Message_Body (R));
    Text_IO.New_Line;

   Client.Close (WC);
   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end H2Post_C_Attachments;
