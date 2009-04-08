------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
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

--  Test XML parse on the fly from client connection program modified for get
--  XML source from AWS HTTP client connection.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with AWS.Client.XML.Input_Sources;
with AWS.Response.Set;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Readers;    use DOM.Readers;
with Sax.Readers;    use Sax.Readers;

with Unicode.CES;

procedure XMLT_Proc (Port : Positive; Security : Boolean) is

   use AWS;
   use AWS.Client.XML.Input_Sources;

   HTTP     : Client.HTTP_Connection;
   Web      : Server.HTTP;

   Dummy : AWS.Response.Data;

   Good_Name : constant String := "/good-doc.xml";
   Bad_Name  : constant String := "/bad-doc.xml";

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Answer : Response.Data;
      URI    : constant String := Status.URI (Request);
   begin
      Response.Set.Append_Body
        (Answer, "<?xml version=""1.0""?>" & ASCII.LF
          & "<!DOCTYPE xmlt [" & ASCII.LF
          & "<!ENTITY standalone ""スタンドアロン"">" & ASCII.LF
          & "<!ENTITY subset     ""サブセット"">" & ASCII.LF
          & "<!ENTITY processor  ""プロセサ"">" & ASCII.LF
          & "]>" & ASCII.LF
          & "<xmlt attrib1=""first"" attrib2=""second"">" & ASCII.LF
          & "<head>文書</head>" & ASCII.LF);

      for J in 1 .. 100 loop
         Response.Set.Append_Body
           (Answer,
            "<p>&standalone;文書宣言においては"
            & ", ""<code>yes</code>"""
            & "の値は，<termref def='dt-docent'>" & ASCII.LF
            & "文書実体</termref>" & ASCII.LF
            & "の外部に（DTDの外部"
            & "&subset;内に，又は内部&subset;" & ASCII.LF
            & "から参照される外部"
            & "パラメタ実体内に），XML"
            & "&processor;から</p>" & ASCII.LF);
      end loop;

      if URI = Good_Name then
         Response.Set.Append_Body (Answer, "</xmlt>");
      else
         --  Force encoding error

         Response.Set.Append_Body (Answer, "�");
      end if;

      return Answer;
   end CB;

   ---------------
   -- Test_Name --
   ---------------

   procedure Test_Name (Name : String) is
      Read           : HTTP_Input;
      My_Tree_Reader : Tree_Reader;
   begin
      AWS.Client.Get (HTTP, Dummy, Name);

      Set_Public_Id (Read, Name);

      Create (HTTP, Read);

      --  Full name is used as the system id
      Set_System_Id (Read, Name);

      Parse (My_Tree_Reader, Read);

      Ada.Text_IO.Put_Line (Name & " parsed.");

      Close (Read);

      Normalize (Get_Tree (My_Tree_Reader));

      Free (My_Tree_Reader);

      Ada.Text_IO.Put_Line ("---------------------------");

   exception
      when E : others =>
         Close (Read);
         Free (My_Tree_Reader);

         raise;
   end Test_Name;

   ---------
   -- URL --
   ---------

   function URL return String is
      Draft : constant String := "://localhost:" & AWS.Utils.Image (Port);
   begin
      if Security then
         return "https" & Draft;
      else
         return "http" & Draft;
      end if;
   end URL;

begin
   --  Parse the command line

   Server.Start
     (Web,
      "XML output",
      CB'Unrestricted_Access,
      Port           => Port,
      Security       => Security,
      Max_Connection => 3);

   Client.Create
     (HTTP,
      Host        => URL,
      Server_Push => True,
      Persistent  => True,
      Timeouts    => Client.Timeouts
        (Connect => 1.0, Send => 12.0, Receive => 12.0, Response => 12.0));

   Test_Name (Good_Name);

   begin
      Test_Name (Bad_Name);
   exception
      when Unicode.CES.Invalid_Encoding | Sax.Readers.XML_Fatal_Error =>
         Put_Line ("Expected error.");
   end;

   delay 0.5;

   Client.Close (HTTP);

   Server.Shutdown (Web);

exception
   when E : others =>
      Put_Line ("Main: " & Exception_Information (E));

      Server.Shutdown (Web);
end XMLT_Proc;
