--  Test XML parse on the fly from client connection program modified for get
--  XML source from AWS HTTP client connection.

--  $Id$

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

   With_URI : Boolean := False;
   HTTP     : Client.HTTP_Connection;
   Web      : Server.HTTP;

   Dummy : AWS.Response.Data;

   Good_Name : constant String := "/good-doc.xml";
   Bad_Name  : constant String := "/bad-doc.xml";

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
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
         --  Forse encoding error.

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
      AWS.Client.Close (HTTP);

      Normalize (Get_Tree (My_Tree_Reader));

      Free (My_Tree_Reader);

      Ada.Text_IO.Put_Line ("---------------------------");

   exception
      when E : others =>
         Close (Read);
         AWS.Client.Close (HTTP);
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
      Timeouts    => (12, 12));

   Test_Name (Good_Name);

   begin
      Test_Name (Bad_Name);
   exception
      when Unicode.CES.Invalid_Encoding =>
         Put_Line ("Expected error.");
   end;

   delay 0.5;

   Server.Shutdown (Web);

exception
   when E : others =>
      Put_Line ("Main: " & Exception_Information (E));

      Server.Shutdown (Web);
end XMLT_Proc;
