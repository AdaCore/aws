--  TestXML program modified for get XML source from AWS HTTP client
--  connection.

with GNAT.Command_Line;  use GNAT.Command_Line;
with DOM.Readers;        use DOM.Readers;
with Sax.Readers;        use Sax.Readers;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Text_IO;        use Ada.Text_IO;

with AWS.Client.XML.Input_Sources;
with AWS.Response;

procedure Testxml is
   use AWS.Client.XML.Input_Sources;

   Silent : Boolean := False;
   With_URI : Boolean := False;
   HTTP : AWS.Client.HTTP_Connection;
   Read : HTTP_Input;
   My_Tree_Reader : Tree_Reader;
   Name_Start : Natural;
   Validate : Boolean := False;
   Must_Normalize : Boolean := False;

   Dummy : AWS.Response.Data;

begin
   --  Parse the command line
   loop
      case Getopt ("silent uri normalize validate") is
         when ASCII.Nul => exit;

         when 's' => Silent := True;
         when 'u' => With_URI := True;
         when 'v' => Validate := True;
         when 'n' => Must_Normalize := True;

         when others => null;
      end case;
   end loop;

   declare
      S : constant String := Get_Argument;
   begin
      if S'Length > 0 then
         AWS.Client.Create
           (HTTP,
            Host        => S,
            Server_Push => True,
            Persistent  => False);

         AWS.Client.Get (HTTP, Dummy);

         --  Base file name should be used as the public Id
         Name_Start := S'Last;

         while Name_Start >= S'First  and then S (Name_Start) /= '/' loop
            Name_Start := Name_Start - 1;
         end loop;

         Set_Public_Id (Read, S (Name_Start + 1 .. S'Last));

         Create (HTTP, Read);

         --  Full name is used as the system id
         Set_System_Id (Read, S);

      else
         Put_Line ("define URL of XML file in command line.");
      end if;
   end;

   Set_Feature (My_Tree_Reader, Validation_Feature, Validate);

   Parse (My_Tree_Reader, Read);
   Close (Read);
   AWS.Client.Close (HTTP);

   if Must_Normalize then
      Normalize (Get_Tree (My_Tree_Reader));
   end if;

   if not Silent then
      Print (Get_Tree (My_Tree_Reader),
             Print_Comments => False,
             Print_XML_PI => False,
             With_URI => With_URI);
   end if;

   Free (My_Tree_Reader);

exception
   when E : XML_Fatal_Error =>
      Close (Read);
      Put_Line (Exception_Message (E));
      Free (My_Tree_Reader);
end Testxml;
