------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2007                            --
--                                 AdaCore                                  --
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

--  This tools is to help tracking down unused or undefined entities in Ajax
--  Web development as it is very difficult to ensure that a referenced Id does
--  actually exist in Web pages for example.

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;

with AWS.Utils;

procedure Webxref is

   use Ada;
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use AWS;

   Syntax_Error : exception;

   Version         : constant String := "0.9 (beta)";

   Max_Line_Length : constant := 1_024;

   --  Reader record is the file iterator context

   type Reader is record
      File    : Text_IO.File_Type;
      Line    : Natural;
      Content : String (1 .. Max_Line_Length);
      Last    : Natural;
      Buffer  : String (1 .. Max_Line_Length * 5);
      Blast   : Natural;
   end record;

   procedure Open (Filename : in String; Iterator : in out Reader);
   --  Open filename and initialize the file iterator

   procedure Close (Iterator : in out Reader);
   --  Close file iterator

   procedure Next (Iterator : in out Reader);
   --  Read next line

   procedure Clear_Content (Iterator : in out Reader);
   --  Clear content from the iterator context

   function Index
     (Iterator : in Reader;
      Pattern  : in String;
      From     : in Positive := 1) return Natural;
   --  Returns the position of Pattern starting at From in iterator content

   function Eof (Iterator : in Reader) return Boolean;
   --  Returns True if end of file reached

   function Is_Ignored (Name : in String) return Boolean;
   --  Returns True if this name is to be ignored

   type Name_Kind is
     (Def_CSS,  -- a definition in a CSS
      Ref_CSS,  -- a reference to a CSS definition (in HTML, XML... documents)
      Def_ML,   -- a definition in an HTML, XML... document
      Ref_ML);  -- a reference to an HTML definition (Found in Ajax response)

   type Kind_Set is array (Name_Kind) of Boolean;

   Null_Kind_Set : constant Kind_Set := (others => False);

   CSS_Kind            : constant Kind_Set :=
                           (Def_CSS => True, others => False);

   Web_Kind            : constant Kind_Set :=
                           (Ref_CSS | Def_ML => True, others => False);

   Ajax_Response_Kind  : constant Kind_Set :=
                           (Ref_ML => True, others => False);

   Def_Kind            : constant Kind_Set :=
                           (Def_CSS | Def_ML => True, others => False);
   Unused_Kind         : constant Kind_Set := Def_Kind;

   Ref_Kind            : constant Kind_Set :=
                           (Ref_CSS | Ref_ML => True, others => False);

   No_Kind             : constant Kind_Set := (others => False);
   All_Kind            : constant Kind_Set := (others => True);

   procedure Process_CSS (Filename : in String);
   --  Process a CSS file

   procedure Process_ML (Filename : in String; Kinds : in Kind_Set);
   --  Process a Meta Language file HTML, XML

   procedure Process (Filename : in String);
   --  Process any file, dispatch to Process_ML or Process_CSS depending on the
   --  file extension.

   type Occurence is record
      Filename     : Unbounded_String;
      Line, Column : Natural;
      Kind         : Kind_Set;
   end record;

   package Occurences is
     new Containers.Indefinite_Vectors (Natural, Occurence);

   procedure Parse_Command_Line;

   --  Options

   type Mode is (Xref, Unused, Undefined);

   Check        : Mode := Xref;
   Dump_Classes : Boolean := True;
   Dump_Ids     : Boolean := True;
   Verbose      : Boolean := False;

   generic
   package Dict is

      type Id is new Positive;

      function Get (Name : in String) return Id;
      --  Returns a uniq Id for Name

      procedure Add (Id  : in Dict.Id; Occ : in Occurence);
      --  Adds given occurence for Id

      function Get (Id : in Dict.Id) return String;
      --  Returns Id's name

      type Node is record
         Name  : Unbounded_String;
         Kinds : Kind_Set;
         Occ   : Occurences.Vector;
      end record;

      function Get (Id : in Dict.Id) return Node;
      --  Returns node for the given Id

      procedure Dump (Filter : in Kind_Set);
      --  Dump dictionary content on the console

   end Dict;

   -------------------
   -- Clear_Content --
   -------------------

   procedure Clear_Content (Iterator : in out Reader) is
   begin
      Iterator.Last := 0;
      Iterator.Blast := 0;
   end Clear_Content;

   -----------
   -- Close --
   -----------

   procedure Close (Iterator : in out Reader) is
   begin
      Text_IO.Close (Iterator.File);
      Clear_Content (Iterator);
   end Close;

   ----------
   -- Dict --
   ----------

   package body Dict is

      package Name_Id is
        new Containers.Indefinite_Hashed_Maps (String, Id, Strings.Hash, "=");

      package Id_Name is
        new Containers.Indefinite_Vectors (Id, Node);

      NId : Name_Id.Map;
      IdN : Id_Name.Vector;

      Current : Id := 1;

      ---------
      -- Add --
      ---------

      procedure Add
        (Id  : in Dict.Id;
         Occ : in Occurence)
      is
         procedure Process (Element : in out Node);

         -------------
         -- Process --
         -------------

         procedure Process (Element : in out Node) is
         begin
            Element.Occ.Append (Occ);
            Element.Kinds := Element.Kinds or Occ.Kind;
         end Process;

      begin
         IdN.Update_Element (Id, Process'Access);
      end Add;

      ----------
      -- Dump --
      ----------

      procedure Dump (Filter : in Kind_Set) is

         procedure Dump (Position : in Id_Name.Cursor);
         --  Dump name and iterates through all occurences

         ----------
         -- Dump --
         ----------

         procedure Dump (Position : in Id_Name.Cursor) is

            procedure Dump (Position : in Occurences.Cursor);
            --  Dump the pointed occurence

            ----------
            -- Dump --
            ----------

            procedure Dump (Position : in Occurences.Cursor) is
            begin
               declare
                  O : constant Occurence := Occurences.Element (Position);
               begin
                  Text_IO.Put ("   ");
                  if (O.Kind and Def_Kind) /= No_Kind then
                     Text_IO.Put ("<def> ");
                  end if;

                  if (O.Kind and Ref_Kind) /= No_Kind then
                     Text_IO.Put ("<ref> ");
                  end if;

                  Text_IO.Put_Line
                    (To_String (O.Filename) & ":"
                     & Utils.Image (O.Line) & ":" & Utils.Image (O.Column));
               end;
            end Dump;

            Element : constant Node := Id_Name.Element (Position);

         begin
            if (Filter and Element.Kinds) = Element.Kinds then
               Text_IO.New_Line;
               if Verbose then
                  for K in Name_Kind loop
                     if Element.Kinds (K) then
                        Text_IO.Put (Name_Kind'Image (K) & " ");
                     end if;
                  end loop;
               end if;

               Text_IO.Put_Line (To_String (Element.Name));

               Element.Occ.Iterate (Dump'Access);
            end if;
         end Dump;

      begin
         IdN.Iterate (Dump'Access);
      end Dump;

      ---------
      -- Get --
      ---------

      function Get (Name : in String) return Id is
      begin
         if NId.Contains (Name) then
            return NId.Element (Name);

         else
            NId.Insert (Name, Current);
            IdN.Append
              (Node'(To_Unbounded_String (Name), Null_Kind_Set, Occ => <>));
            Current := Current + 1;
            return Current - 1;
         end if;
      end Get;

      function Get (Id : in Dict.Id) return String is
      begin
         if Natural (IdN.Length) >= Natural (Id) then
            return To_String (IdN.Element (Id).Name);
         else
            raise Constraint_Error with "Id not found.";
         end if;
      end Get;

      function Get (Id : in Dict.Id) return Node is
      begin
         if Natural (IdN.Length) >= Natural (Id) then
            return IdN.Element (Id);
         else
            raise Constraint_Error with "Id not found.";
         end if;
      end Get;

   end Dict;

   package Class_Dict is new Dict;

   package Id_Dict is new Dict;

   ---------
   -- Eof --
   ---------

   function Eof (Iterator : in Reader) return Boolean is
   begin
      return Text_IO.End_Of_File (Iterator.File);
   end Eof;

   -----------
   -- Index --
   -----------

   function Index
     (Iterator : in Reader;
      Pattern  : in String;
      From     : in Positive := 1) return Natural is
   begin
      return Fixed.Index
        (Iterator.Content (From .. Iterator.Last), Pattern);
   end Index;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Name : in String) return Boolean is
      use type Strings.Maps.Character_Set;
   begin
      --  Ignore if it is a number
      return Strings.Fixed.Index
        (Name, not Strings.Maps.Constants.Decimal_Digit_Set) = 0;
   end Is_Ignored;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Reader) is
   begin
      Text_IO.Get_Line (Iterator.File, Iterator.Content, Iterator.Last);
      Iterator.Buffer
        (Iterator.Blast + 1 .. Iterator.Blast + Iterator.Last) :=
        Iterator.Content (1 .. Iterator.Last);
      Iterator.Blast := Iterator.Blast + Iterator.Last;
      Iterator.Line := Iterator.Line + 1;
   end Next;

   ----------
   -- Open --
   ----------

   procedure Open (Filename : in String; Iterator : in out Reader) is
   begin
      Text_IO.Open (Iterator.File, Text_IO.In_File, Filename);
      Clear_Content (Iterator);
      Iterator.Line := 0;
   end Open;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
      GNAT.Command_Line.Initialize_Option_Scan
        (Stop_At_First_Non_Switch => True);

      loop
         case GNAT.Command_Line.Getopt ("x u d c C i I v h") is
            when ASCII.NUL =>
               exit;

            when 'u' =>
               Check := Unused;

            when 'x' =>
               Check := Xref;

            when 'd' =>
               Check := Undefined;

            when 'c' =>
               Dump_Classes := True;

            when 'C' =>
               Dump_Classes := False;

            when 'i' =>
               Dump_Ids := True;

            when 'I' =>
               Dump_Ids := False;

            when 'v' =>
               Verbose := True;

            when 'h' =>
               raise Syntax_Error;

            when others =>
               raise Syntax_Error;
         end case;
      end loop;
   end Parse_Command_Line;

   -------------
   -- Process --
   -------------

   procedure Process (Filename : in String) is
      Ext : constant String :=
              Characters.Handling.To_Lower (Directories.Extension (Filename));
   begin
      if Verbose then
         Text_IO.Put_Line ("Process file : " & Filename & " as " & Ext);
      end if;

      if Ext = "css" or else Ext = "tcss" then
         Process_CSS (Filename);

      elsif Ext = "xml" or else Ext = "html" or else Ext = "thtml" then
         Process_ML (Filename, Web_Kind);

      elsif Ext = "txml" then
         Process_ML (Filename, Ajax_Response_Kind);
      end if;
   end Process;

   -----------------
   -- Process_CSS --
   -----------------

   procedure Process_CSS (Filename : in String) is

      Seps : constant Maps.Character_Set := Maps.To_Set (" ,{");

      procedure Register_CSS_Path (Iterator : in Reader; Pos : in Natural);
      --  Parse and register a CSS path, we just want to extract the classes
      --  and ids.

      -----------------------
      -- Register_CSS_Path --
      -----------------------

      procedure Register_CSS_Path (Iterator : in Reader; Pos : in Natural) is
         Path : constant String :=
                  Iterator.Buffer (1 .. Iterator.Blast - Iterator.Last + Pos);
         S, E : Natural := 1;
      begin
         --  First look for classes
         loop
            S := Fixed.Index (Path, ".", S);
            exit when S = 0;

            E := Fixed.Index (Path, Seps, S);

            if E = 0 then
               E := Path'Last + 1;
            end if;

            declare
               Name : constant String := Path (S + 1 .. E - 1);
               Id   : constant Class_Dict.Id := Class_Dict.Get (Name);
            begin
               Class_Dict.Add
                 (Id, Occurence'(To_Unbounded_String (Filename),
                  Iterator.Line, S + 1, CSS_Kind));
            end;

            S := E + 1;
         end loop;

         --  First look for ids

         S := 1;
         E := 1;

         loop
            S := Fixed.Index (Path, "#", S);
            exit when S = 0;

            E := Fixed.Index (Path, Seps, S);

            if E = 0 then
               E := Path'Last + 1;
            end if;

            declare
               Name : constant String := Path (S + 1 .. E - 1);
               Id   : constant Id_Dict.Id := Id_Dict.Get (Name);
            begin
               Id_Dict.Add
                 (Id, Occurence'(To_Unbounded_String (Filename),
                  Iterator.Line, S + 1, CSS_Kind));
            end;

            S := E + 1;
         end loop;
      end Register_CSS_Path;

      Iterator : Reader;
      Pos      : Natural;
   begin
      Open (Filename, Iterator);

      loop
         Next (Iterator);

         if Iterator.Last = 0 then
            Clear_Content (Iterator);

         else
            Pos := Index (Iterator, "{");

            if Pos /= 0 then
               Register_CSS_Path (Iterator, Pos);
            end if;

            if Index (Iterator, "}") /= 0 then
               Clear_Content (Iterator);
            end if;
         end if;

         exit when Eof (Iterator);
      end loop;

      Close (Iterator);
   end Process_CSS;

   ----------------
   -- Process_ML --
   ----------------

   procedure Process_ML (Filename : in String; Kinds : in Kind_Set) is

      procedure Find
        (Iterator  : in Reader;
         Attribute : in String;
         Process   : access
           procedure (Iterator : in Reader;
                      Name     : in String;
                      Column   : in Positive));
      --  Call process for value of the given attribute name

      procedure Process_Id
        (Iterator : in Reader; Name : in String; Column : in Positive);

      procedure Process_Class
        (Iterator : in Reader; Name : in String; Column : in Positive);

      procedure Check_Include (Iterator : in Reader);
      --  Check template include. We want here to include as id reference all
      --  parameters to the aws_*.tjs includes.

      -------------------
      -- Check_Include --
      -------------------

      procedure Check_Include (Iterator : in Reader) is
         use type Strings.Maps.Character_Set;
         Blank      : constant Strings.Maps.Character_Set :=
                        Strings.Maps.To_Set (" " & ASCII.HT);
         Identifier : constant Strings.Maps.Character_Set :=
                        Strings.Maps.Constants.Alphanumeric_Set
                          or Strings.Maps.To_Set ("_");
         First, Last : Natural := 1;
      begin
         First := Index (Iterator, "@@INCLUDE@@");

         if First /= 0 then
            First := Strings.Fixed.Index
              (Iterator.Content (First .. Iterator.Last), Blank);

            First := Strings.Fixed.Index
              (Iterator.Content (First .. Iterator.Last), not Blank);

            Last := Strings.Fixed.Index
              (Iterator.Content (First .. Iterator.Last), Blank);

            if Last = 0 then
               --  Last is end-of-line
               Last := Iterator.Last;
            else
               Last := Last - 1;
            end if;

            --  First .. Last is filename

            declare
               I_Filename : constant String :=
                              Iterator.Content (First .. Last);
               Is_First   : Boolean := True;
            begin
               --  Check only aws_*.tjs
               if Strings.Fixed.Index (I_Filename, "aws_") /= 0
                 and then Directories.Extension (I_Filename) = "tjs"
               then
                  loop
                     First := Last + 1;

                     Strings.Fixed.Find_Token
                       (Iterator.Content (First .. Iterator.Last),
                        Identifier,
                        Test  => Strings.Inside,
                        First => First,
                        Last  => Last);
                     exit when Last = 0;

                     if Is_First then
                        Is_First := False;

                     elsif not Is_Ignored
                       (Iterator.Content (First .. Last))
                     then
                        declare
                           Id : constant Id_Dict.Id := Id_Dict.Get
                             (Iterator.Content (First .. Last));
                        begin
                           Id_Dict.Add
                             (Id,
                              (To_Unbounded_String (Filename),
                               Iterator.Line,
                               First,
                               Ajax_Response_Kind));
                        end;
                     end if;
                  end loop;
               end if;
            end;
         end if;
      end Check_Include;

      ----------
      -- Find --
      ----------

      procedure Find
        (Iterator  : in Reader;
         Attribute : in String;
         Process   : access
           procedure (Iterator : in Reader;
                      Name     : in String;
                      Column   : in Positive))
      is
         procedure Check (C : in Character);

         -----------
         -- Check --
         -----------

         procedure Check (C : in Character) is
            First, Last : Natural := 1;
         begin
            loop
               First := Index (Iterator, Attribute & '=' & C, First);
               exit when First = 0;

               Last := Index
                 (Iterator, String'(1 => C), First + Attribute'Length + 2);

               if Last = 0 then
                  First := Iterator.Last;

               else
                  Process
                    (Iterator,
                     Iterator.Content
                       (First + Attribute'Length + 2 .. Last - 1),
                     First + Attribute'Length);
                  First := Last + 1;
               end if;
            end loop;
         end Check;

      begin
         Check (''');
         Check ('"');
      end Find;

      -------------------
      -- Process_Class --
      -------------------

      procedure Process_Class
        (Iterator : in Reader; Name : in String; Column : in Positive)
      is
         Id : constant Class_Dict.Id := Class_Dict.Get (Name);
      begin
         Class_Dict.Add
           (Id,
            (To_Unbounded_String (Filename), Iterator.Line, Column, Kinds));
      end Process_Class;

      ----------------
      -- Process_Id --
      ----------------

      procedure Process_Id
        (Iterator : in Reader; Name : in String; Column : in Positive)
      is
         Id : constant Id_Dict.Id := Id_Dict.Get (Name);
      begin
         Id_Dict.Add
           (Id,
            (To_Unbounded_String (Filename), Iterator.Line, Column, Kinds));
      end Process_Id;

      Iterator : Reader;

   begin
      Open (Filename, Iterator);

      loop
         Next (Iterator);

         Find (Iterator, "id", Process_Id'Access);
         Find (Iterator, "class", Process_Class'Access);

         Check_Include (Iterator);

         Clear_Content (Iterator);
         exit when Eof (Iterator);
      end loop;

      Close (Iterator);
   end Process_ML;

begin
   Parse_Command_Line;

   --  Parse all files

   loop
      declare
         S : constant String :=
               GNAT.Command_Line.Get_Argument (Do_Expansion => True);
      begin
         exit when S'Length = 0;

         Process (S);
      end;
   end loop;

   if Dump_Classes then
      Text_IO.New_Line;
      Text_IO.Put_Line ("*** Class dictionary");

      case Check is
         when Xref      => Class_Dict.Dump (All_Kind);
         when Unused    => Class_Dict.Dump (Unused_Kind);
         when Undefined => Class_Dict.Dump (Ref_Kind);
      end case;
   end if;

   if Dump_Ids then
      Text_IO.New_Line;
      Text_IO.Put_Line ("*** Id dictionary");

      case Check is
         when Xref      => Id_Dict.Dump (All_Kind);
         when Unused    => Id_Dict.Dump (Unused_Kind);
         when Undefined => Id_Dict.Dump (Ref_Kind);
      end case;
   end if;

   Command_Line.Set_Exit_Status (Command_Line.Success);

exception
   when Syntax_Error | GNAT.Command_Line.Invalid_Switch =>
      Text_IO.Put_Line ("webxref - Web Cross-References v" & Version);
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage : webxref [-huxcCiIv] file1 file2...");
      Text_IO.New_Line;
      Text_IO.Put_Line
        ("        -h      : display help");
      Text_IO.Put_Line
        ("        -x      : ouput all cross-references (default)");
      Text_IO.Put_Line
        ("        -u      : output unused entities only");
      Text_IO.Put_Line
        ("        -d      : output referenced but undefined entities only");
      Text_IO.Put_Line
        ("        -i      : handle id elements");
      Text_IO.Put_Line
        ("        -I      : do not handle id elements");
      Text_IO.Put_Line
        ("        -c      : handle class elements");
      Text_IO.Put_Line
        ("        -C      : do not handle class elements");
end Webxref;
