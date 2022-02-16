------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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
--
--  This program is to convert thin binding specification into optional dynamic
--  thin binding. Each routine and object aspect list containing Import aspect
--  and not containing Address aspect will be modified to have Address aspect
--  pointing to the user defined function call Symbol with String parameter
--  returning System.Address. String parameter taken either from External_Name
--  acpect if exists or from lowercased imported object name if External_Name
--  is absent.

with Ada.Command_Line;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;

with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;

function Dynamo return Integer is

   use Ada.Command_Line;
   use Libadalang.Analysis;
   use Libadalang.Common;

   package TIO renames Ada.Text_IO;
   package WCH renames Ada.Wide_Wide_Characters.Handling;

   Source_Path : constant String := Argument (1);
   Main_Source : constant String := Argument (2);

   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit := Ctx.Get_From_File (Source_Path);

   Charset : constant String := Get_Charset (Unit);

   function WW_Argument (P : Positive) return Wide_Wide_String is
     (Langkit_Support.Text.Decode (Argument (P), Charset));
   --  Returns P command line argument in Wide_Wide_String type

   Loader_Pkg : constant Wide_Wide_String := WW_Argument (3);
   No_Rebind  : constant Wide_Wide_String := """_AWS_";
   --  No_Rebind prefix mean that this import should not be rebinded to dinamic
   --  library.

   First : Token_Reference := Unit.First_Token;

   WLF : constant Wide_Wide_Character :=
           Wide_Wide_Character'Val (Character'Pos (ASCII.LF));

   Counter : Positive := 1;

   CU : constant Ada_Node := Unit.Root;
   S  : TIO.File_Type;

   procedure Put (File : TIO.File_Type; Item : Wide_Wide_String);
   --  Encode Text as String with Charset and output it into the File

   procedure Flush_S (Node : Ada_Node'Class; Skip : Boolean);
   --  Output source text from token First to token previous before Node and
   --  move First token into first Node token if Skip is False or to next token
   --  after Node if Skip is True.

   procedure Process_Object_Decl (Node : Ada_Node);
   --  Process object declaration. Changes External_Name aspect into Address.
   --  Address value taken from another generated package with function call
   --  by External_Name aspect value if exists or by lowercased object name
   --  otherwise.

   procedure Process_Subp_Decl (Node : Ada_Node);
   --  Process routine declaration. Changes External_Name aspect into Address.
   --  Address value taken from another generated package with function call
   --  by External_Name aspect value if exists or by lowercased routine name
   --  otherwise.

   procedure Process_Ada_Node_List (Node : Ada_Node);
   --  Iterated over Node children and calls Process_Subp_Decl or
   --  Process_Object_Decl on appropriate nodes.

   procedure Process_Public_Part (Node : Ada_Node);
   --  Choose Ada node list from children and calls Process_Ada_Node_List for
   --  it.

   procedure Process_Package_Decl (Node : Ada_Node);
   --  Choose public part from children and calls Process_Public_Part for it

   procedure Process_Library_Item (Node : Ada_Node);
   --  Choose package declaration from children and calls Process_Package_Decl
   --  for it.

   -------------
   -- Flush_S --
   -------------

   procedure Flush_S (Node : Ada_Node'Class; Skip : Boolean) is
      First : constant Token_Reference := Node.Token_Start;
   begin
      Put (S, Text (Dynamo.First, Previous (First)));

      Dynamo.First := (if Skip then Next (Node.Token_End) else First);
   end Flush_S;

   ---------
   -- Put --
   ---------

   procedure Put (File : TIO.File_Type; Item : Wide_Wide_String) is
   begin
      TIO.Put (File, Langkit_Support.Text.Encode (Item, Charset));
   end Put;

   ------------------
   -- Process_Decl --
   ------------------

   procedure Process_Decl (Name : Wide_Wide_String; SA : Aspect_Spec) is

      Import_Aspect  : constant Wide_Wide_String := "import";
      Address_Aspect : constant Wide_Wide_String := "address";
      External_Name  : constant Wide_Wide_String := "external_name";

      procedure Apply_Name (Name : Wide_Wide_String);

      function Count_Image return Wide_Wide_String;

      -----------------
      -- Count_Image --
      -----------------

      function Count_Image return Wide_Wide_String is
         Result : constant Wide_Wide_String :=
                    Integer'Wide_Wide_Image (Counter);
      begin
         return Result
           (Result'First + (if Result (Result'First) = ' ' then 1 else 0)
            .. Result'Last);
      end Count_Image;

      CI : constant Wide_Wide_String := '_' & Count_Image;

      Name_Applied : Boolean := False;
      Has_Import   : Boolean := False;
      Last_A       : Aspect_Assoc;

      ----------------
      -- Apply_Name --
      ----------------

      procedure Apply_Name (Name : Wide_Wide_String) is
      begin
         Put (S, "Address => " & Loader_Pkg & ".Symbol (" & Name & ")");
         Name_Applied := True;
      end Apply_Name;

   begin
      if SA.Is_Null then
         return;
      end if;

      for A of SA.F_Aspect_Assocs loop
         declare
            A_Name : constant Wide_Wide_String := WCH.To_Lower (A.F_Id.Text);
         begin
            if A_Name = External_Name then
               declare
                  AN : constant Wide_Wide_String := A.F_Expr.Text;
               begin
                  if AN'Length > No_Rebind'Length
                    and then AN (AN'First .. AN'First + No_Rebind'Length - 1)
                             = No_Rebind
                  then
                     return;
                  else
                     Flush_S (A, Skip => True);
                     Apply_Name (AN);
                  end if;
               end;

            else
               if A_Name = Import_Aspect then
                  Has_Import := True;
               elsif A_Name = Address_Aspect then
                  return;
               end if;

               if not Name_Applied then
                  Last_A := Aspect_Assoc (A);
               end if;
            end if;
         end;
      end loop;

      if Has_Import and then not Name_Applied then
         Flush_S (Last_A, Skip => False);
         Apply_Name ('"' & WCH.To_Lower (Name) & '"');
         Put (S, ", ");
      end if;

      Counter := Counter + 1;
   end Process_Decl;

   -------------------------
   -- Process_Object_Decl --
   -------------------------

   procedure Process_Object_Decl (Node : Ada_Node) is
      D : constant Object_Decl := Node.As_Object_Decl;
   begin
      Process_Decl (D.F_Ids.Text, D.F_Aspects);
   end Process_Object_Decl;

   -----------------------
   -- Process_Subp_Decl --
   -----------------------

   procedure Process_Subp_Decl (Node : Ada_Node) is
      D : constant Subp_Decl := Node.As_Subp_Decl;
   begin
      Process_Decl (D.F_Subp_Spec.F_Subp_Name.Text, D.F_Aspects);
   end Process_Subp_Decl;

   ---------------------------
   -- Process_Ada_Node_List --
   ---------------------------

   procedure Process_Ada_Node_List (Node : Ada_Node) is
   begin
      for Item of Node.Children loop
         case Item.Kind is
            when Ada_Subp_Decl =>
               Process_Subp_Decl (Item);

            when Ada_Object_Decl =>
               Process_Object_Decl (Item);

            when others =>
               null;
         end case;
      end loop;
   end Process_Ada_Node_List;

   -------------------------
   -- Process_Public_Part --
   -------------------------

   procedure Process_Public_Part (Node : Ada_Node) is
   begin
      for Item of Node.Children loop
         if Item.Kind = Ada_Ada_Node_List then
            Process_Ada_Node_List (Item);
         end if;
      end loop;
   end Process_Public_Part;

   --------------------------
   -- Process_Package_Decl --
   --------------------------

   procedure Process_Package_Decl (Node : Ada_Node) is
   begin
      for Item of Node.Children loop
         if not Item.Is_Null and then Item.Kind = Ada_Public_Part then
            Process_Public_Part (Item);
         end if;
      end loop;
   end Process_Package_Decl;

   --------------------------
   -- Process_Library_Item --
   --------------------------

   procedure Process_Library_Item (Node : Ada_Node) is
   begin
      for Item of Node.Children loop
         if Item.Kind = Ada_Package_Decl then
            Process_Package_Decl (Item);
         end if;
      end loop;
   end Process_Library_Item;

begin
   if CU.Is_Null then
      TIO.Put_Line
        (TIO.Standard_Error, "Unable to parse file " & Source_Path);
      return 1;
   end if;

   TIO.Create (S, Name => Main_Source);

   for Item of CU.Children loop
      if Item.Kind = Ada_Library_Item then
         Flush_S (Item, Skip => False);
         Put (S, "with " & Loader_Pkg & ';' & WLF & WLF);
         Process_Library_Item (Item);
      end if;
   end loop;

   declare
      Footer : constant Wide_Wide_String :=
                 Text (First, Unit.Last_Token);
   begin
      for L in reverse Footer'Range loop
         if Footer (L) /= WLF then
            Put (S, Footer (Footer'First .. L));
            exit;
         end if;
      end loop;
   end;

   TIO.Close (S);

   return 0;
end Dynamo;
