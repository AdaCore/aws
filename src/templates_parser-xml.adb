------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2004                            --
--                               Pascal Obry                                --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Templates_Parser.XML is

   use Ada;

   Label_Suffix : constant String := "_LABEL";

   -----------
   -- Image --
   -----------

   function Image (Translations : in Translate_Set) return Unbounded_String is
      Result : Unbounded_String;

      procedure Process (Cursor : in Containers.Cursor_Type);
      --  Iterator

      procedure Add (Str : in String);
      pragma Inline (Add);
      --  Add a new line (str) into Result, a LF is added at the end of Str

      ---------
      -- Add --
      ---------

      procedure Add (Str : in String) is
      begin
         Append (Result, Str & ASCII.LF);
      end Add;

      -------------
      -- Process --
      -------------

      procedure Process (Cursor : in Containers.Cursor_Type) is

         Item : constant Association := Containers.Element (Cursor);
         --  Current item

         Var  : constant String := To_String (Item.Variable);
         --  Item variable name

         procedure Process_Std;
         --  Handles standard variables

         procedure Process_Composite;
         --  Handles composite variables

         procedure Add_Label;
         --  Add var label if found in the translation set

         function Is_Label return Boolean;
         --  Returns True if Item is a Label entry

         ---------------
         -- Add_Label --
         ---------------

         procedure Add_Label is
            Var_Label : constant String := Var & Label_Suffix;
         begin
            if Containers.Is_In (Var_Label, Translations.Set.all) then
               --  There is probably a label encoded into this set
               declare
                  Label : constant Association
                    := Containers.Element (Translations.Set.all, Var_Label);
               begin
                  if Label.Kind = Std then
                     --  Definitly a label for this variable
                     Add ("         <Label>"
                          & To_String (Label.Value) & "</Label>");
                  end if;
               end;
            end if;
         end Add_Label;

         --------------
         -- Is_Label --
         --------------

         function Is_Label return Boolean is
            N, L : Natural;
         begin
            --  Tag labels

            if Var'Length > Label_Suffix'Length
              and then Var
               (Var'Last - Label_Suffix'Length + 1 .. Var'Last) = Label_Suffix
              and then Containers.Is_In
               (Var (Var'First .. Var'Last - Label_Suffix'Length),
                Translations.Set.all)
            then
               return True;
            end if;

            --  Nested tag labels

            N := Strings.Fixed.Index (Var, "_DIM");

            if N = 0 or else Var (N + 4) not in '0' .. '9' then
               return False;

            else
               L := N - 1; -- Last character for the tag name
               N := N + 4; -- First character after _DIM

               loop
                  N := N + 1;
                  exit when Var (N) = '_' or else N = Var'Last;

                  if Var (N) not in '0' .. '9' then
                     --  Not a digit here, this is not a label
                     return False;
                  end if;
               end loop;

               return Var (N .. Var'Last) = Label_Suffix
                 and then Containers.Is_In
                   (Var (Var'First .. L), Translations.Set.all);
            end if;
         end Is_Label;

         -----------------
         -- Process_Std --
         -----------------

         procedure Process_Std is
         begin
            Add ("   <SimpleTag>");
            Add ("      <Tag>");
            Add ("         <Name>" & Var & "</Name>");
            Add_Label;
            Add ("      </Tag>");
            Add ("      <V>" & To_String (Item.Value) & "</V>");
            Add ("   </SimpleTag>");
         end Process_Std;

         -----------------------
         -- Process_Composite --
         -----------------------

         procedure Process_Composite is

            Null_Indice : constant Indices := (2 .. 1 => 0);

            procedure Output_Tag (T : in Tag; Pos : in Indices := Null_Indice);
            --  Output recursively tag T, Pos is the current indices for the
            --  parsed items.

            procedure Output_Axis_Label (N : in Positive; T : in Tag);
            --  Output labels for axis number N. Labels are found in tag T.
            --  T must be a vector tag (Nested_Level = 1).

            -----------------------
            -- Output_Axis_Label --
            -----------------------

            procedure Output_Axis_Label (N : in Positive; T : in Tag) is
               P : Tag_Node_Access := T.Head;
               K : Positive := 1;
            begin
               pragma Assert (T.Nested_Level = 1);

               Add ("      <Dim" & Image (N) & ">");
               Add ("         <Labels>");

               while P /= null loop
                  Add ("            <Label ind=""" & Image (K) & """>"
                       & To_String (P.V) & "</Label>");
                  K := K + 1;
                  P := P.Next;
               end loop;

               Add ("         </Labels>");
               Add ("      </Dim" & Image (N) & ">");
            end Output_Axis_Label;

            ----------------
            -- Output_Tag --
            ----------------

            procedure Output_Tag
              (T   : in Tag;
               Pos : in Indices := Null_Indice)
            is

               procedure Output_Value (Pos : in Indices; Value : in String);
               --  Output value whose Tag indices is given by Pos

               ------------------
               -- Output_Value --
               ------------------

               procedure Output_Value (Pos : in Indices; Value : in String) is
                  V : Unbounded_String;
               begin
                  Append (V, "      <V ");

                  for K in Pos'Range loop
                     Append
                       (V, "ind" & Image (K) & "=""" & Image (Pos (K)) & '"');
                     if K /= Pos'Last then
                        Append (V, " ");
                     end if;
                  end loop;

                  Append (V, ">" & Value & "</V>");

                  Add (To_String (V));
               end Output_Value;

               N : Tag_Node_Access := T.Head;
               P : Positive := 1;
            begin
               while N /= null loop
                  if N.Kind = Value then
                     Output_Value (Pos & Indices'(1 => P), To_String (N.V));
                  else
                     Output_Tag (N.VS.all, Pos & Indices'(1 => P));
                  end if;

                  P := P + 1;
                  N := N.Next;
               end loop;
            end Output_Tag;

         begin
            Add ("   <CompositeTag>");
            Add ("      <Tag>");
            Add ("         <Name>" & Var & "</Name>");
            Add_Label;
            Add ("      </Tag>");

            --  Output axis labels

            for K in 1 .. Item.Comp_Value.Nested_Level loop
               declare
                  Label_Var : constant String
                    := Var & "_DIM" & Image (K) & Label_Suffix;
               begin
                  if Containers.Is_In (Label_Var, Translations.Set.all) then
                     declare
                        Item : constant Association
                          := Containers.Element
                            (Translations.Set.all, Label_Var);
                     begin
                        if Item.Kind = Composite
                          and then Item.Comp_Value.Nested_Level = 1
                        then
                           --  This is a vector tag, labels are expected to
                           --  be found on this vector.
                           Output_Axis_Label (K, Item.Comp_Value);
                        end if;
                     end;
                  end if;
               end;
            end loop;

            --  Output values
            Output_Tag (Item.Comp_Value);

            Add ("   </CompositeTag>");
         end Process_Composite;

      begin
         --  Do not process labels encoded for another variable

         if not Is_Label then
            case Item.Kind is
               when Std       => Process_Std;
               when Composite => Process_Composite;
            end case;
         end if;
      end Process;

      -------------
      -- Iterate --
      -------------

      procedure Iterate is new Containers.Generic_Iteration;

   begin
      --  XML header

      Add ("<?xml version=""1.0"" encoding=""UTF-8"" ?>");
      Add ("<Tagged xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">");

      Iterate (Translations.Set.all);

      --  XML footer

      Add ("</Tagged>");

      return Result;
   end Image;

   ----------
   -- Save --
   ----------

   procedure Save (Filename : in String; Translations : in Translate_Set) is
      File : Text_IO.File_Type;
   begin
      Text_IO.Create (File, Text_IO.Out_File, Filename);
      Text_IO.Put (File, To_String (Image (Translations)));
      Text_IO.Close (File);
   end Save;

end Templates_Parser.XML;
