------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                      --
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

with Ada.Strings.Fixed;

with AWS.Net.Buffered;
with AWS.Utils;

package body AWS.Config.Utils is

   -------------------
   -- Parse_Strings --
   -------------------

   procedure Parse_Strings (Vector : in out SV.Vector; Line : String) is
      use Ada.Strings;
      First  : Positive := Line'First;
      Last   : Natural;

      procedure Append (Item : String);

      ------------
      -- Append --
      ------------

      procedure Append (Item : String) is
      begin
         SV.Append
           (Vector, Fixed.Trim (Item, AWS.Utils.Spaces, AWS.Utils.Spaces));
      end Append;

   begin
      SV.Clear (Vector);

      if Line = "" then
         return;
      end if;

      loop
         Last := Fixed.Index (Line (First .. Line'Last), ",");

         if Last = 0 then
            Append (Line (First .. Line'Last));
            exit;
         end if;

         Append (Line (First .. Last - 1));

         First := Last + 1;
      end loop;
   end Parse_Strings;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter
     (Param_Set     : in out Parameter_Set;
      Name          : Parameter_Name;
      Value         : String;
      Error_Context : String)
   is
      procedure Set_Parameter (Param : in out Values);
      --  Set parameter depending on the type (Param.Kind)

      procedure Error (Message : String);
      --  Raises Constraint_Error with associated message and Error_Context
      --  string.

      function "+" (S : String)
        return Unbounded_String
        renames To_Unbounded_String;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
         function Error_Message return String;

         -------------------
         -- Error_Message --
         -------------------

         function Error_Message return String is
         begin
            if Error_Context = "" then
               return Message;
            else
               return Error_Context & ' ' & Message;
            end if;
         end Error_Message;

      begin
         raise Constraint_Error with Error_Message & '.';
      end Error;

      Expected_Type : Unbounded_String;

      -------------------
      -- Set_Parameter --
      -------------------

      procedure Set_Parameter (Param : in out Values) is
      begin
         case Param.Kind is
            when Str =>
               Expected_Type := +"string";
               Param.Str_Value := +Value;

            when Str_Vect =>
               Expected_Type := +"string list";
               Parse_Strings (Param.Strs_Value, Value);

            when Dir =>
               Expected_Type := +"string";

               if Value (Value'Last) = '/'
                 or else Value (Value'Last) = '\'
               then
                  Param.Dir_Value := +Value;
               else
                  Param.Dir_Value := +(Value & '/');
               end if;

            when Pos =>
               Expected_Type := +"positive";
               Param.Pos_Value := Positive'Value (Value);

               if Name = Input_Line_Size_Limit then
                  Net.Buffered.Set_Input_Limit (Param.Pos_Value);
               end if;

            when Nat =>
               Expected_Type := +"natural";
               Param.Nat_Value := Natural'Value (Value);

            when Dur =>
               Expected_Type := +"duration";
               Param.Dur_Value := Duration'Value (Value);

            when Bool =>
               Expected_Type := +"boolean";
               Param.Bool_Value := Boolean'Value (Value);

            when Regexp =>
               Expected_Type := +"regexp (string)";
               Param.Pattern := GNAT.Regexp.Compile (Value);
               Param.Is_Set  := True;
         end case;

      exception
         when others =>
            Error
              ("wrong value for " & Parameter_Name'Image (Name)
               & " " & To_String (Expected_Type) & " expected, but '"
               & Value & "' found");
      end Set_Parameter;

   begin
      if Name not in Param_Set'Range then
         declare
            Not_Supported_Msg : constant String :=
                                  " option '" & Parameter_Name'Image (Name)
                                & "' not supported for this configuration"
                                & " context";
         begin
            if Name in Process_Parameter_Name'Range then
               Error ("Per process" & Not_Supported_Msg);
            else
               Error ("Per server" & Not_Supported_Msg);
            end if;
         end;
         return;

      else
         Set_Parameter (Param_Set (Name));
      end if;
   end Set_Parameter;

   -----------
   -- Value --
   -----------

   function Value
     (Item : String; Error_Context : String) return Parameter_Name is
   begin
      return Parameter_Name'Value (Item);
   exception
      when Constraint_Error =>
         raise Constraint_Error
           with Error_Context & "unrecognized option " & Item;
   end Value;

end AWS.Config.Utils;
