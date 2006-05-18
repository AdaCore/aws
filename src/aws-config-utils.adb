with Ada.Exceptions;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body AWS.Config.Utils is

   procedure Parse_Strings (Vector : in out SV.Vector; Line : String);
   --  Split comma separated values from Line into Vector.
   --  Trim spaces from both sides.

   procedure Parameter
     (Param_Set     : in out Parameter_Set;
      Name          : in     Parameter_Name;
      Value         : in     String;
      Error_Context : in     String)
   is
      procedure Set_Parameter (Param : in out Values);
      --  Set parameter depending on the type (Param.Kind).

      procedure Error (Message : in String);
      --  Raises Constraint_Error with associated message and Error_Context
      --  string.

      function "+" (S : in String)
        return Unbounded_String
        renames To_Unbounded_String;

      -----------
      -- Error --
      -----------

      procedure Error (Message : in String) is
      begin
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity, Error_Context & Message & '.');
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

            when Nat =>
               Expected_Type := +"natural";
               Param.Nat_Value := Natural'Value (Value);

            when Dur =>
               Expected_Type := +"duration";
               Param.Dur_Value := Duration'Value (Value);

            when Bool =>
               Expected_Type := +"boolean";
               Param.Bool_Value := Boolean'Value (Value);
         end case;

      exception
         when others =>
            Error
              ("wrong value for " & Parameter_Name'Image (Name)
               & " " & To_String (Expected_Type) & " expected");
      end Set_Parameter;

   begin
      if Name not in Param_Set'Range then
         declare
            Not_Supported_Msg : constant String
              := " option '" & Parameter_Name'Image (Name)
              & "' not supported for this configuration context.";
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

   end Parameter;

   -------------------
   -- Parse_Strings --
   -------------------

   procedure Parse_Strings (Vector : in out SV.Vector; Line : in String) is
      use Ada.Strings;
      First  : Positive := Line'First;
      Last   : Natural;
      Spaces : constant Maps.Character_Set
        := Maps.To_Set (" " & ASCII.HT & ASCII.CR & ASCII.LF);

      procedure Append (Item : in String);

      ------------
      -- Append --
      ------------

      procedure Append (Item : in String) is
      begin
         SV.Append (Vector, Fixed.Trim (Item, Spaces, Spaces));
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

   -----------
   -- Value --
   -----------

   function Value
     (Item : in String; Error_Context : in String) return Parameter_Name is
   begin
      return Parameter_Name'Value (Item);
   exception
      when Constraint_Error =>
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            Error_Context & "unrecognized option " & Item);
   end Value;

end AWS.Config.Utils;
