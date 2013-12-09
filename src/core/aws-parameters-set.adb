------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

pragma Ada_2012;

with Ada.Streams;
with Ada.Strings.Fixed;

with AWS.Config;
with AWS.Containers.Tables.Set;
with AWS.Server;
with AWS.Translator;
with AWS.URL;
with AWS.Utils;

package body AWS.Parameters.Set is

   use AWS.Containers;

   procedure Add_Internal
     (Parameter_List : in out List;
      Parameters     : String;
      Count          : in out Natural;
      Max_Parameters : Positive);
   --  Add parameters as parsed from Parameters and raised Too_Many_Parameters
   --  if the Max_Parameters count is reached.

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : String;
      Decode         : Boolean := True) is
   begin
      if Parameter_List.Parameters = Null_Unbounded_String then
         Append (Parameter_List.Parameters, "?");
      else
         Append (Parameter_List.Parameters, "&");
      end if;

      if Value = "" then
         Append (Parameter_List.Parameters, Name);
      else
         Append (Parameter_List.Parameters, Name & "=" & Value);
      end if;

      if Decode then
         --  This is default behavior
         Tables.Set.Add
           (Tables.Table_Type (Parameter_List),
            URL.Decode (Name),
            URL.Decode (Value));

      else
         Tables.Set.Add (Tables.Table_Type (Parameter_List), Name, Value);
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Parameter_List : in out List; Parameters : String) is
      Count : Natural := 0;
   begin
      Add_Internal (Parameter_List, Parameters, Count, Positive'Last);
   end Add;

   procedure Add
     (Parameter_List : in out List;
      Parameters     : in out AWS.Containers.Memory_Streams.Stream_Type)
   is
      use Ada.Streams;
      use AWS.Containers.Memory_Streams;
      use AWS.Translator;

      Max_Parameters : constant Positive :=
                         Config.Max_POST_Parameters
                           (Server.Config (Server.Get_Current.all));
      --  For security reasons we only allow a maximum number of parameters per
      --  HTTP request.

      Count          : Natural := 0;

      Amp   : constant Stream_Element := Character'Pos ('&');
      Buffer : Stream_Element_Array
                 (1 .. Stream_Element_Offset'Min
                         (Stream_Element_Offset
                            (AWS.Config.Input_Line_Size_Limit),
                          Size (Parameters)));
      First : Stream_Element_Offset := Buffer'First;
      Last  : Stream_Element_Offset;
      Found : Boolean;
      WNF   : Boolean := False;
      --  Was not found. This flag need to detect more than once 'not found'
      --  cases. If length of parameter name and value no more than
      --  AWS.Config.Input_Line_Size_Limit, 'not Found' case could happen only
      --  at the end of parameters line. In case of twice 'not Found' cases we
      --  raise Too_Long_Parameter.
   begin
      if Buffer'Length = 0 then
         return;
      end if;

      Reset (Parameters);

      loop
         Read (Parameters, Buffer (First .. Buffer'Last), Last);

         Found := False;

         Find_Last_Amp : for J in reverse First .. Last loop
            if Buffer (J) = Amp then
               Found := True;
               Add_Internal
                 (Parameter_List, To_String (Buffer (1 .. J - 1)),
                  Count, Max_Parameters);
               Buffer (1 .. Last - J) := Buffer (J + 1 .. Last);
               First := Last - J + 1;
               exit Find_Last_Amp;
            end if;
         end loop Find_Last_Amp;

         if Found then
            if Last < Buffer'Last then
               Add_Internal
                 (Parameter_List, To_String (Buffer (1 .. First - 1)), Count,
                  Max_Parameters);

               exit;
            end if;

         else
            if WNF and then First <= Last then
               raise Too_Long_Parameter
                 with "HTTP parameter line too long : "
                 & Slice
                   (Parameter_List.Parameters,
                    1, Integer'Min (Length (Parameter_List.Parameters), 64));
            end if;

            WNF := True;

            Add_Internal
              (Parameter_List, To_String (Buffer (1 .. Last)),
               Count, Max_Parameters);

            First := 1;

            exit when Last < Buffer'Last;
         end if;

      end loop;
   end Add;

   ------------------
   -- Add_Internal --
   ------------------

   procedure Add_Internal
     (Parameter_List : in out List;
      Parameters     : String;
      Count          : in out Natural;
      Max_Parameters : Positive)
   is
      use Ada.Strings;

      procedure Add (Name, Value : String) with Inline;
      --  Add Str as parameter, check for Max_Parameters

      ---------
      -- Add --
      ---------

      procedure Add (Name, Value : String) is
      begin
         Count := Count + 1;

         if Count <= Max_Parameters then
            Add (Parameter_List, Name, Value);

         else
            raise Too_Many_Parameters
              with "Maximum number of parameters reached: "
                & Utils.Image (Max_Parameters)
                & ", see Config.Max_POST_Parameters.";
         end if;
      end Add;

      P : String renames Parameters;
      C : Positive := P'First;
      I : Natural;
      S : Positive;
      E : Natural;
   begin
      --  Skip leading question mark if present

      if P /= "" and then P (C) = '?' then
         C := Positive'Succ (C);
      end if;

      while C <= P'Last loop
         E := Fixed.Index (P (C .. P'Last), "&");

         if E = 0 then
            --  Last parameter
            E := P'Last;
         else
            E := E - 1;
         end if;

         I := Fixed.Index (P (C .. E), "=");

         if I = 0 then
            --  No value for this parameter
            S := E + 1;
            I := E;
         else
            S := I + 1;
            I := I - 1;
         end if;

         Add (Name => P (C .. I), Value => P (S .. E));

         if E < P'Last then
            C := E + 2;
         else
            C := E + 1;
         end if;
      end loop;
   end Add_Internal;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Parameter_List : in out List;
      Mode           : Boolean) is
   begin
      Tables.Set.Case_Sensitive (Tables.Table_Type (Parameter_List), Mode);
   end Case_Sensitive;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parameter_List : in out List) is
   begin
      Tables.Set.Reset (Tables.Table_Type (Parameter_List));
      Parameter_List.Parameters := Null_Unbounded_String;
   end Reset;

   ------------
   -- Update --
   ------------

   procedure Update
     (Parameter_List : in out List;
      Name, Value    : String;
      Decode         : Boolean := True)
   is
      First : constant Natural :=
                Index (Parameter_List.Parameters, Name & "=");
      Last  : Natural;
   begin
      if First = 0 then
         --  This Name is not already present, add it
         if Parameter_List.Parameters = Null_Unbounded_String then
            Append (Parameter_List.Parameters, "?");
         else
            Append (Parameter_List.Parameters, "&");
         end if;

         Append (Parameter_List.Parameters, Name & "=" & Value);

      else
         --  Replace the existing value
         Last := Index (Parameter_List.Parameters, "&", From => First);

         if Last = 0 then
            --  This is the last argument
            Last := Length (Parameter_List.Parameters);
         else
            Last := Last - 1;
         end if;

         Replace_Slice
           (Parameter_List.Parameters,
            Low  => First + Name'Length + 1,
            High => Last,
            By   => Value);
      end if;

      if Decode then
         --  This is default behavior
         Tables.Set.Update
           (Tables.Table_Type (Parameter_List),
            URL.Decode (Name),
            URL.Decode (Value));

      else
         Tables.Set.Update (Tables.Table_Type (Parameter_List), Name, Value);
      end if;
   end Update;

end AWS.Parameters.Set;
