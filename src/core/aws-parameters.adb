------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Streams;
with Ada.Strings.Fixed;
with AWS.URL;

with AWS.Config;
with AWS.Server;
with AWS.Translator;
with AWS.Utils;

package body AWS.Parameters is

   use Ada.Strings;

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

   procedure Add (Parameter_List : in out List; Parameters : String) is
      Count : Natural := 0;
   begin
      Add_Internal (Parameter_List, Parameters, Count, Positive'Last);
   end Add;

   procedure Add
     (Parameter_List : in out List;
      Parameters     : in out AWS.Resources.Streams.Memory.Stream_Type'Class)
   is
      use Ada.Streams;
      use AWS.Translator;

      Max_Parameters : constant Positive :=
                         Config.Max_POST_Parameters
                           (Server.Config (Server.Get_Current.all));
      --  For security reasons we only allow a maximum number of parameters per
      --  HTTP request.

      Count          : Natural := 0;

      Amp    : constant Stream_Element := Character'Pos ('&');
      Buffer : Stream_Element_Array
                 (1 .. Stream_Element_Offset'Min
                         (Stream_Element_Offset
                            (AWS.Config.Input_Line_Size_Limit),
                          Parameters.Size));
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

      Parameters.Reset;

      loop
         Parameters.Read (Buffer (First .. Buffer'Last), Last);

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
                 & Parameter_List.URI_Format (Limit => 64);
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

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameter_List : in out List; Name, Value : String; Decode : Boolean) is
   begin
      if Decode then
         Parameter_List.Add (URL.Decode (Name), URL.Decode (Value));
      else
         Parameter_List.Add (Name, Value);
      end if;
   end Add;

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : Unbounded_String;
      Decode         : Boolean) is
   begin
      if Decode then
         Parameter_List.Add (URL.Decode (Name), URL.Decode (Value));
      else
         Parameter_List.Add (Name, Value);
      end if;
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
      procedure Add (Name, Value : String) with Inline;
      --  Add Str as parameter, check for Max_Parameters

      ---------
      -- Add --
      ---------

      procedure Add (Name, Value : String) is
      begin
         Count := Count + 1;

         if Count <= Max_Parameters then
            Add (Parameter_List, Name, Value, Decode => True);

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

   ------------
   -- Update --
   ------------

   procedure Update
     (Parameter_List : in out List; Name, Value : String; Decode : Boolean) is
   begin
      if Decode then
         Parameter_List.Update (URL.Decode (Name), URL.Decode (Value));
      else
         Parameter_List.Update (Name, Value);
      end if;
   end Update;

   procedure Update
     (Parameter_List : in out List;
      Name, Value    : Unbounded_String;
      Decode         : Boolean) is
   begin
      if Decode then
         Parameter_List.Update (URL.Decode (Name), URL.Decode (Value));
      else
         Parameter_List.Update (Name, Value);
      end if;
   end Update;

   ----------------
   -- URI_Format --
   ----------------

   function URI_Format
     (Parameter_List : List; Limit : Positive := Positive'Last) return String
   is
      Delimiter : Character := '?';
      Parameters : Unbounded_String;
      Size : Positive := 1;
   begin
      for J in 1 .. Parameter_List.Count loop
         declare
            Item : constant Containers.Tables.Element :=
               Parameter_List.Get (J);
            function Encode (Item : String) return String is
               (URL.Encode (Item, URL.Parameters_Encoding_Set));
         begin
            Append
              (Parameters,
               Delimiter & Encode (To_String (Item.Name))
               & (if Item.Value = "" then ""
                  else '=' &  Encode (To_String (Item.Value))));
         end;

         if J = 1 then
            Delimiter := '&';
         end if;

         if Length (Parameters) > Limit then
            return Slice (Parameters, 1, Size);
         end if;

         Size := Length (Parameters);
      end loop;

      return To_String (Parameters);
   end URI_Format;

end AWS.Parameters;
