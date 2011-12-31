------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_IO;

with AWS.Utils;

package body WSDL_3 is

   use Ada;
   use AWS;

   ----------------
   -- Image_Rec1 --
   ----------------

   function Image_Rec1 (Rec : Rec1) return String is
   begin
      return "(" & Integer'Image (Rec.Item1)
        & ", " & Natural'Image (Rec.Item2)
        & ", " & Positive'Image (Rec.Item3)
        & ")";
   end Image_Rec1;

   ----------------
   -- Image_Rec2 --
   ----------------

   function Image_Rec2 (Rec : Rec2) return String is
   begin
      return "(" & Image_Rec1 (Rec.Field1)
        & ", " & Rec.Field2
        & ", " & To_String (Rec.Field3)
        & ", " & Long_Float'Image (Rec.Field4)
        & ")";
   end Image_Rec2;

   ----------------
   -- Image_Rec3 --
   ----------------

   function Image_Rec3 (Rec : Rec3) return String is
      R : Unbounded_String;
   begin
      Append (R, "(");

      for K in Rec.S.Item'Range loop
         Append (R, Utils.Image (Rec.S.Item (K)));

         if K < Rec.S.Item'Last then
            Append (R, ", ");
         end if;
      end loop;

      Append (R, ")");

      return To_String (R);
   end Image_Rec3;

end WSDL_3;
