------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with AWS.Utils;
with Ada.Text_IO;

procedure SDig is

   procedure Test (D : Duration; N : Positive) is
   begin
      Ada.Text_IO.Put_Line (AWS.Utils.Significant_Image (D, N));
   end Test;

begin
   Test (0.0123456, 7);
   Test (0.0123456, 6);
   Test (0.0123456, 5);
   Test (0.0123456, 4);
   Test (0.0123456, 3);
   Test (0.0123456, 2);
   Test (0.0123456, 1);
   Test (-0.0123456, 7);
   Test (-0.0123456, 6);
   Test (-0.0123456, 5);
   Test (-0.0123456, 4);
   Test (-0.0123456, 3);
   Test (-0.0123456, 2);
   Test (-0.0123456, 1);
   Test (9876.54321, 1);
   Test (9876.54321, 2);
   Test (9876.54321, 3);
   Test (9876.54321, 4);
   Test (9876.54321, 5);
   Test (9876.54321, 6);
   Test (9876.54321, 7);
   Test (9876.54321, 8);
   Test (9876.54321, 9);
   Test (9876.54321, 10);
   Test (-9876.54321, 1);
   Test (-9876.54321, 2);
   Test (-9876.54321, 3);
   Test (-9876.54321, 4);
   Test (-9876.54321, 5);
   Test (-9876.54321, 6);
   Test (-9876.54321, 7);
   Test (-9876.54321, 8);
   Test (-9876.54321, 9);
   Test (-9876.54321, 10);
   Test (-0.1, 3);
   Test (0.1, 3);
   Test (10.0, 3);
   Test (9.999, 3);
   Test (999.9, 3);
   Test (99.99, 3);
   Test (100.0, 3);
   Test (Duration'Delta, 1);
   Test (Duration'Delta, 2);
   Test (-Duration'Delta, 1);
   Test (-Duration'Delta, 2);
   Test (Duration (Integer'First) - 1.0, 1);
   Test (Duration (Integer'First) - 1.11111, 14);
   Test (Duration'First, 2);
   Test (Duration'First, 24);
   Test (Duration'Last, 2);
   Test (Duration'Last, 24);
   Test (Duration (Integer'Last) * 2.0, 3);
   Test (Duration (Integer'Last) + 1.1, 24);
   Test (Duration (Integer'Last) + 1.1, 3);
   Test (0.0, 1);
   Test (0.0, 2);
   Test (0.0, 3);
end SDig;
