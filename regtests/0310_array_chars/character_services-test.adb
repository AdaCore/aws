------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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

with AWS.Server.Status;

with Character_Services;
with Character_Services_Service.Client;
with Character_Services_Service.Types;
with Character_Types;

package body Character_Services.Test is

   use Character_Types;

   Test_Set : constant Array_Character_Type :=
                ('a', 'A', '¤', 'å', 'Å', '0', '1');

   -----------------
   -- Test_Record --
   -----------------

   procedure Test_Record is
      R : Character_Record_Type;
   begin
      for I of Test_Set loop
         R.Value := I;
         Character_Services_Service.Client.Test_Character_Record
           (Value_Record => R,
            Endpoint     => AWS.Server.Status.Local_URL (H_Server));
      end loop;
   end Test_Record;

   --------------------
   -- Test_Parameter --
   --------------------

   procedure Test_Parameter is
   begin
      for I of Test_Set loop
         Character_Services_Service.Client.Test_Character_Parameter
           (Value_Param => I,
            Endpoint    => AWS.Server.Status.Local_URL (H_Server));
      end loop;
   end Test_Parameter;

   -----------------------
   -- Test_Array_Record --
   -----------------------

   procedure Test_Array_Record is
      use Character_Services_Service.Types;
      R : Array_Character_Record_Type;
   begin
      R.Value := +(Test_Set);
      Character_Services_Service.Client.Test_Array_Character_Record
        (Value_Record => R,
         Endpoint     => AWS.Server.Status.Local_URL (H_Server));
   end Test_Array_Record;

   --------------------------
   -- Test_Array_Parameter --
   --------------------------

   procedure Test_Array_Parameter is
   begin
      Character_Services_Service.Client.Test_Array_Character_Parameter
        (Value_Param => Test_Set,
         Endpoint    => AWS.Server.Status.Local_URL (H_Server));
   end Test_Array_Parameter;

   ------------------------------
   -- Test_Array_Record_Record --
   ------------------------------

   procedure Test_Array_Record_Record is
      use Character_Services_Service.Types;
      R    : Array_Record_Character_Record_Type;
      Item : Array_Record_Character_Type (Test_Set'Range);
   begin
      for I in Test_Set'Range loop
         Item (I).Value := Test_Set (I);
      end loop;

      R.Value := +(Item);
      Character_Services_Service.Client.Test_Array_Record_Character_Record
        (Value_Record => R,
         Endpoint     => AWS.Server.Status.Local_URL (H_Server));
   end Test_Array_Record_Record;

   ---------------------------------
   -- Test_Array_Record_Parameter --
   ---------------------------------

   procedure Test_Array_Record_Parameter is
      Item : Array_Record_Character_Type (Test_Set'Range);
   begin
      for I in Test_Set'Range loop
         Item (I).Value := Test_Set (I);
      end loop;

      Character_Services_Service.Client.Test_Array_Record_Character_Parameter
        (Value_Param => Item,
         Endpoint    => AWS.Server.Status.Local_URL (H_Server));
   end Test_Array_Record_Parameter;

end Character_Services.Test;
