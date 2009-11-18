------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with Ada.Text_IO;

with AWS.Config;
with AWS.Services.Web_Block.Context;

procedure WB_Context is

   use Ada;
   use AWS;

   package WBC renames Services.Web_Block.Context;

   procedure Output (Name : String; C : WBC.Object);
   --  Output context id

   ------------
   -- Output --
   ------------

   procedure Output (Name : String; C : WBC.Object) is
      Id : WBC.Id := C.Register;
   begin
      Text_IO.Put_Line (Name & " Id " & WBC.Image (Id));
   end Output;

   C1, C2, C3 : WBC.Object;
   Id         : WBC.Id;

begin
   Config.Load_Config;

   C1.Set_Value ("NAME1", "VALUE1");
   C1.Set_Value ("NAME2", "VALUE2");
   Output ("C1", C1);
   Id := C1.Register;

   C2.Set_Value ("NAME1", "VALUE1");
   C2.Set_Value ("NAME2", "VALUE2");
   Output ("C2", C2);

   C3 := C1;
   Output ("C3", C3);
   Output ("C1", C1);

   C3.Remove ("NAME1");
   Output ("C3", C3);
   Output ("C1", C1);

   C3.Set_Value ("NAME1", "VALUE1");
   Output ("C3", C3);

   Text_IO.Put_Line ("Exists: " & Boolean'Image (WBC.Exist (Id)));

   delay 3.0;

   C2.Set_Value ("GOOD", "VALUE");
   Output ("C2", C2);

   Text_IO.Put_Line ("Exists: " & Boolean'Image (WBC.Exist (Id)));
end WB_Context;
