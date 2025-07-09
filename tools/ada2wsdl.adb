------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2025, AdaCore                     --
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

with Ada.Exceptions;

with Langkit_Support.Text;

package body Ada2WSDL is

   use Ada;

   --------------
   -- Location --
   --------------

   function Location (Node : LaL.Ada_Node'Class) return String is
   begin
      return Langkit_Support.Text.Image (Node.Full_Sloc_Image);
   end Location;

   ----------------------
   -- Raise_Spec_Error --
   ----------------------

   procedure Raise_Spec_Error
     (Node    : LaL.Ada_Node'Class;
      Message : String) is
   begin
      Exceptions.Raise_Exception
        (Spec_Error'Identity, Location (Node) & Message);
   end Raise_Spec_Error;

end Ada2WSDL;
