------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with Asis; use Asis;

package Ada2WSDL.Parser is

   procedure Initialize;
   --  Reads and checks the command line parameters and initializes the
   --  Gnatstub options. Checks the existence of the files to be processed
   --  by Gnatstub and applicability of the gnatstub options with these files.
   --  Tries to create the tree file, if necessary.
   --  If everything is OK, sets the global Is_Initialized variable True.
   --  This procedure does not use anything from ASIS

   procedure Create_Sample;
   --  If Is_Initialized, generates the sample body. This procedure is an
   --  ASIS application

   procedure Clean_Up;
   --  Now the only thing it does is removing the tree file, if needed

end Ada2WSDL.Parser;
