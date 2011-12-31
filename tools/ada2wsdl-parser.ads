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

package Ada2WSDL.Parser is

   Fatal_Error     : exception renames Ada2WSDL.Fatal_Error;
   Parameter_Error : exception renames Ada2WSDL.Parameter_Error;

   procedure Initialize;
   --  Reads and checks the command line parameters and initializes the
   --  Ada2WSDL options. Checks the existence of the files to be processed
   --  by Ada2WSDL and applicability of the Ada2WSDL options with these files.
   --  Tries to create the tree file, if necessary.
   --  If everything is OK, sets the global Is_Initialized variable True.
   --  This procedure does not use anything from ASIS

   procedure Start;
   --  If Is_Initialized, generates the sample body. This procedure is an
   --  ASIS application

   procedure Add_Option (Option : String);
   --  Add an option to the parser, this will be passed to ASIS module to
   --  create the tree.

   procedure Clean_Up;
   --  Now the only thing it does is removing the tree file, if needed

end Ada2WSDL.Parser;
