------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2001                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with AWS.Status;
with Templates_Parser;

--  This service can be used to browse a file system. The browsing mechanism
--  will gather information (filename, size, directory...) from a specified
--  directory name and will fill a translation table. This table will be used
--  with a template file to render the HTML document. You can design your own
--  browsing template file, here is a description of all tag variables defined
--  in the translation table:
--
--     URI (discrete)
--        The URI pointing to the directory parsed.
--
--     VERSION (discrete)
--        AWS version string.
--
--     IS_DIR (vector)
--        A list of booleans, indicate if Nth entry is a directory or not.
--
--     NAMES (vector)
--        A list of filenames. Nth name is a directory if Nth entry in IS_DIR
--        is set to true.
--
--     SIZES (vector)
--        A list of sizes. Nth entry is the file size of the Nth entry in
--        NAMES.
--
--     TIMES (vector)
--        A list of last modification times. Nth entry is is the last
--        modification time of the Nth entry in NAMES.
--
--     NAME_ORDR
--     DIR_ORDR
--     SIZE_ORDR
--     TIME_ORDR
--     ORIG_ORDR
--     DIR_NAME_ORDR
--     DIR_TIME_ORDR

package AWS.Services.Directory is

   use Templates_Parser;

   function Browse
     (Directory_Name : in String;
      Request        : in AWS.Status.Data)
     return Translate_Table;
   --  Returns a translation table to be used with a directory template fill
   --  with information parsed from Directory_Name.

   function Browse
     (Directory_Name    : in String;
      Template_Filename : in String;
      Request           : in AWS.Status.Data;
      Translations      : in Translate_Table := No_Translation)
     return String;

end AWS.Services.Directory;
