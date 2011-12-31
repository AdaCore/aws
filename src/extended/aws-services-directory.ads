------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--     IS_DIR_V (vector)
--        A list of booleans, indicate if Nth entry is a directory or not.
--
--     NAME_V (vector)
--        A list of filenames. Nth name is a directory if Nth entry in IS_DIR
--        is set to true.
--
--     SIZE_V (vector)
--        A list of sizes. Nth entry is the file size of the Nth entry in
--        NAMES.
--
--     TIME_V (vector)
--        A list of last modification times. Nth entry is is the last
--        modification time of the Nth entry in NAMES.
--
--     NAME_ORDR
--        Rule to either set ordering on name or to revert current name
--        ordering.
--
--     SNME_ORDR
--        Rule to either set ordering on name (case sensitive) or to revert
--        current name (case sensitive) ordering.
--
--     EXT_ORDR
--        Rule to either set ordering on extension or to revert current
--        extension ordering.
--
--     SEXT_ORDR
--        Rule to either set ordering on extension (case sensitive) or to
--        revert current extension (case sensitive) ordering.
--
--     MIME_ORDR
--        Rule to either set ordering on MIME type or to revert current MIME
--        type ordering.
--
--     DIR_ORDR
--        Rule to either set ordering on directory or to revert current
--        directory ordering.
--
--     SIZE_ORDR
--        Rule to either set ordering on size or to revert current size
--        ordering.
--
--     TIME_ORDR
--        Rule to either set ordering on time or to revert current time
--        ordering.
--
--     ORIG_ORDR
--        Rule to either set original ordering (file order as read on the file
--        system) or to revert current original ordering.
--
--     DIR_NAME_ORDR
--        Rule to either set ordering on directory/name or to revert current
--        directory/name ordering.
--
--     DIR_SNME_ORDR
--        Rule to either set ordering on directory/name (case sensitive) or to
--        revert current directory/name (case sensitive) ordering.
--
--     DIR_TIME_ORDR
--        Rule to either set ordering on directory/time or to revert current
--        directory/time ordering.
--

package AWS.Services.Directory is

   use Templates_Parser;

   function Browse
     (Directory_Name : String;
      Request        : AWS.Status.Data) return Translate_Set;
   --  Returns a translation table containing information parsed from
   --  Directory_Name. This is supposed to be used with a directory template.

   function Browse
     (Directory_Name    : String;
      Template_Filename : String;
      Request           : AWS.Status.Data;
      Translations      : Translate_Set := Null_Set) return String;
   --  Parses directory Directory_Name and use Templates_Parser to fill in the
   --  template Template_Filename. It is possible to specified some specifics
   --  tags in Translations.

end AWS.Services.Directory;
