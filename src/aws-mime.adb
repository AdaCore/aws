------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2005                          --
--                                 AdaCore                                  --
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with GNAT.Regexp;

with AWS.Containers.Key_Value;
with AWS.OS_Lib;
with AWS.Resources.Files;

package body AWS.MIME is

   use Ada;
   use Ada.Strings.Unbounded;
   use GNAT;

   Default_Content_Type : constant String := Application_Octet_Stream;
   --  Default MIME Content Type used if nothing matches

   --  Table regexp components

   type R_MIME_Type is record
      Regexp    : GNAT.Regexp.Regexp; -- Filename Regexp
      MIME_Type : Unbounded_String;   -- Associated content type
   end record;

   type Node;
   type Node_Access is access Node;

   type Node is record
      Item : R_MIME_Type;
      Next : Node_Access;
   end record;

   package Key_Value renames Containers.Key_Value.Table.Containers;

   --  Protected Set to access tables handling MIME types

   protected Set is

      function Get (Filename : in String; Default : in String) return String;
      --  Returns Filename's MIME content type

      function Extension (Content_Type : in String) return String;
      --  Returns the best guess of the extension to use for the Content Type

      procedure Add_Extension (Ext : in String; MIME_Type : in String);
      --  Add Ext to the set of known content type extensions

      procedure Add_Regexp
        (Filename  : in Regexp.Regexp;
         MIME_Type : in String);
      --  Add Filename to the set of known content type regular expressions

   private
      Ext_Set : Containers.Key_Value.Set;
      R_Table : Node_Access;
      Last    : Node_Access;
   end Set;

   function To_Lower (Item : in String)
     return String
     renames Ada.Characters.Handling.To_Lower;

   procedure Initialize;
   --  Initialize MIME table

   function File_Extension (Filename : in String) return String;
   pragma Inline (File_Extension);
   --  Returns file extension without the dot

   function Is_Type
     (MIME_Type : in String;
      Type_Name : in String) return Boolean;
   pragma Inline (Is_Type);
   --  Returns True if MIME_Type is of Type_Name type. The type name is the
   --  first part of the MIME Type (the part before the /).

   -------------------
   -- Add_Extension --
   -------------------

   procedure Add_Extension (Ext : in String; MIME_Type : in String) is
   begin
      Set.Add_Extension (Ext, MIME_Type);
   end Add_Extension;

   ----------------
   -- Add_Regexp --
   ----------------

   procedure Add_Regexp (Filename : in String; MIME_Type : in String) is
      R_Filename : Regexp.Regexp;
   begin
      R_Filename := Regexp.Compile (Filename);
      Set.Add_Regexp (R_Filename, MIME_Type);
   exception
      when Regexp.Error_In_Regexp =>
         Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Regexp " & Filename & " can't be compiled");
   end Add_Regexp;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type
     (Filename : in String;
      Default  : in String := Application_Octet_Stream) return String is
   begin
      return Set.Get (Filename, Default => Default);
   end Content_Type;

   ---------------
   -- Extension --
   ---------------

   function Extension (Content_Type : in String) return String is
   begin
      return Set.Extension (Content_Type);
   end Extension;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (Filename : in String) return String is
      Ext : constant String := OS_Lib.File_Extension (Filename);
   begin
      if Ext'Length > 0 then
         return Ext (Ext'First + 1 .. Ext'Last);
      else
         return Ext;
      end if;
   end File_Extension;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      AWS_MIME : constant String := "aws.mime";

      procedure Load;
      --  Load aws.mime file, record every MIME types. Note that this file can
      --  be a resource (embedded file).

      ----------
      -- Load --
      ----------

      procedure Load is
         use type Strings.Maps.Character_Set;

         MIME_Set : constant Strings.Maps.Character_Set
           := not Strings.Maps.To_Set (" " & ASCII.HT);
         --  The token would be any characters sequence
         --  other then space an tab.

         File     : Resources.File_Type;

         Buffer   : String (1 .. 1_024);
         Last     : Natural;

         --  MIME type slice (M_First .. M_Last)

         M_First  : Positive;
         M_Last   : Natural;

         --  Extension slice (E_First .. E_Last)
         E_First  : Positive;
         E_Last   : Natural;

      begin
         Resources.Files.Open (File, AWS_MIME);

         while not Resources.End_Of_File (File) loop
            Resources.Get_Line (File, Buffer, Last);

            --  Look for the MIME type

            Strings.Fixed.Find_Token
              (Buffer (Buffer'First .. Last),
               MIME_Set, Strings.Inside, M_First, M_Last);

            E_First := M_Last + 1;

            Read_Extension : loop
               Strings.Fixed.Find_Token
                 (Buffer (E_First .. Last),
                  MIME_Set, Strings.Inside, E_First, E_Last);

               exit Read_Extension when E_Last = 0;

               if Buffer (E_First) = '/' and then Buffer (E_Last) = '/' then
                  --  Regular expression is inside slashes.
                  Add_Regexp
                    (Buffer (E_First + 1 .. E_Last - 1),
                     Buffer (M_First .. M_Last));
               else
                  Set.Add_Extension
                    (Buffer (E_First .. E_Last),
                     Buffer (M_First .. M_Last));
               end if;

               E_First := E_Last + 1;
            end loop Read_Extension;

         end loop;

         Resources.Close (File);
      end Load;

   begin
      --  Text

      Set.Add_Extension ("css",     Text_CSS);
      Set.Add_Extension ("html",    Text_HTML);
      Set.Add_Extension ("htm",     Text_HTML);
      Set.Add_Extension ("txt",     Text_Plain);
      Set.Add_Extension ("ada",     Text_Plain);
      Set.Add_Extension ("ads",     Text_Plain);
      Set.Add_Extension ("adb",     Text_Plain);
      Set.Add_Extension ("c",       Text_Plain);
      Set.Add_Extension ("h",       Text_Plain);
      Set.Add_Extension ("xml",     Text_XML);
      Set.Add_Extension ("sgml",    Text_X_SGML);
      Set.Add_Extension ("sgm",     Text_X_SGML);

      --  Image

      Set.Add_Extension ("gif",     Image_Gif);
      Set.Add_Extension ("jpg",     Image_Jpeg);
      Set.Add_Extension ("jpeg",    Image_Jpeg);
      Set.Add_Extension ("jpe",     Image_Jpeg);
      Set.Add_Extension ("png",     Image_Png);
      Set.Add_Extension ("tiff",    Image_Tiff);
      Set.Add_Extension ("tif",     Image_Tiff);
      Set.Add_Extension ("pnm",     Image_X_Portable_Anymap);
      Set.Add_Extension ("pbm",     Image_X_Portable_Bitmap);
      Set.Add_Extension ("pgm",     Image_X_Portable_Graymap);
      Set.Add_Extension ("ppm",     Image_X_Portable_Pixmap);
      Set.Add_Extension ("rgb",     Image_X_RGB);
      Set.Add_Extension ("xbm",     Image_X_Xbitmap);
      Set.Add_Extension ("xpm",     Image_X_Xpixmap);
      Set.Add_Extension ("xwd",     Image_X_Xwindowdump);

      --  Application

      Set.Add_Extension ("ps",      Application_Postscript);
      Set.Add_Extension ("eps",     Application_Postscript);
      Set.Add_Extension ("ai",      Application_Postscript);
      Set.Add_Extension ("pdf",     Application_Pdf);
      Set.Add_Extension ("zip",     Application_Zip);
      Set.Add_Extension ("exe",     Application_Octet_Stream);
      Set.Add_Extension ("hqx",     Application_Mac_Binhex40);
      Set.Add_Extension ("doc",     Application_Msword);
      Set.Add_Extension ("ppt",     Application_Powerpoint);
      Set.Add_Extension ("rtf",     Application_Rtf);
      Set.Add_Extension ("Z",       Application_X_Compress);
      Set.Add_Extension ("gtar",    Application_X_GTar);
      Set.Add_Extension ("gz",      Application_X_GZip);
      Set.Add_Extension ("latex",   Application_X_Latex);
      Set.Add_Extension ("sh",      Application_X_Sh);
      Set.Add_Extension ("shar",    Application_X_Shar);
      Set.Add_Extension ("tar",     Application_X_Tar);
      Set.Add_Extension ("tcl",     Application_X_Tcl);
      Set.Add_Extension ("tex",     Application_X_Tex);
      Set.Add_Extension ("texinfo", Application_X_Texinfo);
      Set.Add_Extension ("texi",    Application_X_Texinfo);
      Set.Add_Extension ("t",       Application_X_Troff);
      Set.Add_Extension ("tr",      Application_X_Troff);
      Set.Add_Extension ("roff",    Application_X_Troff);
      Set.Add_Extension ("man",     Application_X_Troff_Man);

      --  Audio

      Set.Add_Extension ("au",      Audio_Basic);
      Set.Add_Extension ("snd",     Audio_Basic);
      Set.Add_Extension ("mpga",    Audio_Mpeg);
      Set.Add_Extension ("mp2",     Audio_Mpeg);
      Set.Add_Extension ("mp3",     Audio_Mpeg);
      Set.Add_Extension ("wav",     Audio_X_Wav);
      Set.Add_Extension ("ram",     Audio_X_Pn_Realaudio);
      Set.Add_Extension ("rpm",     Audio_X_Pn_Realaudio_Plugin);
      Set.Add_Extension ("ra",      Audio_X_Realaudio);

      --  Video

      Set.Add_Extension ("mpeg",    Video_Mpeg);
      Set.Add_Extension ("mpg",     Video_Mpeg);
      Set.Add_Extension ("mpe",     Video_Mpeg);
      Set.Add_Extension ("qt",      Video_Quicktime);
      Set.Add_Extension ("mov",     Video_Quicktime);
      Set.Add_Extension ("avi",     Video_X_Msvideo);

      --  Check if there is a aws.mime file to read

      if AWS.OS_Lib.Is_Regular_File (AWS_MIME) then
         Load;
      end if;
   end Initialize;

   --------------------
   -- Is_Application --
   --------------------

   function Is_Application (MIME_Type : in String) return Boolean is
   begin
      return Is_Type (MIME_Type, "application/");
   end Is_Application;

   --------------
   -- Is_Audio --
   --------------

   function Is_Audio (MIME_Type : in String) return Boolean is
   begin
      return Is_Type (MIME_Type, "audio/");
   end Is_Audio;

   --------------
   -- Is_Image --
   --------------

   function Is_Image (MIME_Type : in String) return Boolean is
   begin
      return Is_Type (MIME_Type, "image/");
   end Is_Image;

   -------------
   -- Is_Text --
   -------------

   function Is_Text (MIME_Type : in String) return Boolean is
   begin
      return Is_Type (MIME_Type, "text/");
   end Is_Text;

   -------------
   -- Is_Type --
   -------------

   function Is_Type
     (MIME_Type : in String;
      Type_Name : in String) return Boolean is
   begin
      return MIME_Type'Length > Type_Name'Length
          and then
        To_Lower
          (MIME_Type
             (MIME_Type'First .. MIME_Type'First + Type_Name'Length - 1))
        = Type_Name;
   end Is_Type;

   --------------
   -- Is_Video --
   --------------

   function Is_Video (MIME_Type : in String) return Boolean is
   begin
      return Is_Type (MIME_Type, "video/");
   end Is_Video;

   ---------
   -- Set --
   ---------

   protected body Set is

      -------------------
      -- Add_Extension --
      -------------------

      procedure Add_Extension (Ext : in String; MIME_Type : in String) is
      begin
         Key_Value.Include (Ext_Set, Ext, To_Unbounded_String (MIME_Type));
      end Add_Extension;

      ----------------
      -- Add_Regexp --
      ----------------

      procedure Add_Regexp
        (Filename  : in Regexp.Regexp;
         MIME_Type : in String)
      is
         Item : constant R_MIME_Type
           := (Filename, To_Unbounded_String (MIME_Type));
      begin
         if R_Table = null then
            R_Table := new Node'(Item, null);
            Last    := R_Table;
         else
            Last.Next := new Node'(Item, null);
            Last      := Last.Next;
         end if;
      end Add_Regexp;

      ---------------
      -- Extension --
      ---------------

      function Extension (Content_Type : in String) return String is

         CT     : constant Unbounded_String :=
                    To_Unbounded_String (Content_Type);
         Result : Unbounded_String;

         Exit_Iteration : exception;

         procedure Process (Position : in Containers.Key_Value.Cursor);
         --  Iterator callback procedure

         -------------
         -- Process --
         -------------

         procedure Process (Position : in Containers.Key_Value.Cursor) is
         begin
            if
              Containers.Key_Value.Table.Containers.Element (Position) = CT
            then
               Result
                 := To_Unbounded_String
                      (Containers.Key_Value.Table.Containers.Key (Position));
               raise Exit_Iteration;
            end if;
         end Process;

      begin
         if Content_Type = Default_Content_Type then
            null; -- We don't want give unknown data the exe extension
         elsif Content_Type = Text_Plain then
            return "txt";
         else
            Key_Value.Iterate (Ext_Set, Process'Access);
         end if;

         return "";

      exception
         when Exit_Iteration =>
            return To_String (Result);
      end Extension;

      ---------
      -- Get --
      ---------

      function Get (Filename : in String; Default : in String) return String is
         Ext    : constant String := File_Extension (Filename);
         Cursor : Containers.Key_Value.Cursor;
      begin
         Cursor := Key_Value.Find (Ext_Set, Ext);

         if Key_Value.Has_Element (Cursor) then
            return To_String (Key_Value.Element (Cursor));

         else
            --  Check now in regexp list

            declare
               N : Node_Access := R_Table;
            begin
               while N /= null loop
                  if Regexp.Match (Filename, N.Item.Regexp) then
                     return To_String (N.Item.MIME_Type);
                  end if;

                  N := N.Next;
               end loop;
            end;
         end if;

         return Default;
      end Get;

   end Set;

begin
   Initialize;
end AWS.MIME;
