------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

pragma Ada_2012;

with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with AWS.Parameters;

package AWS.URL is

   use Ada;
   use Ada.Strings.Unbounded;

   --  The general URL form as described in RFC2616 is:
   --
   --  http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
   --
   --  Note also that there are different RFC describing URL like the 2616 and
   --  1738 but they use different terminologies. Here we try to follow the
   --  names used in RFC2616 but we have implemented some extensions at the
   --  end of this package. For example the way Path and File are separated or
   --  the handling of user/password which is explicitly not allowed in the
   --  RFC but are used and supported in many browsers. Here are the extended
   --  URL supported:
   --
   --  http://user:pass@www.here.com:80/dir1/dir2/xyz.html?p=8&x=doh#anchor
   --   |                    |       | |          |       |         |
   --   protocol             host port path       file   parameters fragment
   --
   --                                  <--  pathname  -->

   type Object is private;

   URL_Error : exception;

   Default_FTP_Port   : constant := 21;
   Default_HTTP_Port  : constant := 80;
   Default_HTTPS_Port : constant := 443;

   function Parse
     (URL            : String;
      Check_Validity : Boolean := True;
      Normalize      : Boolean := False) return Object;
   --  Parse an URL and return an Object representing this URL. It is then
   --  possible to extract each part of the URL with the services bellow.
   --  Raises URL_Error if Check_Validity is true and the URL reference a
   --  resource above the web root directory.

   procedure Normalize (URL : in out Object);
   --  Removes all occurrences to parent directory ".." and current directory
   --  ".". Raises URL_Error if the URL reference a resource above the Web
   --  root directory.

   function Is_Valid (URL : Object) return Boolean;
   --  Returns True if the URL is valid (does not reference directory above
   --  the Web root).

   function URL (URL : Object) return String;
   --  Returns full URL string, this can be different to the URL passed if it
   --  has been normalized.

   function Protocol_Name (URL : Object) return String;
   --  Returns "http" or "https" depending on the protocol used by URL

   function Host
     (URL : Object; IPv6_Brackets : Boolean := False) return String;
   --  Returns the hostname in IPv6 breakets if necessary

   function Port (URL : Object) return Positive;
   --  Returns the port as a positive

   function Port (URL : Object) return String;
   --  Returns the port as a string

   function Port_Not_Default (URL : Object) return String;
   --  Returns the port image (preceded by character ':') if it is not the
   --  default port. Returns the empty string otherwise.

   function Abs_Path
     (URL    : Object;
      Encode : Boolean := False) return String;
   --  Returns the absolute path. This is the complete resource reference
   --  without the query part.

   function Query
     (URL    : Object;
      Encode : Boolean := False) return String;
   --  Returns the Query part of the URL or the empty string if none was
   --  specified. Note that character '?' is not part of the Query and is
   --  therefore not returned.

   --
   --  Below are extended API not part of the RFC 2616 URL specification
   --

   function User (URL : Object) return String;
   --  Returns user name part of the URL. Returns the empty string if user was
   --  not specified.

   function Password (URL : Object) return String;
   --  Returns user's password part of the URL. Returns the empty string if
   --  password was not specified.

   function Server_Name
     (URL : Object; IPv6_Brackets : Boolean := False) return String
     renames Host;

   function Security (URL : Object) return Boolean;
   --  Returns True if it is a Secure HTTP (HTTPS) URL

   function Path (URL : Object; Encode : Boolean := False) return String;
   --  Returns the Path (including the leading slash). If Encode is True then
   --  the URL will be encoded using the Encode routine.

   function File (URL : Object; Encode : Boolean := False) return String;
   --  Returns the File. If Encode is True then the URL will be encoded using
   --  the Encode routine. Not that by File here we mean the latest part of
   --  the URL, it could be a real file or a diretory into the filesystem.
   --  Parent and current directories are part of the path.

   function Parameters
     (URL    : Object;
      Encode : Boolean := False) return String;
   --  Returns the Parameters (including the starting ? character). If Encode
   --  is True then the URL will be encoded using the Encode routine.

   function Pathname
     (URL    : Object;
      Encode : Boolean := False) return String renames Abs_Path;

   function Pathname_And_Parameters
     (URL    : Object;
      Encode : Boolean := False) return String;
   --  Returns the pathname and the parameters. This is equivalent to:
   --  Pathname & Parameters.

   function Parameter
     (URL : Object; Name : String; N : Positive := 1) return String
     with Inline;
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist.

   function Parameters (URL : Object) return AWS.Parameters.List with Inline;
   --  Return the parameter list associated with the URL

   function Fragment (URL : Object) return String with Inline;
   --  Return the part after the # sign (included)

   --
   --  URL Resolution
   --

   function Resolve (URL : Object; Base_URL : Object) return Object;
   --  Resolve an URL relative to a Base_URL. Uses RFC 3986, section 5.2
   --  algorithm.

   function Resolve (URL : String; Base_URL : String) return String;
   --  Resolve an URL relatively to a Base_URL. Same function as above, but
   --  working with Strings.

   --
   --  URL Encoding and Decoding
   --

   Parameters_Encoding_Set : constant Strings.Maps.Character_Set;
   --  Encoding set enought for HTTP parameters

   Default_Encoding_Set : constant Strings.Maps.Character_Set;
   --  Encoding set enought for all URL parts

   function Encode
     (Str          : String;
      Encoding_Set : Strings.Maps.Character_Set := Default_Encoding_Set)
      return String;
   --  Encode Str into a URL-safe form. Many characters are forbiden into an
   --  URL and needs to be encoded. A character is encoded by %XY where XY is
   --  the character's ASCII hexadecimal code. For example a space is encoded
   --  as %20.

   function Decode (Str : String) return String;
   --  This is the opposite of Encode above

   function Decode (Str : Unbounded_String) return Unbounded_String;

private

   use type Ada.Strings.Maps.Character_Set;

   type Path_Status is (Valid, Wrong);

   HTTP  : constant Unbounded_String := To_Unbounded_String ("http");
   HTTPS : constant Unbounded_String := To_Unbounded_String ("https");
   FTP   : constant Unbounded_String := To_Unbounded_String ("ftp");
   WS    : constant Unbounded_String := To_Unbounded_String ("ws");
   WSS   : constant Unbounded_String := To_Unbounded_String ("wss");

   type Object is record
      User       : Unbounded_String;
      Password   : Unbounded_String;
      Host       : Unbounded_String;
      Port       : Natural           := Default_HTTP_Port;
      Protocol   : Unbounded_String  := HTTP;
      Path       : Unbounded_String; -- Original path
      N_Path     : Unbounded_String; -- Normalized path
      File       : Unbounded_String;
      Status     : Path_Status       := Wrong;
      Parameters : aliased AWS.Parameters.List;
      Fragment   : Unbounded_String;
   end record;

   Parameters_Encoding_Set : constant Strings.Maps.Character_Set :=
                               Strings.Maps.To_Set ("&=+ <>?#%@")
                               or Strings.Maps.To_Set
                                    (Span => (Low  => Character'Val (128),
                                              High => Character'Last))
                               or Strings.Maps.To_Set
                                    (Span => (Low  => Character'First,
                                              High => Character'Val (31)));
   Default_Encoding_Set : constant Strings.Maps.Character_Set :=
                            Parameters_Encoding_Set
                            or Strings.Maps.To_Set (";/:$,""{}|\^[]`'");

end AWS.URL;
