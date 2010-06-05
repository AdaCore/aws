------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2010, AdaCore                       --
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

--  A package for basic HTTP state management, ie. cookies. Tokens and
--  attributes adhere to RFC-2109: http://tools.ietf.org/html/rfc2109

with AWS.Default;
with AWS.Response;
with AWS.Status;

package AWS.Cookie is

   Response_Data_Not_Initialized : exception;
   --  The Response_Data_Not_Initialized exception is raised when trying to add
   --  headers to an un-initialized AWS.Response.Data object.
   --  The AWS.Response.Data object is initialized using the
   --  AWS.Response.Build function.

   function Exists
     (Request : Status.Data;
      Key     : String) return Boolean;
   --  Check if the 'Key' cookie exists in AWS.Headers.List. Return Boolean
   --  True of the cookie exists, else Boolean False.

   procedure Expire
     (Content : in out Response.Data;
      Key     :        String;
      Path    :        String := "/");
   --  Expire the 'Key' cookie. This is done by setting the Max-Age attribute
   --  to 0. The Value of the cookie is also set to "", in case a browser does
   --  not honor the Max-Age attribute.

   function Get
     (Request : Status.Data;
      Key     : String) return String;
   --  Return the 'Key' cookie from AWS.Headers.List. If the cookie does not
   --  exist, return an empty string, ie. ""

   function Get
     (Request : Status.Data;
      Key     : String) return Integer;
   --  Return the 'Key' cookie from AWS.Headers.List. If the cookie does not
   --  exist or can't be converted from String to Integer then return 0.

   function Get
     (Request : Status.Data;
      Key     : String) return Float;
   --  Return the 'Key' cookie from AWS.Headers.List. If the cookie does not
   --  exist or can't be converted from String to Float then return 0.0.

   function Get
     (Request : Status.Data;
      Key     : String) return Boolean;
   --  Return the 'Key' cookie from AWS.Headers.List. Only if the cookie value
   --  equals "True" is Boolean True returned, else Boolean False is returned.

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : String;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Duration := Default.Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False);
   --  Set a new cookie named 'Key' with value 'Value'. See RFC 2109 for more
   --  information about the individual cookie attributes:
   --    http://tools.ietf.org/html/rfc2109
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : Integer;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Duration := Default.Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False);
   --  Set a new cookie named 'Key' with Integer value 'Value'. The Integer is
   --  converted to a String, as both cookie keys and values are inherently
   --  strings.
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : Float;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Duration := Default.Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False);
   --  Set a new cookie named 'Key' with Float value 'Value'. The Float is
   --  converted to a String, as both cookie keys and values are inherently
   --  strings.
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : Boolean;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Duration := Default.Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False);
   --  Set a new cookie named 'Key' with Boolean value 'Value'. The Boolean is
   --  converted to a String ("False" or "True"), as both cookie keys and
   --  values are inherently strings.
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

end AWS.Cookie;
