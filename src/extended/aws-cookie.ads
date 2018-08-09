------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

--  A package for basic HTTP state management, ie. cookies. Tokens and
--  attributes adhere to RFC-2109: http://tools.ietf.org/html/rfc2109

with AWS.Default;
with AWS.Response;
with AWS.Status;

package AWS.Cookie is

   use type AWS.Response.Data_Mode;

   Response_Data_Not_Initialized : exception;
   --  The Response_Data_Not_Initialized exception is raised when trying to add
   --  headers to an un-initialized AWS.Response.Data object.
   --  The AWS.Response.Data object is initialized using the
   --  AWS.Response.Build function.

   No_Max_Age : constant Duration;
   --  When no Max-Age is required, this value can be passed to the Set
   --  routines below.

   function Exists
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Boolean;
   --  Check if the 'Key' cookie exists in AWS.Headers.List. Return Boolean
   --  True of the cookie exists, else Boolean False.

   procedure Expire
     (Content : in out Response.Data;
      Key     : String;
      Path    : String := "/");
   --  Expire the 'Key' cookie. This is done by setting the Max-Age attribute
   --  to 0. The Value of the cookie is also set to "", in case a browser does
   --  not honor the Max-Age attribute.

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return String;
   --  Return the 'Key' cookie from AWS.Headers.List. If the cookie does not
   --  exist, return an empty string, ie. ""

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Integer;
   --  Return the 'Key' cookie from AWS.Headers.List. If the cookie does not
   --  exist or can't be converted from String to Integer then return 0.

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Float;
   --  Return the 'Key' cookie from AWS.Headers.List. If the cookie does not
   --  exist or can't be converted from String to Float then return 0.0.

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Boolean;
   --  Return the 'Key' cookie from AWS.Headers.List. Only if the cookie value
   --  equals "True" is Boolean True returned, else Boolean False is returned.

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : String;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False)
   with Pre => Response.Mode (Content) /= Response.No_Data;
   --  Set a new cookie named 'Key' with value 'Value'. See RFC 2109 for more
   --  information about the individual cookie attributes:
   --    http://tools.ietf.org/html/rfc2109
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : Integer;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False)
   with Pre => Response.Mode (Content) /= Response.No_Data;
   --  Set a new cookie named 'Key' with Integer value 'Value'. The Integer is
   --  converted to a String, as both cookie keys and values are inherently
   --  strings.
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : Float;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False)
   with Pre => Response.Mode (Content) /= Response.No_Data;
   --  Set a new cookie named 'Key' with Float value 'Value'. The Float is
   --  converted to a String, as both cookie keys and values are inherently
   --  strings.
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : Boolean;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False)
   with Pre => Response.Mode (Content) /= Response.No_Data;
   --  Set a new cookie named 'Key' with Boolean value 'Value'. The Boolean is
   --  converted to a String ("False" or "True"), as both cookie keys and
   --  values are inherently strings.
   --
   --  Exceptions:
   --    Response_Data_Not_Initialized
   --      Is raised if AWS.Cookie.Set is called before the Content object has
   --      been initialized by a call to AWS.Response.Build

private
   No_Max_Age : constant Duration := Duration'First;
end AWS.Cookie;
