------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

with Ada.Strings.Unbounded;

package AWS.Status is

   type Data is private;

   type Request_Method is (GET, POST);

   procedure Set_Host (D : in out Data; Host : in String);
   function Host (D : in Data) return String;

   procedure Set_Connection (D : in out Data; Connection : in String);
   function Connection (D : in Data) return String;

   procedure Set_Content_Length (D              : in out Data;
                                 Content_Length : in     Natural);
   function Content_Length (D : in Data) return Natural;

   procedure Set_If_Modified_Since (D                 : in out Data;
                                    If_Modified_Since : in     String);
   function If_Modified_Since (D : in Data) return String;

   procedure Set_File_Up_To_Date (D               : in out Data;
                                  File_Up_To_Date : in     Boolean);
   function File_Up_To_Date (D : in Data) return Boolean;

   procedure Set_Request (D            : in out Data;
                          Method       : in     Request_Method;
                          URI          : in     String;
                          HTTP_Version : in     String;
                          Parameters   : in     String := "");

   procedure Set_Parameters (D : in out Data; Parameters : in String);

   function Method       (D : in Data) return Request_Method;
   function URI          (D : in Data) return String;
   function HTTP_Version (D : in Data) return String;
   function Parameter    (D : in Data; N    : in Positive) return String;
   function Parameter    (D : in Data; Name : in String)   return String;

private

   pragma Inline (Set_Host);
   pragma Inline (Host);
   pragma Inline (Set_Request);
   pragma Inline (Method);
   pragma Inline (URI);
   pragma Inline (HTTP_Version);

   use Ada.Strings.Unbounded;

   type Data is record
      Connection        : Unbounded_String;
      Host              : Unbounded_String;
      Method            : Request_Method;
      URI               : Unbounded_String;
      Parameters        : Unbounded_String;
      HTTP_Version      : Unbounded_String;
      Content_Length    : Natural;
      If_Modified_Since : Unbounded_String;
      File_Up_To_Date   : Boolean := False;
   end record;

end AWS.Status;
