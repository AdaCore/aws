------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2007                            --
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

--  with AWS.Parameters.Set;

package body AWS.URL.Set is

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (URL      : in out Object;
      Host     : in     String;
      Port     : in     Positive;
      Security : in     Boolean) is
   begin
      if Host = "" then
         URL.Host := To_Unbounded_String ("localhost");
      else
         URL.Host := To_Unbounded_String (Host);
      end if;

      URL.Port := Port;
      URL.Security := Security;
   end Connection_Data;

   ----------------
   -- Parameters --
   ----------------

   procedure Parameters (URL : in out Object; Set : in AWS.Parameters.List) is
   begin
      URL.Parameters := Set;
   end Parameters;

   function Parameters
     (URL : access Object) return access AWS.Parameters.List is
   begin
      return URL.Parameters'Access;
   end Parameters;

end AWS.URL.Set;
