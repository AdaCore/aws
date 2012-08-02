------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with AWS.Utils;

package body AWS.Net.SSL.Certificate is

   pragma Warnings (Off);

   Error_Message : constant String := "SSL not supported.";

   ---------------------
   -- Activation_Time --
   ---------------------

   function Activation_Time (Certificate : Object) return Calendar.Time is
   begin
      raise Program_Error with Error_Message;
      return Utils.AWS_Epoch;
   end Activation_Time;

   ---------------------
   -- Expiration_Time --
   ---------------------

   function Expiration_Time (Certificate : Object) return Calendar.Time is
   begin
      raise Program_Error with Error_Message;
      return Utils.AWS_Epoch;
   end Expiration_Time;

   ---------
   -- Get --
   ---------

   function Get (Socket : Socket_Type) return Object is
      O : Object;
   begin
      raise Program_Error with Error_Message;
      return O;
   end Get;

   ------------
   -- Issuer --
   ------------

   function Issuer (Certificate : Object) return String is
   begin
      raise Program_Error with Error_Message;
      return "";
   end Issuer;

   -------------------------
   -- Set_Verify_Callback --
   -------------------------

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : Verify_Callback) is
   begin
      raise Program_Error with Error_Message;
   end Set_Verify_Callback;

   -------------
   -- Subject --
   -------------

   function Subject (Certificate : Object) return String is
   begin
      raise Program_Error with Error_Message;
      return "";
   end Subject;

end AWS.Net.SSL.Certificate;
