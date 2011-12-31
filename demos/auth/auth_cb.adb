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

with AWS.Digest;

package body Auth_CB is

   Auth_Username : constant String := "AWS";
   Auth_Password : constant String := "letmein";

   ---------
   -- Get --
   ---------

   function Get (Request : AWS.Status.Data) return AWS.Response.Data is
      use type AWS.Response.Authentication_Mode;
      use type AWS.Status.Authorization_Type;

      Username    : constant String := AWS.Status.Authorization_Name (Request);
      Client_Mode : constant AWS.Status.Authorization_Type
        := AWS.Status.Authorization_Mode (Request);
   begin

      if Client_Mode = AWS.Status.Basic -- It is Basic authentication
        and then Username = Auth_Username
        and then AWS.Status.Authorization_Password (Request) = Auth_Password
        and then (Auth_Mode = AWS.Response.Any
                    or Auth_Mode = AWS.Response.Basic)
      then
         return AWS.Response.Build
           ("text/html",
            "<p>Basic authorization OK!");

      elsif Client_Mode = AWS.Status.Digest -- It is Digest authentication
        and then Username = Auth_Username
        and then AWS.Status.Check_Digest (Request, Auth_Password)
        and then (Auth_Mode = AWS.Response.Any
                    or Auth_Mode = AWS.Response.Digest)
      then
         if AWS.Digest.Check_Nonce
              (AWS.Status.Authorization_Nonce (Request))
         then
            return AWS.Response.Build
              ("text/html",
               "<p>Digest authorization OK!<br>"
                 & AWS.Status.Authorization_NC (Request));
         else
            --  Nonce is stale
            return AWS.Response.Authenticate
              ("AWS restricted usage", Auth_Mode, Stale => True);
         end if;
      else
         --  Unauthorized
         return AWS.Response.Authenticate ("AWS restricted usage", Auth_Mode);
      end if;
   end Get;

end Auth_CB;
