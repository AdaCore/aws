------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WebSock_CB;

package Notification_Center is

   use Ada.Strings.Unbounded;

   type Subscription is record
      Socket : WebSock_CB.Object;
      Key    : Unbounded_String;
   end record;

   package Subscription_Vectors is
      new Ada.Containers.Vectors (Natural, Subscription);

   protected Protected_Center is

      procedure Subscribe (Socket : WebSock_CB.Object; Key : String);

      procedure Unsubscribe (Socket : WebSock_CB.Object; Key : String);

      procedure Unsubscribe (Socket : WebSock_CB.Object);

      procedure Notify (Key : String);

      function Get_Subscriptions_For_Key
        (Key : String) return Subscription_Vectors.Vector;

   private
      Subscriptions : Subscription_Vectors.Vector;
   end Protected_Center;

   procedure Unprotected_Notify (Key : String);

end Notification_Center;
