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

with Ada.Text_IO;

with AWS.Net.WebSocket.Registry;

package body Notification_Center is

   Padding : String (1 .. 64_000) := (others => 'a');
   --  Padding to encourage threading issues

   protected body Protected_Center is

      ---------------
      -- Subscribe --
      ---------------

      procedure Subscribe (Socket : WebSock_CB.Object; Key : String) is
         S : constant Subscription :=
               (Socket => Socket, Key => To_Unbounded_String (Key));
      begin
         Subscriptions.Append (S);
      end Subscribe;

      -----------------
      -- Unsubscribe --
      -----------------

      procedure Unsubscribe (Socket : WebSock_CB.Object; Key : String) is
         use type WebSock_CB.Object;
         New_Subscriptions : Subscription_Vectors.Vector;
      begin
         for S of Subscriptions loop
            if S.Socket /= Socket
              or else S.Key /= To_Unbounded_String (Key)
            then
               New_Subscriptions.Append (S);
            end if;
         end loop;
         Subscriptions := New_Subscriptions;
      end Unsubscribe;

      procedure Unsubscribe (Socket : WebSock_CB.Object) is
         use type WebSock_CB.Object;
         New_Subscriptions : Subscription_Vectors.Vector;
      begin
         for S of Subscriptions loop
            if S.Socket /= Socket then
               New_Subscriptions.Append (S);
            end if;
         end loop;
         Subscriptions := New_Subscriptions;
      end Unsubscribe;

      ------------
      -- Notify --
      ------------

      procedure Notify (Key : String) is
         Message : constant String := Key & "," & Padding;
      begin
         for S of Subscriptions loop
            if To_String (S.Key) = Key then
               AWS.Net.WebSocket.Registry.Send (S.Socket, Message);
            end if;
         end loop;
      end Notify;

      -------------------------------
      -- Get_Subscriptions_For_Key --
      -------------------------------

      function Get_Subscriptions_For_Key
        (Key : String) return Subscription_Vectors.Vector
      is
         New_Subscriptions : Subscription_Vectors.Vector;
      begin
         for S of Subscriptions loop
            if To_String (S.Key) = Key then
               New_Subscriptions.Append (S);
            end if;
         end loop;
         return New_Subscriptions;
      end Get_Subscriptions_For_Key;

   end Protected_Center;

   ------------------------
   -- Unprotected_Notify --
   ------------------------

   --  Occurs outside of the protected block, so we could be executing Send
   --  at the same time as other tasks.

   procedure Unprotected_Notify (Key : String) is
      Message       : constant String := Key & "," & Padding;
      Subscriptions : Subscription_Vectors.Vector :=
                        Protected_Center.Get_Subscriptions_For_Key (Key);
   begin
      for S of Subscriptions loop
         AWS.Net.WebSocket.Registry.Send (S.Socket, Message);
      end loop;
   end Unprotected_Notify;

end Notification_Center;
