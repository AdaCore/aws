------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.LDAP.Client;

procedure Test_LDAP is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   use type AWS.LDAP.Client.LDAP_Message;

   Directory    : LDAP.Client.Directory;
   Response_Set : LDAP.Client.LDAP_Message;
   Message      : LDAP.Client.LDAP_Message;
   BER          : aliased LDAP.Client.BER_Element;

   Host    : String := "ldap.nyu.edu";
   User_DN : String := "";
   User_PW : String := "";

   Base_DN : String := "o=New York University,st=New York,c=US";
   Filter  : String := "(sn=DUPON*)";

begin  -- LDAP_Test
   Directory := LDAP.Client.Init (Host);

   LDAP.Client.Bind (Directory, User_DN, User_PW);

   Response_Set := LDAP.Client.Search
     (Directory, Base_DN, Filter, LDAP.Client.LDAP_Scope_Subtree,
      LDAP.Client.Attributes
        ("cn", "sn", "telephonenumber"));

   --  Get first entry

   Message := LDAP.Client.First_Entry (Directory, Response_Set);

   Text_IO.Put_Line
     ("Found "
        & Natural'Image (LDAP.Client.Count_Entries (Directory, Message)));

   while Message /= LDAP.Client.Null_LDAP_Message loop

      Text_IO.New_Line;
      Text_IO.Put_Line ("Found DN " & LDAP.Client.Get_DN (Directory, Message));
      Text_IO.Put_Line
        ("     UFN "
           & LDAP.Client.DN2UFN (LDAP.Client.Get_DN (Directory, Message)));

      --  Test Explode_DN

      declare
         Set : LDAP.Client.String_Set
           := LDAP.Client.Explode_DN
           (LDAP.Client.Get_DN (Directory, Message), No_Types => False);
      begin
         for K in Set'Range loop
            Text_IO.Put_Line (" -> " & To_String (Set (K)));
         end loop;
      end;

      --  Output all attributes

      declare
         Attrs : Unbounded_String;
      begin
         Attrs := To_Unbounded_String
           (LDAP.Client.First_Attribute (Directory, Message,
                                         BER'Unchecked_Access));
         loop
            Text_IO.Put (" " & To_String (Attrs));

            declare
               RS : LDAP.Client.String_Set := LDAP.Client.Get_Values
                 (Directory, Message, To_String (Attrs));
            begin
               for K in RS'Range loop
                  Text_IO.Put (" " & To_String (RS (K)));
               end loop;
            end;

            Text_IO.New_Line;

            Attrs := To_Unbounded_String
              (LDAP.Client.Next_Attribute (Directory, Message, BER));

            --  Exit when no more attributes
            exit when Attrs = Null_Unbounded_String;
         end loop;

         LDAP.Client.Free (BER);
      end;

      --  Get the next entry

      Message := LDAP.Client.Next_Entry (Directory, Message);
   end loop;

   --  Free memory

   LDAP.Client.Free (Message);
   LDAP.Client.Unbind (Directory);
end Test_LDAP;
