------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2012, AdaCore                       --
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

with Ada.Calendar.Formatting;
with Ada.Text_IO;

package body Cert_CB is

   use Ada;

   procedure Display (Cert : Net.SSL.Certificate.Object);
   --  Display certificate object

   -------------
   -- Display --
   -------------

   procedure Display (Cert : Net.SSL.Certificate.Object) is
   begin
      Text_IO.Put_Line
        ("  issuer          : " & Net.SSL.Certificate.Issuer (Cert));
      Text_IO.Put_Line
        ("  subject         : " & Net.SSL.Certificate.Subject (Cert));
      Text_IO.Put_Line
        ("  activation time : "
         & Calendar.Formatting.Image
           (Net.SSL.Certificate.Activation_Time (Cert)));
      Text_IO.Put_Line
        ("  expiration time : "
         & Calendar.Formatting.Image
           (Net.SSL.Certificate.Expiration_Time (Cert)));
      Text_IO.New_Line;
   end Display;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return Response.Data is
      Sock : constant Net.Socket_Access := Status.Socket (Request);
      Cert : constant Net.SSL.Certificate.Object :=
               Net.SSL.Certificate.Get (Net.SSL.Socket_Type (Sock.all));
   begin
      Text_IO.Put_Line ("Client certificate from user's callback:");
      Display (Cert);
      return Response.Build ("text/html", "<p>Hello world !");
   end HW_CB;

   -----------------
   -- Verify_Cert --
   -----------------

   function Verify_Cert (Cert : Net.SSL.Certificate.Object) return Boolean is
      use type Calendar.Time;
   begin
      Text_IO.Put_Line ("Client certificate from verify routine:");
      Display (Cert);

      --  Return verified status from the SSL layer

      return Net.SSL.Certificate.Verified (Cert);
   end Verify_Cert;

end Cert_CB;
