------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  Server push regression test.

with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Text_IO.Editing;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Push;
with AWS.Status;

procedure Sp is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   type Push_Data_Type is delta 0.01 digits 7;

   function Image
     (Data : in Push_Data_Type;
      Env  : in Editing.Picture)
      return String;

   function CB (Request : in Status.Data) return Response.Data;

   procedure Output (Data : String);
   --  Ignore random string --AWS.Push.Boundary_1044468257,
   --  and by the way ignore ASCII.CR becouse on the Win32 platform
   --  the Ada.Text_IO.Put_Line add the ASCII.CR before ASCII.LF even so
   --  the ASCII.CR already exists before ASCII.LF.

   package Server_Push is new AWS.Server.Push
     (Client_Output_Type => Push_Data_Type,
      Stream_Output_Type => String,
      Client_Environment => Editing.Picture,
      To_Stream_Output   => Image);

   package Format is new Editing.Decimal_Output (Push_Data_Type);

   HTTP        : AWS.Server.HTTP;
   Connect     : array (Server_Push.Mode'Range) of Client.HTTP_Connection;
   Push        : Server_Push.Object;
   Answer      : AWS.Response.Data;

   CRLF        : constant String := ASCII.CR & ASCII.LF;
   End_Of_Part : constant String := '.' & CRLF;

   Data        : Push_Data_Type := 1000.0;

   Picture : array (Server_Push.Mode) of Editing.Picture
     := (Server_Push.Plain     => Editing.To_Picture ("999999.99"),
         Server_Push.Chunked   => Editing.To_Picture ("##_##9.99"),
         Server_Push.Multipart => Editing.To_Picture ("zzzzz9.99"));

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      use AWS.Parameters;
      P_List : List := AWS.Status.Parameters (Request);
      Mode_Image : String := Get (P_List, "mode");
      Mode_Value : Server_Push.Mode := Server_Push.Mode'Value (Mode_Image);
   begin
      Server_Push.Register
        (Server      => Push,
         Client_ID   => Mode_Image,
         Socket      => AWS.Status.Socket (Request),
         Environment => Picture (Mode_Value),
         Kind        => Mode_Value);
      return Response.Socket_Taken;
   end CB;

   -----------
   -- Image --
   -----------

   function Image
     (Data : in Push_Data_Type;
      Env  : in Editing.Picture)
      return String is
   begin
      return Format.Image (Data, Env) & End_Of_Part;
   end Image;

   ------------
   -- Output --
   ------------

   procedure Output (Data : String) is
      Ignore_Sample : constant String := "--AWS.Push.Boundary_";
      use Ada.Strings;
      First : Positive := Data'First;
      Index : Natural;
   begin
      loop
         Index := Fixed.Index (Data (First .. Data'Last), CRLF);
         if Index = 0 then
            Put (Data (First .. Data'Last));
            exit;
         else
            if Index - First < Ignore_Sample'Length
              or else Data (First .. First + Ignore_Sample'Length - 1)
                       /= Ignore_Sample
            then
               Put_Line (Data (First .. Index - 1));
            end if;
            First := Index + CRLF'Length;
         end if;
         exit when First > Data'Last;
      end loop;
   end Output;

begin
   AWS.Server.Start
     (HTTP, "Testing server push.",
      CB'Unrestricted_Access, Port => 1249, Max_Connection => 3);

   --  Init the all modes server push transfers.

   for J in Connect'Range loop
      Client.Create
        (Connection  => Connect (J),
         Host        => "http://localhost:1249",
         Timeouts    => (15, 15),
         Server_Push => True);

      Client.Get (Connect (J), Answer, "/uri?mode="
         & Server_Push.Mode'Image (J));
   end loop;

   for K in 1 .. 32 loop

      Data := Data + 1.0;

      Server_Push.Send (Push, Data, "text/plain");

      for J in Connect'Range loop
         Put_Line ("--------------------------------");
         Output (Client.Read_Until (Connect (J), End_Of_Part));

         Data := Data + 0.01;

         Server_Push.Send_To
           (Push,
            Server_Push.Mode'Image (J),
            Data,
            "text/plain");

         Output (Client.Read_Until (Connect (J), End_Of_Part));
      end loop;

   end loop;

   for J in Connect'Range loop
      Client.Close (Connect (J));
   end loop;
exception
   when E : others =>
      Put_Line ("Error " & Ada.Exceptions.Exception_Information (E));
end Sp;
