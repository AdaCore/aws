------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  Server push regression test

with Ada.Calendar.Formatting;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO.Editing;

with AWS.Client;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Push;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

package body S_Sp_Pck is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   type Push_Data_Type is delta 0.01 digits 7;

   CRLF        : constant String := ASCII.CR & ASCII.LF;
   End_Of_Part : constant String := '.' & CRLF;

   function To_Array
     (Data : Push_Data_Type;
      Env  : Text_IO.Editing.Picture)
      return Ada.Streams.Stream_Element_Array;

   package Server_Push is new AWS.Server.Push
     (Client_Output_Type => Push_Data_Type,
      Client_Environment => Text_IO.Editing.Picture,
      To_Stream_Array    => To_Array);

   function CB (Request : Status.Data) return Response.Data;

   Push : Server_Push.Object;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      use AWS.Parameters;
      P_List     : constant List := AWS.Status.Parameters (Request);
      Mode_Image : String := Get (P_List, "mode");
      Mode_Value : Server_Push.Mode := Server_Push.Mode'Value (Mode_Image);

      Picture : array (Server_Push.Mode) of Editing.Picture
        := (Server_Push.Plain     => Editing.To_Picture ("999999.99"),
            Server_Push.Chunked   => Editing.To_Picture ("##_##9.99"),
            Server_Push.Multipart => Editing.To_Picture ("zzzzz9.99"));
   begin
      Server_Push.Register
        (Server      => Push,
         Client_ID   => Mode_Image,
         Socket      => AWS.Status.Socket (Request),
         Environment => Picture (Mode_Value),
         Kind        => Mode_Value);
      return Response.Socket_Taken;
   end CB;

   ---------
   -- Run --
   ---------

   procedure Run (Security : Boolean) is
      Connect : array (Server_Push.Mode'Range) of Client.HTTP_Connection;
      Answer  : AWS.Response.Data;
      Data    : Push_Data_Type;

      HTTP : AWS.Server.HTTP;

      procedure Output (Data : String);
      --  Ignore random string --AWS.Push.Boundary_1044468257,
      --  and by the way ignore ASCII.CR because on the Win32 platform
      --  the Ada.Text_IO.Put_Line add the ASCII.CR before ASCII.LF even so
      --  the ASCII.CR already exists before ASCII.LF.

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
        (HTTP,
         "Testing server push.",
         CB'Access,
         Port           => 0,
         Security       => Security,
         Max_Connection => 3);

      Data := 1000.0;

      --  Initialize all the push connections

      for J in Connect'Range loop
         Client.Create
           (Connection  => Connect (J),
            Host        => Server.Status.Local_URL (HTTP),
            Timeouts    => Client.Timeouts
              (Connect => 5.0,
               Send => 15.0, Receive => 15.0, Response => 15.0),
            Server_Push => True);

         Client.Get (Connect (J), Answer, "/uri?mode="
            & Server_Push.Mode'Image (J));
      end loop;

      --  Send some data

      for K in 1 .. 32 loop

         Data := Data + 1.0;

         Server_Push.Send (Push, Data => Data, Content_Type => "text/plain");

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
         --  Next line will automatically unregister server push clients,
         --  closed in previous iterations.
         --  Last connection would not be unregistered.

         Server_Push.Send (Push, Data => Data, Content_Type => "text/plain");

         Client.Close (Connect (J));

         if not Server_Push.Wait_Send_Completion (10.0) then
            Put_Line ("Wait send completion timeout.");
         end if;
      end loop;

      if Server_Push.Count (Push) /= 1 then
         declare

            procedure Process
              (Client_Id   : String;
               Address     : String;
               State       : String;
               Environment : Text_IO.Editing.Picture;
               Kind        : Server_Push.Mode;
               Groups      : Server_Push.Group_Set) is
            begin
               Put_Line (Client_Id & ' ' & Address & ' ' & State);
            end Process;

            Clients     : Natural;
            Groups      : Natural;
            Size        : Natural;
            Max_Size    : Natural;
            Max_Size_DT : Ada.Calendar.Time;
            Counter     : Server_Push.Wait_Counter_Type;

         begin
            Server_Push.Info (Push, Clients, Groups, Process'Access);
            Server_Push.Info
              (Size, Max_Size => Max_Size, Max_Size_DT => Max_Size_DT,
               Counter => Counter);
            Put_Line
              ("Auto unregister error " & Clients'Img & Groups'Img & Size'Img
               & Max_Size'Img & Ada.Calendar.Formatting.Image (Max_Size_DT)
               & Counter'Img);
         end;
      end if;

      Server_Push.Unregister_Clients (Push);
   end Run;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Data : Push_Data_Type;
      Env  : Text_IO.Editing.Picture)
      return Ada.Streams.Stream_Element_Array
   is
      package Format is new Text_IO.Editing.Decimal_Output (Push_Data_Type);
   begin
      return Translator.To_Stream_Element_Array
               (Format.Image (Data, Env) & End_Of_Part);
   end To_Array;

end S_Sp_Pck;
