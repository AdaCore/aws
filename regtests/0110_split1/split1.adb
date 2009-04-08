------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Services.Split_Pages;
with AWS.Services.Dispatchers.Transient_Pages;
with AWS.Status;
with AWS.Templates;
with AWS.Utils;

with Get_Free_Port;

procedure Split1 is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 1268;

   M_Body : Unbounded_String;

   Disp : Services.Dispatchers.Transient_Pages.Handler;
   Conf : Config.Object := Config.Get_Current;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      use Templates;

      URI : constant String := Status.URI (Request);

      T1 : Translate_Table
        := (Assoc ("ONE", "one"), Assoc ("TWO", "2"), Assoc ("THREE", "3"));

      T2 : Translate_Table
        := (Assoc ("V1", Vector_Tag'(+"t11" & "t12" & "t13")),
            Assoc ("V2", Vector_Tag'(+"t21" & "t22" & "t23")));

   begin
      if URI = "/main" then
         return Services.Split_Pages.Parse ("split1.tmplt", T1, T2, 2);
      else

         return Response.Acknowledge (Messages.S404);
      end if;
   end CB;

   ----------------
   -- Clean_Body --
   ----------------

   function Clean_Body return String is
      R           : Unbounded_String := M_Body;
      First, Last : Natural;
   begin
      --  Removes everything between [] to always have the same output
      loop
         First := Index (R, "[");
         Last  := Index (R, "]");

         exit when First = 0 or else Last = 0;

         if First = Last - 1 then
            Replace_Slice (R, First, Last, "()");
         else
            Replace_Slice (R, First, Last, "(uri-removed)");
         end if;
      end loop;

      return To_String (R);
   end Clean_Body;

   ---------------
   -- Get_First --
   ---------------

   procedure Get_First is
      R : Response.Data;
   begin
      R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/main");
      M_Body := Response.Message_Body (R);

      Text_IO.Put_Line ("First: " & Clean_Body);
   end Get_First;

   --------------
   -- Get_Next --
   --------------

   procedure Get_Next is
      R : Response.Data;
      First, Last : Natural;
   begin
      First := Index (M_Body, "next=[");
      Last  := Index (M_Body, "]", Strings.Backward);

      R := Client.Get ("http://localhost:" & Utils.Image (Port)
                         & Slice (M_Body, First + 6, Last - 1));

      M_Body := Response.Message_Body (R);

      Text_IO.Put_Line ("Next: " & Clean_Body);
   end Get_Next;

   ------------------
   -- Get_Previous --
   ------------------

   procedure Get_Previous is
      R : Response.Data;
      First, Last : Natural;
   begin
      First := Index (M_Body, "previous=[");
      Last  := Index (M_Body, "]");

      R := Client.Get ("http://localhost:" & Utils.Image (Port)
                         & Slice (M_Body, First + 10, Last - 1));

      M_Body := Response.Message_Body (R);

      Text_IO.Put_Line ("Previous: " & Clean_Body);
   end Get_Previous;

begin
   Get_Free_Port (Port);

   Config.Set.Server_Port (Conf, Port);
   Config.Set.Max_Connection (Conf, 5);

   Services.Dispatchers.Transient_Pages.Register
     (Disp, CB'Unrestricted_Access);

   Server.Start (WS, Disp, Conf);

   Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Get_First;
   Get_Next;
   Get_Previous;

   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");
end Split1;
