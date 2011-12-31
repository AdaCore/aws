------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with AWS.Client;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Cache_Control_Header is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Positive := 8274;

   function Test_CC (Kind : Messages.Cache_Kind; Value : String) return String;
   --  Convert to/from Cache_Data/Cache_Option for testing purpose

   function Test_CC
     (Kind : Messages.Cache_Kind; Value : String) return String is
   begin
      return String
        (Messages.To_Cache_Option
           (Messages.To_Cache_Data
              (Kind, Messages.Cache_Option (Value))));
   end Test_CC;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is

      RCD : constant Messages.Cache_Data :=
              (Messages.Response, No_Cache => True, No_Store => True,
               No_Transform => True, S_Max_Age => 12,
               Must_Revalidate => True, others => <>);

      H : constant Headers.List := Status.Header (Request);

      procedure Output (Header : String);
      --  Output corresponding header value

      ------------
      -- Output --
      ------------

      procedure Output (Header : String) is
      begin
         if Headers.Exist (H, Header) then
            declare
               Value : constant String := Headers.Get (H, Header);
            begin
               Text_IO.Put_Line
                 (Header & ": " & Test_CC (Messages.Request, Value));
            end;

         else
            Text_IO.Put_Line (Header & ": NOT FOUND");
         end if;
      end Output;

   begin
      Text_IO.Put_Line (">>>>> " & Status.URI (Request));
      Output (Messages.Cache_Control_Token);
      Text_IO.New_Line;
      return Response.Build (MIME.Text_HTML, "ok");
   end CB;

   R  : Response.Data;
   H  : Headers.List;

   RCD : constant Messages.Cache_Data :=
           (Messages.Request, No_Cache => True, Max_Stale => 3, others => <>);
begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "Cache Control Header", CB'Unrestricted_Access, Port => Port);

   Headers.Set.Add
     (H,
      Messages.Cache_Control_Token, String (Messages.To_Cache_Option (RCD)));

   R := AWS.Client.Get
     (URL => "http://localhost:" & Utils.Image (Port) & "/get", Headers => H);

   Headers.Set.Reset (H);

   Headers.Set.Add
     (H,
      Messages.Cache_Control_Token,
      String (Messages.To_Cache_Option
                ((Messages.Request,
                  Only_If_Cached => True, Min_Fresh => 12, Others => <>))));

   R := AWS.Client.Head
     (URL => "http://localhost:" & Utils.Image (Port) & "/head", Headers => H);

   R := AWS.Client.Post
     (URL => "http://localhost:" & Utils.Image (Port) & "/post",
      Data => "", Headers => H);

   Text_IO.Put_Line
     ("Cache Option Header : '"
      & Test_CC (Messages.Response, String (Messages.Unspecified)) & ''');
   Text_IO.Put_Line
     ("Cache Option Header : '"
      & Test_CC (Messages.Response, String (Messages.No_Cache)) & ''');
   Text_IO.Put_Line
     ("Cache Option Header : '"
      & Test_CC (Messages.Response, String (Messages.No_Store)) & ''');
   Text_IO.Put_Line
     ("Cache Option Header : '"
      & Test_CC (Messages.Response, String (Messages.Prevent_Cache)) & ''');

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Cache_Control_Header;
