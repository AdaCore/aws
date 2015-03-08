------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with AWS.MIME;
with SOAP.Message.Response.Error;

with Occursservices.Server;
with OccursServices.Types;

package body Occurs_CB is

   use Ada;
   use Ada.Strings.Unbounded;
   use SOAP;

   use OccursServices.Types;

   procedure displayTrack
     (pa_track : T_Track_Type;
      values   : ArrayOfFloat_Type);

   procedure displayTrack
     (pa_track : T_Track_Type;
      values   : ArrayOfFloat_Type)
   is
   begin
      Text_IO.Put_Line
        ("*** p_battleDimension " & Image (pa_track.p_battleDimension));

      for K in pa_track.p_tag.Item'Range loop
         Text_IO.Put_Line
           ("*** p_tag          " & To_String (pa_track.p_tag.Item (K)));
      end loop;

      for K in values'Range loop
         Text_IO.Put_Line
           ("*** values        " & Float'Image (values (K)));
      end loop;
   end displayTrack;

   function DT is new Occursservices.Server.displayTrack_CB (displayTrack);

   -------------
   -- HTTP_CB --
   -------------

   function HTTP_CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build
        (MIME.Text_HTML, "No HTTP request should be called.");
   end HTTP_CB;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : Status.Data)
      return Response.Data is
   begin
      if SOAPAction = "displayTrack" then
         return DT (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
              (Message.Response.Error.Client,
               "Wrong SOAP action " & SOAPAction));
      end if;
   end SOAP_CB;

end Occurs_CB;
