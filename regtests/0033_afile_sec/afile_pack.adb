------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

--  Test file as attachment

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Resources.Streams.Disk;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

package body AFile_Pack is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   FN   : constant String := "test.out";
   Size : constant Response.Content_Length_Type
    := Response.Content_Length_Type (Resources.File_Size (FN));

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      Strm : Resources.Streams.Stream_Access;
      URI  : constant String := Status.URI (Request);
   begin
      if URI = "/first" then
         return Response.File
           (MIME.Application_Octet_Stream, FN,
            Disposition => Response.Inline);

      elsif URI = "/second" then
         return Response.File
           (MIME.Application_Octet_Stream, FN,
            Disposition => Response.Attachment);

      elsif URI = "/third" then
         return Response.File
           (MIME.Application_Octet_Stream, FN,
            User_Filename => "you_got_this.o",
            Disposition => Response.Inline);

      elsif URI = "/fourth" then
         return Response.File
           (MIME.Application_Octet_Stream, FN,
            Disposition   => Response.Attachment,
            User_Filename => "you_got_this.o");

      elsif URI = "/fifth" then
         Strm :=  new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all), FN);
         return Response.Stream
           (MIME.Application_Octet_Stream, Strm,
            Disposition => Response.Inline);

      elsif URI = "/sixth" then
         Strm :=  new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all), FN);
         return Response.Stream
           (MIME.Application_Octet_Stream, Strm,
            Disposition => Response.Attachment);

      elsif URI = "/seventh" then
         Strm :=  new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all), FN);
         return Response.Stream
           (MIME.Application_Octet_Stream, Strm,
            User_Filename => "a_stream.o", Disposition => Response.Inline);

      elsif URI = "/eighth" then
         Strm :=  new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all), FN);
         return Response.Stream
           (MIME.Application_Octet_Stream, Strm,
            Disposition   => Response.Attachment,
            User_Filename => "a_stream.o");

      elsif URI = "/nineth" then
         return Response.File (MIME.Application_Octet_Stream, FN);

      elsif URI = "/tenth" then
         Strm :=  new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all), FN);
         return Response.Stream (MIME.Application_Octet_Stream, Strm);
      else
         return Response.Build
                  (MIME.Text_HTML, "URI """ & URI & """ not supported");
      end if;

   exception
      when E : others =>
         Text_IO.Put_Line
           ("CB error: " & Exceptions.Exception_Information (E));
         return Response.Build
           (MIME.Text_Plain,
            Exceptions.Exception_Message (E),
            Messages.S500);
   end CB;

   ---------
   -- Run --
   ---------

   procedure Run (Protocol : String) is

      Port : Positive := 4000;

      -------------
      -- Call_It --
      -------------

      procedure Call_It (Res : in String) is
         R : Response.Data;
      begin
         R := Client.Get
                (Protocol & "://localhost:" & AWS.Utils.Image (Port)
                  & '/' & Res);

         Text_IO.Put (Res);
         Text_IO.Set_Col (9);
         Text_IO.Put_Line
           ("= " & Response.Header (R, Messages.Content_Disposition_Token, 1));

         declare
            S : constant Response.Content_Length_Type
              := Response.Content_Length (R);
         begin
            if S = Response.Undefined_Length then
               Text_IO.Put_Line ("Error: " & Res & " undefined size.");
            elsif Size /= S then
               Text_IO.Put_Line ("Error : " & Res & " wrong size.");
            end if;
         end;
      end Call_It;

   begin
      Get_Free_Port (Port);

      Server.Start
        (WS,
         "afile " & Protocol,
         CB'Access,
         Security       => Protocol = "https",
         Port           => Port,
         Max_Connection => 5);
      Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

      delay 0.25;

      Call_It ("first");
      Call_It ("second");
      Call_It ("third");
      Call_It ("fourth");
      Call_It ("fifth");
      Call_It ("sixth");
      Call_It ("seventh");
      Call_It ("eighth");
      Call_It ("nineth");
      Call_It ("tenth");

      Server.Shutdown (WS);
      Text_IO.Put_Line ("shutdown");
   end Run;

end AFile_Pack;
