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

with Ada.Text_IO;

with AWS.MIME;

procedure Tmime is

   use Ada;
   use AWS;

   procedure Test (Filename : String) is
   begin
      Text_IO.Put (Filename);
      Text_IO.Set_Col (25);
      Text_IO.Put_Line (" => " & MIME.Content_Type (Filename));
   end Test;

begin
   Test ("aws.doc");
   Test ("aws.txt");
   Test ("aws.exe");
   Test ("aws.html");
   Test ("aws.htm");
   Test ("aws.jpg");
   Test ("aws.JPG");
   Test ("aws.jpeg");
   Test ("aws.gif");
   Test ("aws.mpe");
   Test ("aws.au");
   Test ("aws.avi");
   Test ("aws.rgb");
   Test ("aws.ppt");
   Test ("aws.rtf");
   Test ("known-problems");
   Test ("features-316");
   Test ("features-317");

   MIME.Add_Regexp ("known-problems", MIME.Text_Plain);
   MIME.Add_Regexp ("features-.*", MIME.Text_Plain);

   Test ("known-problems");
   Test ("known-problems-316a");
   Test ("features-316");
   Test ("features-317");

   --  Test for user defined in aws.mime types

   Test ("aaa.jnlp");
   Test ("aaa.jnl");
   Test ("aaa.jtma");
   Test ("aaa.jtmb");
   Test ("aaa.jtmc");
   Test ("aaa.jtmd");
   Test ("readme.file");
   Test ("readme");
   Test ("read.me");

   Text_IO.Put_Line (Boolean'Image (MIME.Is_Text (MIME.Text_XML)));
   Text_IO.Put_Line (Boolean'Image (MIME.Is_Audio (MIME.Audio_Basic)));
   Text_IO.Put_Line (Boolean'Image (MIME.Is_Video (MIME.Video_X_Msvideo)));
   Text_IO.Put_Line (Boolean'Image (MIME.Is_Image (MIME.Image_Png)));
   Text_IO.Put_Line
     (Boolean'Image (MIME.Is_Application (MIME.Application_Msword)));
end Tmime;
