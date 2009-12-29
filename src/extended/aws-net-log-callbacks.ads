------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
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

--  Some ready to use write procedures

package AWS.Net.Log.Callbacks is

   procedure Initialize
     (Filename : String;
      Callback : Write_Callback);
   --  Initialize the logging, must be called before using the callbacks below

   procedure Finalize;
   --  Stop logging, close log file

   procedure Text
     (Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset);
   --  A text output, each chunk is output with an header and footer:
   --     Data sent/received to/from socket <FD> (<size>/<buffer size>)
   --     <data>
   --     Total data sent: <nnn> received: <nnn>

   procedure Binary
     (Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset);
   --  A binary output, each chunk is output with an header and footer. The
   --  data itself is written using a format close to the Emacs hexl-mode:
   --     Data sent/received to/from socket <FD> (<size>/<buffer size>)
   --     HH HH HH HH HH HH HH HH HH HH HH HH   az.rt.mpl..q
   --     Total data sent: <nnn> received: <nnn>
   --
   --  HH is the hex character number, if the character is not printable a dot
   --  is written.

end AWS.Net.Log.Callbacks;
