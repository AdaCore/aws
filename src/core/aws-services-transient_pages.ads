------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with AWS.Config;
with AWS.Resources.Streams;

package AWS.Services.Transient_Pages is

   function Get_URI return String with
     Post => Get_URI'Result'Length > 0;
   --  Create a unique URI, must be used to register a transient web page

   procedure Register
     (URI      : String;
      Resource : Resources.Streams.Stream_Access;
      Lifetime : Duration := Config.Transient_Lifetime);
   --  Register a new transient page, this page will be deleted after Lifetime
   --  seconds.

   function Get (URI : String) return Resources.Streams.Stream_Access;
   --  Returns the stream access for the URI or null if this URI has not been
   --  registered.

private

   -------------------
   --  Cleaner task --
   -------------------

   task type Cleaner with Priority => Config.Service_Priority is
      entry Stop;
   end Cleaner;

   type Cleaner_Access is access Cleaner;

   Cleaner_Task : Cleaner_Access;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected Cleaner_Control is

      procedure Register
        (Transient_Check_Interval : Duration;
         Need_Start               : out Boolean);
      --  Increment number of registerd servers. Set Need_Start flag to True
      --  if it is necessary to start cleaner task.

      procedure Stop (Need_Release : out Boolean);
      --  Stop the cleaner task when there is no more server using it.
      --  Need_Release is set to True if the Cleaner_Task can be released.

   private
      Server_Count : Natural := 0;
   end Cleaner_Control;

end AWS.Services.Transient_Pages;
