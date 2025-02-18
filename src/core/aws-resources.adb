------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2024, AdaCore                     --
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
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

with AWS.Resources.Embedded;
with AWS.Resources.Files;
with AWS.Resources.Streams;

package body AWS.Resources is

   ----------
   -- "or" --
   ----------

   function "or" (I1, I2 : File_Instance) return File_Instance is
   begin
      if I1 = I2 then
         return I1;
      elsif I1 = None then
         return I2;
      elsif I2 = None then
         return I1;
      else
         return Both;
      end if;
   end "or";

   ----------------
   -- Check_Name --
   ----------------

   function Check_Name
     (Name  : String;
      Check : not null access function (Name : String) return Boolean;
      GZip  : in out Boolean) return String is
   begin
      if Is_GZip (Name) then
         if Check (Name) then
            GZip := False;
            return Name;
         else
            return "";
         end if;

      elsif GZip then
         if Check (Name & GZip_Ext) then
            return Name & GZip_Ext;

         elsif Check (Name) then
            GZip := False;
            return Name;

         else
            return "";
         end if;

      elsif Check (Name) then
         return Name;

      elsif Check (Name & GZip_Ext) then
         GZip := True;
         return Name & GZip_Ext;

      else
         return "";
      end if;
   end Check_Name;

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Type) is
   begin
      if Resource /= null then
         Close (Resource.all);
         Unchecked_Free (Resource);
      end if;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : File_Type) return Boolean is
   begin
      return Resource = null or else End_Of_File (Resource.all);
   end End_Of_File;

   -----------
   -- Exist --
   -----------

   function Exist (Name : String) return File_Instance is
   begin
      if Name = "" then
         return None;
      else
         return Embedded.Exist (Name) or Files.Exist (Name);
      end if;
   end Exist;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (Name : String) return Utils.File_Size_Type is
   begin
      if Resources.Embedded.Is_Regular_File (Name) then
         return Resources.Embedded.File_Size (Name);
      else
         return Resources.Files.File_Size (Name);
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : String) return Ada.Calendar.Time is
   begin
      if Resources.Embedded.Is_Regular_File (Name) then
         return Resources.Embedded.File_Timestamp (Name);
      else
         return Resources.Files.File_Timestamp (Name);
      end if;
   end File_Timestamp;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Resource : in out File_Type;
      Buffer   : out String;
      Last     : out Natural)
   is
      Byte     : Stream_Element_Array (1 .. 1);
      Last_Ind : Stream_Element_Offset;
   begin
      Last         := 0;
      Resource.LFT := False;

      for I in Buffer'Range loop
         Read (Resource.all, Byte, Last_Ind);

         exit when Last_Ind < Byte'Last;

         Buffer (I) := Character'Val (Byte (1));

         --  Check for end of line

         if Buffer (I) = ASCII.LF then
            --  This is LF
            if I > Buffer'First
              and then Buffer (I - 1) = ASCII.CR
            then
               --  And previous char was a CR, skip it
               Last := @ - 1;
            end if;

            Resource.LFT := True;
            exit;
         end if;

         Last := @ + 1;
      end loop;
   end Get_Line;

   -------------
   -- Is_GZip --
   -------------

   function Is_GZip (Name : String) return Boolean is
   begin
      return Name'Length > GZip_Ext'Length
        and then Name (Name'Last - GZip_Ext'Length + 1 .. Name'Last)
                   = GZip_Ext;
   end Is_GZip;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : String) return Boolean is
   begin
      return Resources.Embedded.Is_Regular_File (Name)
        or else Resources.Files.Is_Regular_File (Name);
   end Is_Regular_File;

   -------------------
   -- LF_Terminated --
   -------------------

   function LF_Terminated (Resource : File_Type) return Boolean is
   begin
      return Resource.all.LFT;
   end LF_Terminated;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : out File_Type;
      Name : String;
      Form : String    := "";
      GZip : in out Boolean) is
   begin
      Streams.Create (File, Streams.Open (Name, Form, GZip));
   end Open;

   procedure Open
     (File : out File_Type;
      Name : String;
      Form : String    := "")
   is
      GZip : Boolean := False;
   begin
      Open (File, Name, Form, GZip);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is
   begin
      Read (Resource.all, Buffer, Last);
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Resource : in out File_Type) is
   begin
      Reset (Resource.all);
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index
     (Resource : in out File_Type;
      To       : Stream_Element_Offset) is
   begin
      Set_Index (Resource.all, To);
   end Set_Index;

   ----------
   -- Size --
   ----------

   function Size (Resource : File_Type) return Content_Length_Type is
   begin
      return Size (Resource.all);
   end Size;

end AWS.Resources;
