------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2016, CNRS                         --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.String_Split;

with AWS.Messages;
with AWS.MIME;

package body REST_Example is

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String, "=");
   function Split_String (Item : String; Separator : Character)
                         return String_Vectors.Vector;
   function Split_URI (URI : String) return String_Vectors.Vector is
      (Split_String (URI, '/'));
   function Data_To_String (Request : AWS.Status.Data) return String;

   use type Ada.Containers.Count_Type;

   function S404 (Method : String;
                  Message : String := "") return AWS.Response.Data;

   function GET (Object : REST_Conf;
                 Request : AWS.Status.Data)
                return AWS.Response.Data is
      URI : constant String_Vectors.Vector :=
        Split_URI (AWS.Status.URI (Request));
      use Ada.Strings.Unbounded;
   begin
      Ada.Text_IO.Put_Line ("GET get called");
      if URI.Length < 2 then
         return S404 ("GET", "missing information");
      end if;
      if URI.Element (2) = "parameters" then
         if URI.Length = 2 then
            declare
               Result : Unbounded_String;
            begin
               Result := To_Unbounded_String ("<parameters>");
               for Cursor in Object.Map.Iterate loop
                  Result := Result & "<parameter id=""" &
                    String_Maps.Key (Cursor) & """>";
                  Result := Result & String_Maps.Element (Cursor) &
                    "</parameter>";
               end loop;
               Result := Result & "</parameters>";
               return AWS.Response.Build (AWS.MIME.Text_XML, Result);
            end;
         elsif URI.Length = 3 then
            declare
               ID : constant String := URI.Element (3);
            begin
               if Object.Map.Contains (ID) then
                  return AWS.Response.Build (AWS.MIME.Text_XML,
                                             "<parameter id=""" & ID & """>" &
                                               Object.Map.Element (ID) &
                                               "</parameter>");
               else
                  raise REST.REST_Error with ID & " isn't a valid ID";
               end if;
            end;
         end if;
      end if;
      return S404 ("GET");
   end GET;

   function PUT (Object : in out REST_Conf;
                 Request : AWS.Status.Data)
                return AWS.Response.Data is
      URI : constant String_Vectors.Vector :=
        Split_URI (AWS.Status.URI (Request));
      Datas : constant String := Data_To_String (Request);
   begin
      Ada.Text_IO.Put_Line ("PUT get called");
      if URI.Length < 2 then
         return S404 ("PUT", "missing information");
      end if;
      if URI.Element (2) = "parameters" then
         if URI.Length = 2 then
            declare
               Couples : constant String_Vectors.Vector :=
                 Split_String (Datas, ';');
            begin
               Object.Map.Clear;
               for Couple of Couples loop
                  declare
                     Split_Couple : constant String_Vectors.Vector :=
                       Split_String (Couple, '=');
                     ID : constant String := Split_Couple.Element (1);
                     Value : constant String := Split_Couple.Element (2);
                  begin
                     Object.Map.Insert (ID, Value);
                  end;
               end loop;
            end;
         elsif URI.Length = 3 then
            declare
               ID : constant String := URI.Element (3);
            begin
               if Object.Map.Contains (ID) then
                  Object.Map.Replace (ID, Datas);
               else
                  Object.Map.Insert (ID, Datas);
               end if;
            end;
         else
            return S404 ("PUT");
         end if;
      else
         return S404 ("PUT");
      end if;
      return AWS.Response.Build (AWS.MIME.Text_Plain, "PUT OK");
   end PUT;

   function DELETE (Object : in out REST_Conf;
                    Request : AWS.Status.Data)
                   return AWS.Response.Data is
      URI : constant String_Vectors.Vector :=
        Split_URI (AWS.Status.URI (Request));
   begin
      Ada.Text_IO.Put_Line ("DELETE get called");
      if URI.Length < 2 then
         return S404 ("DELETE", "missing information");
      end if;
      if URI.Element (2) = "parameters" then
         if URI.Length = 2 then
            Object.Map.Clear;
         elsif URI.Length = 3 then
            declare
               ID : constant String := URI.Element (3);
            begin
               if Object.Map.Contains (ID) then
                  Object.Map.Delete (ID);
               else
                  raise REST.REST_Error with
                    ID & " can't be delete - doesn't present in map";
               end if;
            end;
         else
            return S404 ("DELETE");
         end if;
      else
         return S404 ("DELETE");
      end if;
      return AWS.Response.Build (AWS.MIME.Text_Plain, "DELETE OK");
   end DELETE;

   function POST (Object : in out REST_Conf;
                  Request : AWS.Status.Data)
                 return AWS.Response.Data is
      URI : constant String_Vectors.Vector :=
        Split_URI (AWS.Status.URI (Request));
      Datas : constant String := Data_To_String (Request);
   begin
      Object.Post_Call := Object.Post_Call + 1;
      Ada.Text_IO.Put_Line ("POST get called :" & Object.Post_Call'Img);
      if URI.Length < 2 then
         return S404 ("POST", "missing information");
      end if;
      if URI.Element (2) = "parameters" then
         if URI.Length = 2 then
            declare
               Couples : constant String_Vectors.Vector :=
                 Split_String (Datas, ';');
            begin
               for Couple of Couples loop
                  declare
                     Split_Couple : constant String_Vectors.Vector :=
                       Split_String (Couple, '=');
                     ID : constant String := Split_Couple.Element (1);
                     Value : constant String := Split_Couple.Element (2);
                  begin
                     if Object.Map.Contains (ID) then
                        raise REST.REST_Error with
                          ID & " already present in map";
                     else
                        Object.Map.Insert (ID, Value);
                     end if;
                  end;
               end loop;
            end;
         elsif URI.Length = 3 then
            declare
               ID : constant String := URI.Element (3);
            begin
               if Object.Map.Contains (ID) then
                  raise REST.REST_Error with ID & " already present in map";
               else
                  Object.Map.Insert (ID, Datas);
               end if;
            end;
         else
            return S404 ("POST");
         end if;
      else
         return S404 ("POST");
      end if;
      return AWS.Response.Build (AWS.MIME.Text_Plain, "POST OK");
   end POST;

   function Split_String (Item : String; Separator : Character)
                         return String_Vectors.Vector is
      List : String_Vectors.Vector;
      Slices : GNAT.String_Split.Slice_Set;
      use GNAT.String_Split;
   begin
      Create (Slices, From => Item, Separators => Separator & "");
      for I in 1 .. Slice_Count (Slices) loop
         List.Append (Slice (Slices, I));
      end loop;
      return List;
   end Split_String;

   function Data_To_String (Request : AWS.Status.Data) return String is
      Base : Ada.Streams.Stream_Element_Array (1 .. 2048);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      AWS.Status.Read_Body (Request, Base, Last);
      declare
         Answer : String (1 .. Positive (Last));
         for Answer'Address use Base (Base'First)'Address;
      begin
         return Answer;
      end;
   end Data_To_String;

   function S404 (Method : String;
                  Message : String := "")
                 return AWS.Response.Data is
   begin
      return AWS.Response.Acknowledge
        (AWS.Messages.S404,
         "<p>request not handled by " & Method & " callback - " & Message);
   end S404;

end REST_Example;
