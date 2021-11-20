------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

with Ada.Finalization;

with AWS.Headers;
with AWS.HTTP2.Frame.List;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Context;
with AWS.Status;

limited with AWS.HTTP2.Stream;

private with Ada.Strings.Unbounded;
private with AWS.Resources.Streams;
private with AWS.Utils;

package AWS.HTTP2.Message is

   use Ada;

   use type AWS.HTTP2.Frame.List.Count_Type;
   use type Response.Data_Mode;

   type Object is new Finalization.Controlled with private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Headers   : AWS.Headers.List;
      Data      : Stream_Element_Array;
      Stream_Id : HTTP2.Stream_Id) return Object
     with Post => Create'Result.Is_Defined;
   --  Create a message out of a request object

   procedure Append_Body
     (Self : in out Object;
      Data : String)
     with Pre => Self.Is_Defined and then Data'Length > 0, Inline;
   --  Append Data to the current body of message

   procedure Append_Body
     (Self : in out Object;
      Data : Stream_Element_Array)
     with Pre => Self.Is_Defined and then Data'Length > 0, Inline;
   --  Append Data to the current body of message

   function Create
     (Answer    : in out Response.Data;
      Request   : AWS.Status.Data;
      Stream_Id : HTTP2.Stream_Id) return Object
     with Post => Create'Result.Is_Defined;
   --  Create a message out of a response object

   function Stream_Id (Self : Object) return HTTP2.Stream_Id
     with Pre => Self.Is_Defined;

   function Headers (Self : Object) return AWS.Headers.List
     with Pre => Self.Is_Defined;
   --  Get the headers for this message

   function Has_Body (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if message has a body

   function To_Frames
     (Self   : in out Object;
      Ctx    : Server.Context.Object;
      Stream : HTTP2.Stream.Object) return AWS.HTTP2.Frame.List.Object
     with Pre  => Self.Is_Defined,
          Post => To_Frames'Result.Length > 0
                    and then
                  (for all P of To_Frames'Result => P.Is_Defined);
   --  Get frames for this message. The headers is returned as an HEADERS frame
   --  and the payload as a DATA frame. The list may not contain the while data
   --  payload depending on the current flow control window. If some more data
   --  is available, More_Frame below will return true.

   function More_Frames (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if some more data are available and so should be sent

   function Status_Code (Self : Object) return Messages.Status_Code
     with Pre => Self.Is_Defined;
   --  Returns message status code

private

   use Ada.Strings.Unbounded;

   use type Resources.Streams.Stream_Access;
   use type Utils.File_Size_Type;

   type Kind_Type is (K_Request, K_Response);

   type Object is new Finalization.Controlled with record
      Kind      : Kind_Type := K_Response;
      Mode      : Response.Data_Mode;
      Ranges    : Unbounded_String;
      Stream_Id : HTTP2.Stream_Id;
      Headers   : AWS.Headers.List;
      Sent      : Utils.File_Size_Type := 0;
      H_Sent    : Boolean := False; -- Whether the header has been sent
      B_Sent    : Boolean := False; -- Whether the body has been sent
      M_Body    : Resources.Streams.Stream_Access;
      Ref       : Utils.Counter_Access;
   end record;

   overriding procedure Adjust (O : in out Object);
   overriding procedure Initialize (O : in out Object);
   overriding procedure Finalize (O : in out Object);

   function Headers (Self : Object) return AWS.Headers.List is (Self.Headers);

   function Has_Body (Self : Object) return Boolean is
     (Self.Mode in Response.Message | Response.File | Response.File_Once
        | Response.Stream and then Self.M_Body /= null);

   function Is_Defined (Self : Object) return Boolean is
     (Self.Mode /= Response.No_Data);

   function Stream_Id (Self : Object) return HTTP2.Stream_Id is
     (Self.Stream_Id);

   function More_Frames (Self : Object) return Boolean is
     (if Self.Has_Body
      then not Self.B_Sent
      else not Self.H_Sent);

   function Status_Code (Self : Object) return Messages.Status_Code is
     (Messages.Status_Code'Value
        ('S' & Self.Headers.Get (Messages.Status_Token)));

   Undefined : constant Object :=
                 (Finalization.Controlled with Mode => Response.No_Data,
                  Stream_Id => 0, others => <>);

end AWS.HTTP2.Message;
