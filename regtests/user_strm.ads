with AWS.Resources.Streams;
with Ada.Streams;
package User_Strm is

   use AWS.Resources;
   use Ada.Streams;

   type File_Tagged is new Streams.Stream_Type with private;

   function End_Of_File
     (Resource : in File_Tagged)
      return Boolean;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   procedure Close (File : in out File_Tagged);

   procedure Create
     (Resource : in out AWS.Resources.Streams.Stream_Type'Class;
      Size     : in     Stream_Element_Offset);

private

   type File_Tagged is new Streams.Stream_Type with record
      Offset : Stream_Element_Offset;
      Size   : Stream_Element_Offset;
   end record;

end User_Strm;