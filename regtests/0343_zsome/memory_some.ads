with Ada.Streams;
private with AWS.Containers.Memory_Streams;

package Memory_Some is

   use Ada.Streams;

   type Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Stream_Type;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   --  Returns a chunck of data in Buffer, Last point to the last element
   --  returned in Buffer.
   --  Last will be less then Buffer'Last in most cases, even if not at the end
   --  of stream.

   overriding procedure Write
     (Stream : in out Stream_Type; Item : Stream_Element_Array);

   function End_Of_Stream (Stream : Stream_Type) return Boolean;

   function Size (Stream : Stream_Type) return Stream_Element_Offset;

private

   package MS renames AWS.Containers.Memory_Streams;

   type Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Internal : MS.Stream_Type;
   end record;

   function End_Of_Stream (Stream : Stream_Type) return Boolean is
     (MS.End_Of_File (Stream.Internal));

   function Size (Stream : Stream_Type) return Stream_Element_Offset is
     (MS.Size (Stream.Internal));

end Memory_Some;
