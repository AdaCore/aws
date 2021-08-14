with AWS.Utils;
with Ada.Text_IO;

package body Memory_Some is

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stream_Type;
      Buffer :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      use type AWS.Utils.Random_Integer;
   begin
      Last := Buffer'First +
        Stream_Element_Offset (AWS.Utils.Random rem Buffer'Length);
      MS.Read (Stream.Internal, Buffer (Buffer'First .. Last), Last);
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stream_Type; Item : Stream_Element_Array) is
   begin
      MS.Append (Stream.Internal, Item);
   end Write;

end Memory_Some;
