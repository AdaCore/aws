package body AWS.Resources.Streams.Memory is

   ------------
   -- Append --
   ------------

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : in     Stream_Element_Array) is
   begin
      Containers.Append (Resource.Data, Buffer);
   end Append;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Stream_Type) is
   begin
      Containers.Close (File.Data);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : in Stream_Type) return Boolean is
   begin
      return Containers.End_Of_File (Resource.Data);
   end End_Of_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset) is
   begin
      Containers.Read (Resource.Data, Buffer, Last);
   end Read;

   ----------
   -- Size --
   ----------

   function Size (Resource : in Stream_Type) return Stream_Element_Offset is
   begin
      return Containers.Size (Resource.Data);
   end Size;

end AWS.Resources.Streams.Memory;
