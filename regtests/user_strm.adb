package body User_Strm is

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Tagged) is
   begin
      null;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (Resource : in out AWS.Resources.Streams.Stream_Type'Class;
      Size     : in     Stream_Element_Offset)
   is
   begin
      File_Tagged (Resource).Size   := Size;
      File_Tagged (Resource).Offset := 0;
   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (Resource : in File_Tagged)
      return Boolean
   is
   begin
      return Resource.Offset >= Resource.Size;
   end End_Of_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      Symbol_First  : Character := '0';
      Symbol_Last   : Character := 'z';
      Symbol_Length : Stream_Element := Character'Pos (Symbol_Last)
         - Character'Pos (Symbol_First) + 1;
   begin
      Last := Buffer'First - 1;
      for I in Buffer'Range loop
         exit when End_Of_File (Resource);
         Last            := I;

         Resource.Offset := Resource.Offset + 1;
         Buffer (I) := Character'Pos (Symbol_First)
            + Stream_Element (Resource.Offset) mod Symbol_Length;

         if Stream_Element (Resource.Offset)
            mod (Symbol_Length - 1) = 0
         then
            Buffer (I) := 10;
         end if;

      end loop;
   end Read;

end User_Strm;
