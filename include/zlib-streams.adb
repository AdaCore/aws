----------------------------------------------------------------
--                ZLib for Ada thick binding.                 --
--                                                            --
--         Copyright (C) 2002-2019, Dmitriy Anisimkov         --
--              Copyright (C) 2019-2024, AdaCore              --
--                                                            --
--  Open source license information is in the zlib.ads file.  --
----------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body ZLib.Streams is

   -----------
   -- Close --
   -----------

   procedure Close (Stream : in out Stream_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Stream_Element_Array, Buffer_Access);
   begin
      if Stream.Mode = Out_Stream or Stream.Mode = Duplex then
         --  We should flush the data written by the writer

         Flush (Stream, Finish);

         Close (Stream.Writer);
      end if;

      if Stream.Mode = In_Stream or Stream.Mode = Duplex then
         Close (Stream.Reader);
         Unchecked_Free (Stream.Buffer);
      end if;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (Stream            :    out Stream_Type;
      Mode              : in     Stream_Mode;
      Back              : in     Stream_Access;
      Back_Compressed   : in     Boolean;
      Level             : in     Compression_Level := Default_Compression;
      Strategy          : in     Strategy_Type     := Default_Strategy;
      Header            : in     Header_Type       := Default;
      Read_Buffer_Size  : in     Stream_Element_Offset := Default_Buffer_Size;
      Write_Buffer_Size : in     Stream_Element_Offset := Default_Buffer_Size)
   is
      subtype Buffer_Subtype is Stream_Element_Array (1 .. Read_Buffer_Size);

      procedure Init_Filter
        (Filter : in out Filter_Type; Compress : in Boolean);

      -----------------
      -- Init_Filter --
      -----------------

      procedure Init_Filter
        (Filter : in out Filter_Type; Compress : in Boolean) is
      begin
         if Compress then
            Deflate_Init (Filter, Level, Strategy, Header => Header);
         else
            Inflate_Init (Filter, Header => Header);
         end if;
      end Init_Filter;

   begin
      Stream.Back := Back;
      Stream.Mode := Mode;

      if Mode = Out_Stream or Mode = Duplex then
         Init_Filter (Stream.Writer, Back_Compressed);
         Stream.Buffer_Size := Write_Buffer_Size;
      else
         Stream.Buffer_Size := 0;
      end if;

      if Mode = In_Stream or Mode = Duplex then
         Init_Filter (Stream.Reader, not Back_Compressed);

         Stream.Buffer     := new Buffer_Subtype;
         Stream.Rest_First := Stream.Buffer'Last + 1;
         Stream.Rest_Last  := Stream.Buffer'Last;
         Stream.Ahead_Last := Stream.Buffer'First - 1;
      end if;
   end Create;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Stream : in out Stream_Type;
      Mode   : in     Flush_Mode := Sync_Flush)
   is
      Buffer : Stream_Element_Array (1 .. Stream.Buffer_Size);
      Last   : Stream_Element_Offset;
   begin
      loop
         Stream.Writer.Flush (Buffer, Last, Mode);

         exit when Last < Buffer'First;

         Stream.Back.Write (Buffer (1 .. Last));
      end loop;
   end Flush;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Stream : in Stream_Type) return Boolean is
   begin
      return Is_Open (Stream.Reader) or else Is_Open (Stream.Writer);
   end Is_Open;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is

      procedure Read
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset)
        with Inline;

      ----------
      -- Read --
      ----------

      procedure Read
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Stream.Back.Read (Item, Last);
      end Read;

      procedure Read is new ZLib.Read
         (Read       => Read,
          Buffer     => Stream.Buffer.all,
          Rest_First => Stream.Rest_First,
          Rest_Last  => Stream.Rest_Last);

      Ahead_First : Stream_Element_Offset;
      Ahead_Last  : Stream_Element_Offset;

   begin
      if Stream.Ahead_Last > Stream.Rest_Last then
         Last := Item'First - 1;
         Ahead_First := Stream.Rest_Last + 1;

         loop
            if Last = Item'Last then
               Ahead_Last :=
                 Stream.Rest_Last + Stream.Ahead_Last - Ahead_First + 1;
               Stream.Buffer
                 (Stream.Rest_Last + 1 .. Ahead_Last) :=
                 Stream.Buffer (Ahead_First .. Stream.Ahead_Last);
               Stream.Ahead_Last := Ahead_Last;
               return;
            end if;

            Last := @ + 1;

            Item (Last) := Stream.Buffer (Ahead_First);

            if Ahead_First = Stream.Ahead_Last then
               Stream.Ahead_Last := Stream.Buffer'First - 1;
               exit;
            end if;

            Ahead_First := @ + 1;
         end loop;

         if Last < Item'Last then
            Read (Stream.Reader, Item (Last + 1 .. Item'Last), Last);
         end if;

      else
         Read (Stream.Reader, Item, Last);
      end if;

      if not Stream.Reader.Stream_End
        and then Stream.Rest_First > Stream.Rest_Last
      then
         --  Try read ahead to detect end of stream early

         Read (Stream.Buffer.all, Stream.Rest_Last);
         Stream.Rest_First := Stream.Buffer'First;

         if Stream.Rest_Last = Stream.Buffer'Last then
            -- No space to read ahead
            return;
         end if;

         Translate
           (Stream.Reader,
            Stream.Buffer (Stream.Rest_First .. Stream.Rest_Last),
            In_Last  => Stream.Rest_First,
            Out_Data =>
              Stream.Buffer (Stream.Rest_Last + 1 .. Stream.Buffer'Last),
            Out_Last => Stream.Ahead_Last,
            Flush    => (if Stream.Rest_First > Stream.Rest_Last
                         then Finish
                         else No_Flush));

         Stream.Rest_First := @ + 1;
      end if;
   end Read;

   -------------------
   -- Read_Total_In --
   -------------------

   function Read_Total_In (Stream : in Stream_Type) return Count is
   begin
      return Total_In (Stream.Reader);
   end Read_Total_In;

   --------------------
   -- Read_Total_Out --
   --------------------

   function Read_Total_Out (Stream : in Stream_Type) return Count is
   begin
      return Total_Out (Stream.Reader);
   end Read_Total_Out;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   : in     Stream_Element_Array)
   is
      procedure Write (Item : in Stream_Element_Array)
        with Inline;

      -----------
      -- Write --
      -----------

      procedure Write (Item : in Stream_Element_Array) is
      begin
         Stream.Back.Write (Item);
      end Write;

      procedure Write is new ZLib.Write
         (Write       => Write,
          Buffer_Size => Stream.Buffer_Size);

   begin
      Write (Stream.Writer, Item, No_Flush);
   end Write;

   --------------------
   -- Write_Total_In --
   --------------------

   function Write_Total_In (Stream : in Stream_Type) return Count is
   begin
      return Total_In (Stream.Writer);
   end Write_Total_In;

   ---------------------
   -- Write_Total_Out --
   ---------------------

   function Write_Total_Out (Stream : in Stream_Type) return Count is
   begin
      return Total_Out (Stream.Writer);
   end Write_Total_Out;

end ZLib.Streams;
