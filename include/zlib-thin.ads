----------------------------------------------------------------
--  ZLib for Ada thick binding.                               --
--                                                            --
--  Copyright (C) 2002-2019, Dmitriy Anisimkov                --
--                                                            --
--  Open source license information is in the zlib.ads file.  --
----------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C.Strings;

with System;

private package ZLib.Thin is

   --  From zconf.h

   MAX_MEM_LEVEL : constant := 9;
   MAX_WBITS     : constant := 15;   -- 32K LZ77 window

   SEEK_SET : constant := 0; -- Seek from beginning of file
   SEEK_CUR : constant := 1; -- Seek from current position
   SEEK_END : constant := 2; -- Set file pointer to EOF plus "offset"

   type Byte  is new Interfaces.C.unsigned_char; -- 8 bits
   type UInt  is new Interfaces.C.unsigned;      -- 16 bits or more
   type Int   is new Interfaces.C.int;
   type ULong is new Interfaces.C.unsigned_long; -- 32 bits or more

   subtype Chars_Ptr is Interfaces.C.Strings.chars_ptr;

   type ULong_Access is access ULong;
   type Int_Access is access Int;

   subtype Voidp is System.Address;

   subtype Byte_Access is Voidp;

   Nul : constant Voidp := System.Null_Address;

   Z_NO_FLUSH            : constant := 0;
   Z_PARTIAL_FLUSH       : constant := 1;
   Z_SYNC_FLUSH          : constant := 2;
   Z_FULL_FLUSH          : constant := 3;
   Z_FINISH              : constant := 4;
   Z_OK                  : constant := 0;
   Z_STREAM_END          : constant := 1;
   Z_NEED_DICT           : constant := 2;
   Z_ERRNO               : constant := -1;
   Z_STREAM_ERROR        : constant := -2;
   Z_DATA_ERROR          : constant := -3;
   Z_MEM_ERROR           : constant := -4;
   Z_BUF_ERROR           : constant := -5;
   Z_VERSION_ERROR       : constant := -6;
   Z_NO_COMPRESSION      : constant := 0;
   Z_BEST_SPEED          : constant := 1;
   Z_BEST_COMPRESSION    : constant := 9;
   Z_DEFAULT_COMPRESSION : constant := -1;
   Z_FILTERED            : constant := 1;
   Z_HUFFMAN_ONLY        : constant := 2;
   Z_DEFAULT_STRATEGY    : constant := 0;
   Z_BINARY              : constant := 0;
   Z_ASCII               : constant := 1;
   Z_UNKNOWN             : constant := 2;
   Z_DEFLATED            : constant := 8;
   Z_NULL                : constant := 0;

   type gzFile is new Voidp;

   type Z_Stream is private;

   type Z_Streamp is access all Z_Stream;

   type alloc_func is access function
     (Opaque : in Voidp;
      Items  : in UInt;
      Size   : in UInt) return Voidp;

   type free_func is access procedure (opaque : in Voidp; address : in Voidp);

   function zlibVersion return Chars_Ptr;

   function Deflate (strm : in Z_Streamp; flush : in Int) return Int;

   function DeflateEnd (strm : in Z_Streamp) return Int;

   function Inflate (strm : in Z_Streamp; flush : in Int) return Int;

   function InflateEnd (strm : in Z_Streamp) return Int;

   function deflateSetDictionary
     (strm       : in Z_Streamp;
      dictionary : in Byte_Access;
      dictLength : in UInt) return Int;

   function deflateCopy
     (dest : in Z_Streamp; source : in Z_Streamp) return Int;

   function deflateReset (strm : in Z_Streamp) return Int;

   function deflateParams
     (strm     : in Z_Streamp;
      level    : in Int;
      strategy : in Int) return Int;

   function inflateSetDictionary
     (strm       : in Z_Streamp;
      dictionary : in Byte_Access;
      dictLength : in UInt) return Int;

   function inflateSync (strm : in Z_Streamp) return Int;

   function inflateReset (strm : in Z_Streamp) return Int;

   function compress
     (dest      : in Byte_Access;
      destLen   : in ULong_Access;
      source    : in Byte_Access;
      sourceLen : in ULong) return Int;

   function compress2
     (dest      : in Byte_Access;
      destLen   : in ULong_Access;
      source    : in Byte_Access;
      sourceLen : in ULong;
      level     : in Int) return Int;

   function uncompress
     (dest      : in Byte_Access;
      destLen   : in ULong_Access;
      source    : in Byte_Access;
      sourceLen : in ULong) return Int;

   function gzopen (path : in Chars_Ptr; mode : in Chars_Ptr) return gzFile;

   function gzdopen (fd : in Int; mode : in Chars_Ptr) return gzFile;

   function gzsetparams
     (file     : in gzFile;
      level    : in Int;
      strategy : in Int) return Int;

   function gzread
     (file : in gzFile;
      buf  : in Voidp;
      len  : in UInt) return Int;

   function gzwrite
     (file : in gzFile;
      buf  : in Voidp;
      len  : in UInt) return Int;

   function gzprintf (file : in gzFile; format : in Chars_Ptr) return Int;

   function gzputs (file : in gzFile; s : in Chars_Ptr) return Int;

   function gzgets
     (file : in gzFile;
      buf  : in Chars_Ptr;
      len  : in Int) return Chars_Ptr;

   function gzputc (file : in gzFile; char : in Int) return Int;

   function gzgetc (file : in gzFile) return Int;

   function gzflush (file : in gzFile; flush : in Int) return Int;

   function gzseek
     (file : in gzFile; offset : in Int; whence : in Int) return Int;

   function gzrewind (file : in gzFile) return Int;

   function gztell (file : in gzFile) return Int;

   function gzeof (file : in gzFile) return Int;

   function gzclose (file : in gzFile) return Int;

   function gzerror
     (file : in gzFile; errnum : in Int_Access) return Chars_Ptr;

   function adler32
     (adler : in ULong;
      buf   : in Byte_Access;
      len   : in UInt) return ULong;

   function crc32
     (crc  : in ULong;
      buf  : in Byte_Access;
      len  : in UInt) return ULong;

   function deflateInit
     (strm        : in Z_Streamp;
      level       : in Int;
      version     : in Chars_Ptr;
      stream_size : in Int) return Int;

   function deflateInit2
     (strm        : in Z_Streamp;
      level       : in Int;
      method      : in Int;
      windowBits  : in Int;
      memLevel    : in Int;
      strategy    : in Int;
      version     : in Chars_Ptr;
      stream_size : in Int) return Int;

   function Deflate_Init
     (strm       : in Z_Streamp;
      level      : in Int;
      method     : in Int;
      windowBits : in Int;
      memLevel   : in Int;
      strategy   : in Int) return Int with Inline;

   function inflateInit
     (strm        : in Z_Streamp;
      version     : in Chars_Ptr;
      stream_size : in Int) return Int;

   function inflateInit2
     (strm        : in Z_Streamp;
      windowBits  : in Int;
      version     : in Chars_Ptr;
      stream_size : in Int) return Int;

   function inflateBackInit
     (strm        : in Z_Streamp;
      windowBits  : in Int;
      window      : in Byte_Access;
      version     : in Chars_Ptr;
      stream_size : in Int) return Int;
   --  Size of window have to be 2**windowBits

   function Inflate_Init (strm : in Z_Streamp; windowBits : in Int) return Int
     with Inline;

   function zError (err : in Int) return Chars_Ptr;

   function inflateSyncPoint (z : in Z_Streamp) return Int;

   function get_crc_table return ULong_Access;

   --  Interface to the available fields of the z_stream structure.
   --  The application must update next_in and avail_in when avail_in has
   --  dropped to zero. It must update next_out and avail_out when avail_out
   --  has dropped to zero. The application must initialize zalloc, zfree and
   --  opaque before calling the init function.

   procedure Set_In
     (Strm   : in out Z_Stream;
      Buffer : in Voidp;
      Size   : in UInt)
     with Inline;

   procedure Set_Out
     (Strm   : in out Z_Stream;
      Buffer : in Voidp;
      Size   : in UInt)
     with Inline;

   procedure Set_Mem_Func
     (Strm   : in out Z_Stream;
      Opaque : in Voidp;
      Alloc  : in alloc_func;
      Free   : in free_func)
     with Inline;

   function Last_Error_Message (Strm : in Z_Stream) return String with Inline;

   function Avail_Out (Strm : in Z_Stream) return UInt with Inline;

   function Avail_In (Strm : in Z_Stream) return UInt with Inline;

   function Total_In (Strm : in Z_Stream) return ULong with Inline;

   function Total_Out (Strm : in Z_Stream) return ULong with Inline;

   function inflateCopy
     (dest : in Z_Streamp; Source : in Z_Streamp) return Int;

   function compressBound (Source_Len : in ULong) return ULong;

   function deflateBound
     (Strm : in Z_Streamp; Source_Len : in ULong) return ULong;

   function gzungetc (C : in Int; File : in gzFile) return Int;

   function zlibCompileFlags return ULong;

private

   type Z_Stream is record            -- zlib.h:68
      Next_In   : Voidp      := Nul;  -- next input byte
      Avail_In  : UInt       := 0;    -- number of bytes available at next_in
      Total_In  : ULong      := 0;    -- total nb of input bytes read so far
      Next_Out  : Voidp      := Nul;  -- next output byte should be put there
      Avail_Out : UInt       := 0;    -- remaining free space at next_out
      Total_Out : ULong      := 0;    -- total nb of bytes output so far
      msg       : Chars_Ptr;          -- last error message, NULL if no error
      state     : Voidp;              -- not visible by applications
      zalloc    : alloc_func := null; -- used to allocate the internal state
      zfree     : free_func  := null; -- used to free the internal state
      opaque    : Voidp;
      --  private data object passed to zalloc and zfree
      data_type : Int;
      --  best guess about the data type: ascii or binary
      adler     : ULong;
      --  adler32 value of the uncompressed data
      reserved  : ULong;              -- reserved for future use
   end record with Convention => C;

   pragma Import (C, zlibVersion, "zlibVersion");
   pragma Import (C, Deflate, "deflate");
   pragma Import (C, DeflateEnd, "deflateEnd");
   pragma Import (C, Inflate, "inflate");
   pragma Import (C, InflateEnd, "inflateEnd");
   pragma Import (C, deflateSetDictionary, "deflateSetDictionary");
   pragma Import (C, deflateCopy, "deflateCopy");
   pragma Import (C, deflateReset, "deflateReset");
   pragma Import (C, deflateParams, "deflateParams");
   pragma Import (C, inflateSetDictionary, "inflateSetDictionary");
   pragma Import (C, inflateSync, "inflateSync");
   pragma Import (C, inflateReset, "inflateReset");
   pragma Import (C, compress, "compress");
   pragma Import (C, compress2, "compress2");
   pragma Import (C, uncompress, "uncompress");
   pragma Import (C, gzopen, "gzopen");
   pragma Import (C, gzdopen, "gzdopen");
   pragma Import (C, gzsetparams, "gzsetparams");
   pragma Import (C, gzread, "gzread");
   pragma Import (C, gzwrite, "gzwrite");
   pragma Import (C, gzprintf, "gzprintf");
   pragma Import (C, gzputs, "gzputs");
   pragma Import (C, gzgets, "gzgets");
   pragma Import (C, gzputc, "gzputc");
   pragma Import (C, gzgetc, "gzgetc");
   pragma Import (C, gzflush, "gzflush");
   pragma Import (C, gzseek, "gzseek");
   pragma Import (C, gzrewind, "gzrewind");
   pragma Import (C, gztell, "gztell");
   pragma Import (C, gzeof, "gzeof");
   pragma Import (C, gzclose, "gzclose");
   pragma Import (C, gzerror, "gzerror");
   pragma Import (C, adler32, "adler32");
   pragma Import (C, crc32, "crc32");
   pragma Import (C, deflateInit, "deflateInit_");
   pragma Import (C, inflateInit, "inflateInit_");
   pragma Import (C, deflateInit2, "deflateInit2_");
   pragma Import (C, inflateInit2, "inflateInit2_");
   pragma Import (C, zError, "zError");
   pragma Import (C, inflateSyncPoint, "inflateSyncPoint");
   pragma Import (C, get_crc_table, "get_crc_table");

   --  since zlib 1.2.0:

   pragma Import (C, inflateCopy, "inflateCopy");
   pragma Import (C, compressBound, "compressBound");
   pragma Import (C, deflateBound, "deflateBound");
   pragma Import (C, gzungetc, "gzungetc");
   pragma Import (C, zlibCompileFlags, "zlibCompileFlags");

   pragma Import (C, inflateBackInit, "inflateBackInit_");

   --  I stopped binding the inflateBack routines, becouse realize that
   --  it does not support zlib and gzip headers for now, and have no
   --  symmetric deflateBack routines.
   --  ZLib-Ada is symmetric regarding deflate/inflate data transformation
   --  and has a similar generic callback interface for the
   --  deflate/inflate transformation based on the regular Deflate/Inflate
   --  routines.

   --  pragma Import (C, inflateBack, "inflateBack");
   --  pragma Import (C, inflateBackEnd, "inflateBackEnd");

end ZLib.Thin;
