with Interfaces.C.Strings;
with Ada.Text_IO;

procedure SSL_Version is

   SSLEAY_VERSION  : constant := 0;
   SSLEAY_CFLAGS   : constant := 2;
   SSLEAY_BUILT_ON : constant := 3;
   SSLEAY_PLATFORM : constant := 4;
   SSLEAY_DIR      : constant := 5;

   procedure Print (Info : Integer);

   -----------
   -- Print --
   -----------

   procedure Print (Info : Integer) is
      use Interfaces.C.Strings;

      function SSLeay_version (T : Integer) return chars_ptr;
      pragma Import (C, SSLeay_version, "SSLeay_version");

   begin
      Ada.Text_IO.Put_Line (Value (SSLeay_version (Info)));
   end Print;

begin
   Ada.Text_IO.Put_Line ("------------");
   Print (SSLEAY_VERSION);
   Print (SSLEAY_CFLAGS);
   Print (SSLEAY_BUILT_ON);
   Print (SSLEAY_PLATFORM);
   Print (SSLEAY_DIR);
   Ada.Text_IO.Put_Line ("------------");
end SSL_Version;