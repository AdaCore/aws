
with Ada.Text_IO;
with AWS.MIME;

procedure MIME_Comment is
   use Ada;
   Example : constant String := "filename.is";
begin
   AWS.MIME.Load ("mime.types");
   Text_IO.Put_Line
      ('"' & Example & """ translates to """
       & AWS.MIME.Content_Type(Example) & """.");
end MIME_Comment;
