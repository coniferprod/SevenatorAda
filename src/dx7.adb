with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

package body DX7 is

   function Make_Range_Exception_Message (Text : String; Actual, First, Last : Integer; Offset : Natural) return String is
   begin
      return Offset'Image & ": " & Text & ". Got " & Actual'Image & ", expected " & First'Image & " .. "
         & Last'Image;
   end Make_Range_Exception_Message;

   function Make_Length_Exception_Message (Text : String; Actual, Expected : Integer; Offset : Natural) return String is
   begin
      return Offset'Image & ": " & Text & ". Length was " & Actual'Image
         & ", expected " & Expected'Image;
   end Make_Length_Exception_Message;

end DX7;
