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

   procedure Inc (I : in out Integer; Amount : in Integer := 1) is
   begin
      I := I + Amount;
   end Inc;

   function Slice (BV : Byte_Vector; Start_Index : Natural; End_Index : Natural) return Byte_Vector is
      Length : Natural := End_Index - Start_Index;
      Result : Byte_Vector;
      I : Natural := Start_Index;
   begin
      for I in Start_Index .. End_Index loop
         Result.Append (BV (I));
      end loop;
      return Result;
   end Slice;

   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector is
      BV : Byte_Vector;
   begin
      for I in Data'Range loop
         BV.Append (Byte (Data (I)));
      end loop;
      return BV;
   end To_Byte_Vector;

   function Hex_Dump (Data : Byte_Array) return String is
   begin
      return Sixten.Hex_Dump (To_Byte_Vector (Data));
   end Hex_Dump;

   procedure Put_Byte_Array_Information (A : Byte_Array; Name : String) is
   begin
      Ada.Text_IO.Put (Name & ": " & A'Length'Image & " [");
      Ada.Text_IO.Put (A'First'Image & " .. " & A'Last'Image & "]");
      Ada.Text_IO.New_Line;
   end Put_Byte_Array_Information;

end DX7;
