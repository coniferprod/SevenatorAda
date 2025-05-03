with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

package body DX7 is

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

end DX7;
