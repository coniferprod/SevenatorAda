package body DX7.Cartridges is

   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector is
      BV : Byte_Vector;
   begin
      for I in Data'Range loop
         BV.Append (Byte (Data (I)));
      end loop;
      return BV;
   end To_Byte_Vector;

   procedure Parse_Cartridge (Data : in Cartridge_Data_Type; Cartridge : out Cartridge_Type) is
      Packed_Voice_Data : Packed_Voice_Data_Type;
      Voice_Data : Voice_Data_Type;
      Voice : Voice_Type;
      Offset : Natural := 0;
   begin
      for I in Voice_Index loop
         Packed_Voice_Data := Data (Offset .. Offset + Packed_Voice_Data_Length - 1);
         Unpack_Voice (Data => Packed_Voice_Data, Result => Voice_Data);
         Parse_Voice (Data => Voice_Data, Voice => Voice);
         Cartridge.Voices (I) := Voice;
         Offset := Offset + Packed_Voice_Data_Length;
      end loop;
   end Parse_Cartridge;

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit (Cartridge : in Cartridge_Type; Result : out Cartridge_Data_Type) is
      Data : Cartridge_Data_Type;
      Offset : Natural;
   begin
      Offset := 0;
      for I in Voice_Index loop
         declare
            Original : Voice_Data_Type;
            Packed : Packed_Voice_Data_Type;
            Start_Index : Integer;
            End_Index : Integer;
         begin
            Emit (Cartridge.Voices (I), Original);
            Pack_Voice (Original, Packed);

            Start_Index := Offset;
            End_Index := Offset + Packed_Voice_Data_Length - 1;
            Result (Start_Index .. End_Index) :=
               Packed (0 .. Packed_Voice_Data_Length - 1);
            --for V in Packed'Range loop
            --   Result (Offset) := Packed (V);
            --   Offset := Offset + 1;
            --end loop;
            Offset := Offset + Packed_Voice_Data_Length;
         end;
      end loop;
   end Emit;

end DX7.Cartridges;
