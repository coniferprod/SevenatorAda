package body DX7.Cartridges is

   procedure Parse (Data : in Cartridge_Data_Type; Cartridge : out Cartridge_Type) is
   begin
      null;
   end Parse;

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit
     (Cartridge : in Cartridge_Type; Data : out Cartridge_Data_Type)
   is
      Offset : Positive;
      Voice_Data : Voice_Data_Type;
      Voice_Packed_Data : Voice_Packed_Data_Type;
   begin
      Offset := 1;
      for I in Voice_Index loop
         Emit (Cartridge.Voices (I), Voice_Data);
         Pack_Voice (Voice_Data, Voice_Packed_Data);
         for B of Voice_Packed_Data loop
            Data (Offset) := B;
            Offset        := Offset + 1;
         end loop;
      end loop;
   end Emit;

end DX7.Cartridges;
