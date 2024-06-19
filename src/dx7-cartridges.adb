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
   begin
      Offset := 1;
      for I in Voice_Index loop
         for B of Emit_Packed (Cartridge.Voices (I)) loop
            Data (Offset) := B;
            Offset        := Offset + 1;
         end loop;
      end loop;
   end Get_Data;

end DX7.Cartridges;
