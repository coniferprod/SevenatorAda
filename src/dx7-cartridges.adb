package body DX7.Cartridges is

    -- Gets the cartridge data as bytes for MIDI System Exclusive.
    procedure Get_Data (Cartridge : in Cartridge_Type; Data : out Cartridge_Data_Type) is
        Offset : Integer;
    begin
        Offset := 1;
        for I in Voice_Index loop
            for B of Get_Packed_Data (Cartridge.Voices (I)) loop
                Data (Offset) := B;
                Offset := Offset + 1;
            end loop; 
        end loop;
    end Get_Data;

end DX7.Cartridges;
