with DX7.Voices; use DX7.Voices;

package DX7.Cartridges is    
    type Cartridge_Type is record
        Voices : Voice_Array;
    end record;

    Cartridge_Data_Length: constant Positive := 4096;
    subtype Cartridge_Data_Type is Data_Type (1 .. Cartridge_Data_Length);

    -- Gets the cartridge data as bytes for MIDI System Exclusive.
    procedure Get_Data (Cartridge : in Cartridge_Type; Data : out Cartridge_Data_Type);

end DX7.Cartridges;
