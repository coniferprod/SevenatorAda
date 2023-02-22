with DX7.Voices; use DX7.Voices;

package DX7.Cartridges is
    
    type Cartridge_Type is record
        Voices : Voice_Array;
    end record;

    function Get_Data (Cartridge : Cartridge_Type) return Byte_Vector;

end DX7.Cartridges;
