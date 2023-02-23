package body DX7.Cartridges is

    function Get_Data (Cartridge : Cartridge_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        for i in Voice_Index loop
            BV.Append(Get_Packed_Data(Cartridge.Voices(i)));
        end loop;
        return BV;
    end Get_Data;


end DX7.Cartridges;
