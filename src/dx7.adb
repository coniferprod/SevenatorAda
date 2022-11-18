with Helpers; use Helpers;

package body DX7 is

    function New_Envelope (Rates : Rate_Array; Levels : Level_Array) return Envelope_Type is
        E : Envelope_Type := (Rates, Levels);
    begin
        -- TODO: Need to validate?
        return E;
    end New_Envelope;

    function Get_Envelope_Rate (Envelope : Envelope_Type; N : Rate_Index) return Rate_Type is
    begin
        return Envelope.Rates (N);
    end Get_Envelope_Rate;

    procedure Set_Envelope_Rate (Envelope : in out Envelope_Type; N : Rate_Index; V : Rate_Type) is
    begin
        Envelope.Rates (N) := V;
    end Set_Envelope_Rate;

    function Get_Envelope_Level (Envelope : Envelope_Type; N : Level_Index) return Level_Type is
    begin
        return Envelope.Levels (N);
    end Get_Envelope_Level;

    procedure Set_Envelope_Level (Envelope : in out Envelope_Type; N : Level_Index; V : Level_Type) is
    begin
        Envelope.Levels (N) := V;
    end Set_Envelope_Level;

    function Get_Data (Envelope : Envelope_Type) return Helpers.Byte_Vectors.Vector is
        BV : Byte_Vectors.Vector;
    begin
        BV.Append (Byte (Envelope.Rates (1)));
        BV.Append (Byte (Envelope.Rates (2)));
        BV.Append (Byte (Envelope.Rates (3)));
        BV.Append (Byte (Envelope.Rates (4)));

        BV.Append (Byte (Envelope.Levels (1)));
        BV.Append (Byte (Envelope.Levels (2)));
        BV.Append (Byte (Envelope.Levels (3)));
        BV.Append (Byte (Envelope.Levels (4)));

        return BV;
    end Get_Data;

    function Get_Data (Operator : Operator_Type) return Helpers.Byte_Vectors.Vector is
        BV : Byte_Vectors.Vector;
    begin
        BV.Append (Get_Data (Operator.EG));
        return BV;
    end Get_Data;

    function Get_Data (Voice : Voice_Type) return Helpers.Byte_Vectors.Vector is
        BV : Byte_Vectors.Vector;
    begin
        for op in Operator_Index loop
            BV.Append (Get_Data (Voice.Operators (op)));
        end loop;
        return BV;
    end Get_Data;

    function Pack (Voice_Data : Helpers.Byte_Vectors.Vector) return Helpers.Byte_Vectors.Vector is
        BV : Byte_Vectors.Vector;
    begin
        BV := Voice_Data;
        -- TODO: Actually pack the voice data
        return BV;
    end Pack;

    function Get_Data (Cartridge : Cartridge_Type) return Helpers.Byte_Vectors.Vector is
        BV : Byte_Vectors.Vector;
        Voice_Data : Byte_Vectors.Vector;
        Packed_Voice_Data : Byte_Vectors.Vector;
    begin
        for i in Voice_Index loop
            Voice_Data := Get_Data (Cartridge.Voices (i));
            Packed_Voice_Data := Pack (Voice_Data);
            BV.Append (Packed_Voice_Data);
        end loop;
        -- TODO: Add checksum
        return BV;
    end Get_Data;

end DX7;
