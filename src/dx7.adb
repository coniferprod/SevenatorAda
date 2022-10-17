package body DX7 is

    function Envelope(Rates: Rate_Array; Levels: Level_Array) return Envelope_Type is
        E : Envelope_Type := (Rates, Levels);
    begin
        -- TODO: Need to validate?
        return E;
    end Envelope;

    function Envelope_Rate(Envelope: Envelope_Type; N: Rate_Index) return Rate_Type is
    begin
        return Envelope.Rates (N);
    end Envelope_Rate;

    function Envelope_Level(Envelope: Envelope_Type; N: Level_Index) return Level_Type is
    begin
        return Envelope.Levels (N);
    end Envelope_Level;

end DX7;
