-- Child package of DX7 for envelope generator definitions
package DX7.Envelopes is
    -- Define type for level. Also define an array index
    -- for an array of level values, to be used in an envelope.
    type Level_Type is range 0 .. 99;
    type Level_Index is range 1 .. 4;
    type Level_Array is array (Level_Index) of Level_Type;

    -- Similarly to rate, define range, array type and array index.
    type Rate_Type is range 0 .. 99;
    type Rate_Index is range 1 .. 4;
    type Rate_Array is array (Rate_Index) of Rate_Type;

    -- An envelope is represented by rates and levels.
    type Envelope_Type is record
        Rates : Rate_Array;
        Levels : Level_Array;
    end record;

    -- MIDI System Exclusive data length of envelope
    Envelope_Data_Length: constant := Rate_Array'Length + Level_Array'Length;

    subtype Envelope_Data_Type is Data_Type (1 .. Envelope_Data_Length);

    -- Gets the MIDI System Exclusive data for an envelope.
    function Get_Data (Envelope : Envelope_Type) return Envelope_Data_Type;

    -- Makes an envelope with random parameters.
    function Random_Envelope return Envelope_Type;

end DX7.Envelopes;
