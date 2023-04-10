with Helpers; use Helpers;

-- Child package of DX7 for envelope generator definitions
package DX7.Envelopes is

    -- Define type for level. Also define an array index
    -- for an array of level values, to be used in an envelope.
    type Level_Type is range 0 .. 99;
    type Level_Index is range 1 .. 4;
    type Level_Array is array (Level_Index) of Level_Type;

    -- Similarly to rate, define an range, an array type and array index.
    type Rate_Type is range 0 .. 99;
    type Rate_Index is range 1 .. 4;
    type Rate_Array is array (Rate_Index) of Rate_Type;

    -- An envelope is represented by rates and levels.
    type Envelope_Type is record
        Rates : Rate_Array;
        Levels : Level_Array;
    end record;

    Envelope_Data_Length: constant Integer := 8; -- SysEx data length

    subtype Envelope_Data_Type is Data_Type (1 .. Envelope_Data_Length);

    function Get_Data (Envelope : Envelope_Type) return Envelope_Data_Type;

    function Random_Envelope return Envelope_Type;

end DX7.Envelopes;
