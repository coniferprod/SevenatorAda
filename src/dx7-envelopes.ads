with Helpers; use Helpers;

-- Child package of DX7
package DX7.Envelopes is

    -- Define type for level. Also define an array index
    -- for an array of level values, to be used in an envelope.
    type Level_Type is range 0 .. 99;
    type Level_Index is range 1 .. 4;
    type Level_Array is array (Level_Index) of Level_Type;

    type Rate_Type is range 0 .. 99;
    type Rate_Index is range 1 .. 4;
    type Rate_Array is array (Rate_Index) of Rate_Type;

    type Envelope_Type is record
        Rates : Rate_Array;
        Levels : Level_Array;
    end record;

    function Get_Data (Envelope : Envelope_Type) return Byte_Vector;

    function Random_Envelope return Envelope_Type;

end DX7.Envelopes;
