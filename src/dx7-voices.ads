with DX7.Envelopes; use DX7.Envelopes;
with DX7.Operators; use DX7.Operators;

package DX7.Voices is

    Voice_Name_Length : constant Integer := 10;
    subtype Voice_Name_Type is String (1 .. Voice_Name_Length);

    type Transpose_Type is range -2 .. 2;

    -- Enumeration type for LFO waveforms
    type LFO_Waveform_Type is (
        Triangle,
        SawDown,
        SawUp,
        Square,
        Sine,
        SampleAndHold
    );

    type LFO_Type is record
        Speed : Level_Type;
        LFO_Delay : Level_Type;
        PMD : Level_Type;
        AMD : Level_Type;
        Sync : Boolean;
        Wave : LFO_Waveform_Type;
        Pitch_Mod_Sens : Depth_Type;
    end record;

    type Voice_Type is record
        Operators : Operator_Array;
        Pitch_Envelope : Envelope_Type;
        Algorithm : Algorithm_Type;
        Feedback : Depth_Type;
        Osc_Sync : Boolean;
        LFO : LFO_Type;
        Transpose: Transpose_Type;
        Name : Voice_Name_Type;
    end record;

    type Voice_Index is range 1 .. 32;
    type Voice_Array is array (Voice_Index) of Voice_Type;

    function Get_Data (Voice : Voice_Type) return Byte_Vector;
    function Get_Packed_Data (Voice : Voice_Type) return Byte_Vector;

    function Get_Data (LFO : LFO_Type) return Byte_Vector;
    function Get_Packed_Data (LFO : LFO_Type) return Byte_Vector;

end DX7.Voices;
