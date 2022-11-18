with Helpers;

package DX7 is

    type Coarse_Type is range 0 .. 31;
    type Detune_Type is range -7 .. 7;

    type Algorithm_Type is range 1 .. 32;
    type Depth_Type is range 0 .. 7;

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

    type Curve_Style_Type is (Linear, Exponential);

    type Scaling_Curve_Type is record
        Curve : Curve_Style_Type := Linear;
        Positive : Boolean := False;
    end record;

    Lin_Neg_Curve : constant Scaling_Curve_Type := (Curve => Linear, Positive => False);
    Lin_Pos_Curve : constant Scaling_Curve_Type := (Curve => Linear, Positive => True);
    Exp_Neg_Curve : constant Scaling_Curve_Type := (Curve => Exponential, Positive => False);
    Exp_Pos_Curve : constant Scaling_Curve_Type := (Curve => Exponential, Positive => True);

    type MIDI_Note_Type is range 0 .. 127;

    type Keyboard_Level_Scaling_Type is record
        Breakpoint : MIDI_Note_Type;
        Left_Depth : Depth_Type;
        Right_Depth : Depth_Type;
        Left_Curve : Scaling_Curve_Type;
        Right_Curve : Scaling_Curve_Type;
    end record;

    type Operator_Mode is (Ratio, Fixed);

    type Operator_Type is record
        EG : Envelope_Type;
        Kbd_Level_Scaling : Keyboard_Level_Scaling_Type;
        Kbd_Rate_Scaling : Depth_Type;
        Amp_Mod_Sens : Integer;  -- TODO: Make a type for this (0 .. 3)
        Key_Vel_Sens : Depth_Type;
        Output_Level : Level_Type;
        Mode : Operator_Mode;
        Coarse : Coarse_Type;
        Fine : Level_Type;
        Detune : Detune_Type;
    end record;

    type Operator_Index is range 1 .. 6;
    type Operator_Array is array (Operator_Index) of Operator_Type;

    Voice_Name_Length : constant Integer := 10;
    subtype Voice_Name_Type is String (1 .. Voice_Name_Length);

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
        Name : Voice_Name_Type;
        Operators : Operator_Array;
        Pitch_Envelope : Envelope_Type;
        Algorithm : Algorithm_Type;
        Feedback : Depth_Type;
        Osc_Sync : Boolean;
        LFO : LFO_Type;
    end record;

    function New_Envelope (Rates : Rate_Array; Levels : Level_Array) return Envelope_Type;
    function Get_Envelope_Rate (Envelope : Envelope_Type; N : Rate_Index) return Rate_Type;
    procedure Set_Envelope_Rate (Envelope : in out Envelope_Type; N : Rate_Index; V : Rate_Type);
    function Get_Envelope_Level (Envelope : Envelope_Type; N : Level_Index) return Level_Type;
    procedure Set_Envelope_Level (Envelope : in out Envelope_Type; N : Level_Index; V : Level_Type);

    -- Use overloading by argument to define Get_Data for each type as required
    function Get_Data (Envelope : Envelope_Type) return Helpers.Byte_Vectors.Vector;
    function Get_Data (Operator : Operator_Type) return Helpers.Byte_Vectors.Vector;
    function Get_Data (Voice : Voice_Type) return Helpers.Byte_Vectors.Vector;
private

end DX7;
