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
        Pitch_Modulation_Sensitivity : Depth_Type;
    end record;

    type Voice_Type is record
        Operators : Operator_Array;
        Pitch_Envelope : Envelope_Type;
        Algorithm : Algorithm_Type;
        Feedback : Depth_Type;
        Oscillator_Sync : Boolean;
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

    Brass1 : constant Voice_Type := (
        Operators => (
            ( --OP1
                EG => (
                    Rates => (72, 76, 99, 71),
                    Levels => (99, 88, 96, 0)
                ),
                Keyboard_Level_Scaling => (
                    Breakpoint => 60 - 21, 
                    Left_Depth => 0,
                    Right_Depth => 0,
                    Left_Curve => Linear_Positive_Curve,
                    Right_Curve => Linear_Positive_Curve
                ),
                Keyboard_Rate_Scaling => 0,
                AMS => 0,
                Keyboard_Velocity_Sensitivity => 0,
                Output_Level => 98,
                Mode => Ratio,
                Coarse => 1,
                Fine => 0,
                Detune => 7
            ),
            ( --OP2
                EG => (
                    Rates => (62, 51, 29, 71),
                    Levels => (82, 95, 96, 0)
                ),
                Keyboard_Level_Scaling => (
                    Breakpoint => 48 - 21, 
                    Left_Depth => 0,
                    Right_Depth => 0,
                    Left_Curve => Linear_Positive_Curve,
                    Right_Curve => Exponential_Negative_Curve
                ),
                Keyboard_Rate_Scaling => 0,
                AMS => 0,
                Keyboard_Velocity_Sensitivity => 0,
                Output_Level => 86,
                Mode => Ratio,
                Coarse => 0,
                Fine => 0,
                Detune => 7
            ),
            ( --OP3
                EG => (
                    Rates => (77, 76, 82, 71),
                    Levels => (99, 98, 98, 0)
                ),
                Keyboard_Level_Scaling => (
                    Breakpoint => 48 - 21, 
                    Left_Depth => 0,
                    Right_Depth => 0,
                    Left_Curve => Linear_Positive_Curve,
                    Right_Curve => Exponential_Negative_Curve
                ),
                Keyboard_Rate_Scaling => 0,
                AMS => 0,
                Keyboard_Velocity_Sensitivity => 2,
                Output_Level => 86,
                Mode => Ratio,
                Coarse => 0,
                Fine => 0,
                Detune => -2
            ),
            ( --OP4
                EG => (
                    Rates => (77, 36, 41, 71),
                    Levels => (99, 98, 98, 0)
                ),
                Keyboard_Level_Scaling => (
                    Breakpoint => 48 - 21, 
                    Left_Depth => 0,
                    Right_Depth => 0,
                    Left_Curve => Linear_Positive_Curve,
                    Right_Curve => Exponential_Negative_Curve
                ),
                Keyboard_Rate_Scaling => 0,
                AMS => 0,
                Keyboard_Velocity_Sensitivity => 2,
                Output_Level => 86,
                Mode => Ratio,
                Coarse => 0,
                Fine => 0,
                Detune => 1
            ),
            ( --OP5
                EG => (
                    Rates => (77, 36, 41, 71),
                    Levels => (99, 98, 98, 0)
                ),
                Keyboard_Level_Scaling => (
                    Breakpoint => 48 - 21, 
                    Left_Depth => 0,
                    Right_Depth => 0,
                    Left_Curve => Linear_Positive_Curve,
                    Right_Curve => Exponential_Negative_Curve
                ),
                Keyboard_Rate_Scaling => 0,
                AMS => 0,
                Keyboard_Velocity_Sensitivity => 2,
                Output_Level => 98,
                Mode => Ratio,
                Coarse => 0,
                Fine => 0,
                Detune => 1
            ),
            ( --OP6
                EG => (
                    Rates => (49, 99, 28, 68),
                    Levels => (98, 98, 91, 0)
                ),    
                Keyboard_Level_Scaling => (
                    Breakpoint => 60 - 21, 
                    Left_Depth => 50,
                    Right_Depth => 0,
                    Left_Curve => Exponential_Negative_Curve,
                    Right_Curve => Exponential_Negative_Curve
                ),
                Keyboard_Rate_Scaling => 4,
                AMS => 0,
                Keyboard_Velocity_Sensitivity => 2,
                Output_Level => 98,
                Mode => Ratio,
                Coarse => 0,
                Fine => 0,
                Detune => 1
            )
        ),
        Pitch_Envelope => (
            Rates => (84, 95, 95, 60),
            Levels => (50, 50, 50, 50)
        ),
        Algorithm => 22,
        Feedback => 7,
        Oscillator_Sync => True,
        LFO => (
            Speed => 37,
            LFO_Delay => 0,
            PMD => 5,
            AMD => 0,
            Sync => False,
            Wave => Sine,
            Pitch_Modulation_Sensitivity => 3
        ),
        Transpose => 0,
        Name => "BRASS   1 "
    );

end DX7.Voices;
