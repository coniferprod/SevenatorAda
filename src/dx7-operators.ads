with DX7.Envelopes; use DX7.Envelopes;

package DX7.Operators is

type Coarse_Type is range 0 .. 31;
    type Detune_Type is range -7 .. 7;

    type Algorithm_Type is range 1 .. 32;
    type Depth_Type is range 0 .. 7;

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
    type MIDI_Channel_Type is range 1 .. 16;

    type Keyboard_Level_Scaling_Type is record
        Breakpoint : MIDI_Note_Type;
        Left_Depth : Depth_Type;
        Right_Depth : Depth_Type;
        Left_Curve : Scaling_Curve_Type;
        Right_Curve : Scaling_Curve_Type;
    end record;

    type Operator_Mode is (Ratio, Fixed);

    type AMS_Type is range 0 .. 3;

    type Operator_Type is record
        EG : Envelope_Type;
        Kbd_Level_Scaling : Keyboard_Level_Scaling_Type;
        Kbd_Rate_Scaling : Depth_Type;
        AMS : AMS_Type;
        Key_Vel_Sens : Depth_Type;
        Output_Level : Level_Type;
        Mode : Operator_Mode;
        Coarse : Coarse_Type;
        Fine : Level_Type;
        Detune : Detune_Type;
    end record;

    type Operator_Index is range 1 .. 6;
    type Operator_Array is array (Operator_Index) of Operator_Type;

    function Get_Data (Operator : Operator_Type) return Byte_Vector;
    function Get_Packed_Data (Operator : Operator_Type) return Byte_Vector;
    function Get_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector;
    function Get_Packed_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector;

end DX7.Operators;
