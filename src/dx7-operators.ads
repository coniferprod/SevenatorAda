with DX7.Envelopes; use DX7.Envelopes;

package DX7.Operators is

    type Coarse_Type is range 0 .. 31;
    type Detune_Type is range -7 .. 7;

    type Algorithm_Type is range 1 .. 32;
    type Depth_Type is range 0 .. 7;

    type Curve_Style_Type is (Linear, Exponential);

    type Scaling_Depth_Type is range 0 .. 99;

    type Scaling_Curve_Type is record
        Curve : Curve_Style_Type := Linear;
        Positive : Boolean := False;
    end record;

    Linear_Negative_Curve : constant Scaling_Curve_Type 
        := (Curve => Linear, Positive => False);
    Linear_Positive_Curve : constant Scaling_Curve_Type 
        := (Curve => Linear, Positive => True);
    Exponential_Negative_Curve : constant Scaling_Curve_Type 
        := (Curve => Exponential, Positive => False);
    Exponential_Positive_Curve : constant Scaling_Curve_Type 
        := (Curve => Exponential, Positive => True);

    -- Breakpoint is a key from A-1 to C8, with C3 = 0x27 (39) in SysEx.
    -- It can be expressed as a subtype of MIDI_Note_Type.
    -- In Yamaha notation, A-1 is 21, while C8 is 120.
    -- The SysEx value needs a conversion: +/- 21, which is +/- (60 - 39).
    subtype Breakpoint_Type is MIDI_Note_Type range 21 .. 120;

    type Keyboard_Level_Scaling_Type is record
        Breakpoint : Breakpoint_Type;
        Left_Depth : Scaling_Depth_Type;
        Right_Depth : Scaling_Depth_Type;
        Left_Curve : Scaling_Curve_Type;
        Right_Curve : Scaling_Curve_Type;
    end record;

    Keyboard_Level_Scaling_Data_Length : constant Integer := 5;
    subtype Keyboard_Level_Scaling_Data_Type is 
        Data_Type (1 .. Keyboard_Level_Scaling_Data_Length);

    Keyboard_Level_Scaling_Packed_Data_Length : constant Integer := 4;
    subtype Keyboard_Level_Scaling_Packed_Data_Type is 
        Data_Type (1 .. Keyboard_Level_Scaling_Packed_Data_Length);

    type Operator_Mode is (Ratio, Fixed);

    type AMS_Type is range 0 .. 3;

    type Operator_Type is record
        EG : Envelope_Type;
        Keyboard_Level_Scaling : Keyboard_Level_Scaling_Type;
        Keyboard_Rate_Scaling : Scaling_Depth_Type;
        AMS : AMS_Type;
        Keyboard_Velocity_Sensitivity : Depth_Type;
        Output_Level : Level_Type;
        Mode : Operator_Mode;
        Coarse : Coarse_Type;
        Fine : Level_Type;
        Detune : Detune_Type;
    end record;

    type Operator_Index is range 1 .. 6;
    type Operator_Array is array (Operator_Index) of Operator_Type;

    Operator_Data_Length : constant Integer := 21;
    subtype Operator_Data_Type is Data_Type (1 .. Operator_Data_Length);

    Operator_Packed_Data_Length : constant Integer := 17;
    subtype Operator_Packed_Data_Type is Data_Type (1 .. Operator_Packed_Data_Length);

    function Get_Data (Operator : Operator_Type) 
        return Operator_Data_Type;
    function Get_Packed_Data (Operator : Operator_Type) 
        return Operator_Packed_Data_Type;

    function Get_Data (KLS : Keyboard_Level_Scaling_Type) 
        return Keyboard_Level_Scaling_Data_Type;
    function Get_Packed_Data (KLS : Keyboard_Level_Scaling_Type) 
        return Keyboard_Level_Scaling_Packed_Data_Type;

    -- Converts a SysEx MIDI data byte to a breakpoint
    function Get_Breakpoint (Data : Byte) return Breakpoint_Type;

    -- Converts a breakpoint to SysEx MIDI data byte
    function Get_Data (Breakpoint : Breakpoint_Type) return Byte;
    
end DX7.Operators;
