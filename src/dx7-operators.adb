package body DX7.Operators is

    function Get_Data (KLS: Keyboard_Level_Scaling_Type) 
        return Keyboard_Level_Scaling_Data_Type is
    begin
        return (Get_Data (KLS.Breakpoint),
                Byte (KLS.Left_Depth),
                Byte (KLS.Right_Depth),
                Byte (case KLS.Left_Curve.Curve is
                      when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
                      when Exponential => (if KLS.Left_Curve.Positive then 2 else 1)),
                Byte (case KLS.Right_Curve.Curve is
                      when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
                      when Exponential => (if KLS.Right_Curve.Positive then 2 else 1)));
    end Get_Data;

    function Get_Packed_Data (KLS: Keyboard_Level_Scaling_Type) 
        return Keyboard_Level_Scaling_Packed_Data_Type is
        Data : Keyboard_Level_Scaling_Packed_Data_Type;
    begin
        Data (1) := Byte (KLS.Breakpoint);
        Data (2) := Byte (KLS.Left_Depth);
        Data (3) := Byte (KLS.Right_Depth);

        declare
            LeftSC, RightSC, SC : Byte;
        begin
            LeftSC := (case KLS.Left_Curve.Curve is
                when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
                when Exponential => (if KLS.Left_Curve.Positive then 2 else 1));

            RightSC := (case KLS.Right_Curve.Curve is
                when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
                when Exponential => (if KLS.Right_Curve.Positive then 2 else 1));

            SC := LeftSC or (Shift_Left(RightSC, 2));

            Data (4) := SC;
        end;

        return Data;
    end Get_Packed_Data;

    function Get_Data (Operator : Operator_Type) return Operator_Data_Type is
        Data : Operator_Data_Type;
        Offset : Positive;
    begin
        Offset := 1;
        for EG_Byte of Get_Data (Operator.EG) loop
            Data (Offset) := EG_Byte;
            Offset := Offset + 1;
        end loop;

        for KLS_Byte of Get_Data (Operator.Keyboard_Level_Scaling) loop
            Data (Offset) := KLS_Byte;
            Offset := Offset + 1; 
        end loop;

        Data (Offset) := Byte (Operator.Keyboard_Rate_Scaling); 
        Data (Offset + 1) := Byte (Operator.Amplitude_Modulation_Scaling);
        Data (Offset + 2) := Byte (Operator.Keyboard_Velocity_Sensitivity);
        Data (Offset + 3) := Byte (Operator.Output_Level);
        Data (Offset + 4) := Byte (Operator_Mode'Pos (Operator.Mode));
        Data (Offset + 5) := Byte (Operator.Coarse);
        Data (Offset + 6) := Byte (Operator.Fine);
        Data (Offset + 7) := Byte (Operator.Detune + 7); -- adjust to 0...14 for SysEx
        
        return Data;
    end Get_Data;

    function Get_Packed_Data (Operator : Operator_Type) 
        return Operator_Packed_Data_Type is
        Detune_Byte: Byte;
        Byte12: Byte;
        Byte13: Byte;
        Byte15: Byte;
        Data : Operator_Packed_Data_Type;
        Offset : Positive;
    begin
        -- Normal and packed EG data are the same
        Offset := 1;
        for EG_Byte of Get_Data (Operator.EG) loop
            Data (Offset) := EG_Byte;
            Offset := Offset + 1;
        end loop;

        -- Using packed data for keyboard level scaling
        for KLS_Byte of Get_Packed_Data (Operator.Keyboard_Level_Scaling) loop
            Data (Offset) := KLS_Byte;
            Offset := Offset + 1; 
        end loop;

        Detune_Byte := Byte (Operator.Detune + 7); -- adjust to 0...14 for SysEx

        Byte12 := Byte (Operator.Keyboard_Rate_Scaling) or Shift_Left (Detune_Byte, 3);
        Data (Offset) := Byte12;

        Byte13 := Byte(Operator.Amplitude_Modulation_Scaling) 
            or Shift_Left(Byte(Operator.Keyboard_Velocity_Sensitivity), 2);
        Data (Offset + 1) := Byte13;

        Data (Offset + 2) := Byte (Operator.Output_Level);

        Byte15 := Byte(Operator_Mode'Pos(Operator.Mode)) 
            or Shift_Left(Byte(Operator.Coarse), 1);
        Data (Offset + 3) := Byte15;

        Data (Offset + 4) := Byte (Operator.Fine);

        return Data;
    end Get_Packed_Data;

    function Get_Breakpoint (Data : Byte) return Breakpoint_Type is
    begin
        -- We get a byte 0 .. 99, which needs to be adjusted
        -- to the breakpoint MIDI note range 21 .. 120.
        return Breakpoint_Type (Data + 21);
    end Get_Breakpoint;

    function Get_Data (Breakpoint : Breakpoint_Type) return Byte is
    begin
        return Byte (Breakpoint - 21);
    end Get_Data;

end DX7.Operators;
