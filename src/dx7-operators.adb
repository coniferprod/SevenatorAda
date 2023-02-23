package body DX7.Operators is

    function Get_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector is
        BV : Byte_Vector;
        LeftSC : Byte;
        RightSC : Byte;
    begin
        BV.Append(Byte(KLS.Breakpoint));
        BV.Append(Byte(KLS.Left_Depth));
        BV.Append(Byte(KLS.Right_Depth));

        LeftSC := (case KLS.Left_Curve.Curve is
            when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Left_Curve.Positive then 2 else 1));

        RightSC := (case KLS.Right_Curve.Curve is
            when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Right_Curve.Positive then 2 else 1));

        BV.Append(Byte(LeftSC));
        BV.Append(Byte(RightSC));

        return BV;
    end Get_Data;

    function Get_Packed_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector is
        BV : Byte_Vector;
        LeftSC : Byte;
        RightSC : Byte;
        SC : Byte;
    begin
        BV.Append(Byte(KLS.Breakpoint));
        BV.Append(Byte(KLS.Left_Depth));
        BV.Append(Byte(KLS.Right_Depth));

        LeftSC := (case KLS.Left_Curve.Curve is
            when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Left_Curve.Positive then 2 else 1));

        RightSC := (case KLS.Right_Curve.Curve is
            when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Right_Curve.Positive then 2 else 1));

        -- Byte is a modular type (see Helpers), so bitwise operators are defined.
        -- Shift_Left is from the Interfaces package.
        SC := Byte(LeftSC) or (Shift_Left(Byte(RightSC), 2));

        BV.Append(SC);

        return BV;
    end Get_Packed_Data;

    function Get_Data (Operator : Operator_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        BV.Append(Get_Data(Operator.EG));
        BV.Append(Get_Data(Operator.Keyboard_Level_Scaling));
        BV.Append(Byte(Operator.Keyboard_Rate_Scaling));
        BV.Append(Byte(Operator.AMS));
        BV.Append(Byte(Operator.Keyboard_Velocity_Sensitivity));
        BV.Append(Byte(Operator.Output_Level));
        BV.Append(Byte(Operator_Mode'Pos(Operator.Mode)));
        BV.Append(Byte(Operator.Coarse));
        BV.Append(Byte(Operator.Fine));
        BV.Append(Byte(Operator.Detune + 7)); -- adjust to 0...14 for SysEx
        return BV;
    end Get_Data;

    function Get_Packed_Data (Operator : Operator_Type) return Byte_Vector is
        BV : Byte_Vector;
        Detune_Byte: Byte;
        Byte12: Byte;
        Byte13: Byte;
        Byte15: Byte;
    begin
        BV.Append(Get_Data(Operator.EG)); -- normal and packed are the same
        BV.Append(Get_Packed_Data(Operator.Keyboard_Level_Scaling));

        Detune_Byte := Byte(Operator.Detune + 7); -- adjust to 0...14 for SysEx
        BV.Append(Detune_Byte);

        Byte12 := Byte(Operator.Keyboard_Rate_Scaling) or Shift_Left(Detune_Byte, 3);
        BV.Append(Byte12);

        Byte13 := Byte(Operator.AMS) 
            or Shift_Left(Byte(Operator.Keyboard_Velocity_Sensitivity), 2);
        BV.Append(Byte13);

        BV.Append(Byte(Operator.Output_Level));

        Byte15 := Byte(Operator_Mode'Pos(Operator.Mode)) 
            or Shift_Left(Byte(Operator.Coarse), 1);
        BV.Append(Byte15);

        BV.Append(Byte(Operator.Fine));

        return BV;
    end Get_Packed_Data;

end DX7.Operators;
