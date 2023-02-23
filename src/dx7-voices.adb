package body DX7.Voices is

    function Get_Data (LFO : LFO_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        BV.Append(Byte(LFO.Speed));
        BV.Append(Byte(LFO.LFO_Delay));
        BV.Append(Byte(LFO.PMD));
        BV.Append(Byte(LFO.AMD));
        BV.Append(Byte(if LFO.Sync = True then 1 else 0));

        -- Convert enum value to Byte (first enum is pos zero)
        BV.Append(Byte(LFO_Waveform_Type'Pos(LFO.Wave)));

        return BV;
    end Get_Data;

    function Get_Packed_Data (LFO : LFO_Type) return Byte_Vector is
        BV : Byte_Vector;
        Byte116: Byte;
    begin
        BV.Append(Byte(LFO.Speed));
        BV.Append(Byte(LFO.LFO_Delay));
        BV.Append(Byte(LFO.PMD));
        BV.Append(Byte(LFO.AMD));

        Byte116 := (if LFO.Sync then 1 else 0);

        -- Waveform type starts at bit 1
        Byte116 := Byte116 
            or Shift_Left(Byte(LFO_Waveform_Type'Pos(LFO.Wave)), 1);
        -- TODO: How to set a bit range?
        -- b116.replaceBits(1...3, with: Byte(LFO.Wave))

        -- NOTE: In the packed format, this byte contains also the
        -- pitch modulation sensitivity, which applies to all operators.
        -- The corresponding bits need to be set later.

        BV.Append(Byte116);
        return BV;
    end Get_Packed_Data;

    function Get_Data (Voice : Voice_Type) return Byte_Vector is
        Ch: Character;
        BV : Byte_Vector;
    begin
        -- Note: the operators appear in reverse order: OP6, OP5 etc.
        for op in reverse Operator_Index loop
            BV.Append (Get_Data (Voice.Operators (op)));
        end loop;

        BV.Append(Get_Data(Voice.Pitch_Envelope));
        BV.Append(Byte(Voice.Algorithm - 1)); -- adjust to 0...31 for SysEx
        BV.Append(Byte(Voice.Feedback));
        BV.Append(Byte(if Voice.Oscillator_Sync = True then 1 else 0));
        BV.Append(Get_Data(Voice.LFO));
        BV.Append(Byte((Voice.Transpose + 2) * 12));  -- adjust -2..+2 to 0...48 for SysEx

        for i in 1 .. Voice_Name_Length loop
            Ch := Voice.Name(i);
            BV.Append(Character'Pos(Ch));
        end loop;

        return BV;
    end Get_Data;

    function Get_Packed_Data (Voice : Voice_Type) return Byte_Vector is
        Ch: Character;
        BV : Byte_Vector;
        Byte111: Byte;
    begin
        -- Note: the operators appear in reverse order: OP6, OP5 etc.
        for op in reverse Operator_Index loop
            BV.Append (Get_Packed_Data (Voice.Operators (op)));
        end loop;

        BV.Append(Get_Data(Voice.Pitch_Envelope));

        BV.Append(Byte(Voice.Algorithm - 1)); -- adjust to 0...31 for SysEx

        Byte111 := Byte(Voice.Feedback) 
            or Shift_Left(Byte(if Voice.Oscillator_Sync then 1 else 0), 3);
        BV.Append(Byte111);

        BV.Append(Get_Packed_Data(Voice.LFO));

        -- TODO: Handle the pitch mod sens value

        -- Adjust -2..+2 to 0...48 for SysEx
        BV.Append(Byte((Voice.Transpose + 2) * 12));

        for i in 1 .. Voice_Name_Length loop
            Ch := Voice.Name(i);
            BV.Append(Character'Pos(Ch));
        end loop;

        return BV;
    end Get_Packed_Data;

end DX7.Voices;
