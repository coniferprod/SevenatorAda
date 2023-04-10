package body DX7.Voices is

    -- Gets the LFO data as bytes for MIDI System Exclusive.
    -- The normal format is used for individual voice data.
    function Get_Data (LFO : LFO_Type) return LFO_Data_Type is
    begin
        return (Byte (LFO.Speed),
                Byte (LFO.LFO_Delay),
                Byte (LFO.PMD),
                Byte (LFO.AMD),
                Byte (if LFO.Sync = True then 1 else 0),
                -- Convert enum value to Byte (first enum is pos zero)
                Byte (LFO_Waveform_Type'Pos (LFO.Wave)),
                Byte (LFO.Pitch_Modulation_Sensitivity)
        );
    end Get_Data;

    -- Gets the LFO data as packed bytes for MIDI System Exclusive.
    -- The packed format is used when voice data is inside cartridge data.
    function Get_Packed_Data (LFO : LFO_Type) return LFO_Packed_Data_Type is
    begin
        return (Byte (LFO.Speed),
                Byte (LFO.LFO_Delay),
                Byte (LFO.PMD),
                Byte (LFO.AMD),

                -- Waveform type starts at bit 1
                Byte (if LFO.Sync = True then 1 else 0)
                    or Shift_Left (Byte (LFO_Waveform_Type'Pos (LFO.Wave)), 1));
        -- TODO: How to set a bit range?
        -- b116.replaceBits(1...3, with: Byte(LFO.Wave))
    end Get_Packed_Data;

    -- Gets the voice data bytes for MIDI System Exclusive.
    -- The normal format is used for individual voices.
    function Get_Data (Voice : Voice_Type) return Voice_Data_Type is
        Ch: Character;
        Data : Voice_Data_Type;
        Offset : Integer;
    begin
        Offset := 1;
        -- Note: the operators appear in reverse order: OP6, OP5 etc.
        for Op in reverse Operator_Index loop
            for B of Get_Data (Voice.Operators (Op)) loop
                Data (Offset) := B;
                Offset := Offset + 1;
            end loop;
        end loop;

        for B of Get_Data (Voice.Pitch_Envelope) loop
            Data (Offset) := B;
            Offset := Offset + 1;
        end loop;

        Data (Offset) := Byte (Voice.Algorithm - 1); -- adjust to 0...31 for SysEx
        Data (Offset + 1) := Byte (Voice.Feedback);
        Data (Offset + 2) := Byte (if Voice.Oscillator_Sync = True then 1 else 0);

        Offset := 2;
        for B of Get_Data (Voice.LFO) loop
            Data (Offset) := B;
            Offset := Offset + 1;
        end loop;

        -- Adjust -2..+2 to 0...48 for SysEx
        Data (Offset) := Byte ((Voice.Transpose + 2) * 12);
        Offset := Offset + 1;

        for I in 1 .. Voice_Name_Length loop
            Ch := Voice.Name (I);
            Data (Offset) := Character'Pos (Ch);
            Offset := Offset + 1;
        end loop;

        return Data;
    end Get_Data;

    -- Gets the voice data bytes in packed format for MIDI System Exclusive.
    -- The packed format is used when voice data is embedded in cartridge data.
    function Get_Packed_Data (Voice : Voice_Type) return Voice_Packed_Data_Type is
        Ch: Character;
        Byte_111: Byte;
        LFO_Data : LFO_Packed_Data_Type;
        Data : Voice_Packed_Data_Type;
        Offset : Integer;
    begin
        Offset := 1;
        -- Note: the operators appear in reverse order: OP6, OP5 etc.
        for Op in reverse Operator_Index loop
            for B of Get_Packed_Data (Voice.Operators (Op)) loop
                Data (Offset) := B;
                Offset := Offset + 1;
            end loop;
        end loop;

        for B of Get_Data (Voice.Pitch_Envelope) loop
            Data (Offset) := B;
            Offset := Offset + 1;
        end loop;

        Data (Offset) := Byte (Voice.Algorithm - 1); -- adjust to 0...31 for SysEx
        Offset := Offset + 1;

        Byte_111 := Byte (Voice.Feedback) 
            or Shift_Left (Byte (if Voice.Oscillator_Sync then 1 else 0), 3);
        Data (Offset) := Byte_111;
        Offset := Offset + 1;

        LFO_Data := Get_Packed_Data (Voice.LFO);

        -- Adjust -2..+2 to 0...48 for SysEx
        Data (Offset) := Byte ((Voice.Transpose + 2) * 12);
        Offset := Offset + 1;

        for I in 1 .. Voice_Name_Length loop
            Ch := Voice.Name (I);
            Data (Offset) := Character'Pos (Ch);
            Offset := Offset + 1;
        end loop;

        return Data;
    end Get_Packed_Data;

end DX7.Voices;
