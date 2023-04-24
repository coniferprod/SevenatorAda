with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling;

package body DX7.Voices is

    package Rand_Level is new Ada.Numerics.Discrete_Random (Level_Type);
    package Rand_Depth is new Ada.Numerics.Discrete_Random (Depth_Type);
    package Rand_Wave is new Ada.Numerics.Discrete_Random (LFO_Waveform_Type);
    package Rand_Sync is new Ada.Numerics.Discrete_Random (Boolean);
    package Rand_Alg is new Ada.Numerics.Discrete_Random (Algorithm_Type);

    -- Gets the LFO data as bytes for MIDI System Exclusive.
    -- The normal format is used for individual voice data.
    function Get_Data (LFO : LFO_Type) return LFO_Data_Type is
    begin
        return (Byte (LFO.Speed),
                Byte (LFO.LFO_Delay),
                Byte (LFO.PMD),
                Byte (LFO.AMD),
                (if LFO.Sync then 1 else 0),
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
                Byte (if LFO.Sync then 1 else 0)
                    or Shift_Left (Byte (LFO_Waveform_Type'Pos (LFO.Wave)), 1));
        -- TODO: How to set a bit range?
        -- b116.replaceBits(1...3, with: Byte(LFO.Wave))
    end Get_Packed_Data;

    -- Gets the voice data bytes for MIDI System Exclusive.
    -- The normal format is used for individual voices.
    function Get_Data (Voice : Voice_Type) return Voice_Data_Type is
        Ch: Character;
        Data : Voice_Data_Type;
        Offset : Positive;
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
        Data (Offset + 2) := (if Voice.Oscillator_Sync = True then 1 else 0);

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
        Offset : Positive;
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

    function Random_LFO return LFO_Type is
        LFO : LFO_Type;
        Level_Gen : Rand_Level.Generator;
        Depth_Gen : Rand_Depth.Generator;
        Wave_Gen : Rand_Wave.Generator;
        Sync_Gen : Rand_Sync.Generator;
    begin
        Rand_Level.Reset (Level_Gen);
        Rand_Depth.Reset (Depth_Gen);
        Rand_Wave.Reset (Wave_Gen);
        Rand_Sync.Reset (Sync_Gen);

        LFO.Speed := Rand_Level.Random (Level_Gen);
        LFO.LFO_Delay := Rand_Level.Random (Level_Gen);
        LFO.PMD := Rand_Level.Random (Level_Gen);
        LFO.AMD := Rand_Level.Random (Level_Gen);
        LFO.Sync := Rand_Sync.Random (Sync_Gen);
        LFO.Wave := Rand_Wave.Random (Wave_Gen);
        LFO.Pitch_Modulation_Sensitivity := Rand_Depth.Random (Depth_Gen);

        return LFO;
    end Random_LFO;

    function Random_Voice return Voice_Type is
        Voice : Voice_Type;
        Alg_Gen : Rand_Alg.Generator;
        Depth_Gen : Rand_Depth.Generator;
        Sync_Gen : Rand_Sync.Generator;
    begin
        Rand_Alg.Reset (Alg_Gen);
        Rand_Depth.Reset (Depth_Gen);
        Rand_Sync.Reset (Sync_Gen);

        --Operators : Operator_Array;
        Voice.Pitch_Envelope := Random_Envelope;

        Voice.Algorithm := Rand_Alg.Random (Alg_Gen);
        Voice.Feedback := Rand_Depth.Random (Depth_Gen);
        Voice.Oscillator_Sync := Rand_Sync.Random (Sync_Gen);
        Voice.LFO := Random_LFO;
        Voice.Transpose := 0; -- TODO: randomize
        Voice.Name := "Random    ";

        return Voice;
    end Random_Voice;

    function Random_Voice_Name return Voice_Name_Type is        
        subtype Syllable_Type is String (1 .. 2);

        type Vowel_Index is range 1 .. 5;
        type Vowel_List is array (Vowel_Index) of Character;

        type Consonant_Index is range 1 .. 14;
        type Consonant_List is array (Consonant_Index) of Character;

        package Rand_Vowel is new Ada.Numerics.Discrete_Random (Vowel_Index);
        package Rand_Consonant is new Ada.Numerics.Discrete_Random (Consonant_Index);

        Vowels : constant Vowel_List := ('a', 'i', 'u', 'e', 'o');
        Consonants : constant Consonant_List := 
            ('k', 's', 't', 'n', 'h', 'm',
             'y', 'r', 'w', 'g', 'z', 'd', 'b', 'p');

        Vowel_Gen : Rand_Vowel.Generator;
        Consonant_Gen : Rand_Consonant.Generator;

        function Random_Syllable return Syllable_Type is
            Syllable : Syllable_Type;
        begin
            Rand_Vowel.Reset (Vowel_Gen);
            Rand_Consonant.Reset (Consonant_Gen);

            Syllable (1) := Ada.Characters.Handling.To_Upper (Consonants (Rand_Consonant.Random (Consonant_Gen)));
            Syllable (2) := Ada.Characters.Handling.To_Upper (Vowels (Rand_Vowel.Random (Vowel_Gen)));

            return Syllable;
        end Random_Syllable;

        Name : Voice_Name_Type;
        Syllable_Start_Index : Positive := 1;
    begin
        Name := 5 * "  ";  -- initialize with empty "syllables"
        for I in 1 .. 5 loop
            Insert (Name, Syllable_Start_Index, Random_Syllable);
            Syllable_Start_Index := Syllable_Start_Index + 2;
        end loop;

        return Name;
    end Random_Voice_Name;

end DX7.Voices;
