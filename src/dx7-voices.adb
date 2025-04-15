with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with System;

with DX7.Operators; use DX7.Operators;

package body DX7.Voices is

   package Random_Levels is new Ada.Numerics.Discrete_Random (Level_Type);
   package Random_Depths is new Ada.Numerics.Discrete_Random (Depth_Type);
   package Random_Waveforms is new Ada.Numerics.Discrete_Random (LFO_Waveform_Type);
   package Random_Booleans is new Ada.Numerics.Discrete_Random (Boolean);
   package Random_Algorithms is new Ada.Numerics.Discrete_Random (Algorithm_Type);

   Debugging : constant Boolean := True;

   procedure Put_Offset (Offset : Natural; Message : String; Data_Offset : Natural) is
   begin
      if Debugging then
         Ada.Text_IO.Put_Line ("offset = " & Integer'Image (Offset) & " " & Message
            & " from " & Integer'Image (Data_Offset));
      end if;
   end Put_Offset;

   -- Helper function to increment an integer value by the given amount.
   procedure Inc (I : in out Integer; Amount : in Integer := 1) is
   begin
      I := I + Amount;
   end Inc;

   procedure Emit (LFO : in LFO_Type; Result : out LFO_Data_Type) is
   begin
      Result := 
        (Byte (LFO.Speed), Byte (LFO.Delay_Time), Byte (LFO.Pitch_Modulation_Depth),
         Byte (LFO.Amplitude_Modulation_Depth), (if LFO.Key_Sync then 1 else 0),
         -- Convert enum value to Byte (first enum is pos zero)
         Byte (LFO_Waveform_Type'Pos (LFO.Waveform)));
   end Emit;

   -- Gets the voice data bytes for MIDI System Exclusive.
   -- The normal format is used for individual voices.
   procedure Emit (Voice : in Voice_Type; Result : out Voice_Data_Type) is
      Ch     : Character;
      Data   : Voice_Data_Type;
      Offset : Natural;
      Op_Data : Operator_Data_Type;
      PEG_Data : Envelope_Data_Type;
      LFO_Data : LFO_Data_Type;
   begin
      Offset := 0;
      
      -- Note: the operators appear in reverse order: OP6, OP5 etc.
      for Op in reverse Operator_Index loop
         Emit (Voice.Operators (Op), Op_Data);
         for B of Op_Data loop
            Data (Offset) := B;
            Inc (Offset);
         end loop;
      end loop;

      Emit (Voice.Pitch_Envelope, PEG_Data);
      for B of PEG_Data loop
         Data (Offset) := B;
         Offset        := Offset + 1;
      end loop;

      Data (Offset)     :=
        Byte (Voice.Algorithm - 1); -- adjust to 0...31 for SysEx
      Data (Offset + 1) := Byte (Voice.Feedback);
      Data (Offset + 2) := (if Voice.Oscillator_Sync = True then 1 else 0);
      Offset := Offset + 3;

      Emit (Voice.LFO, LFO_Data);
      for B of LFO_Data loop
         Data (Offset) := B;
         Offset := Offset + 1;
      end loop;

      Data (Offset) := Byte (Voice.Pitch_Modulation_Sensitivity);
      Offset        := Offset + 1;

      -- Adjust -2..+2 to 0...48 for SysEx
      Data (Offset) := Byte ((Voice.Transpose + 2) * 12);
      Offset        := Offset + 1;

      for I in 1 .. Voice_Name_Length loop
         Ch            := Voice.Name (I);
         Data (Offset) := Character'Pos (Ch);
         Offset        := Offset + 1;
      end loop;

      Result := Data;
   end Emit;

   --  Packs voice data for cartridge use.
   procedure Pack_Voice (Data : in Voice_Data_Type; Result : out Packed_Voice_Data_Type) is
      -- Use the Ada representation facilities to make a type that
      -- packs several fields into one byte. For details, see
      -- https://en.wikibooks.org/wiki/Ada_Programming/Attributes/%27Bit_Order

      -- byte             bit #
      --  #     6   5   4   3   2   1   0   param A       range  param B       range
      -- 111    0   0   0 |OKS|    FB     | OSC KEY SYNC  0-1    FEEDBACK      0-7

      type Byte111_Type is record
         Feedback        : Depth_Type;
         Oscillator_Sync : Boolean;
      end record;

      for Byte111_Type use record
         Feedback        at 0 range 0 .. 2;
         Oscillator_Sync at 0 range 3 .. 3;
      end record;

      for Byte111_Type'Size use 8;  -- one 8-bit byte, please
      -- Make bit 0 the least significant
      for Byte111_Type'Bit_Order use System.Low_Order_First;

      type Byte116_Type is record
         Sync : Boolean;
         Waveform : LFO_Waveform_Type;
         PMS : Depth_Type;
      end record;

      for Byte116_Type use record
         Sync at 0 range 0 .. 0;
         Waveform at 0 range 1 .. 3;
         PMS at 0 range 4 .. 6;
      end record;

      for Byte116_Type'Size use 8;
      for Byte116_Type'Bit_Order use System.Low_Order_First;

      Ch       : Character;
      Byte111  : Byte111_Type;
      PEG_Data : Envelope_Data_Type;
      LFO_Data : LFO_Data_Type;
      Byte116  : Byte116_Type;
      Data_Offset, Offset   : Natural;
      Op_Data : Operator_Data_Type;
      Packed_Op_Data : Packed_Operator_Data_Type;
      B : Byte;
      Op_Index : Natural;

      function Byte111_Type_To_Byte is new Ada.Unchecked_Conversion
        (Byte111_Type, Byte);
      function Byte116_Type_To_Byte is new Ada.Unchecked_Conversion
        (Byte116_Type, Byte);

   begin
      Data_Offset := 0; -- into the parameter Data : Voice_Data_Type
      Offset := 0;      -- into Result

      -- The operator data is already in reverse order (OP6 first),
      -- so just take each chunk and pack it.      

      Op_Index := 0;
      Offset := Op_Index * Packed_Operator_Data_Length;
      for Op in reverse Operator_Index loop
         Put_Offset (Offset, "Packed operator", Data_Offset);
         Op_Data := Data (Data_Offset .. Data_Offset + 21 - 1);
         Pack_Operator (Op_Data, Packed_Op_Data);
         Result (Offset .. Offset + Packed_Operator_Data_Length - 1) := Packed_Op_Data;
         Inc (Offset, Packed_Operator_Data_Length);
         Inc (Data_Offset, Operator_Data_Length);
      end loop;

      -- Copy the PEG as is
      Put_Offset (Offset, "PEG", Data_Offset);
      PEG_Data := Data (Data_Offset .. Data_Offset + Envelope_Data_Length - 1);
      Result (Offset .. Offset + Envelope_Data_Length - 1) := PEG_Data;
      Inc (Offset, Envelope_Data_Length);
      Inc (Data_Offset, Envelope_Data_Length);

      Put_Offset (Offset, "Algorithm", Data_Offset);
      Result (Offset) := Data (Data_Offset);
      Inc (Offset);
      Inc (Data_Offset);

      Put_Offset (Offset, "FB + osc sync", Data_Offset);
      B := Data (Data_Offset)  -- feedback
         or Shift_Left (Data (Data_Offset + 1), 3);  -- osc sync
      Result (Offset) := B;
      Inc (Offset);
      Inc (Data_Offset, 2);  -- note that we took two bytes from original

      -- LFO speed, delay, PMD, AMD
      Put_Offset (Offset, "Four LFO bytes", Data_Offset);
      Result (Offset .. Offset + 3) := Data (Data_Offset .. Data_Offset + 3);
      Inc (Offset, 4);
      Inc (Data_Offset, 4);

      B := (Data (Data_Offset) 
         or Shift_Left (Data (Data_Offset + 1), 1))  -- LFO waveform
         or (Shift_Left (Data (Data_Offset + 2), 4)); -- PMS (voice)

      Ada.Text_IO.Put_Line ("LFO waveform at " & Integer'Image (Data_Offset + 1) & " = "
         & Integer'Image (Integer (Data (Data_Offset + 1))));
      Put_Offset (Offset, "Byte 116", Data_Offset);
      Byte116 := (Sync => (if Data (Data_Offset) = 1 then True else False),
         Waveform => LFO_Waveform_Type'Val (Data (Data_Offset + 1)), 
         PMS => Depth_Type (Data (Data_Offset + 2)));
      Result (Offset) := Byte116_Type_To_Byte (Byte116);
      Inc (Offset, 1);
      Inc (Data_Offset, 3);

      -- Adjust -2..+2 to 0...48 for SysEx
      Put_Offset (Offset, "Transpose", Data_Offset);
      Result (Offset) := Data (Data_Offset);
      Inc (Offset);
      Inc (Data_Offset);

      for I in 1 .. Voice_Name_Length loop
         Put_Offset (Offset, "Character", Data_Offset);
         Result (Offset) := Data (Data_Offset);
         Inc (Offset);
         Inc (Data_Offset);
      end loop;
   end Pack_Voice;

   procedure Unpack_Voice (Data : in Packed_Voice_Data_Type; Result : out Voice_Data_Type) is
   begin
      null;
   end Unpack_Voice;

   function Random_LFO return LFO_Type is
      LFO       : LFO_Type;
      Level_Gen : Random_Levels.Generator;
      Waveform_Gen  : Random_Waveforms.Generator;
      Sync_Gen  : Random_Booleans.Generator;
   begin
      Random_Levels.Reset (Level_Gen);
      Random_Waveforms.Reset (Waveform_Gen);
      Random_Booleans.Reset (Sync_Gen);

      LFO.Speed     := Random_Levels.Random (Level_Gen);
      LFO.Delay_Time := Random_Levels.Random (Level_Gen);
      LFO.Pitch_Modulation_Depth       := Random_Levels.Random (Level_Gen);
      LFO.Amplitude_Modulation_Depth       := Random_Levels.Random (Level_Gen);
      LFO.Key_Sync      := Random_Booleans.Random (Sync_Gen);
      LFO.Waveform      := Random_Waveforms.Random (Waveform_Gen);

      return LFO;
   end Random_LFO;

   function Random_Voice return Voice_Type is
      Voice     : Voice_Type;
      Alg_Gen   : Random_Algorithms.Generator;
      Depth_Gen : Random_Depths.Generator;
      Sync_Gen  : Random_Booleans.Generator;
   begin
      Random_Algorithms.Reset (Alg_Gen);
      Random_Depths.Reset (Depth_Gen);
      Random_Booleans.Reset (Sync_Gen);

      Voice.Pitch_Envelope := Random_Envelope;
      Voice.Algorithm       := Random_Algorithms.Random (Alg_Gen);
      Voice.Feedback        := Random_Depths.Random (Depth_Gen);
      Voice.Oscillator_Sync := Random_Booleans.Random (Sync_Gen);
      Voice.LFO             := Random_LFO;
      Voice.Transpose       := 0; -- TODO: randomize
      Voice.Name            := "Random    ";

      return Voice;
   end Random_Voice;

   function Random_Voice_Name return Voice_Name_Type is
      subtype Syllable_Type is String (1 .. 2);

      type Consonant is
        ('k', 's', 't', 'n', 'h', 'm', 'y', 'r', 'w', 'g', 'z', 'd', 'b', 'p');
      type Vowel is ('a', 'i', 'u', 'e', 'o');

      package Random_Consonants is new Ada.Numerics.Discrete_Random (Consonant);
      package Random_Vowels is new Ada.Numerics.Discrete_Random (Vowel);

      Vowel_Gen     : Random_Vowels.Generator;
      Consonant_Gen : Random_Consonants.Generator;

      function To_Char (C : Consonant) return Character is
      begin
         -- The image of a character literal is enclosed in single quotes.
         -- We just take the character from between them, at index 2.
         return Consonant'Image (C) (2);
      end To_Char;

      function To_Char (V : Vowel) return Character is -- as above
      begin
         return Vowel'Image (V) (2);
      end To_Char;

      function Random_Syllable return Syllable_Type is
         Syllable : Syllable_Type;
      begin
         Random_Vowels.Reset (Vowel_Gen);
         Random_Consonants.Reset (Consonant_Gen);

         -- Get random consonant and vowel as upper case characters.
         Syllable (1) :=
           Ada.Characters.Handling.To_Upper
             (To_Char (Random_Consonants.Random (Consonant_Gen)));
         Syllable (2) :=
           Ada.Characters.Handling.To_Upper
             (To_Char (Random_Vowels.Random (Vowel_Gen)));

         return Syllable;
      end Random_Syllable;

      Name                 : Voice_Name_Type;
      Syllable_Start_Index : Positive := 1;
   begin
      Name := 5 * "  ";  -- initialize with empty "syllables"
      for I in 1 .. 5 loop
         Insert (Name, Syllable_Start_Index, Random_Syllable);
         Syllable_Start_Index := Syllable_Start_Index + 2;
      end loop;

      return Name;
   end Random_Voice_Name;

   procedure Parse_Voice (Data : in Voice_Data_Type; Voice : out Voice_Type) is
      Ops              : Operator_Array;
      Op_Start, Op_End : Natural;
      Offset           : Natural;
      LFO              : LFO_Type;
   begin
      Op_Start := 0;
      for I in reverse Operator_Index loop
         Op_End := Op_Start + Operator_Data_Length - 1;
         Parse_Operator (Data (Op_Start .. Op_End), Ops (I));
         Op_Start := Op_Start + Operator_Data_Length;
      end loop;
      Offset := Offset + Operator_Data_Length;

      Voice.Operators := Ops;

      Parse_Envelope
        (Data (Offset .. Offset + Envelope_Data_Length - 1), Voice.Pitch_Envelope);
      Offset := Offset + Envelope_Data_Length;

      Voice.Algorithm       := Algorithm_Type (Data (Offset) + 1);
      Offset                := Offset + 1;
      Voice.Feedback        := Depth_Type (Data (Offset));
      Offset                := Offset + 1;
      Voice.Oscillator_Sync := (Data (Offset) = 1);
      Offset                := Offset + 1;

      Parse_LFO (Data (Offset .. Offset + LFO_Data_Length), LFO);
      Offset := Offset + LFO_Data_Length;

      Voice.Pitch_Modulation_Sensitivity := Depth_Type (Data (Offset));
      Offset := Offset + 1;

      -- Transpose is 0...48 in the SysEx spec. 0 = -2 octaves, 48 = +2 octaves
      declare
         Semitones : constant Integer :=
           Integer (Data (Offset)) - 24; -- bring into range -24...24
      begin
         Voice.Transpose := Transpose_Type (Semitones / 12);
      end;

      for I in 1 .. Voice_Name_Length loop
         Voice.Name (I) := Character'Val (Data (Offset + I));
         Offset         := Offset + 1;
      end loop;
   end Parse_Voice;

   procedure Parse_LFO (Data : in LFO_Data_Type; LFO : out LFO_Type) is
   begin
      LFO :=
        (Speed => Level_Type (Data (1)), Delay_Time => Level_Type (Data (2)),
         Pitch_Modulation_Depth  => Level_Type (Data (3)),
         Amplitude_Modulation_Depth => Level_Type (Data (4)),
         Key_Sync  => (Data (5) = 1),
         Waveform  => LFO_Waveform_Type'Val (Data (6)));
   end Parse_LFO;

end DX7.Voices;
