with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with System;

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
      return
        (Byte (LFO.Speed), Byte (LFO.Delay_Time), Byte (LFO.Pitch_Modulation_Depth),
         Byte (LFO.Amplitude_Modulation_Depth), (if LFO.Key_Sync then 1 else 0),
         -- Convert enum value to Byte (first enum is pos zero)
         Byte (LFO_Waveform_Type'Pos (LFO.Wave)),
         Byte (LFO.Amplitude_Modulation_Depth));
   end Get_Data;

   -- Gets the LFO data as packed bytes for MIDI System Exclusive.
   -- The packed format is used when voice data is inside cartridge data.
   function Get_Packed_Data (LFO : LFO_Type) return LFO_Packed_Data_Type is
   begin
      return
        (Byte (LFO.Speed), Byte (LFO.Delay_Time), Byte (LFO.Pitch_Modulation_Depth),
         Byte (LFO.Amplitude_Modulation_Depth),

         -- Waveform type starts at bit 1
         Byte (if LFO.Key_Sync then 1 else 0) or
         Shift_Left (Byte (LFO_Waveform_Type'Pos (LFO.Wave)), 1));
      -- TODO: How to set a bit range?
      -- b116.replaceBits(1...3, with: Byte(LFO.Wave))
   end Get_Packed_Data;

   -- Gets the voice data bytes for MIDI System Exclusive.
   -- The normal format is used for individual voices.
   function Get_Data (Voice : Voice_Type) return Voice_Data_Type is
      Ch     : Character;
      Data   : Voice_Data_Type;
      Offset : Natural;
   begin
      Offset := 0;
      -- Note: the operators appear in reverse order: OP6, OP5 etc.
      for Op in reverse Operator_Index loop
         for B of Get_Data (Voice.Operators (Op)) loop
            Data (Offset) := B;
            Offset        := Offset + 1;
         end loop;
      end loop;

      for B of Get_Data (Voice.Pitch_Envelope) loop
         Data (Offset) := B;
         Offset        := Offset + 1;
      end loop;

      Data (Offset)     :=
        Byte (Voice.Algorithm - 1); -- adjust to 0...31 for SysEx
      Data (Offset + 1) := Byte (Voice.Feedback);
      Data (Offset + 2) := (if Voice.Oscillator_Sync = True then 1 else 0);

      Offset := 2;
      for B of Get_Data (Voice.LFO) loop
         Data (Offset) := B;
         Offset        := Offset + 1;
      end loop;

      -- Adjust -2..+2 to 0...48 for SysEx
      Data (Offset) := Byte ((Voice.Transpose + 2) * 12);
      Offset        := Offset + 1;

      for I in 1 .. Voice_Name_Length loop
         Ch            := Voice.Name (I);
         Data (Offset) := Character'Pos (Ch);
         Offset        := Offset + 1;
      end loop;

      return Data;
   end Get_Data;

   -- Gets the voice data bytes in packed format for MIDI System Exclusive.
   -- The packed format is used when voice data is embedded in cartridge data.
   function Get_Packed_Data (Voice : Voice_Type) return Voice_Packed_Data_Type
   is
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

      Ch       : Character;
      Byte111  : Byte111_Type;
      LFO_Data : LFO_Packed_Data_Type;
      Data     : Voice_Packed_Data_Type;
      Offset   : Natural;

      function Byte111_Type_To_Byte is new Ada.Unchecked_Conversion
        (Byte111_Type, Byte);

   begin
      Offset := 0;
      -- Note: the operators appear in reverse order: OP6, OP5 etc.
      for Op in reverse Operator_Index loop
         for B of Get_Packed_Data (Voice.Operators (Op)) loop
            Data (Offset) := B;
            Offset        := Offset + 1;
         end loop;
      end loop;

      for B of Get_Data (Voice.Pitch_Envelope) loop
         Data (Offset) := B;
         Offset        := Offset + 1;
      end loop;

      Data (Offset) :=
        Byte (Voice.Algorithm - 1); -- adjust to 0...31 for SysEx
      Offset        := Offset + 1;

      Byte111       :=
        (Feedback => Voice.Feedback, Oscillator_Sync => Voice.Oscillator_Sync);
      Data (Offset) := Byte111_Type_To_Byte (Byte111);
      Offset        := Offset + 1;

      LFO_Data := Get_Packed_Data (Voice.LFO);

      -- Adjust -2..+2 to 0...48 for SysEx
      Data (Offset) := Byte ((Voice.Transpose + 2) * 12);
      Offset        := Offset + 1;

      for I in 1 .. Voice_Name_Length loop
         Ch            := Voice.Name (I);
         Data (Offset) := Character'Pos (Ch);
         Offset        := Offset + 1;
      end loop;

      return Data;
   end Get_Packed_Data;

   function Random_LFO return LFO_Type is
      LFO       : LFO_Type;
      Level_Gen : Rand_Level.Generator;
      Wave_Gen  : Rand_Wave.Generator;
      Sync_Gen  : Rand_Sync.Generator;
   begin
      Rand_Level.Reset (Level_Gen);
      Rand_Wave.Reset (Wave_Gen);
      Rand_Sync.Reset (Sync_Gen);

      LFO.Speed     := Rand_Level.Random (Level_Gen);
      LFO.Delay_Time := Rand_Level.Random (Level_Gen);
      LFO.Pitch_Modulation_Depth       := Rand_Level.Random (Level_Gen);
      LFO.Amplitude_Modulation_Depth       := Rand_Level.Random (Level_Gen);
      LFO.Key_Sync      := Rand_Sync.Random (Sync_Gen);
      LFO.Wave      := Rand_Wave.Random (Wave_Gen);

      return LFO;
   end Random_LFO;

   function Random_Voice return Voice_Type is
      Voice     : Voice_Type;
      Alg_Gen   : Rand_Alg.Generator;
      Depth_Gen : Rand_Depth.Generator;
      Sync_Gen  : Rand_Sync.Generator;
   begin
      Rand_Alg.Reset (Alg_Gen);
      Rand_Depth.Reset (Depth_Gen);
      Rand_Sync.Reset (Sync_Gen);

      --Operators : Operator_Array;
      Voice.Pitch_Envelope := Random_Envelope;

      Voice.Algorithm       := Rand_Alg.Random (Alg_Gen);
      Voice.Feedback        := Rand_Depth.Random (Depth_Gen);
      Voice.Oscillator_Sync := Rand_Sync.Random (Sync_Gen);
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

      package Rand_Consonant is new Ada.Numerics.Discrete_Random (Consonant);
      package Rand_Vowel is new Ada.Numerics.Discrete_Random (Vowel);

      Vowel_Gen     : Rand_Vowel.Generator;
      Consonant_Gen : Rand_Consonant.Generator;

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
         Rand_Vowel.Reset (Vowel_Gen);
         Rand_Consonant.Reset (Consonant_Gen);

         -- Get random consonant and vowel as upper case characters.
         Syllable (1) :=
           Ada.Characters.Handling.To_Upper
             (To_Char (Rand_Consonant.Random (Consonant_Gen)));
         Syllable (2) :=
           Ada.Characters.Handling.To_Upper
             (To_Char (Rand_Vowel.Random (Vowel_Gen)));

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

   procedure Parse (Data : in Voice_Data_Type; Voice : out Voice_Type) is
      Ops              : Operator_Array;
      Op_Start, Op_End : Natural;
      Offset           : Natural;
      LFO              : LFO_Type;
   begin
      Op_Start := 0;
      for I in reverse Operator_Index loop
         Op_End := Op_Start + Operator_Data_Length;
         Parse (Data (Op_Start .. Op_End), Ops (I));
         Op_Start := Op_Start + Operator_Data_Length;
      end loop;

      Voice.Operators := Ops;

      Offset := Op_End + 1;
      Parse
        (Data (Offset .. Offset + Envelope_Data_Length), Voice.Pitch_Envelope);
      Offset := Offset + Envelope_Data_Length;

      Voice.Algorithm       := Algorithm_Type (Data (Offset) + 1);
      Offset                := Offset + 1;
      Voice.Feedback        := Depth_Type (Data (Offset));
      Offset                := Offset + 1;
      Voice.Oscillator_Sync := (Data (Offset) = 1);
      Offset                := Offset + 1;

      Parse (Data (Offset .. Offset + LFO_Data_Length), LFO);
      Offset := Offset + LFO_Data_Length;

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
   end Parse;

   procedure Parse (Data : in LFO_Data_Type; LFO : out LFO_Type) is
   begin
      LFO :=
        (Speed => Level_Type (Data (1)), Delay_Time => Level_Type (Data (2)),
         Pitch_Modulation_Depth  => Level_Type (Data (3)),
         Amplitude_Modulation_Depth => Level_Type (Data (4)),
         Key_Sync  => (Data (5) = 1),
         Wave  => LFO_Waveform_Type'Val (Data (6)),
         Pitch_Modulation_Sensitivity => Depth_Type (Data (7)));
   end Parse;

end DX7.Voices;
