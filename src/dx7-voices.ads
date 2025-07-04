with Sixten; use Sixten;

with DX7; use DX7;
with DX7.Envelopes; use DX7.Envelopes;
with DX7.Operators; use DX7.Operators;

package DX7.Voices is
   Voice_Data_Length : constant := 155;
   subtype Voice_Data_Type is Byte_Array (1 .. Voice_Data_Length);

   Packed_Voice_Data_Length : constant := 128;
   subtype Packed_Voice_Data_Type is Byte_Array (1 .. Packed_Voice_Data_Length);

   Voice_Name_Length : constant := 10;
   subtype Voice_Name_Type is String (1 .. Voice_Name_Length);

   -- Transpose is -24...+24 semitones (-2...+2 octaves)
   subtype Transpose_Type is Integer range -24 .. 24;

   -- Enumeration type for LFO waveforms
   type LFO_Waveform_Type is
     (Triangle, Saw_Down, Saw_Up, Square, Sine, Sample_And_Hold);

   type LFO_Type is record
      Speed     : Level_Type        := 35; --  LFS
      Delay_Time : Level_Type        := 0;  --  LFD
      Pitch_Modulation_Depth       : Level_Type        := 0;  --  LPMD
      Amplitude_Modulation_Depth       : Level_Type        := 0; -- LAMD
      Key_Sync      : Boolean           := True;  --  LFKS
      Waveform      : LFO_Waveform_Type := Triangle;  --  LFW
   end record;

   LFO_Data_Length : constant := 6;
   subtype LFO_Data_Type is Byte_Array (1 .. LFO_Data_Length);

   Init_LFO : constant LFO_Type := (others => <>);

   type Amplitude_Sensitivity_Type is
     array (Operator_Index) of Sensitivity_Type;

   type Modulation_Sensitivity_Type is record
      Pitch     : Depth_Type;
      Amplitude : Amplitude_Sensitivity_Type;
   end record;

   type Voice_Type is record
      Operators              : Operator_Array;
      Pitch_Envelope         : Envelope_Type := Init_Pitch_Envelope;
      Algorithm              : Algorithm_Type := 1;
      Feedback               : Depth_Type     := 0;
      Oscillator_Sync        : Boolean        := True;
      LFO                    : LFO_Type := Init_LFO;
      Pitch_Modulation_Sensitivity : Depth_Type := 0;
      Transpose              : Transpose_Type := 0;
      Name                   : Voice_Name_Type := "INIT VOICE";
   end record;

   type Voice_Index is range 1 .. 32;
   type Voice_Array is array (Voice_Index) of Voice_Type;

   procedure Emit (Voice : in Voice_Type; Result : out Voice_Data_Type);
   procedure Pack_Voice (Data : in Voice_Data_Type; Result : out Packed_Voice_Data_Type);

   -- Makes a voice with random parameters.
   function Random_Voice return Voice_Type;

   -- Makes a random voice name.
   function Random_Voice_Name return Voice_Name_Type;

   procedure Parse_Voice (Data : in Byte_Array; Result : out Voice_Type);
   procedure Unpack_Voice (Data : in Byte_Array; Result : out Byte_Array);

   --------------
   --  LFO
   --------------

   -- Makes an LFO with random parameters.
   function Random_LFO return LFO_Type;

   procedure Emit (LFO : in LFO_Type; Result : out LFO_Data_Type);
   procedure Parse_LFO (Data : in Byte_Array; Result : out LFO_Type);

   Init_Voice : constant Voice_Type :=
      (Operators => (Init_Operator, others => Silent_Init_Operator),
      others => <>);

   Brass1 : constant Voice_Type :=
     (Operators              =>
        (
         ( --OP1
      EG => (Rates => (72, 76, 99, 71), Levels => (99, 88, 96, 0)),
          Keyboard_Level_Scaling =>
            (Breakpoint  => 60,  -- Yamaha note C3
             Left => (Depth  => 0, Curve => Linear_Positive_Curve),
             Right => (Depth => 0, Curve => Linear_Positive_Curve)),
          Keyboard_Rate_Scaling  => 0,
          Amplitude_Modulation_Sensitivity => 0,
          Touch_Sensitivity => 0,
          Output_Level           => 98, Mode => Ratio, Coarse => 1, Fine => 0,
          Detune                 => 7
          ),
         ( --OP2
      EG => (Rates => (62, 51, 29, 71), Levels => (82, 95, 96, 0)),
          Keyboard_Level_Scaling =>
            (Breakpoint  => 48, -- Yamaha note C2
             Left => (Depth  => 0, Curve => Linear_Positive_Curve),
             Right => (Depth => 0, Curve => Exponential_Negative_Curve)),
          Keyboard_Rate_Scaling  => 0,
          Amplitude_Modulation_Sensitivity => 0,
          Touch_Sensitivity => 0,
          Output_Level           => 86, Mode => Ratio, Coarse => 0, Fine => 0,
          Detune                 => 7),
         ( --OP3
      EG => (Rates => (77, 76, 82, 71), Levels => (99, 98, 98, 0)),
          Keyboard_Level_Scaling =>
            (Breakpoint  => 48, -- Yamaha note C2
             Left => (Depth  => 0, Curve => Linear_Positive_Curve),
             Right => (Depth => 0, Curve => Exponential_Negative_Curve)),
          Keyboard_Rate_Scaling  => 0,
          Amplitude_Modulation_Sensitivity => 0,
          Touch_Sensitivity => 2,
          Output_Level           => 86, Mode => Ratio, Coarse => 0, Fine => 0,
          Detune                 => -2),
         ( --OP4
      EG => (Rates => (77, 36, 41, 71), Levels => (99, 98, 98, 0)),
          Keyboard_Level_Scaling =>
            (Breakpoint  => 48, -- Yamaha note C2
             Left => (Depth  => 0, Curve => Linear_Positive_Curve),
             Right => (Depth => 0, Curve => Exponential_Negative_Curve)),
          Keyboard_Rate_Scaling  => 0,
          Amplitude_Modulation_Sensitivity => 0,
          Touch_Sensitivity => 2,
          Output_Level           => 86, Mode => Ratio, Coarse => 0, Fine => 0,
          Detune                 => 1),
         ( --OP5
      EG => (Rates => (77, 36, 41, 71), Levels => (99, 98, 98, 0)),
          Keyboard_Level_Scaling =>
            (Breakpoint  => 48, -- Yamaha note C2
             Left => (Depth  => 0, Curve => Linear_Positive_Curve),
             Right => (Depth => 0, Curve => Exponential_Negative_Curve)),
          Keyboard_Rate_Scaling  => 0,
          Amplitude_Modulation_Sensitivity => 0,
          Touch_Sensitivity => 2,
          Output_Level           => 98, Mode => Ratio, Coarse => 0, Fine => 0,
          Detune                 => 1),
         ( --OP6
      EG => (Rates => (49, 99, 28, 68), Levels => (98, 98, 91, 0)),
          Keyboard_Level_Scaling =>
            (Breakpoint  => 60, -- Yamaha note C3
             Left => (Depth  => 54, Curve => Exponential_Negative_Curve),
             Right => (Depth => 50, Curve => Exponential_Negative_Curve)),
          Keyboard_Rate_Scaling  => 4,
          Amplitude_Modulation_Sensitivity => 0,
          Touch_Sensitivity => 2,
          Output_Level           => 98, Mode => Ratio, Coarse => 0, Fine => 0,
          Detune                 => 1)),
      Pitch_Envelope         =>
        (Rates => (84, 95, 95, 60), Levels => (50, 50, 50, 50)),
      Algorithm              => 22, Feedback => 7, Oscillator_Sync => True,
      LFO                    =>
        (Speed => 37, Delay_Time => 0, Pitch_Modulation_Depth => 5,
        Amplitude_Modulation_Depth => 0, Key_Sync => False,
         Waveform  => Sine),
         Pitch_Modulation_Sensitivity => 3,
      Transpose              => 0, Name => "BRASS   1 ");

end DX7.Voices;
