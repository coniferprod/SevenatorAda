with DX7.Envelopes; use DX7.Envelopes;

package DX7.Operators is

   type Curve_Style is (Linear, Exponential);
   type Curve_Sign is (Negative, Positive);

   type Scaling_Depth_Type is range 0 .. 99;

   type Scaling_Curve_Type is record
      Style    : Curve_Style := Linear;
      Sign : Curve_Sign := Negative;
   end record;

   type Scaling_Type is record
      Depth : Scaling_Depth_Type;
      Curve : Scaling_Curve_Type;
   end record;

   -- Curve = 0=-LIN, 1=-EXP, 2=+EXP, 3=+LIN

   Linear_Negative_Curve      : constant Scaling_Curve_Type :=
     (Style => Linear, Sign => Negative);
   Linear_Positive_Curve      : constant Scaling_Curve_Type :=
     (Style => Linear, Sign => Positive);
   Exponential_Negative_Curve : constant Scaling_Curve_Type :=
     (Style => Exponential, Sign => Negative);
   Exponential_Positive_Curve : constant Scaling_Curve_Type :=
     (Style => Exponential, Sign => Positive);

   -- Breakpoint is a key from A-1 to C8, with C3 = 0x27 (39) in SysEx.
   -- It can be expressed as a subtype of MIDI_Note_Type.
   -- In Yamaha notation, A-1 is 21, while C8 is 120.
   -- The SysEx value needs a conversion: +/- 21, which is +/- (60 - 39).
   -- The breakpoint value's data range is specified as 0 - 99.
   subtype Breakpoint_Type is MIDI_Note_Type range 21 .. 120;

   type Keyboard_Level_Scaling_Type is record
      Breakpoint  : Breakpoint_Type;
      Left : Scaling_Type;
      Right : Scaling_Type;
   end record;

   -- MIDI System Exclusive data for Keyboard Level Scaling (normal)
   Keyboard_Level_Scaling_Data_Length : constant := 5;
   subtype Keyboard_Level_Scaling_Data_Type is
      Byte_Array (1 .. Keyboard_Level_Scaling_Data_Length);

   -- MIDI System Exclusive data for Keyboard Level Scaling (packed)
   Packed_Keyboard_Level_Scaling_Data_Length : constant := 4;
   subtype Packed_Keyboard_Level_Scaling_Data_Type is
     Byte_Array (1 .. Packed_Keyboard_Level_Scaling_Data_Length);

   type Operator_Mode is (Ratio, Fixed);

   type Amplitude_Modulation_Sensitivity_Type is range 0 .. 3;

   Init_Keyboard_Level_Scaling : constant Keyboard_Level_Scaling_Type :=
      (Breakpoint => 39,
      Left => (Depth => 0, Curve => Linear_Negative_Curve),
      Right => (Depth => 0, Curve => Linear_Negative_Curve));

   -- Represents an operator with its parameters.
   type Operator_Type is record
      EG                            : Envelope_Type := Init_Envelope;
      Keyboard_Level_Scaling        : Keyboard_Level_Scaling_Type := Init_Keyboard_Level_Scaling;
      Keyboard_Rate_Scaling         : Scaling_Depth_Type := 0;
      Amplitude_Modulation_Sensitivity : Amplitude_Modulation_Sensitivity_Type := 0; -- AMS
      Touch_Sensitivity : Depth_Type := 0; -- TS
      Output_Level                  : Level_Type := 99;
      Mode                          : Operator_Mode := Ratio;
      Coarse                        : Coarse_Type := 1;
      Fine                          : Fine_Type := 0;
      Detune                        : Detune_Type := 0;
   end record;

   Init_Operator : constant Operator_Type := (others => <>);

   Silent_Init_Operator : constant Operator_Type := (Output_Level => 0, others => <>);

   -- The DX7 engine has six operators, OP1 ... OP6.
   -- They are arranged in a voice as an array.
   type Operator_Index is range 1 .. 6;
   type Operator_Array is array (Operator_Index) of Operator_Type;

   -- There are two variants of operator System Exclusive data.
   -- The normal version is used in individual voices, while the
   -- packed version is used in cartridges.

   Operator_Data_Length : constant := 21;
   subtype Operator_Data_Type is
      Byte_Array (1 .. Operator_Data_Length);

   Packed_Operator_Data_Length : constant := 17;
   subtype Packed_Operator_Data_Type is
      Byte_Array (1 .. Packed_Operator_Data_Length);

   -- Gets the data for the normal voice version of
   -- the operator for MIDI System Exclusive.
   procedure Emit (Operator : in Operator_Type; Data : out Operator_Data_Type);

   -- Gets the data for the packed cartridge version of
   -- the operator for MIDI System Exclusive.
   procedure Pack_Operator (Data : Operator_Data_Type; Result : out Packed_Operator_Data_Type);

   -- Gets the data for the normal voice version of
   -- the keyboard level scaling definition.
   procedure Emit
     (KLS : in Keyboard_Level_Scaling_Type; Data : out Keyboard_Level_Scaling_Data_Type);

   -- Gets the data for the packed cartridge version of
   -- the keyboard level scaling definition.
   procedure Pack_Scaling (Data : Keyboard_Level_Scaling_Data_Type; Result : out Packed_Keyboard_Level_Scaling_Data_Type);

   -- Converts a SysEx MIDI data byte to a breakpoint.
   function Get_Breakpoint (Data : Byte) return Breakpoint_Type;

   procedure Parse
     (Data : in     Keyboard_Level_Scaling_Data_Type;
      KLS  :    out Keyboard_Level_Scaling_Type);

   procedure Parse
     (Data         : in     Operator_Data_Type; Op : out Operator_Type);

   -- Unpacks the packed operator data into normal data for parsing.
   procedure Unpack_Operator (Data : in Packed_Operator_Data_Type; Result : out Operator_Data_Type);

end DX7.Operators;
