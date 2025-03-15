with Interfaces; use Interfaces;

package body DX7.Operators is

   procedure Emit
     (KLS : in Keyboard_Level_Scaling_Type; Data : Keyboard_Level_Scaling_Data_Type) is
   begin
      Data := (Get_Data (KLS.Breakpoint), Byte (KLS.Left_Depth),
         Byte (KLS.Right_Depth),
         (case KLS.Left_Curve.Style is
            when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Left_Curve.Positive then 2 else 1)),
         (case KLS.Right_Curve.Style is
            when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Right_Curve.Positive then 2 else 1)));
   end Emit;

   function Get_Packed_Data
     (KLS : Keyboard_Level_Scaling_Type)
      return Keyboard_Level_Scaling_Packed_Data_Type
   is
      Data : Keyboard_Level_Scaling_Packed_Data_Type;
   begin
      Data (1) := Byte (KLS.Breakpoint);
      Data (2) := Byte (KLS.Left_Depth);
      Data (3) := Byte (KLS.Right_Depth);

      declare
         LeftSC, RightSC, SC : Byte;
      begin
         LeftSC :=
           (case KLS.Left_Curve.Style is
              when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
              when Exponential => (if KLS.Left_Curve.Positive then 2 else 1));

         RightSC :=
           (case KLS.Right_Curve.Style is
              when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
              when Exponential => (if KLS.Right_Curve.Positive then 2 else 1));

         SC := LeftSC or (Shift_Left (RightSC, 2));

         Data (4) := SC;
      end;

      return Data;
   end Get_Packed_Data;

   procedure Emit (Operator : in Operator_Type; Data : out Operator_Data_Type) is
      Offset : Positive;
   begin
      Offset := 1;

      declare
         EG_Data : Data (1 .. Envelope_Data_Length);
         KLS_Data :Data (1 .. Keyboard_Level_Scaling_Data_Length);
      begin
         Emit (Operator.EG, EG_Data);
         Data (Offset .. Offset + Envelope_Data_Length) := EG_Data;
         Offset        := Offset + EG_Data'Length;

         Emit (Operator.Keyboard_Level_Scaling, KLS_Data);
         Data (Offset .. Offset + KLS_Data'Length);
         Offset := Offset + KLS_Data'Length;
      end;

      Data (Offset)     := Byte (Operator.Keyboard_Rate_Scaling);
      Data (Offset + 1) := Byte (Operator.Amplitude_Modulation_Sensitivity);
      Data (Offset + 2) := Byte (Operator.Touch_Sensitivity);
      Data (Offset + 3) := Byte (Operator.Output_Level);
      Data (Offset + 4) := Byte (Operator_Mode'Pos (Operator.Mode));
      Data (Offset + 5) := Byte (Operator.Coarse);
      Data (Offset + 6) := Byte (Operator.Fine);
      Data (Offset + 7) :=
        Byte (Operator.Detune + 7); -- adjust to 0...14 for SysEx
   end Get_Data;

   function Get_Packed_Data
     (Operator : Operator_Type) return Operator_Packed_Data_Type
   is
      Detune_Byte : Byte;
      Byte12      : Byte;
      Byte13      : Byte;
      Byte15      : Byte;
      Data        : Operator_Packed_Data_Type;
      Offset      : Positive;
   begin
      -- Normal and packed EG data are the same
      Offset := 1;
      for EG_Byte of Get_Data (Operator.EG) loop
         Data (Offset) := EG_Byte;
         Offset        := Offset + 1;
      end loop;

      -- Using packed data for keyboard level scaling
      for KLS_Byte of Get_Packed_Data (Operator.Keyboard_Level_Scaling) loop
         Data (Offset) := KLS_Byte;
         Offset        := Offset + 1;
      end loop;

      Detune_Byte := Byte (Operator.Detune + 7); -- adjust to 0...14 for SysEx

      Byte12        :=
        Byte (Operator.Keyboard_Rate_Scaling) or Shift_Left (Detune_Byte, 3);
      Data (Offset) := Byte12;

      Byte13            :=
        Byte (Operator.Amplitude_Modulation_Sensitivity) or
        Shift_Left (Byte (Operator.Touch_Sensitivity), 2);
      Data (Offset + 1) := Byte13;

      Data (Offset + 2) := Byte (Operator.Output_Level);

      Byte15            :=
        Byte (Operator_Mode'Pos (Operator.Mode)) or
        Shift_Left (Byte (Operator.Coarse), 1);
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

   procedure Parse
     (Data : in     Keyboard_Level_Scaling_Data_Type;
      KLS  :    out Keyboard_Level_Scaling_Type)
   is
      procedure Parse (Data : Byte; Curve : out Scaling_Curve_Type) is
         C : Scaling_Curve_Type;
      begin
         -- Curve = 0=-LIN, 1=-EXP, 2=+EXP, 3=+LIN
         case Data is
            when 0 => C := Linear_Negative_Curve;
            when 1 => C := Exponential_Negative_Curve;
            when 2 => C := Exponential_Positive_Curve;
            when 3 => C := Linear_Positive_Curve;
            when others => raise Parse_Error;
         end case;
         Curve := C;
      end Parse;

      Left_Curve : Scaling_Curve_Type;
      Right_Curve : Scaling_Curve_Type;
   begin
      Parse (Data (3), Left_Curve);
      Parse (Data (4), Right_Curve);

      KLS :=
        (Breakpoint  => MIDI_Note_Type (Data (0)),
         Left_Depth  => Scaling_Depth_Type (Data (1)),
         Right_Depth => Scaling_Depth_Type (Data (2)),
         Left_Curve  => Left_Curve,
         Right_Curve => Right_Curve);
   end Parse;

   procedure Parse
     (Data : in     Keyboard_Level_Scaling_Packed_Data_Type;
      KLS  :    out Keyboard_Level_Scaling_Type)
   is
   begin
      null;
   end Parse;

   procedure Parse
     (Data         : in     Operator_Data_Type; Op : out Operator_Type)
   is
      EG  : Envelope_Type;
      KLS : Keyboard_Level_Scaling_Type;
      Mode : Operator_Mode;
   begin
      Parse (Data (0 .. 7), EG);
      Parse (Data (8 .. 12), KLS);

      if Data (17) = 0 then
         Mode := Fixed;
      else
         Mode := Ratio;
      end if;

      Op :=
        (EG                            => EG, Keyboard_Level_Scaling => KLS,
         Keyboard_Rate_Scaling         => Scaling_Depth_Type (Data (13)),
         Amplitude_Modulation_Sensitivity => Amplitude_Modulation_Sensitivity_Type (Data (14)),
         Touch_Sensitivity => Depth_Type (Data (15)), Output_Level => Level_Type (Data (16)),
         Mode => Mode, Coarse => Coarse_Type (Data (18)), Fine => Fine_Type (Data (19)),
         Detune                        => Detune_Type (Data (20)));
   end Parse;

   procedure Unpack (Data : in Operator_Packed_Data_Type; Result : out Operator_Data_Type) is
   begin
      -- Operator EG rates and levels are unpacked, so just copy them as is.
      -- KLS breakpoint, left depth and right depth are also unpacked.
      Result (0 .. 10) := Data (0 .. 10);

      -- KLS left and right curve are both in byte #11.
      -- Left curve is in bits 0..1, right curve is in bits 2...3.
      Result (11) := Data (11) and 2#00000011#;
      Result (12) := Data (11) and 2#00001100#;

      -- Operator detune and rate scaling are both in byte #12.
      -- Detune is in bits 3...6, RS is in bits 0...2.
      Result (13) := Data (12) and 2#00000111#; -- RS
      Result (20) := Data (12) and 2#01111000#; -- detune

      -- Key Vel Sens and Amp Mod Sens are both in byte #13.
      -- KVS is in bits 2...4, AMS is in bits 0...1.
      Result (15) := Data (13) and 2#00011100#;
      Result (14) := Data (13) and 2#00000011#;

      -- Operator output level is not packed with anything else.
      Result (16) := Data (14);

      -- Osc mode (ratio/fixed) is in bit0 of byte #15.
      Result (17) := Data (15) and 2#00000001#;

      -- Freq coarse is in bits 1...5 of byte #15.
      Result (18) := Data (15) and 2#001111110#;

      -- Freq fine is not packed with anything else.
      Result (19) := Data (16);
   end Unpack;

end DX7.Operators;
