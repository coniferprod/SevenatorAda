with Ada.Text_IO;

package body DX7.Operators is
   Debugging : constant Boolean := False;

   -- Emits the bytes of KLS for SysEx (five bytes).
   procedure Emit
     (KLS : in Keyboard_Level_Scaling_Type; Data : out Keyboard_Level_Scaling_Data_Type) is
   begin
      Data := (Byte (KLS.Breakpoint - 21), Byte (KLS.Left.Depth),
         Byte (KLS.Right.Depth),
         (case KLS.Left.Curve.Style is
            when Linear => (if KLS.Left.Curve.Sign = Positive then 3 else 0),
            when Exponential => (if KLS.Left.Curve.Sign = Positive then 2 else 1)),
         (case KLS.Right.Curve.Style is
            when Linear => (if KLS.Right.Curve.Sign = Positive then 3 else 0),
            when Exponential => (if KLS.Right.Curve.Sign = Positive then 2 else 1)));
   end Emit;

   -- Packs the KLS data for use in a cartridge (from five bytes to four)
   procedure Pack_Scaling (Data : Keyboard_Level_Scaling_Data_Type; Result : out Packed_Keyboard_Level_Scaling_Data_Type) is
   begin
      -- The first three bytes are copied as is
      Result (1 .. 3) := Data (1 .. 3);

      -- The fourth byte combines the left and right curves
      -- 11    0   0   0 |  RC   |   LC  | SCL LEFT CURVE 0-3   SCL RGHT CURVE 0-3
      Result (4) := Data (4) or (Shift_Left (Data (5), 2));
   end Pack_Scaling;

   procedure Emit (Operator : in Operator_Type; Data : out Operator_Data_Type) is
      Offset : Natural;
      EG_Data : Envelope_Data_Type;
      KLS_Data : Keyboard_Level_Scaling_Data_Type;
   begin
      Offset := 1;

      Emit (Operator.EG, EG_Data);
      if Debugging then
         Ada.Text_IO.Put_Line ("EG data is "
            & Integer'Image (Envelope_Data_Type'First)
            & Integer'Image (Envelope_Data_Type'Last));
      end if;

      Data (Offset .. Offset + Envelope_Data_Length - 1) := EG_Data;
      Inc (Offset, Envelope_Data_Length);

      Emit (Operator.Keyboard_Level_Scaling, KLS_Data);
      Data (Offset .. Offset + Keyboard_Level_Scaling_Data_Length - 1) := KLS_Data;
      Inc (Offset, Keyboard_Level_Scaling_Data_Length);

      Data (Offset)     := Byte (Operator.Keyboard_Rate_Scaling);
      Data (Offset + 1) := Byte (Operator.Amplitude_Modulation_Sensitivity);
      Data (Offset + 2) := Byte (Operator.Touch_Sensitivity);
      Data (Offset + 3) := Byte (Operator.Output_Level);
      Data (Offset + 4) := Byte (Operator_Mode'Pos (Operator.Mode));
      Data (Offset + 5) := Byte (Operator.Coarse);
      Data (Offset + 6) := Byte (Operator.Fine);
      Data (Offset + 7) :=
        Byte (Operator.Detune + 7); -- adjust to 0...14 for SysEx
   end Emit;

   procedure Pack_Operator (Data : Operator_Data_Type; Result : out Packed_Operator_Data_Type) is
      Data_Offset : Integer := 1;
      Offset : Integer := 1;
   begin
      --  Copy the EG bytes as is, then KLS breakpoint, left and right depths
      Result (Offset .. Offset + 11) := Data (Data_Offset .. Data_Offset + 11);
      Inc (Offset, 11);
      Inc (Data_Offset, 11);

      --  Combine bytes 11 and 12 into one:
      Result (Offset) := Data (11) or Shift_Left (Data (12), 2);
      Inc (Offset);

      Result (Offset) := Data (13) or Shift_Left (Data (20), 3);
      Inc (Offset);

      Result (Offset) := Data (14) or Shift_Left (Data (15), 2);
      Inc (Offset);

      Result (Offset) := Data (16);
      Inc (Offset);

      -- coarse + mode
      Result (Offset) := Data (17) or Shift_Left (Data (18), 1);
      Inc (Offset);

      Result (Offset) := Data (19);  -- fine
   end Pack_Operator;

   function Get_Breakpoint (Data : Byte) return Breakpoint_Type is
   begin
      -- We get a byte 0 .. 99, which needs to be adjusted
      -- to the breakpoint MIDI note range 21 .. 120.
      return Breakpoint_Type (Data + 21);
   end Get_Breakpoint;

   procedure Parse
     (Data : in     Keyboard_Level_Scaling_Data_Type;
      KLS  :    out Keyboard_Level_Scaling_Type)
   is
      procedure Parse_Curve (Data : Byte; Curve : out Scaling_Curve_Type) is
         C : Scaling_Curve_Type;
      begin
         -- Curve = 0=-LIN, 1=-EXP, 2=+EXP, 3=+LIN
         Curve := (case Data is
            when 0 => Linear_Negative_Curve,
            when 1 => Exponential_Negative_Curve,
            when 2 => Exponential_Positive_Curve,
            when 3 => Linear_Positive_Curve,
            when others => raise Parse_Error);
      end Parse_Curve;

      Left_Curve : Scaling_Curve_Type;
      Right_Curve : Scaling_Curve_Type;
   begin
      Parse_Curve (Data (4), Left_Curve);
      Parse_Curve (Data (5), Right_Curve);

      KLS :=
        (Breakpoint  => Breakpoint_Type (Data (1) + 21),  -- from 0 ... 99
         Left => (Depth => Scaling_Depth_Type (Data (2)), Curve => Left_Curve),
         Right => (Depth => Scaling_Depth_Type (Data (3)), Curve => Right_Curve));
   end Parse;

   procedure Parse
     (Data         : in     Operator_Data_Type; Op : out Operator_Type)
   is
      EG  : Envelope_Type;
      KLS : Keyboard_Level_Scaling_Type;
      Mode : Operator_Mode;
      EG_Data : Envelope_Data_Type;
      KLS_Data : Keyboard_Level_Scaling_Data_Type;
   begin
      Ada.Text_IO.Put_Line ("Operator data (" & Integer'Image (Data'Length) & " bytes) = " & Hex_Dump (Data));

      EG_Data := Data (1 .. 8);
      Ada.Text_IO.Put_Line ("EG data = " & Hex_Dump (EG_Data));
      Parse (EG_Data, EG);

      KLS_Data := Data (9 .. 13);
      Ada.Text_IO.Put_Line ("KLS data = " & Hex_Dump (KLS_Data));
      Parse (KLS_Data, KLS);

      Op :=
        (EG                            => EG,
         Keyboard_Level_Scaling => KLS,
         Keyboard_Rate_Scaling         => Scaling_Depth_Type (Data (14)),
         Amplitude_Modulation_Sensitivity => Amplitude_Modulation_Sensitivity_Type (Data (15)),
         Touch_Sensitivity => Depth_Type (Data (16)),
         Output_Level => Level_Type (Data (17)),
         Mode => (if Data(18) = 0 then Fixed else Ratio),
         Coarse => Coarse_Type (Data (19)),
         Fine => Fine_Type (Data (20)),
         Detune => Detune_Type (Integer (Data (21)) - 7));
   end Parse;

   procedure Unpack_Operator (Data : in Packed_Operator_Data_Type; Result : out Operator_Data_Type) is
   begin
      Ada.Text_IO.Put_Line ("packed operator data = " & Hex_Dump (Data));

      -- Operator EG rates and levels are unpacked, so just copy them as is.
      -- KLS breakpoint, left depth and right depth are also unpacked.
      Result (1 .. 11) := Data (1 .. 11);

      -- KLS left and right curve are both in byte #11.
      -- Left curve is in bits 0..1, right curve is in bits 2...3.
      Result (12) := Data (12) and 2#00000011#;
      Result (13) := Shift_Right (Data (12) and 2#00001100#, 2);

      -- Operator detune and rate scaling are both in byte #12.
      -- Detune is in bits 3...6, RS is in bits 0...2.
      Result (14) := Data (13) and 2#00000111#; -- RS
      Result (21) := Shift_Right (Data (13) and 2#01111000#, 3); -- detune

      -- Key Vel Sens and Amp Mod Sens are both in byte #13.
      -- KVS is in bits 2...4, AMS is in bits 0...1.
      Result (16) := Shift_Right (Data (14) and 2#00011100#, 2);
      Result (15) := Data (14) and 2#00000011#;

      -- Operator output level is not packed with anything else.
      Result (17) := Data (15);

      -- Osc mode (ratio/fixed) is in bit0 of byte #15.
      Result (18) := Data (16) and 2#00000001#;

      -- Freq coarse is in bits 1...5 of byte #15.
      Result (19) := Shift_Right (Data (16) and 2#001111110#, 1);

      -- Freq fine is not packed with anything else.
      Result (20) := Data (17);

      Ada.Text_IO.Put_Line ("unpacked operator data = " & Hex_Dump (Result));
   end Unpack_Operator;

end DX7.Operators;
