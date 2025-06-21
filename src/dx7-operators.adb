with Ada.Text_IO;

package body DX7.Operators is
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

   procedure Parse_Scaling (Data : in Byte_Array; Result : out Keyboard_Level_Scaling_Type) is
      procedure Parse_Curve (Data : Byte; Curve : out Scaling_Curve_Type) is
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
      Value : Integer;
   begin
      if Data'Length /= Keyboard_Level_Scaling_Data_Length then
         raise Parse_Error
            with Make_Length_Exception_Message (Text => "KLS data length mismatch", 
               Actual => Data'Length, Expected => Keyboard_Level_Scaling_Data_Length, Offset => 1);
      end if;

      if Debugging then
         Ada.Text_IO.Put_Line ("KLS Data: First = " & Data'First'Image & " Last = " & Data'Last'Image);
         Ada.Text_IO.Put ("KLS data: ");
         Ada.Text_IO.Put_Line (Hex_Dump (Data));
      end if;

      Parse_Curve (Data (4), Left_Curve);
      Parse_Curve (Data (5), Right_Curve);

      Value := Integer (Data (1) + 21);
      if Value in Breakpoint_Type then
         Result.Breakpoint := Breakpoint_Type (Value); -- from 0 ... 99
      else
         raise Parse_Error;
      end if;

      Result.Left := (Depth => Scaling_Depth_Type (Data (2)), Curve => Left_Curve);
      Result.Right := (Depth => Scaling_Depth_Type (Data (3)), Curve => Right_Curve);
   end Parse_Scaling;

   procedure Parse_Operator (Data : in Byte_Array; Result : out Operator_Type) is
      Value : Integer;
      EG_Data : Byte_Array (1 .. Envelope_Data_Length);
      KLS_Data : Byte_Array (1 .. Keyboard_Level_Scaling_Data_Length);
   begin
      if Data'Length /= Operator_Data_Length then
         raise Parse_Error
            with Make_Length_Exception_Message (Text => "Operator data length mismatch", 
               Actual => Data'Length, Expected => Operator_Data_Length, Offset => 0);
      end if;

      EG_Data := Data (1 .. 8);
      Parse (EG_Data, Result.EG);

      KLS_Data := Data (9 .. 13);
      Parse_Scaling (KLS_Data, Result.Keyboard_Level_Scaling);

      Value := Integer (Data (14));
      if Value in Scaling_Depth_Type then
         Result.Keyboard_Rate_Scaling := Scaling_Depth_Type (Value);
      else
         raise Parse_Error;
      end if;

      Value := Integer (Data (15));
      if Value in Amplitude_Modulation_Sensitivity_Type then
         Result.Amplitude_Modulation_Sensitivity := Amplitude_Modulation_Sensitivity_Type (Value);
      else
         raise Parse_Error;
      end if;

      Value := Integer (Data (16));
      if Value in Depth_Type then
         Result.Touch_Sensitivity := Depth_Type (Value);
      else
         raise Parse_Error;
      end if;

      Value := Integer (Data (17));
      if Value in Level_Type then
         Result.Output_Level := Level_Type (Value);
      else
         raise Parse_Error;
      end if;

      Value := Integer (Data (18));
      if Value = 1 then
         Result.Mode := Ratio;
      elsif Value = 0 then
         Result.Mode := Fixed;
      else
         raise Parse_Error;
      end if;

      Value := Integer (Data (19));
      if Value in Coarse_Type then
         Result.Coarse := Coarse_Type (Value);
      else
         raise Parse_Error;
      end if;

      Value := Integer (Data (20));
      if Value in Fine_Type then
         Result.Fine := Fine_Type (Value);
      else
         raise Parse_Error;
      end if;

      Value := Integer (Data (21)) - 7;
      if Value in Detune_Type then
         Result.Detune := Detune_Type (Value);
      else
         raise Parse_Error
            with Make_Range_Exception_Message (Text => "Error parsing detune", 
                  Actual => Value, First => Detune_Type'First, Last => Detune_Type'Last, 
                  Offset => 21);

      end if;
   end Parse_Operator;

   procedure Unpack_Operator (Data : in Byte_Array; Result : out Byte_Array) is
   begin
      -- Operator EG rates and levels are unpacked, so just copy them as is (0..7 / 1..8)
      -- KLS breakpoint, left depth and right depth are also unpacked (8..10 / 9..11)
      Result (1 .. 11) := Data (1 .. 11);

      -- KLS left and right curve are both in byte #11 (12).
      -- Left curve is in bits 0..1, right curve is in bits 2...3.
      Result (12) := Data (12) and 2#00000011#;
      Result (13) := Shift_Right (Data (12) and 2#00001100#, 2);

      -- Operator detune and rate scaling are both in byte #12 (13).
      -- RS goes to 13/14, detune goes to 20/21.
      -- Detune is in bits 3..6, RS is in bits 0..2.
      Result (14) := Data (13) and 2#00000111#; -- RS
      Result (21) := Shift_Right (Data (13) and 2#01111000#, 3); -- detune (0...14)

      -- Key Vel Sens and Amp Mod Sens are both in byte #13 (14).
      -- KVS is in bits 2..4, AMS is in bits 0..1.
      -- KVS goes to 15/16, AMS goes to 14/15.
      Result (16) := Shift_Right (Data (14) and 2#00011100#, 2);
      Result (15) := Data (14) and 2#00000011#;

      -- Operator output level (14/15) is not packed with anything else.
      Result (17) := Data (15);

      -- Osc mode (ratio/fixed) is in bit0 of byte #15/16.
      -- It goes to byte 17/18.
      Result (18) := Data (16) and 2#00000001#;

      -- Freq coarse is in bits 1...5 of byte #15/16.
      -- It goes to byte 18/19.
      Result (19) := Shift_Right (Data (16) and 2#00111110#, 1);

      -- Freq fine (in 16/17) is not packed with anything else.
      -- It goes to byte 19/20.
      Result (20) := Data (17);
   end Unpack_Operator;

end DX7.Operators;
