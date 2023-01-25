with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with Helpers; use Helpers;


package body DX7 is


    function Get_Data (Operator : Operator_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        BV.Append(Get_Data(Operator.EG));
        BV.Append(Get_Data(Operator.Kbd_Level_Scaling));
        BV.Append(Byte(Operator.Kbd_Rate_Scaling));
        BV.Append(Byte(Operator.AMS));
        BV.Append(Byte(Operator.Key_Vel_Sens));
        BV.Append(Byte(Operator.Output_Level));
        BV.Append(Byte(Operator_Mode'Pos(Operator.Mode)));
        BV.Append(Byte(Operator.Coarse));
        BV.Append(Byte(Operator.Fine));
        BV.Append(Byte(Operator.Detune + 7)); -- adjust to 0...14 for SysEx
        return BV;
    end Get_Data;

    function Get_Packed_Data (Operator : Operator_Type) return Byte_Vector is
        BV : Byte_Vector;
        Detune_Byte: Byte;
        Byte12: Byte;
        Byte13: Byte;
        Byte15: Byte;
    begin
        BV.Append(Get_Data(Operator.EG)); -- normal and packed are the same
        BV.Append(Get_Packed_Data(Operator.Kbd_Level_Scaling));

        Detune_Byte := Byte(Operator.Detune + 7); -- adjust to 0...14 for SysEx
        BV.Append(Detune_Byte);

        Byte12 := Byte(Operator.Kbd_Rate_Scaling) or Shift_Left(Detune_Byte, 3);
        BV.Append(Byte12);

        Byte13 := Byte(Operator.AMS) or Shift_Left(Byte(Operator.Key_Vel_Sens), 2);
        BV.Append(Byte13);

        BV.Append(Byte(Operator.Output_Level));

        Byte15 := Byte(Operator_Mode'Pos(Operator.Mode)) or Shift_Left(Byte(Operator.Coarse), 1);
        BV.Append(Byte15);

        BV.Append(Byte(Operator.Fine));

        return BV;
    end Get_Packed_Data;

    function Get_Data (Voice : Voice_Type) return Byte_Vector is
        Ch: Character;
        BV : Byte_Vector;
    begin
        for op in Operator_Index loop
            BV.Append (Get_Data (Voice.Operators (op)));
        end loop;

        BV.Append(Get_Data(Voice.Pitch_Envelope));
        BV.Append(Byte(Voice.Algorithm));
        BV.Append(Byte(Voice.Feedback));
        BV.Append(Byte(if Voice.Osc_Sync = True then 1 else 0));
        BV.Append(Get_Data(Voice.LFO));
        BV.Append(Byte((Voice.Transpose + 2) * 12));  -- adjust -2..+2 to 0...48 for SysEx

        for i in 1 .. Voice_Name_Length loop
            Ch := Voice.Name(i);
            BV.Append(Character'Pos(Ch)); -- this seems to work OK
        end loop;

        return BV;
    end Get_Data;

    function Get_Packed_Data (Voice : Voice_Type) return Byte_Vector is
        Ch: Character;
        BV : Byte_Vector;
        Byte111: Byte;
    begin
        for op in Operator_Index loop
            BV.Append (Get_Packed_Data (Voice.Operators (op)));
        end loop;

        BV.Append(Get_Data(Voice.Pitch_Envelope));
        BV.Append(Byte(Voice.Algorithm));

        Byte111 := Byte(Voice.Feedback) or Shift_Left(Byte(if Voice.Osc_Sync = True then 1 else 0), 3);
        BV.Append(Byte111);

        BV.Append(Get_Packed_Data(Voice.LFO));

        BV.Append(Byte((Voice.Transpose + 2) * 12));  -- adjust -2..+2 to 0...48 for SysEx

        for i in 1 .. Voice_Name_Length loop
            Ch := Voice.Name(i);
            BV.Append(Character'Pos(Ch)); -- this seems to work OK
        end loop;

        return BV;
    end Get_Packed_Data;

    function Get_Data (Cartridge : Cartridge_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        for i in Voice_Index loop
            BV.Append(Get_Packed_Data(Cartridge.Voices(i)));
        end loop;
        -- TODO: Add checksum
        return BV;
    end Get_Data;

    function Get_Data (Manufacturer : Manufacturer_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        case Manufacturer.Kind is
            when Development_Kind =>
                BV.Append (Helpers.Byte (16#7D#));
            when Standard_Kind =>
                BV.Append (Manufacturer.Standard_Identifier);
            when Extended_Kind =>
                BV.Append (Manufacturer.Extended_Identifier (1));
                BV.Append (Manufacturer.Extended_Identifier (2));
                BV.Append (Manufacturer.Extended_Identifier (3));
        end case;
        return BV;
    end Get_Data;

    function Get_Data (Message : Message_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        BV.Append (Byte (16#F0#));
        BV.Append (Get_Data (Message.Manufacturer));
        BV.Append (Message.Payload);
        BV.Append (Byte (16#F7#));
        return BV;
    end Get_Data;

    function Get_Data (LFO : LFO_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        BV.Append(Byte(LFO.Speed));
        BV.Append(Byte(LFO.LFO_Delay));
        BV.Append(Byte(LFO.PMD));
        BV.Append(Byte(LFO.AMD));
        BV.Append(Byte(if LFO.Sync = True then 1 else 0));
        BV.Append(Byte(LFO_Waveform_Type'Pos(LFO.Wave)));  -- convert enum value to Byte (first enum is pos zero)
        BV.Append(Byte(LFO.Pitch_Mod_Sens));
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

        -- TODO: How to set a bit range?

        BV.Append(Byte116);
        return BV;
    end Get_Packed_Data;

    function Get_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector is
        BV : Byte_Vector;
        LeftSC : Byte;
        RightSC : Byte;
    begin
        BV.Append(Byte(KLS.Breakpoint));
        BV.Append(Byte(KLS.Left_Depth));
        BV.Append(Byte(KLS.Right_Depth));

        LeftSC := (case KLS.Left_Curve.Curve is
            when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Left_Curve.Positive then 2 else 1));

        RightSC := (case KLS.Right_Curve.Curve is
            when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Right_Curve.Positive then 2 else 1));

        BV.Append(Byte(LeftSC));
        BV.Append(Byte(RightSC));

        return BV;
    end Get_Data;

    function Get_Packed_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector is
        BV : Byte_Vector;
        LeftSC : Byte;
        RightSC : Byte;
        SC : Byte;
    begin
        BV.Append(Byte(KLS.Breakpoint));
        BV.Append(Byte(KLS.Left_Depth));
        BV.Append(Byte(KLS.Right_Depth));

        LeftSC := (case KLS.Left_Curve.Curve is
            when Linear => (if KLS.Left_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Left_Curve.Positive then 2 else 1));

        RightSC := (case KLS.Right_Curve.Curve is
            when Linear => (if KLS.Right_Curve.Positive then 3 else 0),
            when Exponential => (if KLS.Right_Curve.Positive then 2 else 1));

        -- Byte is a modular type (see Helpers), so bitwise operators are defined
        SC := Byte(LeftSC) or (Shift_Left(Byte(RightSC), 2));  -- Shift_Left is from the Interfaces package

        BV.Append(SC);

        return BV;
    end Get_Packed_Data;

end DX7;
