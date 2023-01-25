with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Helpers; use Helpers;
with DX7.Envelopes; use DX7.Envelopes;

package DX7 is
    -- The definitions are based on the example set by AdaCore's "Introduction to Ada",
    -- section "Strongly typed language":
    -- https://learn.adacore.com/courses/intro-to-ada/chapters/strongly_typed_language.html#integers

    type Coarse_Type is range 0 .. 31;
    type Detune_Type is range -7 .. 7;

    type Algorithm_Type is range 1 .. 32;
    type Depth_Type is range 0 .. 7;

    type Curve_Style_Type is (Linear, Exponential);

    type Scaling_Curve_Type is record
        Curve : Curve_Style_Type := Linear;
        Positive : Boolean := False;
    end record;

    Lin_Neg_Curve : constant Scaling_Curve_Type := (Curve => Linear, Positive => False);
    Lin_Pos_Curve : constant Scaling_Curve_Type := (Curve => Linear, Positive => True);
    Exp_Neg_Curve : constant Scaling_Curve_Type := (Curve => Exponential, Positive => False);
    Exp_Pos_Curve : constant Scaling_Curve_Type := (Curve => Exponential, Positive => True);

    type MIDI_Note_Type is range 0 .. 127;
    type MIDI_Channel_Type is range 1 .. 16;

    type Keyboard_Level_Scaling_Type is record
        Breakpoint : MIDI_Note_Type;
        Left_Depth : Depth_Type;
        Right_Depth : Depth_Type;
        Left_Curve : Scaling_Curve_Type;
        Right_Curve : Scaling_Curve_Type;
    end record;

    type Operator_Mode is (Ratio, Fixed);

    type AMS_Type is range 0 .. 3;

    type Operator_Type is record
        EG : Envelope_Type;
        Kbd_Level_Scaling : Keyboard_Level_Scaling_Type;
        Kbd_Rate_Scaling : Depth_Type;
        AMS : AMS_Type;
        Key_Vel_Sens : Depth_Type;
        Output_Level : Level_Type;
        Mode : Operator_Mode;
        Coarse : Coarse_Type;
        Fine : Level_Type;
        Detune : Detune_Type;
    end record;

    type Operator_Index is range 1 .. 6;
    type Operator_Array is array (Operator_Index) of Operator_Type;

    Voice_Name_Length : constant Integer := 10;
    subtype Voice_Name_Type is String (1 .. Voice_Name_Length);

    type Transpose_Type is range -2 .. 2;

    -- Enumeration type for LFO waveforms
    type LFO_Waveform_Type is (
        Triangle,
        SawDown,
        SawUp,
        Square,
        Sine,
        SampleAndHold
    );

    type LFO_Type is record
        Speed : Level_Type;
        LFO_Delay : Level_Type;
        PMD : Level_Type;
        AMD : Level_Type;
        Sync : Boolean;
        Wave : LFO_Waveform_Type;
        Pitch_Mod_Sens : Depth_Type;
    end record;

    type Voice_Type is record
        Operators : Operator_Array;
        Pitch_Envelope : Envelope_Type;
        Algorithm : Algorithm_Type;
        Feedback : Depth_Type;
        Osc_Sync : Boolean;
        LFO : LFO_Type;
        Transpose: Transpose_Type;
        Name : Voice_Name_Type;
    end record;

    type Voice_Index is range 1 .. 32;
    type Voice_Array is array (Voice_Index) of Voice_Type;

    type Cartridge_Type is record
        Voices : Voice_Array;
    end record;

    type Byte_Triplet is array (1 .. 3) of Byte;

    -- Use a variant record to describe the manufacturer
    -- in a MIDI System Exclusive Message.
    type Manufacturer_Kind is (Development_Kind, Standard_Kind, Extended_Kind);
    type Manufacturer_Type (Kind : Manufacturer_Kind := Development_Kind) is
        record
            case Kind is
                when Development_Kind => Development_Identifier : Byte;
                when Standard_Kind => Standard_Identifier : Byte;
                when Extended_Kind => Extended_Identifier : Byte_Triplet;
            end case;
        end record;

    type Message_Type is record
        Manufacturer : Manufacturer_Type;
        Payload : Byte_Vector;
    end record;

    -- Use overloading by argument to define Get_Data for each type as required
    function Get_Data (Operator : Operator_Type) return Byte_Vector;
    function Get_Packed_Data (Operator : Operator_Type) return Byte_Vector;
    function Get_Data (Voice : Voice_Type) return Byte_Vector;
    function Get_Packed_Data (Voice : Voice_Type) return Byte_Vector;
    function Get_Data (Cartridge : Cartridge_Type) return Byte_Vector;
    function Get_Data (Manufacturer : Manufacturer_Type) return Byte_Vector;
    function Get_Data (Message : Message_Type) return Byte_Vector;
    function Get_Data (LFO : LFO_Type) return Byte_Vector;
    function Get_Packed_Data (LFO : LFO_Type) return Byte_Vector;
    function Get_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector;
    function Get_Packed_Data (KLS: Keyboard_Level_Scaling_Type) return Byte_Vector;

end DX7;
