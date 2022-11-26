with Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;
with Helpers; use Helpers;
with DX7; use DX7;

procedure Main is
    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

    EG : Envelope_Type;
    KLS : Keyboard_Level_Scaling_Type;
    V : Voice_Type;
    Op1 : Operator_Type;
    Lfo : LFO_Type;
    Cartridge : Cartridge_Type;
    Manufacturer : Manufacturer_Type (Kind => Standard_Kind);
    Message : Message_Type;
    Payload : Byte_Vector;
    Channel : MIDI_Channel_Type := 1;
    Data : Byte_Vector;
    In_Data : Byte_Array_Access;
    Out_Data : Byte_Vector;

begin
    for i in 1 .. CLI.Argument_Count loop
        IO.Put_Line (Item => CLI.Argument (Number => i));
    end loop;

    In_Data := Read_File ("cartridge.bin");
    -- Do something with the data
    Put(In_Data'Length);

    for I in In_Data'Range loop
        Out_Data.Append (In_Data (I));
    end loop;

    Delete (In_Data);

    IO.Put_Line(Hex_Dump(Out_Data));

    IO.Put_Line("Generating new cartridge...");

    EG := New_Envelope ((99, 99, 99, 99), (99, 99, 99, 0));

    KLS := (
        Breakpoint => 60 - 21,
        Left_Depth => 0,
        Right_Depth => 0,
        Left_Curve => Lin_Neg_Curve,
        Right_Curve => Lin_Neg_Curve
    );

    Op1 := (
        EG => EG,
        Kbd_Level_Scaling => KLS,
        Kbd_Rate_Scaling => 0,
        Amp_Mod_Sens => 0,
        Key_Vel_Sens => 0,
        Output_Level => 0,
        Mode => Ratio,
        Coarse => 1,
        Fine => 0,
        Detune => 0
    );

    Lfo := (
        Speed => 0,
        LFO_Delay => 0,
        PMD => 0,
        AMD => 0,
        Sync => True,
        Wave => Triangle,
        Pitch_Mod_Sens => 0
    );

    V := (
        Name => "INIT VOICE",
        Operators => (Op1, Op1, Op1, Op1, Op1, Op1),
        Pitch_Envelope => EG,
        Algorithm => 1,
        Feedback => 0,
        Osc_Sync => False,
        LFO => Lfo
    );

    for i in Voice_Index loop
        Cartridge.Voices (i) := V;
    end loop;

    Manufacturer := (
        Standard_Kind,
        Standard_Identifier => Helpers.Byte (16#43#)  -- identifier for Yamaha
    );

    Payload.Append (Byte (Channel - 1));
    Payload.Append (Byte (16#09#));  -- format = 9 (32 voices)
    Payload.Append (Byte (16#20#));  -- byte count (MSB)
    Payload.Append (Byte (16#00#));  -- byte count (LSB)
    Payload.Append (Get_Data (Cartridge));
    Payload.Append (Byte (0));  -- TODO: compute checksum for cartridge

    Message := (
        Manufacturer,
        Payload
    );
    Data := Get_Data (Message);
    Helpers.Write_File ("cartridge.bin", Data);

    IO.Put_Line(Hex_Dump(Data));

end Main;
