with Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Helpers; use Helpers;
with DX7; use DX7;
with DX7.Envelopes; use DX7.Envelopes;
with DX7.Operators; use DX7.Operators;
with DX7.Voices; use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;

procedure Main is
    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

    V : Voice_Type;
    Op1 : Operator_Type;
    Cartridge : Cartridge_Type;
    Cartridge_Data : Byte_Vector;
    Cartridge_Checksum : Byte;

    Manufacturer : Manufacturer_Type;
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

    --In_Data := Read_File ("cartridge.bin");
    -- Do something with the data
    --Put(In_Data'Length);

    --for I in In_Data'Range loop
    --    Out_Data.Append (In_Data (I));
    --end loop;

    --Delete (In_Data);

    --IO.Put_Line(Hex_Dump(Out_Data));

    declare
        Name : String := CLI.Argument (1);
        Size : Ada.Directories.File_Size := Ada.Directories.Size (Name);
        Data : Byte_Array (0 .. Size - 1);

    begin
        Ada.Integer_Text_IO.Put (Integer (Size));
        Ada.Text_IO.New_Line;

        Read_All_Bytes (Name, Data);

        for I in Data'Range loop
            Ada.Text_IO.Put (Hex (Data (I)));
            Ada.Text_IO.Put (" ");
        end loop;
    end;

    -- Try to create a random envelope
    declare
        Random_EG : Envelope_Type := Random_Envelope;
    begin
        IO.New_Line;
        IO.Put_Line ("Random EG:");
        for R in Random_EG.Rates'Range loop
            IO.Put (Random_EG.Rates (R)'Image);
            IO.Put (" ");
        end loop;
        for L in Random_EG.Levels'Range loop
            IO.Put (Random_EG.Levels (L)'Image);
            IO.Put (" ");
        end loop;
        IO.New_Line;
    end;
    
    IO.Put_Line("Generating new cartridge...");

    Op1 := (
        EG => (
            Rates => (99, 99, 99, 99), 
            Levels => (99, 99, 99, 0)
        ),
        Kbd_Level_Scaling => (
            Breakpoint => 60 - 21,
            Left_Depth => 0,
            Right_Depth => 0,
            Left_Curve => Lin_Neg_Curve,
            Right_Curve => Lin_Neg_Curve
        ),
        Kbd_Rate_Scaling => 0,
        AMS => 0,
        Key_Vel_Sens => 0,
        Output_Level => 0,
        Mode => Ratio,
        Coarse => 1,
        Fine => 0,
        Detune => 0
    );

    V := (
        Name => "INIT VOICE",
        Operators => (Op1, Op1, Op1, Op1, Op1, Op1),
        Pitch_Envelope => (
            Rates => (99, 99, 99, 99), 
            Levels => (99, 99, 99, 0)
        ),
        Algorithm => 1,
        Feedback => 0,
        Osc_Sync => False,
        LFO => (
            Speed => 0,
            LFO_Delay => 0,
            PMD => 0,
            AMD => 0,
            Sync => True,
            Wave => Triangle,
            Pitch_Mod_Sens => 0
        ),
        Transpose => 0
    );

    for i in Voice_Index loop
        Cartridge.Voices (i) := V;
    end loop;

    Manufacturer := (
        Standard_Kind,
        Standard_Identifier => Helpers.Byte (16#43#)  -- identifier for Yamaha
    );

    Payload.Append (Byte (Channel - 1));
    Payload.Append (Byte (9));       -- format = 9 (32 voices)
    Payload.Append (Byte (16#20#));  -- byte count (MSB)
    Payload.Append (Byte (16#00#));  -- byte count (LSB)

    Cartridge_Data := Get_Data (Cartridge);
    Payload.Append (Cartridge_Data);

    Cartridge_Checksum := Checksum (Cartridge_Data);
    Payload.Append (Cartridge_Checksum);

    Message := (
        Manufacturer,
        Payload
    );
    Data := Get_Data (Message);
    Helpers.Write_File ("cartridge.bin", Data);

    -- IO.Put_Line(Hex_Dump(Data));

end Main;
