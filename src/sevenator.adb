with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Helpers; use Helpers;
with DX7; use DX7;
with DX7.Envelopes; use DX7.Envelopes;
with DX7.Operators; use DX7.Operators;
with DX7.Voices; use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;

procedure Sevenator is
    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

    Manufacturer : Manufacturer_Type;
    Message : Message_Type;
    Payload : Byte_Vector;
    Channel : MIDI_Channel_Type := 1;  -- MIDI channel number
    Data : Byte_Vector;
    Out_Data : Byte_Vector;

begin
    --for i in 1 .. CLI.Argument_Count loop
    --    IO.Put_Line (Item => CLI.Argument (Number => i));
    --end loop;
    if CLI.Argument_Count < 1 then
        Put_Line("No input file specified");
        return;
    end if;

    declare
        Name : String := CLI.Argument (1);
        Size : Ada.Directories.File_Size := Ada.Directories.Size (Name);
        Data : Byte_Array (0 .. Size - 1);
    begin
        Put ("Input file: ");
        Put (Name);
        New_Line;

        Put ("Size (bytes): ");
        Put (Integer (Size));
        New_Line;

        Read_All_Bytes (Name, Data);

        --for I in Data'Range loop
        --    Ada.Text_IO.Put (Hex (Data (I)));
        --    Ada.Text_IO.Put (" ");
        --end loop;
    end;

    -- Make a random envelope
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
    
    Manufacturer := (
        Standard_Kind,
        Standard_Identifier => Byte (16#43#)  -- identifier for Yamaha
    );

    Payload.Clear;
    Payload.Append (Byte (Channel - 1));
    Payload.Append (Byte (9));       -- format = 9 (32 voices)
    Payload.Append (Byte (16#20#));  -- byte count (MSB)
    Payload.Append (Byte (16#00#));  -- byte count (LSB) (b=4096; 32 voices)

    declare
        Cartridge : Cartridge_Type;
        Cartridge_Data : Cartridge_Data_Type;
        Cartridge_Checksum : Byte;
    begin
        IO.Put_Line("Generating new cartridge...");

        -- Just fill the cartridge with copies of the "BRASS1" voice
        for I in Voice_Index loop
            Cartridge.Voices (I) := Brass1;
        end loop;

        Get_Data (Cartridge, Cartridge_Data);
        IO.Put ("Cartridge data length = ");
        IO.Put_Line (Cartridge_Data'Length'Image);
        for B of Cartridge_Data loop
            Payload.Append (B);
        end loop;

        Cartridge_Checksum := Checksum (Cartridge_Data);
        Payload.Append (Cartridge_Checksum);
    end;

    IO.Put_Line (Hex_Dump (Payload));

    Message := (Manufacturer, Payload);
    Data := Get_Data (Message);
    Helpers.Write_File ("cartridge.bin", Data);

    declare
        Voice : Voice_Type;
        Voice_Data : Voice_Data_Type;
        Offset : Integer;
    begin
        IO.Put_Line("Generating new voice...");
        Voice := Brass1;

        Payload.Clear;
        Payload.Append (Byte (Channel - 1));  -- adjust channel to 0...15
        Payload.Append (Byte (0));            -- format = 0 (1 voice)
        Payload.Append (Byte (16#01#));  -- byte count (MSB)
        Payload.Append (Byte (16#1B#));  -- byte count (LSB) (b=155; 1 voice)

        Voice_Data := Get_Data (Voice);
        Offset := 1;
        for B of Voice_Data loop
            Payload.Append (B);
            Offset := Offset + 1;
        end loop;

        Payload.Append (Checksum (Voice_Data));
    end;        

    Message := (Manufacturer, Payload);
    Data := Get_Data (Message);
    Helpers.Write_File ("voice.bin", Data);

end Sevenator;
