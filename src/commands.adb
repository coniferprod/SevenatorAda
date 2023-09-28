with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use type Ada.Directories.File_Size;
with Helpers; use Helpers;
with DX7; use DX7;
with DX7.Envelopes; use DX7.Envelopes;
with DX7.Voices; use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;

package body Commands is

-- DX7 patch file formats: 0 = one voice, 9 = cartridge of 32 voices
Voice_Format : constant Byte := 0;
Cartridge_Format : constant Byte := 9;

Manufacturer : constant Manufacturer_Type :=
    (Standard_Kind,
        Standard_Identifier => 16#43#  -- identifier for Yamaha
    );


procedure Run_Dump (Name: String) is
    Size : constant Ada.Directories.File_Size := Ada.Directories.Size (Name);
    Data : Byte_Array (0 .. Size - 1);
    Message : Message_Type;
begin
    Put ("Input file: ");
    Put (Name);
    New_Line;

    Put ("Size (bytes): ");
    Put (Item => Size'Image);
    New_Line;

    Read_File (Name, Data);

    for I in Data'Range loop
        Put (Hex (Data (I)));
        Put (" ");
    end loop;

    Parse_Message (Data, Message);
end Run_Dump;

procedure Run_Cartridge (Name : String) is
    -- Make a random envelope
    Random_EG : Envelope_Type := Random_Envelope;

    Cartridge : Cartridge_Type;
    Cartridge_Data : Cartridge_Data_Type;
    Cartridge_Checksum : Byte;

    Payload : Byte_Vector;
    Channel : constant MIDI_Channel_Type := 1;  -- MIDI channel number
    Message : Message_Type;
    Data : Byte_Vector;

begin
    -- Test code to print envelope generator:
    New_Line;
    Put_Line ("Random EG:");
    for R in Random_EG.Rates'Range loop
        Put (Random_EG.Rates (R)'Image);
        Put (" ");
    end loop;
    for L in Random_EG.Levels'Range loop
        Put (Random_EG.Levels (L)'Image);
        Put (" ");
    end loop;
    New_Line;

    Payload.Clear;
    Payload.Append (Byte (Channel - 1));
    Payload.Append (Cartridge_Format);       -- format = 9 (32 voices)
    Payload.Append (16#20#);  -- byte count (MSB)
    Payload.Append (16#00#);  -- byte count (LSB) (b=4096; 32 voices)

    Put_Line("Generating new cartridge...");

    -- Just fill the cartridge with copies of the "BRASS1" voice
    for I in Voice_Index loop
        Cartridge.Voices (I) := Brass1;

        -- But change the name to a random one:
        Cartridge.Voices (I).Name := Random_Voice_Name;
    end loop;

    Get_Data (Cartridge, Cartridge_Data);
    Put ("Cartridge data length = ");
    Put_Line (Cartridge_Data'Length'Image);
    for B of Cartridge_Data loop
        Payload.Append (B);
    end loop;

    Cartridge_Checksum := Checksum (Cartridge_Data);
    Payload.Append (Cartridge_Checksum);

    Message := (Manufacturer, Payload);
    Data := Get_Data (Message);
    Helpers.Write_File (Name, Data);
end Run_Cartridge;

procedure Run_Voice (Name : String) is
    Payload : Byte_Vector;
    Channel : constant MIDI_Channel_Type := 1;  -- MIDI channel number
    Message : Message_Type;
    Data : Byte_Vector;

    Voice : Voice_Type;
    Voice_Data : Voice_Data_Type;
    Offset : Integer;
begin
    Put_Line ("Generating new voice...");
    Voice := Brass1;

    Payload.Clear;
    Payload.Append (Byte (Channel - 1));  -- adjust channel to 0...15
    Payload.Append (Voice_Format);        -- format = 0 (1 voice)
    Payload.Append (16#01#);  -- byte count (MSB)
    Payload.Append (16#1B#);  -- byte count (LSB) (b=155; 1 voice)

    Voice_Data := Get_Data (Voice);
    Offset := 1;
    for B of Voice_Data loop
        Payload.Append (B);
        Offset := Offset + 1;
    end loop;

    Payload.Append (Checksum (Voice_Data));

    Message := (Manufacturer, Payload);
    Data := Get_Data (Message);
    Helpers.Write_File (Name, Data);
end Run_Voice;

end Commands;
