with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Helpers; use Helpers;
with DX7; use DX7;
with DX7.Envelopes; use DX7.Envelopes;
with DX7.Voices; use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;

procedure Sevenator is
    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

    Manufacturer : constant Manufacturer_Type := 
        (Standard_Kind,
         Standard_Identifier => 16#43#  -- identifier for Yamaha
        );

    Message : Message_Type;
    Payload : Byte_Vector;
    Channel : constant MIDI_Channel_Type := 1;  -- MIDI channel number
    Data : Byte_Vector;

    -- DX7 patch file formats: 0 = one voice, 9 = cartridge of 32 voices
    Voice_Format : constant Byte := 0;
    Cartridge_Format : constant Byte := 9;

begin
    -- Test random name generation
    --for I in 1 .. 10 loop
    --    Ada.Text_IO.Put_Line (Random_Voice_Name);
    --end loop;

    -- Example of echoing the command line arguments:
    --for i in 1 .. CLI.Argument_Count loop
    --    IO.Put_Line (Item => CLI.Argument (Number => i));
    --end loop;

    if CLI.Argument_Count < 2 then
        Put_Line ("Usage: sevenator command filename");
        Put_Line ("  dump = show contents of DX7 file <filename>");
        Put_Line ("  cartridge = make new cartridge with random voices and write to <filename>");
        Put_Line ("  voice = make new random voice and write to <filename>");
        return;
    end if;

    declare
        Command : constant String := CLI.Argument (1);
        Name : constant String := CLI.Argument (2);
    begin
        if Command = "dump" then
            declare
                Size : constant Ada.Directories.File_Size := Ada.Directories.Size (Name);
                Data : Byte_Array (0 .. Size - 1);
            begin
                Put ("Input file: ");
                Put (Name);
                New_Line;

                Put ("Size (bytes): ");
                Put (Integer (Size));
                New_Line;

                Read_File (Name, Data);

                for I in Data'Range loop
                    Ada.Text_IO.Put (Hex (Data (I)));
                    Ada.Text_IO.Put (" ");
                end loop;
            end;
        elsif Command = "cartridge" then
            -- Make a random envelope
            declare
                Random_EG : Envelope_Type := Random_Envelope;
            begin
                -- Test code to print envelope generator:
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
            
            Payload.Clear;
            Payload.Append (Byte (Channel - 1));
            Payload.Append (Cartridge_Format);       -- format = 9 (32 voices)
            Payload.Append (16#20#);  -- byte count (MSB)
            Payload.Append (16#00#);  -- byte count (LSB) (b=4096; 32 voices)

            declare
                Cartridge : Cartridge_Type;
                Cartridge_Data : Cartridge_Data_Type;
                Cartridge_Checksum : Byte;
            begin
                IO.Put_Line("Generating new cartridge...");

                -- Just fill the cartridge with copies of the "BRASS1" voice
                for I in Voice_Index loop
                    Cartridge.Voices (I) := Brass1;

                    -- But change the name to a random one:
                    Cartridge.Voices (I).Name := Random_Voice_Name;
                end loop;

                Get_Data (Cartridge, Cartridge_Data);
                IO.Put ("Cartridge data length = ");
                IO.Put_Line (Cartridge_Data'Length'Image);
                for B of Cartridge_Data loop
                    Payload.Append (B);
                end loop;

                Cartridge_Checksum := Checksum (Cartridge_Data);
                Payload.Append (Cartridge_Checksum);

                Message := (Manufacturer, Payload);
                Data := Get_Data (Message);
                Helpers.Write_File (Name, Data);
            end;
        elsif Command = "voice" then
            declare
                Voice : Voice_Type;
                Voice_Data : Voice_Data_Type;
                Offset : Integer;
            begin
                IO.Put_Line ("Generating new voice...");
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
            end;        

            Message := (Manufacturer, Payload);
            Data := Get_Data (Message);
            Helpers.Write_File (Name, Data);
        else
            Put_Line ("Unknown command: " & Command);
        end if;
    end;

end Sevenator;
