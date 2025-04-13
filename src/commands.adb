with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
use type Ada.Directories.File_Size;

with Sixten; use Sixten;
with Sixten.Manufacturers; use Sixten.Manufacturers;

with DX7;            use DX7;
with DX7.Envelopes;  use DX7.Envelopes;
with DX7.Voices;     use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;
with DX7.System_Exclusive; use DX7.System_Exclusive;

package body Commands is
   procedure Run_Dump (Name : String) is
      Size : constant Ada.Directories.File_Size := Ada.Directories.Size (Name);
      Data    : Byte_Array (0 .. Integer (Size) - 1);
      Message : Message_Type;
   begin
      Put ("Input file: ");
      Put (Name);
      New_Line;

      Put ("Size (bytes): ");
      Put (Item => Size'Image);
      New_Line;

      Read_File (Name, Data);

      --for I in Data'Range loop
      --   Put (Hex (Data (I)));
      --   Put (" ");
      --end loop;

      Parse (Data, Message);

      --for B of Message.Payload loop
      --   Put (Hex (B));
      --   Put (" ");
      --end loop;

      Put_Line ("Channel = " & Message.Payload.Header.Channel'Image);

      declare
         package Format_IO is new Ada.Text_IO.Enumeration_IO(Enum => Format_Type);
      begin
         Put ("Format = ");
         Format_IO.Put (Message.Payload.Format);
      end;

      case Message.Payload.Format is
         when Voice =>
            declare
               Voice : Voice_Type;
               Data : Voice_Data_Type;
            begin
               for I in Message.Payload.Voice_Data'First .. Message.Payload.Voice_Data'Last loop
                  Data (I) := Message.Payload.Voice_Data (I);
               end loop;
               Parse (Data, Voice);

               Ada.Text_IO.Put_Line ("Voice name = " & Voice.Name);
            end;
         when Cartridge =>
            declare
               Cartridge : Cartridge_Type;
               Data : Cartridge_Data_Type;
            begin
               for I in Message.Payload.Cartridge_Data'First .. Message.Payload.Cartridge_Data'Last loop
                  Data (I) := Message.Payload.Cartridge_Data (I);
               end loop;
               Parse (Data, Cartridge);

               for V of Cartridge.Voices loop
                  Put_Line (V.Name);
               end loop;
            end;
      end case;

   end Run_Dump;

   procedure Run_Cartridge (Name : String) is
      Random_EG : constant Envelope_Type := Random_Envelope;

      Channel : constant MIDI_Channel_Type := 1;  -- MIDI channel number
      Message : Message_Type;
      Data    : Byte_Vector;
      Header : Header_Type;
      Payload : Payload_Type;
   begin
      Header := (Sub_Status => 0, Channel => 1, Format => Cartridge, Byte_Count => 4096);
      Payload := (Cartridge, Header, Checksum => 0,
                                 Cartridge_Data => (others => 0));

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

      Put_Line ("Generating new cartridge...");

      declare
         Cartridge          : Cartridge_Type;
         Cartridge_Data     : Cartridge_Data_Type;
      begin
         -- Just fill the cartridge with copies of the "BRASS1" voice
         for I in Voice_Index loop
            Cartridge.Voices (I) := Brass1;

            -- But change the name to a random one:
            Cartridge.Voices (I).Name := Random_Voice_Name;
         end loop;

         Emit (Cartridge, Cartridge_Data);
         Put ("Cartridge data length = ");
         Put_Line (Cartridge_Data'Length'Image);

         Payload.Cartridge_Data := Cartridge_Data;
         Payload.Checksum := Checksum (Cartridge_Data);
      end;

      Message := (Sixten.Manufacturers.Yamaha, Payload);
      Data    := Emit (Message);
      Write_File (Name, Data);
   end Run_Cartridge;

   procedure Run_Voice (Name : String) is
      Channel : constant MIDI_Channel_Type := 1;  -- MIDI channel number
      Message : Message_Type;
      Data    : Byte_Vector;
      Header : Header_Type;
      Payload : Payload_Type;

   begin
      Header := (Sub_Status => 0, Channel => 1, Format => Voice, Byte_Count => 155);

      Payload := (Voice, Header,
                                 Checksum => 0,
                                 Voice_Data => (others => 0));

      Put_Line ("Generating new voice...");

      declare
         Voice      : Voice_Type;
         Voice_Data : Voice_Data_Type;
      begin
         Voice := Brass1;
         Emit (Voice, Voice_Data);
         Payload.Voice_Data := Voice_Data;
         Payload.Checksum := Checksum (Voice_Data);
      end;

      Message := (Sixten.Manufacturers.Yamaha, Payload);
      Data    := Emit (Message);
      Write_File (Name, Data);
   end Run_Voice;

end Commands;
