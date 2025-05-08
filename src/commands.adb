with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
use type Ada.Directories.File_Size;

with Sixten; use Sixten;
with Sixten.Manufacturers; use Sixten.Manufacturers;
with Sixten.Messages; use Sixten.Messages;

with DX7;            use DX7;
with DX7.Envelopes;  use DX7.Envelopes;
with DX7.Voices;     use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;
with DX7.System_Exclusive; use DX7.System_Exclusive;

package body Commands is
   procedure Run_Dump (Name : String) is
      Size : constant Ada.Directories.File_Size := Ada.Directories.Size (Name);
      Data : Byte_Array (1 .. Natural (Size));
      BV : Byte_Vector;
      Manufacturer : Sixten.Manufacturers.Manufacturer_Type;
      Raw_Message : Sixten.Messages.Message_Type;
      Payload : DX7.System_Exclusive.Payload_Type;
      Start_Index, End_Index : Natural;
   begin
      Read_File (Name, Data);
      Parse (Data, Raw_Message);
      --  Now we should have the System Exclusive message
      --  with type and payload in Raw_Message

      Start_Index := Raw_Message.Payload'First;
      End_Index := Raw_Message.Payload'Last;
      Parse (Raw_Message.Payload, Payload);

      case Payload.Header.Format is
         when Voice =>
            declare
               Voice : Voice_Type;
            begin
               Parse (Payload.Voice_Data, Voice);
               Ada.Text_IO.Put_Line ("Voice name = " & Voice.Name);
            end;
         when Cartridge =>
            declare
               Cartridge : Cartridge_Type;
            begin
               Put_Line ("BEGIN Cartridge parsing");
               Ada.Text_IO.Put_Line ("Cartridge payload (" & Integer'Image (Payload.Cartridge_Data'Length) & " bytes) = "
                  & Hex_Dump (Payload.Cartridge_Data));

               Parse (Payload.Cartridge_Data, Cartridge);
               Put_Line ("END Cartridge parsing");

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
      Header  : Header_Type;
      Payload : Payload_Type;
   begin
      Header := (Sub_Status => Voice_Cartridge, Channel => 1, Format => Cartridge);
      Payload := (Format => Cartridge, Header => Header, Checksum => 0,
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
         Payload.Cartridge_Data := Cartridge_Data;
         Payload.Checksum := Checksum (Cartridge_Data);
      end;

      declare
         Size : Natural := Header_Size + Cartridge_Data_Length + 1;  -- include checksum
         Payload_Data : Byte_Array (1 .. Size);
      begin
         Emit (Payload, Payload_Data);

         Message := (Kind => Manufacturer_Specific,
            Payload_Size => Size,
            Payload => Payload_Data,
            Manufacturer => Yamaha);
         Emit (Message, Data);
         Write_File (Name, Data);
      end;
   end Run_Cartridge;

   procedure Run_Voice (Name : String) is
      Message : Message_Type;
      Data    : Byte_Vector;
      Header  : Header_Type;
      Payload : Payload_Type;
   begin
      Header := (Sub_Status => Voice_Cartridge, Channel => 1, Format => Voice);

      Payload := (Format => Voice,
                  Header => Header,
                  Checksum => 0,
                  Voice_Data => (others => 0));

      Put_Line ("Generating new voice...");

      Emit (Brass1, Payload.Voice_Data);
      Payload.Checksum := Checksum (Payload.Voice_Data);

      declare
         Size : Natural := Header_Size + Voice_Data_Length + 1;  -- include checksum
         Payload_Data : Byte_Array (1 .. Size);
      begin
         Emit (Payload, Payload_Data);
         Message := (Kind => Manufacturer_Specific,
                     Payload_Size => Size,
                     Payload => Payload_Data,
                     Manufacturer => Yamaha);
         Emit (Message, Data);
         Write_File (Name, Data);
      end;
   end Run_Voice;

end Commands;
