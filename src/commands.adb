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

      function Slice (BV : Byte_Vector; Start_Index : Natural; End_Index : Natural) return Byte_Vector is
         Length : Natural := End_Index - Start_Index;
         Result : Byte_Vector;
         I : Natural := Start_Index;
      begin
         for I in Start_Index .. End_Index loop
            Result.Append (BV (I));
         end loop;
         return Result;
      end Slice;

      Size : constant Ada.Directories.File_Size := Ada.Directories.Size (Name);
      Data    : Byte_Array (0 .. Integer (Size) - 1);
      BV : Byte_Vector;
      Manufacturer : Sixten.Manufacturers.Manufacturer_Type;
      Raw_Message : Sixten.Messages.Message_Type;
      Payload : DX7.System_Exclusive.Payload_Type;
      A_Slice : Byte_Vector;
   begin
      --Put ("Input file: "); Put (Name); New_Line;
      --Put ("Size (bytes): "); Put (Item => Size'Image); New_Line;

      --Read_File (Name, Data);
      --BV := DX7.Cartridges.To_Byte_Vector (Data);
      BV := Sixten.Read_All_Bytes (Name);
      Sixten.Messages.Parse (BV, Raw_Message);
      --  Now we should have the System Exclusive message
      --  with type and payload in Raw_Message

      Put_Line ("Raw message parsed");

      A_Slice := Slice (Raw_Message.Payload, Raw_Message.Payload.First_Index, Raw_Message.Payload.Last_Index - 1);
      Put_Line ("Got a slice");
      Parse_Payload (A_Slice, Payload);
      --Put (Header);

      case Payload.Header.Format is
         when Voice =>
            declare
               Voice : Voice_Type;
               Data : Voice_Data_Type;
            begin
               for I in Payload.Voice_Data'First .. Payload.Voice_Data'Last loop
                  Data (I) := Payload.Voice_Data (I);
               end loop;
               Parse_Voice (Data, Voice);

               Ada.Text_IO.Put_Line ("Voice name = " & Voice.Name);
            end;
         when Cartridge =>
            declare
               Cartridge : Cartridge_Type;
               Data : Cartridge_Data_Type;
            begin
               for I in Payload.Cartridge_Data'First .. Payload.Cartridge_Data'Last loop
                  Data (I) := Payload.Cartridge_Data (I);
               end loop;
               Parse_Cartridge (Data, Cartridge);

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
         C_Bytes : Byte_Array (0 .. Cartridge_Data_Length - 1);
      begin
         -- Just fill the cartridge with copies of the "BRASS1" voice
         for I in Voice_Index loop
            Cartridge.Voices (I) := Brass1;

            -- But change the name to a random one:
            Cartridge.Voices (I).Name := Random_Voice_Name;
         end loop;

         Emit (Cartridge, Cartridge_Data);
         Payload.Cartridge_Data := Cartridge_Data;

         for I in Cartridge_Data'Range loop
            C_Bytes (I) := Cartridge_Data (I);
         end loop;
         Payload.Checksum := Checksum (C_Bytes);
      end;

      Message := (Manufacturer_Specific, Emit_Payload (Payload), Sixten.Manufacturers.Yamaha);
      Sixten.Messages.Emit (Message, Data);
      Write_File (Name, Data);
   end Run_Cartridge;

   procedure Run_Voice (Name : String) is
      Channel : constant MIDI_Channel_Type := 1;  -- MIDI channel number
      Message : Message_Type;
      Data    : Byte_Vector;
      Header : Header_Type;
      Payload : Payload_Type;
      V_Bytes : Byte_Array (1 .. Voice_Data_Length);
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

         for I in Voice_Data'Range loop
            V_Bytes (I) := Voice_Data (I);
         end loop;

         Payload.Checksum := Checksum (V_Bytes);
      end;

      Message := (Manufacturer_Specific, Emit_Payload (Payload), Sixten.Manufacturers.Yamaha);
      Emit (Message, Data);
      Write_File (Name, Data);
   end Run_Voice;

end Commands;
