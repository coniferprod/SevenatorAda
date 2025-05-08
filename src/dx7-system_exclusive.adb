with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;
with Sixten;
with Sixten.Messages;

package body DX7.System_Exclusive is
   procedure Emit (Header : in Header_Type; Result : out Header_Data_Type) is
   begin
      Result (1) := Byte (Header.Channel - 1) or Shift_Left (Byte (Status_Type'Pos (Header.Sub_Status)), 4);
      Result (2) := Byte (Format_Type'Pos (Header.Format));
      case Header.Format is
         when Cartridge =>
            Result (3) := 16#20#;
            Result (4) := 0;
         when Voice =>
            Result (3) := 1;
            Result (4) := 16#1B#;
      end case;
   end Emit;


   procedure Emit (Payload : in Payload_Type; Result : out Byte_Array) is
      Header_Data : Header_Data_Type;
      Size : Natural;
      Start_Index, End_Index : Natural;
   begin
      Emit (Payload.Header, Header_Data);
      Result (1 .. Header_Size) := Header_Data;
      Start_Index := Header_Size + 1;
      case Payload.Format is
         when Cartridge =>
            Size := Payload.Cartridge_Data'Length;
            End_Index := Start_Index + Size;
            Result (Start_Index .. End_Index) := Payload.Cartridge_Data;
         when Voice =>
            Size := Payload.Voice_Data'Length;
            End_Index := Start_Index + Size;
            Result (Start_Index .. End_Index) := Payload.Voice_Data;
      end case;
   end Emit;

   procedure Parse (Data : in Byte_Array; Header : out Header_Type) is
      Channel : MIDI_Channel_Type;
      Format : Format_Type;
   begin
      Channel := MIDI_Channel_Type (Data (1) + 1);
      Format := (if Data (2) = 1 then Voice else Cartridge);
      Header := (Status_Type'Val (Data (1)), Channel, Format);
   end Parse;

   procedure Parse (Data : in Byte_Array; Payload : out Payload_Type) is
      Header_Data : Header_Data_Type;
      Header : Header_Type;
      Checksum : Byte;
      Temp_Payload : Payload_Type;
   begin
      Header_Data := Data (1 .. Header_Size);
      Parse (Header_Data, Header);
      Checksum := Data (Data'Last);

      declare
         Voice_Data : Voice_Data_Type;
         Cartridge_Data : Cartridge_Data_Type;
         Offset : Natural;
      begin
         Offset := Header_Size + 1;
         case Header.Format is
            when Voice =>
               Voice_Data := Data (Offset .. Offset + Voice_Data_Length - 1);
               Temp_Payload := (Voice, Header, Checksum, Voice_Data);
            when Cartridge =>
               Cartridge_Data := Data (Offset .. Offset + Cartridge_Data_Length - 1);
               Temp_Payload := (Cartridge, Header, Checksum, Cartridge_Data);
         end case;
      end;
      Payload := Temp_Payload;
   end Parse;

   -- Computes the checksum byte for voice or cartridge data.
   function Checksum (Data : Byte_Array) return Byte is
      Sum    : Byte := 0;
      Result : Byte;
   begin
      for B of Data loop
         Sum := Sum + B;
      end loop;

      Result := Sum and 16#FF#;
      Result := not Result;
      Result := Result and 16#7F#;
      Result := Result + 1;
      return Result;
   end Checksum;

   procedure Put (Header : Header_Type) is
      package Format_IO is new Ada.Text_IO.Enumeration_IO (Enum => Format_Type);
   begin
      Ada.Text_IO.Put ("Header: Channel = " & Integer'Image (Integer (Header.Channel)));
      Ada.Text_IO.Put (" Format = ");
      Format_IO.Put (Header.Format);
      Ada.Text_IO.New_Line;
   end Put;

end DX7.System_Exclusive;
