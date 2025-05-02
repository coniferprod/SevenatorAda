with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;
with Sixten;
with Sixten.Messages;

package body DX7.System_Exclusive is
   function Emit_Header (Header : Header_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      BV.Append (Byte ((Header.Channel - 1)) or Shift_Left (Header.Sub_Status, 4));
      BV.Append (Byte (Format_Type'Pos (Header.Format)));
      case Header.Format is
         when Cartridge =>
            BV.Append (16#20#);
            BV.Append (0);
         when Voice =>
            BV.Append (1);
            BV.Append (16#1B#);
      end case;
      return BV;
   end Emit_Header;

   function Emit_Payload (Payload : Payload_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      BV.Append_Vector (Emit_Header (Payload.Header));
      case Payload.Format is
         when Cartridge =>
            BV.Append (Sixten.To_Byte_Vector (Payload.Cartridge_Data));
         when Voice =>
            BV.Append (Sixten.To_Byte_Vector (Payload.Voice_Data));
      end case;
      BV.Append (Payload.Checksum);
      return BV;
   end Emit_Payload;

   procedure Parse_Header (Data : in Byte_Array; Header : out Header_Type) is
      Channel : MIDI_Channel_Type;
      Byte_Count : Natural;
      Format : Format_Type;
   begin
      Channel := MIDI_Channel_Type (Data (0) + 1);
      Format := (if Data (1) = 1 then Voice else Cartridge);
      Byte_Count := (if Format = Voice then 155 else 4096);
      Header := (Data (0), Channel, Format, Byte_Count);
   end Parse_Header;

   procedure Parse_Payload (Data : in Byte_Vector; Payload : out Payload_Type) is
      Header : Header_Type;
      Header_Data : Byte_Array (0 .. 3);
      Temp_Payload : Payload_Type;
      Checksum : Byte;
   begin
      for I in 0 .. Header_Data_Length - 1 loop
         Header_Data (I) := Data.Element (I);
      end loop;
      Parse_Header (Header_Data, Header);
      Ada.Text_IO.Put_Line ("Header parsed");

      Checksum := Data.Element (Data.Last_Index);
      Ada.Text_IO.Put_Line ("Checksum = " & Integer'Image (Integer (Checksum)));

      declare
         Voice_Data : Voice_Data_Type;
         Cartridge_Data : Cartridge_Data_Type;
         Offset : Natural;
      begin
         Offset := Header_Data_Length;
         case Header.Format is
            when Voice =>
               for I in 0 .. Voice_Data_Length - 1 loop
                  Voice_Data (I) := Data.Element (Offset);
               end loop;
               Temp_Payload := (Voice, Header, Checksum, Voice_Data);
            when Cartridge =>
               Ada.Text_IO.Put_Line (Integer'Image (Data.First_Index) & " to " & Integer'Image (Data.Last_Index));
               for I in 0 .. Cartridge_Data_Length - 1 loop
                  --Ada.Text_IO.Put_Line ("I = " & Integer'Image (I));
                  Cartridge_Data (I) := Data.Element (Offset);
               end loop;
               Temp_Payload := (Cartridge, Header, Checksum, Cartridge_Data);
               Ada.Text_IO.Put_Line ("Got cartridge temp payload");
         end case;
      end;
      Payload := Temp_Payload;
      Ada.Text_IO.Put_Line ("Payload parsed");
   end Parse_Payload;

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
      Ada.Text_IO.Put (" Byte count = " & Integer'Image (Header.Byte_Count));
      Ada.Text_IO.New_Line;
   end Put;

end DX7.System_Exclusive;
