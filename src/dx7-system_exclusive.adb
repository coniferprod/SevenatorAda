with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;
with DX7.System_Exclusive;

package body DX7.System_Exclusive is
   function Emit (Message : Message_Type) return Byte_Vector is
      BV : Byte_Vector;
      Manuf_Bytes : Byte_Array := To_Bytes (Message.Manufacturer);
   begin
      BV.Append (System_Exclusive_Initiator);

      for I in Manuf_Bytes'First .. Manuf_Bytes'Last loop
         BV.Append (Manuf_Bytes (I));
      end loop;

      BV.Append (Byte ((Message.Payload.Header.Channel - 1)) or Shift_Left (Message.Payload.Header.Sub_Status, 4));
      BV.Append (Byte (Format_Type'Pos (Message.Payload.Header.Format)));
      case Message.Payload.Format is
         when Cartridge =>
            BV.Append (16#20#);
            BV.Append (0);
            BV.Append (To_Byte_Vector (Message.Payload.Cartridge_Data));
         when Voice =>
            BV.Append (1);
            BV.Append (16#1B#);
            BV.Append (To_Byte_Vector (Message.Payload.Voice_Data));
      end case;

      BV.Append (Message.Payload.Checksum);

      BV.Append (System_Exclusive_Terminator);
      return BV;
   end Emit;

   procedure Put (Header : Header_Type) is
   begin
      Ada.Text_IO.Put_Line ("Header: channel = " & Integer'Image (Integer (Header.Channel))
         & " format = " & Integer'Image (Format_Type'Enum_Rep (Header.Format))
         & " byte count = " & Integer'Image (Header.Byte_Count));
   end Put;

   procedure Parse_Header (Data : in Byte_Array; Header : out Header_Type) is
      Channel : MIDI_Channel_Type;
      Byte_Count : Natural;
      Format : Format_Type;
   begin
      Ada.Text_IO.Put_Line ("Parsing header");

      -- sub_status: (data[0] >> 4) & 0b00000111,
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
      Ada.Text_IO.Put_Line ("Parsing payload");

      for I in 0 .. 3 loop
         Header_Data (I) := Data.Element (I);
      end loop;
      Parse_Header (Header_Data, Header);
      Put (Header);

      Checksum := Data.Element (Data.Last_Index);

      declare
         Voice_Data : Voice_Data_Type;
         Cartridge_Data : Cartridge_Data_Type;
         Offset : Natural;
      begin
         Offset := Header_Data_Length;
         case Header.Format is
            when Voice => 
               for I in 0 .. 154 loop
                  Voice_Data (I) := Data.Element (I + Offset);
               end loop;
               Temp_Payload := (Voice, Header, Checksum, Voice_Data);
            when Cartridge =>
               for I in 0 .. 4095 loop
                  Cartridge_Data (I) := Data.Element (I + Offset);
               end loop;
               Temp_Payload := (Cartridge, Header, Checksum, Cartridge_Data);
         end case;
      end;
      Payload := Temp_Payload;
   end Parse_Payload;

   procedure Parse_Message (Data : in Byte_Array; Message : out Message_Type)
   is
      Manuf  : Manufacturer_Type;
      Payload_Start : Natural := 2; -- zero-based, defaults to the position after standard manufacturer ID
      Payload_Data : Byte_Vector;
      Payload : Payload_Type;
   begin
      Ada.Text_IO.Put_Line ("Parsing message");

      if Data (0) /= System_Exclusive_Initiator then
         raise Parse_Error;
      end if;

      --Put (Data'Last'Image);

      if Data (Data'Last) /= System_Exclusive_Terminator then
         raise Parse_Error;
      end if;

      Manuf :=
        (if Data (1) /= 0 then
            Manufacturer (Data (1))
         else
            Manufacturer (Data (1), Data (2), Data (3)));

      if Kind (Manuf) = Extended then
         Payload_Start :=
           Payload_Start + 2; -- move past the extended manufacturer ID
      end if;

      -- Copy the payload until the last but one byte (zero-based)
      for I in Payload_Start .. Data'Last - 2 loop
         Payload_Data.Append (Data (I));
      end loop;

      Parse_Payload (Payload_Data, Payload);

      Message := (Manufacturer => Manuf, Payload => Payload);
   end Parse_Message;

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

end DX7.System_Exclusive;
