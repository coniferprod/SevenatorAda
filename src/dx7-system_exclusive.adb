with Ada.Directories; use Ada.Directories;

package body DX7.System_Exclusive is

   function Emit (Manufacturer : Manufacturer_Type) return Byte_Vector is
      BV : Sixten.Byte_Vector;
      Bytes : Sixten.Byte_Array := To_Bytes (Manufacturer);
   begin
      for B of Bytes loop
         BV.Append (B);
      end loop;
      return BV;
   end Emit;

   function Emit (Message : Message_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      BV.Append (System_Exclusive_Initiator);
      BV.Append (Emit (Message.Manufacturer));
      BV.Append (Emit (Message.Payload));
      BV.Append (System_Exclusive_Terminator);
      return BV;
   end Emit;

   function Emit (Header : Header_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      return BV;
   end Emit;

   function Emit (Payload : Payload_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      case Payload.Format is
         when Voice => 
            for B of Payload.Voice_Data loop
               BV.Append (B);
            end loop;
         when Cartridge =>
            for B of Payload.Cartridge_Data loop
               BV.Append (B);
            end loop;
      end case;
      return BV;
   end Emit;

   procedure Parse (Data : in Byte_Array; Header : out Header_Type) is
      Channel : MIDI_Channel_Type;
      Byte_Count : Natural;
      Format : Format_Type;
      Result : Header_Type;
   begin
      -- sub_status: (data[0] >> 4) & 0b00000111,
      Channel := MIDI_Channel_Type (Data (0) + 1);
      Format := (if Data (1) = 1 then Voice else Cartridge);
      Byte_Count := (if Format = Voice then 155 else 4096);
      Header := (Data (0), Channel, Format, Byte_Count);
   end Parse;

   procedure Parse (Data : in Byte_Vector; Payload : out Payload_Type) is
      Header : Header_Type;
      Header_Data : Byte_Array (0 .. 4);
      Temp_Payload : Payload_Type;
      Checksum : Byte;
   begin
      for I in 0 .. 4 loop
         Header_Data (I) := Data (I);
      end loop;
      Parse (Header_Data, Header);
      Checksum := Data (Data.Last_Index);
      declare
         Voice_Data : Voice_Data_Type;
         Cartridge_Data : Cartridge_Data_Type;
      begin
         if Data (1) = 0 then
            for I in 0 .. 154 loop
               Voice_Data (I) := Data (I);
            end loop;
            Temp_Payload := (Voice, Header, Checksum, Voice_Data);
         else
            for I in 0 .. 4095 loop
               Cartridge_Data (I) := Data (I);
            end loop;
            Temp_Payload := (Cartridge, Header, Checksum, Cartridge_Data);
         end if;
      end;
      Payload := Temp_Payload;
   end Parse;

   procedure Parse (Data : in Byte_Array; Message : out Message_Type)
   is
      Manuf  : Manufacturer_Type;
      Payload_Start : Natural := 2; -- zero-based, defaults to the position after standard manufacturer ID
      Payload_Data : Byte_Vector;
      Payload : Payload_Type;
   begin
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

      Parse (Payload_Data, Payload);

      Message := (Manufacturer => Manuf, Payload => Payload);
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

end DX7.System_Exclusive;
