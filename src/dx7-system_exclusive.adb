with Ada.Directories; use Ada.Directories;
with Sixten.Manufacturers; use Sixten.Manufacturers;

package body DX7.System_Exclusive is

   function Emit (Manufacturer : Manufacturer_Type) return Sixten.Byte_Vector is
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
      Voice_Data : Voice_Data_Type;
      Cartridge_Data : Cartridge_Data_Type;
   begin
      case Payload.Format is
         when Voice => 
            DX7.Voices.Emit (Payload.Voice_Data, Voice_Data);
            for I in Voice_Data'Range loop
               BV.Append (Voice_Data (I));
            end loop;
         when Cartridge =>
            Emit (Payload.Cartridge_Data, Cartridge_Data);
            for I in Cartridge_Data'Range loop
               BV.Append (Cartridge_Data (I));
            end loop;
      end case;
      return BV;
   end Emit;

   procedure Parse_Header (Data : in Byte_Array; Header : out Header_Type) is
   begin
      null;
   end Parse_Header;

   procedure Parse_Payload (Data : in Byte_Vector; Payload : out Payload_Type) is
      Data_Size : Natural;
      Temp_Payload : Payload_Type;
      Channel : MIDI_Channel_Type;
      Byte_Count : Natural;
      Checksum : Byte;
   begin
      Channel := MIDI_Channel_Type (Data (0) + 1);
      Byte_Count := Natural (Data (2)) * 256 + Natural (Data (3));
      Checksum := Data (Data.Last_Index);
      if Data (1) = 0 then
         Temp_Payload := (Voice, Channel, Byte_Count, Checksum, Data (0 .. 154));
      else
         Temp_Payload := (Cartridge, Channel, Byte_Count, Checksum, Data (0 .. 4095));
      end if;

      Payload := Temp_Payload;
   end Parse_Payload;

   procedure Parse_Message (Data : in Byte_Array; Message : out Message_Type)
   is
      Manufacturer  : Manufacturer_Type;
      Payload_Start : Ada.Directories.File_Size :=
        2;  -- zero-based, defaults to the position after standard manufacturer ID
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

      Manufacturer :=
        (if Data (1) /= 0 then
           (Kind => Normal,
              Identifier => Data (1))
         else
            (Kind => Extended,
              Identifier_1 => Data (2),
              Identifier_2 => Data (3))
        );

      if Manufacturer.Kind = Extended then
         Payload_Start :=
           Payload_Start + 2; -- move past the extended manufacturer ID
      end if;

      -- Copy the payload until the last but one byte (zero-based)
      for I in Payload_Start .. Data'Last - 2 loop
         Payload_Data.Append (Data (I));
      end loop;

      Parse_Payload (Payload_Data, Payload);

      Message := (Manufacturer => Manufacturer, Payload => Payload);
   end Parse_Message;

   -- Computes the checksum byte for voice or cartridge data.
   function Checksum (Data : Data_Type) return Byte is
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
