package body DX7.System_Exclusive is

   function Emit (Manufacturer : Manufacturer_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      case Manufacturer.Kind is
         when Normal =>
            BV.Append (Manufacturer.Identifier);
         when Extended =>
            BV.Append (0);  -- start of extended identifier
            BV.Append (Manufacturer.Identifier_1);
            BV.Append (Manufacturer.Identifier_2);
      end case;
      return BV;
   end Get_Data;

   function Emit (Message : Message_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      BV.Append (System_Exclusive_Initiator);
      BV.Append (Emit (Message.Manufacturer));
      BV.Append (Message.Payload);
      BV.Append (System_Exclusive_Terminator);
      return BV;
   end Get_Data;

   procedure Parse_Payload (Data : in Byte_Vector; Payload : out Payload_Type) is
      Data_Size : Natural;
   begin
      Payload.Channel := Data (0) + 1;
      Payload.Format := (if Data (1) = 0 then Voice else Cartridge);
      Payload.Byte_Count := Data (2) * 256 + Data (3);

      if Payload.Format = Voice then
         Data_Size := 155;
      else
         Data_Size := 4096;
      end if;

      declare
         Patch_Data : Data_Type (0 .. Data_Size - 1);
         Index : Natural := 0;
      begin
         for I in 4 .. Data.Last_Index - 1 loop
            Patch_Data (Index) := Data (I);
         end loop;
         Payload.Data := Patch_Data;
      end;

      Payload.Checksum := Data (Data.Last_Index);
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

end DX7.System_Exclusive;
