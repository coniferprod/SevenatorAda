with Ada.Text_IO;

with Ada.Text_IO;

package body DX7.Cartridges is
   procedure Parse (Data : in Byte_Array; Result : out Cartridge_Type) is
      Packed_Voice_Data : Byte_Array (1 .. Packed_Voice_Data_Length);
      Voice_Data : Byte_Array (1 .. Voice_Data_Length);
      Voice_First, Voice_Last : Natural;
   begin
      if Debugging then
         Ada.Text_IO.Put_Line ("Parsing cartridge...");   
         Put_Byte_Array_Information (Data, "Cartridge data");
      end if;

      Voice_First := 1;
      for I in Voice_Index loop
         Voice_Last := Voice_First + Packed_Voice_Data_Length - 1;
         Packed_Voice_Data := Data (Voice_First .. Voice_Last);
         if Debugging then
            Ada.Text_IO.Put_Line ("Parsing voice #" & I'Image);
            Put_Byte_Array_Information (Packed_Voice_Data, "Packed voice data");
         end if;

         Unpack_Voice (Data => Packed_Voice_Data, Result => Voice_Data);
         if Debugging then
            Put_Byte_Array_Information (Voice_Data, "Unpacked voice data");
         end if;
         Parse_Voice (Data => Voice_Data, Result => Result.Voices (I));
         Voice_First := Voice_First + Packed_Voice_Data_Length;
      end loop;
   end Parse;

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit (Cartridge : in Cartridge_Type; Result : out Cartridge_Data_Type) is
      Data : Cartridge_Data_Type;
      Offset : Natural;
   begin
      Offset := 1;
      for I in Voice_Index loop
         declare
            Original : Voice_Data_Type;
            Packed : Packed_Voice_Data_Type;
            Start_Index : Natural;
            End_Index : Natural;
         begin
            Emit (Cartridge.Voices (I), Original);
            Pack_Voice (Original, Packed);

            Start_Index := Offset;
            End_Index := Offset + Packed_Voice_Data_Length;
            Result (Start_Index .. End_Index) :=
               Packed (1 .. Packed_Voice_Data_Length);
            Offset := Offset + Packed_Voice_Data_Length;
         end;
      end loop;
   end Emit;

end DX7.Cartridges;
