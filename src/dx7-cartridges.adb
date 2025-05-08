with Ada.Text_IO;

package body DX7.Cartridges is

   procedure Parse (Data : in Cartridge_Data_Type; Cartridge : out Cartridge_Type) is
      Packed_Voice_Data : Packed_Voice_Data_Type;
      Voice_Data : Voice_Data_Type;
      Voice : Voice_Type;
      Offset : Natural;
   begin
      --Ada.Text_IO.Put_Line ("Parse_Cartridge: Data = " &
      --   Natural'Image (Data'First) & " .. " & Natural'Image (Data'Last));
      Offset := 1;
      for I in Voice_Index loop
         Ada.Text_IO.Put_Line ("VOICE " & Integer'Image (Integer (I)));
         Packed_Voice_Data := Data (Offset .. Offset + Packed_Voice_Data_Length - 1);
         Unpack_Voice (Data => Packed_Voice_Data, Result => Voice_Data);
         Ada.Text_IO.Put_Line ("voice data (" & Integer'Image (Voice_Data'Length) & " bytes) = " & Hex_Dump (Voice_Data));
         Parse (Data => Voice_Data, Voice => Voice);
         Cartridge.Voices (I) := Voice;
         Offset := Offset + Packed_Voice_Data_Length;
         Ada.Text_IO.New_Line;
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
