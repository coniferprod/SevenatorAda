with Sixten; use Sixten;
with DX7.Voices; use DX7.Voices;

package DX7.Cartridges is
   type Cartridge_Type is record
      Voices : Voice_Array;
   end record;

   Cartridge_Data_Length : constant Positive := 4096;
   subtype Cartridge_Data_Type is Byte_Array (1 .. Cartridge_Data_Length);

   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector;

   procedure Parse_Cartridge (Data : in Cartridge_Data_Type; Cartridge : out Cartridge_Type);

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit (Cartridge : in Cartridge_Type; Result : out Cartridge_Data_Type);

end DX7.Cartridges;
