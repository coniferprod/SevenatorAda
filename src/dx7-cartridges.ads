with Sixten; use Sixten;
with DX7.Voices; use DX7.Voices;

package DX7.Cartridges is
   type Cartridge_Type is record
      Voices : Voice_Array;
   end record;

   Cartridge_Data_Length : constant Positive := 4_096;
   subtype Cartridge_Data_Type is Byte_Array (0 .. Cartridge_Data_Length - 1);

   function To_Byte_Vector (Data : Cartridge_Data_Type) return Byte_Vector;

   procedure Parse (Data : in Cartridge_Data_Type; Cartridge : out Cartridge_Type);

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit (Cartridge : in Cartridge_Type; Result : out Cartridge_Data_Type);

end DX7.Cartridges;
