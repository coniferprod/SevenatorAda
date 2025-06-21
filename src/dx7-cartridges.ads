with Sixten; use Sixten;
with DX7.Voices; use DX7.Voices;
with DX7.XML; use DX7.XML;

package DX7.Cartridges is
   type Cartridge_Type is record
      Voices : Voice_Array;
   end record;

   Cartridge_Data_Length : constant Positive := 4096;
   subtype Cartridge_Data_Type is Byte_Array (1 .. Cartridge_Data_Length);

   procedure Parse (Data : in Byte_Array; Result : out Cartridge_Type);

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit (Cartridge : in Cartridge_Type; Result : out Cartridge_Data_Type);

   procedure To_XML (Cartridge: in Cartridge_Type; Result : out DX7.XML.Document_Type);
end DX7.Cartridges;
