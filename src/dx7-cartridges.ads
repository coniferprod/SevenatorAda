with DX7.Voices; use DX7.Voices;

package DX7.Cartridges is
   type Cartridge_Type is record
      Voices : Voice_Array;
   end record;

   Cartridge_Data_Length : constant Positive := 4_096;
   subtype Cartridge_Data_Type is Data_Type (1 .. Cartridge_Data_Length);

   procedure Parse (Data : in Cartridge_Data_Type; Cartridge : out Cartridge_Type);

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit
     (Cartridge : in Cartridge_Type; Data : out Cartridge_Data_Type);

end DX7.Cartridges;
