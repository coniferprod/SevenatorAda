with Sixten; use Sixten;
with Sixten.Manufacturers; use Sixten.Manufacturers;

with DX7.Voices; use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;

package DX7.System_Exclusive is

   type Format_Type is (Voice, Cartridge);
   for Format_Type use (Voice => 1, Cartridge => 9);

   type Header_Type is record
      Sub_Status : Byte; -- 0=voice/cartridge, 1=parameter
      Channel : MIDI_Channel_Type;
      Format : Format_Type;
      Byte_Count : Natural;  -- 14-bit number distributed evenly over two bytes
      -- voice=155 (00000010011011 = 0x009B, appears as "01 1B")
      -- cartridge=4096 (01000000000000 = 0x1000, appears as "20 00")
   end record;

   type Payload_Type (Format : Format_Type := Voice) is record
      Header : Header_Type;
      Checksum : Byte;
      case Format is
         when Voice =>
            Voice_Data : Voice_Data_Type;
         when Cartridge =>
            Cartridge_Data : Cartridge_Data_Type;
      end case;
   end record;

   -- MIDI System Exclusive message
   type Message_Type is record
      Manufacturer : Manufacturer_Type;
      Payload      : Payload_Type;
   end record;

   System_Exclusive_Initiator  : constant Byte := 16#F0#;
   System_Exclusive_Terminator : constant Byte := 16#F7#;
   Development_Identifier      : constant Byte := 16#7D#;

   function Emit (Message : Message_Type) return Byte_Vector;

   procedure Parse (Data : in Byte_Array; Header : out Header_Type);
   procedure Parse (Data : in Byte_Array; Message : out Message_Type);
   procedure Parse (Data : in Byte_Vector; Payload : out Payload_Type);

   function Checksum (Data : Byte_Array) return Byte;
end DX7.System_Exclusive;
