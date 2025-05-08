with Sixten; use Sixten;
with Sixten.Manufacturers; use Sixten.Manufacturers;
with Sixten.Messages; use Sixten.Messages;

with DX7.Voices; use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;

package DX7.System_Exclusive is

   type Format_Type is (Voice, Cartridge);
   for Format_Type use (Voice => 1, Cartridge => 9);

   type Status_Type is (Voice_Cartridge, Parameter);
   for Status_Type use (Voice_Cartridge => 0, Parameter => 1);

   type Header_Type is record
      Sub_Status : Status_Type;
      Channel : MIDI_Channel_Type;
      Format : Format_Type;
   end record;

   -- The byte count in the header is a 14-bit number distributed evenly over two bytes
   -- voice=155 (00000010011011 = 0x009B, appears as "01 1B")
   -- cartridge=4096 (01000000000000 = 0x1000, appears as "20 00")
   -- We have the format information, so we can just generate the byte count as needed.

   Header_Size : constant := 4;
   subtype Header_Data_Type is Byte_Array (1 .. Header_Size);

   procedure Emit (Header : in Header_Type; Result : out Header_Data_Type);

   procedure Put (Header : Header_Type);

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

   --function Emit_Payload (Payload : Payload_Type) return Byte_Vector;
   procedure Emit (Payload : in Payload_Type; Result : out Byte_Array);

   procedure Parse (Data : in Byte_Array; Header : out Header_Type);
   --procedure Parse_Payload (Data : in Byte_Vector; Payload : out Payload_Type);
   procedure Parse (Data : in Byte_Array; Payload : out Payload_Type);
   function Checksum (Data : Byte_Array) return Byte;
end DX7.System_Exclusive;
