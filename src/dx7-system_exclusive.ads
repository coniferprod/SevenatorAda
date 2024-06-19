with DX7.Voices; use DX7.Voices;
with DX7.Cartridges; use DX7.Cartridges;

package DX7.System_Exclusive is

   type Format_Type is (Voice, Cartridge);
   for Format_Type use (Voice => 1, Cartridge => 9);

   type Payload_Type (Format : Format_Type) is record
      Channel : MIDI_Channel_Type;
      Byte_Count : Natural;
      Checksum : Byte;
      case Format is
         when Voice => Voice_Data : Voice_Data_Type;
         when Cartridge => Cartridge_Data : Cartridge_Data_Type;
      end case;
   end record;

   -- Use a variant record to describe the manufacturer
   -- in a MIDI System Exclusive Message.

   -- Would have used "Standard" instead of normal, but it
   -- causes trouble because it is the name of an Ada package.
   -- Using "Normal" instead.
   type Manufacturer_Kind is (Normal, Extended);
   type Manufacturer_Type (Kind : Manufacturer_Kind := Normal) is
   record
      case Kind is
         when Normal =>
            Identifier : Byte;
         when Extended =>
            Identifier_1 : Byte;
            Identifier_2 : Byte;
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

   -- Get the MIDI System Exclusive data for manufacturer or message.
   -- Use overloading by argument to define Get_Data for each type as required.
   function Emit (Manufacturer : Manufacturer_Type) return Byte_Vector;
   function Emit (Message : Message_Type) return Byte_Vector;

   procedure Parse_Message (Data : in Byte_Array; Message : out Message_Type);
   procedure Parse_Payload (Data : in Byte_Vector; Payload : out Payload_Type);

end DX7.System_Exclusive;
