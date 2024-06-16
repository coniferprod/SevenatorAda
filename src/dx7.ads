with Helpers; use Helpers;

-- The various parts of the data model are split into
-- child packages. "If your root pkg needs the child
-- in its spec then you have a design problem." (Jeffrey R. Carter)

package DX7 is
   type Byte_Triplet is array (1 .. 3) of Byte;

   -- Use a variant record to describe the manufacturer
   -- in a MIDI System Exclusive Message.
   type Manufacturer_Kind is (Development_Kind, Standard_Kind, Extended_Kind);
   type Manufacturer_Type (Kind : Manufacturer_Kind := Development_Kind) is
   record
      case Kind is
         when Development_Kind =>
            Development_Identifier : Byte;
         when Standard_Kind =>
            Standard_Identifier : Byte;
         when Extended_Kind =>
            Extended_Identifier : Byte_Triplet;
      end case;
   end record;

   -- MIDI System Exclusive message
   type Message_Type is record
      Manufacturer : Manufacturer_Type;
      Payload      : Byte_Vector;
   end record;

   System_Exclusive_Initiator  : constant Byte := 16#F0#;
   System_Exclusive_Terminator : constant Byte := 16#F7#;
   Development_Identifier      : constant Byte := 16#7D#;

   -- Get the MIDI System Exclusive data for manufacturer or message.
   -- Use overloading by argument to define Get_Data for each type as required.
   function Get_Data (Manufacturer : Manufacturer_Type) return Byte_Vector;
   function Get_Data (Message : Message_Type) return Byte_Vector;

   procedure Parse_Message (Data : in Byte_Array; Message : out Message_Type);

end DX7;
