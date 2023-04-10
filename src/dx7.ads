with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Helpers; use Helpers;

-- The various parts of the data model are split into 
-- child packages. "If your root pkg needs the child
-- in its spec then you have a design problem." (Jeffrey R. Carter)

package DX7 is
    -- The definitions are based on the example set by 
    -- AdaCore's "Introduction to Ada", section "Strongly typed language":
    -- https://learn.adacore.com/courses/intro-to-ada/chapters/strongly_typed_language.html#integers

    type Byte_Triplet is array (1 .. 3) of Byte;

    -- Use a variant record to describe the manufacturer
    -- in a MIDI System Exclusive Message.
    type Manufacturer_Kind is (Development_Kind, Standard_Kind, Extended_Kind);
    type Manufacturer_Type (Kind : Manufacturer_Kind := Development_Kind) is
        record
            case Kind is
                when Development_Kind => Development_Identifier : Byte;
                when Standard_Kind => Standard_Identifier : Byte;
                when Extended_Kind => Extended_Identifier : Byte_Triplet;
            end case;
        end record;

    type Message_Type is record
        Manufacturer : Manufacturer_Type;
        Payload : Byte_Vector;
    end record;

    -- Use overloading by argument to define Get_Data for each type as required
    function Get_Data (Manufacturer : Manufacturer_Type) return Byte_Vector;
    function Get_Data (Message : Message_Type) return Byte_Vector;

end DX7;
