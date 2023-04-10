with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with Helpers; use Helpers;

package body DX7 is

    function Get_Data (Manufacturer : Manufacturer_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        case Manufacturer.Kind is
            when Development_Kind =>
                BV.Append (Helpers.Byte (16#7D#));
            when Standard_Kind =>
                BV.Append (Manufacturer.Standard_Identifier);
            when Extended_Kind =>
                BV.Append (Manufacturer.Extended_Identifier (1));
                BV.Append (Manufacturer.Extended_Identifier (2));
                BV.Append (Manufacturer.Extended_Identifier (3));
        end case;
        return BV;
    end Get_Data;

    function Get_Data (Message : Message_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        BV.Append (Byte (16#F0#));
        BV.Append (Get_Data (Message.Manufacturer));
        BV.Append (Message.Payload);
        BV.Append (Byte (16#F7#));
        return BV;
    end Get_Data;

end DX7;
