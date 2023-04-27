with Ada.Directories; use Ada.Directories;

package body DX7 is

    function Get_Data (Manufacturer : Manufacturer_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        case Manufacturer.Kind is
            when Development_Kind =>
                BV.Append (Development_Identifier);
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
        BV.Append (System_Exclusive_Initiator);
        BV.Append (Get_Data (Message.Manufacturer));
        BV.Append (Message.Payload);
        BV.Append (System_Exclusive_Terminator);
        return BV;
    end Get_Data;

    procedure Parse_Message (Data : in Byte_Array; Message : out Message_Type) is
        Manufacturer : Manufacturer_Type;
        Payload_Start : Ada.Directories.File_Size := 3;  -- default to the position after standard manufacturer ID
        Payload : Byte_Vector;
    begin
        if Data (1) /= System_Exclusive_Initiator then
            return;
        end if;

        Manufacturer := (case Data (2) is
            when Development_Identifier => (
                Kind => Development_Kind,
                Development_Identifier => Development_Identifier
            ),
            when 0 => (
                Kind => Extended_Kind,
                Extended_Identifier => (Data (2), Data (3), Data (4))
            ),
            when others => (
                Kind => Standard_Kind,
                Standard_Identifier => Data (2)
            )
        );

        if Manufacturer.Kind = Extended_Kind then
            Payload_Start := Payload_Start + 2; -- move past the extended manufacturer ID
        end if;

        -- Copy the payload until the last but one byte
        for I in Payload_Start .. Data'Last - 1 loop
            Payload.Append (Data (I));
        end loop;

        Message := (
            Manufacturer => Manufacturer,
            Payload => Payload
        );

    end Parse_Message;

end DX7;
