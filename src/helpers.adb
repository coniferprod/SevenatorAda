with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Helpers is

    procedure Write_File(File_Name: String; Contents: Byte_Vector) is
        use Byte_IO;

        F : Byte_IO.File_Type;
    begin
        Create (F, Out_File, File_Name);

        for B of Contents loop
            Write (F, B);
        end loop;

        Close (F);
    end Write_File;

    -- Influenced by: https://ada.tips/using-adasequential_io-to-create-simple-hexdump-utility.html
    function Hex (B : Byte) return String is
        Hex_Chars : constant array (Byte range 0 .. 15) of Character :=
            ('0', '1', '2', '3', '4', '5', '6', '7',
             '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
        Half_Byte_1 : constant Byte := B mod 16;
        Half_Byte_2 : constant Byte := B / 16;
    begin
        return Hex_Chars (Half_Byte_2) & Hex_Chars (Half_Byte_1);
    end Hex;

    function Hex_Dump(Data : Byte_Vector) return String is
        Result : Unbounded_String := Null_Unbounded_String;
    begin
        for B of Data loop
            Result := Result & Hex (B) & " ";
        end loop;
        return To_String (Result);
    end Hex_Dump;

end Helpers;
