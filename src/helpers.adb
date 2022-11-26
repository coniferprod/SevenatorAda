with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Direct_IO;

package body Helpers is

    -- Adapted from https://stackoverflow.com/a/67644332
    function Read_File(File_Name: String) return Byte_Array_Access is
        package SIO renames Ada.Streams.Stream_IO;

        Binary_File_Size : File_Size := Ada.Directories.Size (File_Name);
        Binary_File_Data : Byte_Array_Access;
        S : SIO.Stream_Access;
        File : SIO.File_Type;

    begin
        -- Allocate memory from the heap
        Binary_File_Data := new Byte_Array (1 .. Binary_File_Size);

        SIO.Open (File, SIO.In_File, File_Name);
        S := SIO.Stream (File);

        -- Read the entire file into the buffer
        Byte_Array'Read (S, Binary_File_Data.all);

        SIO.Close (File);

        return Binary_File_Data;
    end Read_File;

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
