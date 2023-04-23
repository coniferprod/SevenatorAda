with Ada.Streams.Stream_IO;

package body Helpers is
    procedure Write_File (Name : String; Contents : Byte_Vector) is
        use Byte_IO;

        F : File_Type;
    begin
        Create (F, Out_File, Name);

        for B of Contents loop
            Write (F, B);
        end loop;

        Close (F);
    end Write_File;

    procedure Read_File (Name : String; Contents : out Byte_Array) is
        package SIO renames Ada.Streams.Stream_IO;

        Input_File : SIO.File_Type;
        Input_Stream : SIO.Stream_Access;
        Index : File_Size := 0;
        B : Byte;
    begin
        SIO.Open (Input_File, SIO.In_File, Name);

        Input_Stream := SIO.Stream (Input_File);
        while not SIO.End_Of_File (Input_File) loop
            Byte'Read (Input_Stream, B);
            Contents (Index) := B;
            Index := Index + 1;
        end loop;

        SIO.Close (Input_File);
    end Read_File;

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

    -- Generates a hex dump of the byte vector as a string.
    function Hex_Dump (Data : Byte_Vector) return String is
        Result : Unbounded_String := Null_Unbounded_String;
    begin
        for B of Data loop
            Result := Result & Hex (B) & " ";
        end loop;
        return To_String (Result);
    end Hex_Dump;

    -- Computes the checksum byte for voice or cartridge data.
    function Checksum (Data : Data_Type) return Byte is
        Sum : Byte := 0;
        Result : Byte;
    begin
        for B of Data loop
            Sum := Sum + B;
        end loop;

        Result := Sum and 16#FF#;
        Result := not Result;
        Result := Result and 16#7F#;
        Result := Result + 1;
        return Result;
    end Checksum;

    type Note_Index is range 0 .. 11;
    type Note_Names_Type is array (Note_Index) of Note_Name;

    Note_Names : constant Note_Names_Type :=
        (To_Unbounded_String ("C"),
        To_Unbounded_String ("C#"),
        To_Unbounded_String ("D"),
        To_Unbounded_String ("D#"),
        To_Unbounded_String ("E"),
        To_Unbounded_String ("F"),
        To_Unbounded_String ("F#"),
        To_Unbounded_String ("G"),
        To_Unbounded_String ("G#"),
        To_Unbounded_String ("A"),
        To_Unbounded_String ("A#"),
        To_Unbounded_String ("B"));

    function Get_Note_Name (Note_Number : MIDI_Note_Type;
                            Naming : Octave_Naming_Type)
                            return Note_Name is
        Octave_Offset : constant Integer :=
            (case Naming is
             when Roland => -1,
             when Yamaha => -2);
        Octave : constant Integer := 
            (Integer (Note_Number) / 12) + Octave_Offset;
    begin
        return Note_Names (Note_Index (Note_Number mod 12)) & Octave'Image;
    end Get_Note_Name;

end Helpers;
