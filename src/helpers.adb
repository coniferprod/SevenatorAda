package body Helpers is

    procedure Write_File(File_Name: String; Contents: Byte_Vectors.Vector) is
        use Byte_IO;

        F : Byte_IO.File_Type;
        -- File_Name : constant String := "allbytes.bin";
    begin

        Create (F, Out_File, File_Name);

        for B of Contents loop
            Write (F, B);
        end loop;

        Close (F);

    end Write_File;

end Helpers;
