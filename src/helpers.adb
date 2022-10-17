package body Helpers is

    procedure Write_File is
        package Byte_IO is
            new Ada.Sequential_IO (Byte);
        use Byte_IO;

        F : Byte_IO.File_Type;
        File_Name : constant String := "allbytes.bin";
    begin

        Create (F, Out_File, File_Name);
        Write (F, 16#F0#);
        Write (F, 16#F7#);
        Close (F);

    end Write_File;

end Helpers;
