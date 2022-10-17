with Ada.Sequential_IO;
with Ada.Containers.Vectors;

package Helpers is

    type Byte is mod 2**8;

    package Byte_Vectors is new
        Ada.Containers.Vectors
            (Index_Type => Natural,
             Element_Type => Byte);

    package Byte_IO is
        new Ada.Sequential_IO (Byte);

    procedure Write_File(File_Name: String; Contents: Byte_Vectors.Vector);

end Helpers;
