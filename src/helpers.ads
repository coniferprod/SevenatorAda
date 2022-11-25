with Ada.Sequential_IO;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Helpers is

    type Byte is mod 2**8;

    package Byte_Vectors is
        new Ada.Containers.Vectors
            (Index_Type => Natural,
             Element_Type => Byte);

    subtype Byte_Vector is Helpers.Byte_Vectors.Vector;

    package Byte_IO is
        new Ada.Sequential_IO (Byte);

    procedure Write_File(File_Name: String; Contents: Byte_Vector);

    function Hex (B : Byte) return String;
    function Hex_Dump (Data : Byte_Vector) return String;

end Helpers;
