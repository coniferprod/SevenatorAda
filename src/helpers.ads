with Ada.Sequential_IO;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Directories; use Ada.Directories;
with Interfaces;

package Helpers is

    type Byte is new Interfaces.Unsigned_8;
    type Byte_Array is array (File_Size range <>) of Byte;
    type Byte_Array_Access is access Byte_Array;

    package Byte_Vectors is
        new Ada.Containers.Vectors
            (Index_Type => Natural,
             Element_Type => Byte);

    subtype Byte_Vector is Helpers.Byte_Vectors.Vector;

    package Byte_IO is
        new Ada.Sequential_IO (Byte);

    procedure Delete is new Ada.Unchecked_Deallocation (Byte_Array, Byte_Array_Access);

    function Read_File(File_Name: String) return Byte_Array_Access;
    procedure Write_File(File_Name: String; Contents: Byte_Vector);

    procedure Read_All_Bytes (Name : String; Buffer : out Byte_Array);

    function Hex (B : Byte) return String;
    function Hex_Dump (Data : Byte_Vector) return String;

end Helpers;
