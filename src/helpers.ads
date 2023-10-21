with Ada.Sequential_IO;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Directories; use Ada.Directories;
with Interfaces;

package Helpers is
    -- Define a type for an 8-bit byte compatible with the outside world.
    type Byte is new Interfaces.Unsigned_8;

    -- An array of 8-bit bytes.
    type Byte_Array is array (File_Size range <>) of Byte;
    type Byte_Array_Access is access Byte_Array;

    package Byte_Vectors is
        new Ada.Containers.Vectors
            (Index_Type => Natural,
             Element_Type => Byte);

    subtype Byte_Vector is Helpers.Byte_Vectors.Vector;

    -- Unconstrained array type for various patch data components
    type Data_Type is array (Natural range <>) of Byte;

    package Byte_IO is
        new Ada.Sequential_IO (Byte);

    procedure Delete is new Ada.Unchecked_Deallocation (Byte_Array, Byte_Array_Access);

    procedure Write_File(Name : String; Contents: Byte_Vector);
    procedure Read_File (Name : String; Contents : out Byte_Array);

    function Hex (B : Byte) return String;
    function Hex_Dump (Data : Byte_Vector) return String;

    function Checksum (Data : Data_Type) return Byte;

    -- MIDI note number (7 bits)
    type MIDI_Note_Type is range 0 .. 127;

    -- MIDI channel number
    type MIDI_Channel_Type is range 1 .. 16;

    -- Name of MIDI note.
    subtype Note_Name is String (1 .. 2);

    function Get_Note_Name (Note_Number : in MIDI_Note_Type)
                            return Note_Name;
end Helpers;
