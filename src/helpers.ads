with Ada.Sequential_IO;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
    type Data_Type is array (Positive range <>) of Byte;

    package Byte_IO is
        new Ada.Sequential_IO (Byte);

    procedure Delete is new Ada.Unchecked_Deallocation (Byte_Array, Byte_Array_Access);

    function Read_File(File_Name: String) return Byte_Array_Access;
    procedure Write_File(File_Name: String; Contents: Byte_Vector);

    procedure Read_All_Bytes (Name : String; Buffer : out Byte_Array);

    function Hex (B : Byte) return String;
    function Hex_Dump (Data : Byte_Vector) return String;

    function Checksum (Data : Data_Type) return Byte;

    -- MIDI note number (7 bits)
    type MIDI_Note_Type is range 0 .. 127;

    -- MIDI channel number
    type MIDI_Channel_Type is range 1 .. 16;

    -- Naming type, affects what MIDI note 60 is called.
    -- Roland calls is C4, while Yamaha calls it C3.
    type Octave_Naming_Type is (Roland, Yamaha);

    -- Name of MIDI note. Using unbounded string because it 
    -- needs to be variable length. 
    subtype Note_Name is Unbounded_String;

    function Get_Note_Name (Note_Number : MIDI_Note_Type;
                            Naming : Octave_Naming_Type)
                            return Note_Name;
end Helpers;
