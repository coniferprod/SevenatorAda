with Sixten; use Sixten;

-- The various parts of the data model are split into
-- child packages. "If your root pkg needs the child
-- in its spec then you have a design problem." (Jeffrey R. Carter)

package DX7 is
   Parse_Error : exception;

   subtype Coarse_Type is Integer range 0 .. 31;
   subtype Fine_Type is Integer range 0 .. 99;
   subtype Detune_Type is Integer range -7 .. 7;

   subtype Algorithm_Type is Integer range 1 .. 32;
   subtype Depth_Type is Integer range 0 .. 7;
   subtype Sensitivity_Type is Integer range 0 .. 3;

   function Make_Range_Exception_Message (Text : String; Actual, First, Last : Integer; Offset : Natural) return String;
   function Make_Length_Exception_Message (Text : String; Actual, Expected : Integer; Offset : Natural) return String;

   Debugging : Boolean := True;
   -- Helper function to increment an integer value by the given amount.
   procedure Inc (I : in out Integer; Amount : in Integer := 1);

   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector;
   function Hex_Dump (Data : Byte_Array) return String;
   function Slice (BV : Byte_Vector; Start_Index : Natural; End_Index : Natural) return Byte_Vector;

end DX7;
