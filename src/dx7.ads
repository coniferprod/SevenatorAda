with Sixten; use Sixten;

-- The various parts of the data model are split into
-- child packages. "If your root pkg needs the child
-- in its spec then you have a design problem." (Jeffrey R. Carter)

package DX7 is
   Parse_Error : exception;

   type Coarse_Type is range 0 .. 31;
   type Fine_Type is range 0 .. 99;
   type Detune_Type is range -7 .. 7;

   type Algorithm_Type is range 1 .. 32;
   type Depth_Type is range 0 .. 7;
   type Sensitivity_Type is range 0 .. 3;

   -- Helper function to increment an integer value by the given amount.
   procedure Inc (I : in out Integer; Amount : in Integer := 1);

   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector;
   function Hex_Dump (Data : Byte_Array) return String;
   function Slice (BV : Byte_Vector; Start_Index : Natural; End_Index : Natural) return Byte_Vector;

end DX7;
