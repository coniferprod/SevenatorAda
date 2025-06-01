with Sixten; use Sixten;

-- The various parts of the data model are split into
-- child packages. "If your root pkg needs the child
-- in its spec then you have a design problem." (Jeffrey R. Carter)

package DX7 is
   pragma Elaborate_Body;

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
end DX7;
