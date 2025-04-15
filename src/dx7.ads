with Sixten; use Sixten;

-- The various parts of the data model are split into
-- child packages. "If your root pkg needs the child
-- in its spec then you have a design problem." (Jeffrey R. Carter)

package DX7 is
   pragma Elaborate_Body;

   Parse_Error : exception;

   type Coarse_Type is range 0 .. 31;
   type Fine_Type is range 0 .. 99;
   type Detune_Type is range -7 .. 7;

   type Algorithm_Type is range 1 .. 32;
   type Depth_Type is range 0 .. 7;
   type Sensitivity_Type is range 0 .. 3;

end DX7;
