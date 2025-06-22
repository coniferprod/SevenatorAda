with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with DX7;
with DX7.Voices;
with DX7.Operators;

package DX7.XML is

   pragma Elaborate_Body;

   package Line_Vectors is new
      Ada.Containers.Vectors (Index_Type => Natural, 
                              Element_Type => Ada.Strings.Unbounded.Unbounded_String);
      
   subtype Document_Type is Line_Vectors.Vector;
   
   package Attribute_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
         (Key_Type => String,
          Element_Type => String,
          Hash => Ada.Strings.Hash, 
          Equivalent_Keys => "=");

   subtype Attributes_Type is Attribute_Maps.Map;

   function To_String (Value : Integer) return String;
   function To_String (Value : Boolean) return String;
   function To_String (Value : DX7.Voices.LFO_Waveform_Type) return String;
   function To_String (Value : DX7.Operators.Operator_Mode) return String;
   function To_String (Value : DX7.Operators.Scaling_Curve_Type) return String;

   function Element (Name : String; Attributes : Attributes_Type; Is_Empty : Boolean := False) return Unbounded_String;
   function Element (Name : String; Attributes : Attributes_Type; Content : Unbounded_String) return Unbounded_String;

   Indent_Level : Natural := 0;
   Indent_Space_Count : Positive := 4;

   -- The number of lines in a document representing a cartridge:
   -- 1 XML prolog
   -- 2 root element (cartridge) start and end
   -- 2 voices element start and end
   -- 32 voices (69 lines each):
   --     voice start and end 2 lines
   --     peg 4 lines
   --     lfo 1 line
   --     operators: start and end 2 lines, then 10 lines each, so 2 + 6 * 10 = 62
   -- Total: 32*69 + 5 = 2213
   --type Document_Type is array (1 .. 32*69+5) of Ada.Strings.Unbounded.Unbounded_String;

   --  Convenience function to make unbounded strings.
   function "+" (S : String) return Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

end DX7.XML;