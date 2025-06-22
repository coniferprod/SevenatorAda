with Ada.Integer_Text_IO;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with DX7;
with DX7.Operators; use DX7.Operators;

package body DX7.XML is
   function Element (Name : String; Attributes : Attributes_Type; Is_Empty : Boolean := False) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Append (Result, +"<");
      Append (Result, +Name);
      Append (Result, +" ");

      for Attr in Attributes.Iterate loop
         Append (Result, Attribute_Maps.Key (Attr) & "=""" & Attributes (Attr) & """ ");
      end loop;

      if Is_Empty then
         Append (Result, +"/>");
      else
         Append (Result, +">");
      end if;
      return Result;
   end Element;

   function Element (Name : String; Attributes : Attributes_Type; Content : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Result := Element (Name, Attributes);
      Append (Result, Content);
      return Result;
   end Element;

   -- Converts the Integer value to a string and trims any spaces around the result.
   function To_String (Value : Integer) return String is
      (Ada.Strings.Fixed.Trim (Integer'Image (Value), Ada.Strings.Both));

   -- Converts the Boolean value to a lowercase string.
   function To_String (Value : Boolean) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Value'Image);
   end To_String;

   -- Converts the LFO waveform value to a string with lowercase and dashes.
   function To_String (Value : DX7.Voices.LFO_Waveform_Type) return String is
      Name : String := DX7.Voices.LFO_Waveform_Type'Image (Value);
   begin
      -- Replace underscores with dashes
      for I in Name'First .. Name'Last loop
         if Name (I) = '_' then
            Name (I) := '-';
         end if;
      end loop;

      -- Convert to lower case and return
      return Ada.Characters.Handling.To_Lower (Name);
   end To_String;

   function To_String (Value : DX7.Operators.Operator_Mode) return String is
   begin
      if Value = DX7.Operators.Ratio then
         return "ratio";
      end if;
      return "fixed";
   end To_String;

   -- Converts the scaling curve type to a string +LIN, -LIN, +EXP, -EXP.
   function To_String (Value : DX7.Operators.Scaling_Curve_Type) return String is
      Result : String (1 .. 4);
   begin
      case Value.Sign is
         when DX7.Operators.Positive => Result (1) := '+';
         when DX7.Operators.Negative => Result (1) := '-';
      end case;

      case Value.Style is
         when Linear => Result (2 .. 4) := "LIN";
         when Exponential => Result (2 .. 4) := "EXP";
      end case;

      return Result;
   end To_String;

end DX7.XML;
