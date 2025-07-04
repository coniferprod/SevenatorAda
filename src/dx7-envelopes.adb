with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

package body DX7.Envelopes is
   -- Make new packages for random rate and level
   package Random_Rates is new Ada.Numerics.Discrete_Random (Rate_Type);
   package Random_Levels is new Ada.Numerics.Discrete_Random (Level_Type);

   procedure Emit (Envelope : in Envelope_Type; Data : out Envelope_Data_Type) is
   begin
      for I in Rate_Index loop
         Data (I) := Byte (Envelope.Rates (I));
      end loop;

      for I in Level_Index loop
         Data (I + Rate_Index'Last) :=
           Byte (Envelope.Levels (I));
      end loop;
   end Emit;

   function Random_Envelope return Envelope_Type is
      Envelope  : Envelope_Type;
      Rate_Gen  : Random_Rates.Generator;
      Level_Gen : Random_Levels.Generator;
   begin
      Random_Rates.Reset (Rate_Gen);
      for I in Rate_Index loop
         Envelope.Rates (I) := Random_Rates.Random (Rate_Gen);
      end loop;

      Random_Levels.Reset (Level_Gen);
      for I in Level_Index loop
         Envelope.Levels (I) := Random_Levels.Random (Level_Gen);
      end loop;

      return Envelope;
   end Random_Envelope;

   procedure Put_Offset (Offset : Natural; Message : String; Data_Offset : Natural) is
   begin
      if Debugging then
         Ada.Text_IO.Put_Line ("offset = " & Integer'Image (Offset) & " " & Message
            & " from " & Integer'Image (Data_Offset));
      end if;
   end Put_Offset;

   procedure Parse (Data : in Byte_Array; Result : out Envelope_Type) is
      B : Byte;
      Value : Integer;
   begin
      if Debugging then
         Put_Byte_Array_Information (Data, "EG");
         Ada.Text_IO.Put_Line (Hex_Dump (Data));
      end if;

      for I in Rate_Index loop
         B := Data (I);
         Value := Integer (B);
         if Value in Rate_Type then
            Result.Rates (I) := Rate_Type (Value);
         else
            raise Parse_Error 
               with Make_Range_Exception_Message (Text => "Error parsing envelope rate", 
                  Actual => Value, First => Rate_Type'First, Last => Rate_Type'Last, 
                  Offset => I);
         end if;
      end loop;

      for I in Level_Index loop
         B := Data (I + 4);
         Value := Integer (B);
         if Value in Level_Type then
            Result.Levels (I) := Level_Type (Value);
         else
            raise Parse_Error
               with Make_Range_Exception_Message (Text => "Error parsing envelope level", 
                  Actual => Value, First => Level_Type'First, Last => Level_Type'Last, 
                  Offset => I);
         end if;
      end loop;
   end Parse;

end DX7.Envelopes;
