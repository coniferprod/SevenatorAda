with Ada.Numerics.Discrete_Random;

with Sixten; use Sixten;

package body DX7.Envelopes is

   -- Make new packages for random rate and level
   package Random_Rates is new Ada.Numerics.Discrete_Random (Rate_Type);
   package Random_Levels is new Ada.Numerics.Discrete_Random (Level_Type);

   function Data (Envelope : in Envelope_Type) return Byte_Vector is
      BV : Byte_Vector;
   begin
      for Rate of Envelope.Rates loop
         BV.Append (Byte (Rate));
      end loop;

      for Level of Envelope.Levels loop
         BV.Append (Byte (Level));
      end loop;

      return BV;
   end Data;

   procedure Emit (Envelope : in Envelope_Type; Data : out Envelope_Data_Type) is
   begin
      for I in Rate_Index loop
         Data (Integer (I)) := Byte (Envelope.Rates (I));
      end loop;

      for I in Level_Index loop
         Data (Integer (I) + Integer (Rate_Index'Last)) :=
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

   procedure Parse (Data : in Envelope_Data_Type; Result : out Envelope_Type) is
      R : Rate_Array;
      L : Level_Array;
   begin
      for I in Rate_Index loop
         R (I) := Rate_Type (Data (Integer (I)));
      end loop;

      for I in Level_Index loop
         L (I) := Level_Type (Data (Integer (I + 4)));
      end loop;

      Result := (Rates => R, Levels => L);
   end Parse;

end DX7.Envelopes;
