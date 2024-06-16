with Ada.Numerics.Discrete_Random;

package body DX7.Envelopes is

   -- Make new packages for random rate and level
   package Rand_Rate is new Ada.Numerics.Discrete_Random (Rate_Type);
   package Rand_Level is new Ada.Numerics.Discrete_Random (Level_Type);

   function Get_Data (Envelope : Envelope_Type) return Envelope_Data_Type is
      Data : Envelope_Data_Type;
   begin
      for I in Rate_Index loop
         Data (Integer (I)) := Byte (Envelope.Rates (I));
      end loop;

      for I in Level_Index loop
         Data (Integer (I) + Integer (Rate_Index'Last)) :=
           Byte (Envelope.Levels (I));
      end loop;

      return Data;
   end Get_Data;

   function Random_Envelope return Envelope_Type is
      Envelope  : Envelope_Type;
      Rate_Gen  : Rand_Rate.Generator;
      Level_Gen : Rand_Level.Generator;
   begin
      Rand_Rate.Reset (Rate_Gen);
      for I in Rate_Index loop
         Envelope.Rates (I) := Rand_Rate.Random (Rate_Gen);
      end loop;

      Rand_Level.Reset (Level_Gen);
      for I in Level_Index loop
         Envelope.Levels (I) := Rand_Level.Random (Level_Gen);
      end loop;

      return Envelope;
   end Random_Envelope;

   procedure Parse (Data : in Envelope_Data_Type; EG : out Envelope_Type) is
      R : Rate_Array;
      L : Level_Array;
   begin
      for I in Rate_Index loop
         R (I + 1) := Rate_Type (Data (Integer (I)));
      end loop;

      for I in Level_Index loop
         L (I + 1) := Level_Type (Data (Integer (I + 4)));
      end loop;

      --EG := (Rates => Data (0..3), Levels => (Data (4..7)));
      EG := (Rates => R, Levels => L);
   end Parse;

end DX7.Envelopes;
