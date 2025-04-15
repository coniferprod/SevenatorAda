with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

with Sixten; use Sixten;

package body DX7.Envelopes is
   Debugging : constant Boolean := True;

   -- Make new packages for random rate and level
   package Random_Rates is new Ada.Numerics.Discrete_Random (Rate_Type);
   package Random_Levels is new Ada.Numerics.Discrete_Random (Level_Type);

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

   procedure Put_Offset (Offset : Natural; Message : String; Data_Offset : Natural) is
   begin
      if Debugging then
         Ada.Text_IO.Put_Line ("offset = " & Integer'Image (Offset) & " " & Message
            & " from " & Integer'Image (Data_Offset));
      end if;
   end Put_Offset;

   procedure Parse_Envelope (Data : in Envelope_Data_Type; Result : out Envelope_Type) is
      R : Rate_Array;
      L : Level_Array;
      B : Byte;
      Offset : Natural;
   begin
      Ada.Text_IO.Put_Line ("EG data = " & Integer'Image (Data'First) & ".." 
         & Integer'Image (Data'Last));

      Offset := 1;
      Put_Offset (Offset, "Rates", Offset);
      for I in Rate_Index loop
         B := Data (Integer (I));
         R (I) := Rate_Type (B);
         Ada.Text_IO.Put_Line ("Rate I = " & Integer'Image (Integer (I)) & Integer'Image (Integer (B)));
      end loop;

      for I in Level_Index loop
         B := Data (Integer (I + 4));
         Ada.Text_IO.Put_Line ("Level I = " & Integer'Image (Integer (I)) & Integer'Image (Integer (B)));
         L (I) := Level_Type (B);
      end loop;

      Result := (Rates => R, Levels => L);
   end Parse_Envelope;

end DX7.Envelopes;
