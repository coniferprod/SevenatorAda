with Helpers; use Helpers;
with Ada.Numerics.Discrete_Random;

package body DX7.Envelopes is

    -- Make new packages for random rate and level
    package Rand_Rate is new Ada.Numerics.Discrete_Random(Rate_Type);
    package Rand_Level is new Ada.Numerics.Discrete_Random(Level_Type);
    
    function Get_Data (Envelope : Envelope_Type) return Byte_Vector is
        BV : Byte_Vector;
    begin
        BV.Append (Byte (Envelope.Rates (1)));
        BV.Append (Byte (Envelope.Rates (2)));
        BV.Append (Byte (Envelope.Rates (3)));
        BV.Append (Byte (Envelope.Rates (4)));

        BV.Append (Byte (Envelope.Levels (1)));
        BV.Append (Byte (Envelope.Levels (2)));
        BV.Append (Byte (Envelope.Levels (3)));
        BV.Append (Byte (Envelope.Levels (4)));

        return BV;
    end Get_Data;

    function Random_Envelope return Envelope_Type is
        Envelope : Envelope_Type;
        Rate_Gen : Rand_Rate.Generator;
        Level_Gen : Rand_Level.Generator;
    begin
        Rand_Rate.Reset (Rate_Gen);
        Envelope.Rates (1) := Rand_Rate.Random (Rate_Gen);
        Envelope.Rates (2) := Rand_Rate.Random (Rate_Gen);
        Envelope.Rates (3) := Rand_Rate.Random (Rate_Gen);
        Envelope.Rates (4) := Rand_Rate.Random (Rate_Gen);

        Rand_Level.Reset (Level_Gen);
        Envelope.Levels (1) := Rand_Level.Random (Level_Gen);
        Envelope.Levels (2) := Rand_Level.Random (Level_Gen);
        Envelope.Levels (3) := Rand_Level.Random (Level_Gen);
        Envelope.Levels (4) := Rand_Level.Random (Level_Gen);

        return Envelope;
    end Random_Envelope;

end DX7.Envelopes;
