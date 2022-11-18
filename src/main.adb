with Ada.Text_IO;
with Ada.Command_Line;
with Helpers;
with DX7; use DX7;

procedure Main is
    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

    EG : Envelope_Type;
    KLS : Keyboard_Level_Scaling_Type;
    V : Voice_Type;
    Op1 : Operator_Type;
    Lfo : LFO_Type;
    Cartridge : Cartridge_Type;

begin
    for i in 1 .. CLI.Argument_Count loop
        IO.Put_Line (Item => CLI.Argument (Number => i));
    end loop;

    EG := New_Envelope ((99, 99, 99, 99), (99, 99, 99, 0));

    KLS := (
        Breakpoint => 60 - 21,
        Left_Depth => 0,
        Right_Depth => 0,
        Left_Curve => Lin_Neg_Curve,
        Right_Curve => Lin_Neg_Curve
    );

    Op1 := (
        EG => EG,
        Kbd_Level_Scaling => KLS,
        Kbd_Rate_Scaling => 0,
        Amp_Mod_Sens => 0,
        Key_Vel_Sens => 0,
        Output_Level => 0,
        Mode => Ratio,
        Coarse => 1,
        Fine => 0,
        Detune => 0
    );

    Lfo := (
        Speed => 0,
        LFO_Delay => 0,
        PMD => 0,
        AMD => 0,
        Sync => True,
        Wave => Triangle,
        Pitch_Mod_Sens => 0
    );

    V := (
        Name => "INIT VOICE",
        Operators => (Op1, Op1, Op1, Op1, Op1, Op1),
        Pitch_Envelope => EG,
        Algorithm => 1,
        Feedback => 0,
        Osc_Sync => False,
        LFO => Lfo
    );

    for i in Voice_Index loop
        Cartridge.Voices (i) := V;
    end loop;

    Helpers.Write_File ("cartridge.bin", Get_Data (Cartridge));

end Main;
