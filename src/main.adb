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

begin
    for i in 1 .. CLI.Argument_Count loop
        IO.Put_Line (Item => CLI.Argument (Number => i));
    end loop;

    EG := New_Envelope ((99, 99, 99, 99), (99, 99, 99, 0));

    Helpers.Write_File("eg.bin", Get_Envelope_Data(EG));

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

    V.Operators := (Op1, Op1, Op1, Op1, Op1, Op1);

end Main;
