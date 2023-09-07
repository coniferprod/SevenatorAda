with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Helpers; use Helpers;
with DX7; use DX7;
with Commands; use Commands;

procedure Sevenator is
    package CLI renames Ada.Command_Line;

begin
    -- Test random name generation
    --for I in 1 .. 10 loop
    --    Ada.Text_IO.Put_Line (Random_Voice_Name);
    --end loop;

    -- Example of echoing the command line arguments:
    --for i in 1 .. CLI.Argument_Count loop
    --    IO.Put_Line (Item => CLI.Argument (Number => i));
    --end loop;

    if CLI.Argument_Count < 2 then
        Put_Line ("Usage: sevenator command filename");
        Put_Line ("  dump = show contents of DX7 file <filename>");
        Put_Line ("  cartridge = make new cartridge with random voices and write to <filename>");
        Put_Line ("  voice = make new random voice and write to <filename>");
        return;
    end if;

    declare
        Command : constant String := CLI.Argument (1);
        Name : constant String := CLI.Argument (2);
    begin
        case Command_Type'Value (Command) is
            when Dump => Run_Dump (Name);
            when Cartridge => Commands.Run_Cartridge (Name);
            when Voice => Commands.Run_Voice (Name);
            --when others => Put_Line ("Unknown command: " & Command);
        end case;
    end;
end Sevenator;
