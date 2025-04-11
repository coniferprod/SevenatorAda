with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Commands;
with Sixten;
with Sixten.Manufacturers;

procedure Sevenator is
   package CLI renames Ada.Command_Line;

begin
   declare
      Manuf : Sixten.Manufacturers.Manufacturer_Type := Sixten.Manufacturers.Yamaha;
   begin
      Put_Line (Sixten.Manufacturers.Name (Manuf));
   end;

   if CLI.Argument_Count < 2 then
      Put_Line ("Usage: sevenator command filename");
      Put_Line ("  dump = show contents of DX7 file <filename>");
      Put_Line
        ("  cartridge = make new cartridge with random voices and write to <filename>");
      Put_Line ("  voice = make new random voice and write to <filename>");
      return;
   end if;

   declare
      Command   : constant String := CLI.Argument (1);
      File_Name : constant String := CLI.Argument (2);
   begin
      case Commands.Command_Type'Value (Command) is
         when Commands.Dump =>
            Commands.Run_Dump (File_Name);
         when Commands.Cartridge =>
            Commands.Run_Cartridge (File_Name);
         when Commands.Voice =>
            Commands.Run_Voice (File_Name);
      end case;
   end;
end Sevenator;
