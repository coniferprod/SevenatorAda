with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Commands;
with Sixten;
with Sixten.Manufacturers;

procedure Sevenator is
   package CLI renames Ada.Command_Line;

begin
   if CLI.Argument_Count < 2 then
      Put_Line ("Usage: sevenator command filename");
      Put_Line ("  list = list the voice names of cartridge <filename>");
      Put_Line ("  dump = show contents of DX7 file <filename>");
      Put_Line ("  cartridge = make new cartridge with random voices and write to <filename>");
      Put_Line ("  voice = make new random voice and write to <filename>");
      Put_Line ("  to_xml = convert cartridge in <filename> to XML format");
      return;
   end if;

   declare
      Command   : constant String := CLI.Argument (1);
      File_Name : constant String := CLI.Argument (2);
   begin
      case Commands.Command_Type'Value (Command) is
         when Commands.List =>
            Commands.Run_List (File_Name);
         when Commands.Dump =>
            Commands.Run_Dump (File_Name);
         when Commands.Cartridge =>
            Commands.Run_Cartridge (File_Name);
         when Commands.Voice =>
            Commands.Run_Voice (File_Name);
         when Commands.To_XML =>
            Commands.Run_To_XML (File_Name);
      end case;
   end;
end Sevenator;
