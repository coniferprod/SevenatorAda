package Commands is
   type Command_Type is (List, Dump, Cartridge, Voice, To_XML);

   procedure Run_List (Name : String);
   procedure Run_Dump (Name : String);
   procedure Run_Cartridge (Name : String);
   procedure Run_Voice (Name : String);
   procedure Run_To_XML (Name : String);

end Commands;
