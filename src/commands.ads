package Commands is
    type Command_Type is (Dump, Cartridge, Voice);

    procedure Run_Dump (Name : String);
    procedure Run_Cartridge (Name : String);
    procedure Run_Voice (Name : String);

end Commands;
