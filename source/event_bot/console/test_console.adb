with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Wide_Wide_Latin_1;
with League.Strings;
use League.Strings;
with Event_Console.Commands;

procedure Test_Console is

    function Read_String return Universal_String;

    function Read_String return Universal_String is
        Command_String : Universal_String;
        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
        End_Command_String : constant Wide_Wide_String := "end command";
    begin
        loop
            Put ("> ");
            declare
                Line : constant Wide_Wide_String := Get_Line;
            begin
                Command_String.Append (Line);
                Command_String.Append (Lf);

                exit when Line = End_Command_String;
            end;
        end loop;

        Put_Line ("Command string entered:");
        Put_Line (To_Wide_Wide_String (Command_String));

        return Command_String;
    end Read_String;

    Current_Command : Event_Console.Commands.Command;
    Command_String : Universal_String;
begin
    Put_Line ("This is a testing program.");
    Put_Line ("Write command to parse.");

    loop
        Command_String := Read_String;

        Put_Line ("--  Parsing command.");
        Current_Command.Initialize (Command_String);
        Put_Line ("--  Parsed:");
        Put_Line (Current_Command.To_Wide_Wide_String);

        exit when Current_Command.Is_Name (Event_Console.Commands.Bot_End);
    end loop;
end Test_Console;
