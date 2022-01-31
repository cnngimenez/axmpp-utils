--  event_bot.adb ---

--  Copyright 2020 cnngimenez
--
--  Author: cnngimenez

--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Characters.Conversions;
use Ada.Characters.Conversions;
with Ada.Environment_Variables;

with Event_Sessions;
with Event_Handlers;
with XMPP.Logger;

with Configs;
with Pipe_Manager;
use Pipe_Manager;

with League.Strings;
use League.Strings;

with Event_Console.Commands;

procedure Event_Bot is

    function S2Us (S : String) return Universal_String;

    function S2Us (S : String) return Universal_String is
    begin
        return To_Universal_String
          (To_Wide_Wide_String (S));
    end S2Us;

    Home_Path : constant String := Ada.Environment_Variables.Value ("HOME");
    Config : Configs.Config_Type;
    Session : constant not null Event_Sessions.Session_Access :=
      new Event_Sessions.Session;
    Handler : constant not null Event_Handlers.Event_Handler_Access :=
      new Event_Handlers.Event_Handler;

    Send_To : Universal_String;

    Current_Command : Event_Console.Commands.Command;
    Command_String, Line : Universal_String;
    End_Command_String : constant Universal_String := S2Us ("end command");

    Output_Pipe, Input_Pipe : Pipe_Type;

    Startup_Pipe_Message : constant String := "Starting main loop...";
    Dry_Run : Boolean := False;
begin
    if Argument_Count < 3 then
        Put_Line ("Synopsis:");
        Put_Line ("    bin/event_bot JID input_pipefile output_pipefile ");
        New_Line;
        Put_Line ("Connect to the configured XMPP account and wait for " &
                    "commands from the input_pipefile.");
        Put_Line ("See source/event_bot/readme.org for more information.");
        return;
    end if;

    Put_Line ("Loading config from " & Home_Path
                &  "/.config/axmpp-utils/connection.cfg");
    Config.Load (Home_Path & "/.config/axmpp-utils/connection.cfg");
    Handler.Set_Config (Config);
    Put_Line ("Config loaded.");

    if Argument_Count >= 4 and then Argument (4) = "dry-run" then
        Dry_Run := True;
    end if;

    XMPP.Logger.Enable_Debug;
    Session.Initialize;

    Session.Set_Stream_Handler (Handler);
    Handler.Set_Session_Object (Session);

    Session.Set_Host (Config.Host);
    Session.Set_JID (Config.JID);
    Session.Set_Password (Config.Password);
    Session.Set_Resource (Config.Resource_Name);

    Session.Set_Config (Config);
    Handler.Set_Config (Config);
    Send_To := S2Us (Argument (1));
    Session.Set_To_JID (Send_To);
    Handler.Set_To_JID (Send_To);

    if Dry_Run then
        Put_Line ("Session: Opening not possible: dry run is in effect.");
    else
        Put_Line ("Session: Opening...");
        Session.Open;
    end if;

    Input_Pipe.Initialize (Argument (2), Input_Only);
    Put_Line ("Input pipe: " & Argument (2));
    Output_Pipe.Initialize (Argument (3), Output_Only);
    Put_Line ("Output file: " & Argument (3));

    Handler.Set_Output_Pipe (Output_Pipe);
    Output_Pipe.Write_Message (Startup_Pipe_Message);

    while not Session.Has_Ended loop
        Ada.Wide_Wide_Text_IO.Put_Line ("Input pipe: waiting for input");
        Line := Input_Pipe.Attend_Pipe;
        Ada.Wide_Wide_Text_IO.Put_Line ("Input pipe> " &
                                          To_Wide_Wide_String (Line));
        Command_String.Append (Line);

        --  if Line = End_Command_String then
        --      Current_Command.Initialize (Command_String);
        --      Current_Command.Run (Session, Output_Pipe);
        --      Command_String.Clear;
        --  end if;

        exit when Current_Command.Is_Name (Event_Console.Commands.Bot_End);
    end loop;

    Put_Line ("End");
end Event_Bot;

--  Local Variables:
--  indent-tabs-mode: nil
--  End:
