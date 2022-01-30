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

    function Get_Message (Line : Universal_String) return Universal_String;
    function Get_To (Line : Universal_String) return Universal_String;
    function S2Us (S : String) return Universal_String;

    function Get_Message (Line : Universal_String) return Universal_String is
    begin
        if Line.Starts_With ("to=") then
            return Line.Tail_From (Index (Line, " ") + 1);
        else
            return Line;
        end if;
    end Get_Message;

    function Get_To (Line : Universal_String) return Universal_String is
    begin
        if Line.Starts_With ("to=") then
            return Line.Slice (4, Index (Line, " ") - 1);
        else
            return Empty_Universal_String;
        end if;
    end Get_To;

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
begin
    if Argument_Count < 3 then
        Put_Line ("Synopsis:");
        Put_Line ("    bin/event_bot JID input_pipefile output_pipefile ");
        New_Line;
        Put_Line ("Send messages to the given JID.");
        return;
    end if;

    Put_Line ("Loading config from " & Home_Path
                &  "/.config/axmpp-utils/connection.cfg");
    Config.Load (Home_Path & "/.config/axmpp-utils/connection.cfg");
    Handler.Set_Config (Config);
    Put_Line ("Config loaded.");

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

    Put_Line ("Opening...");
    Session.Open;

    Input_Pipe.Initialize (Argument (2), Input_Only);
    Output_Pipe.Initialize (Argument (3), Output_Only);

    Handler.Set_Output_Pipe (Output_Pipe);
    Output_Pipe.Write_Message (Startup_Pipe_Message);

    while not Session.Has_Ended loop
        Line := Input_Pipe.Attend_Pipe;
        Command_String.Append (Line);

        if Line = End_Command_String then
            Current_Command.Initialize (Command_String);
            Current_Command.Run (Session);
            Command_String.Clear;
        end if;

        exit when Current_Command.Is_Name (Event_Console.Commands.End_Bot);
    end loop;

    Put_Line ("End");
end Event_Bot;

--  Local Variables:
--  indent-tabs-mode: nil
--  End:
