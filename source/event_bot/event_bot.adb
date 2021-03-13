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
with XMPP.Sessions;
with XMPP.Logger;

with Configs;
with Pipe_Manager;
use Pipe_Manager;

with League.Strings;
use League.Strings;

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

    Line : Universal_String;
    Bye_Text : constant Universal_String := S2Us ("bye");
    Pipe : Pipe_Type;
begin
    if Argument_Count < 1 then
        Put_Line ("Synopsis:");
        Put_Line ("    bin/event_bot JID pipefile");
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
    XMPP.Sessions.Initialize;

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

    Pipe.Initialize (Argument (2));

    loop
        Line := Pipe.Attend_Pipe;
        Session.Send_Message (Line);

        exit when Line = Bye_Text;
    end loop;

    Put_Line ("End");
end Event_Bot;

--  Local Variables:
--  indent-tabs-mode: nil
--  End:
