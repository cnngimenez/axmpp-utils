--  event_console-implementations.adb ---

--  Copyright 2022 cnngimenez
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

with Ada.Strings.Unbounded;
with Files;
use Files;

package body Event_Console.Implementations is

    function To_Path_String (S : Universal_String) return String;

    procedure Bot_End is
    begin
        null;
    end Bot_End;

    procedure Bot_Is_Connected
      (Session : not null Event_Sessions.Session_Access;
       Output_Pipe : in out Pipe_Manager.Pipe_Type) is
    begin
        Output_Pipe.Write_Message (String'("User: bot:is connected"));
        if Session.Is_TLS_Established then
            Output_Pipe.Write_Message
              (String'("Bot answer: TLS established."));
        else
            Output_Pipe.Write_Message
              (String'("Bot answer: TLS not established."));
        end if;
    end Bot_Is_Connected;

    procedure Send_File
      (Session : not null Event_Sessions.Session_Access;
       Handler : not null Event_Handlers.Event_Handler_Access;
       Output_Pipe : in out Pipe_Manager.Pipe_Type;
       Jid_To : Universal_String;
       Path : Universal_String) is
        File_Data : File_Information;
        Path_Str : constant String := To_Path_String (Path);
    begin
        File_Data := Create (Path_Str);
        if not File_Data.File_Exists then
            Output_Pipe.Write_Message
              (String'("Bot error: file " & Path_Str
                         & " not found and cannot be uploaded."));
            return;
        end if;

        Session.Send_Upload_IQ_Request (File_Data);
        Handler.Add_New_Upload_File (File_Data, Jid_To);
    end Send_File;

    procedure Send_Message (Session : not null Event_Sessions.Session_Access;
                            Jid_To : Universal_String;
                            Message : Universal_String) is
    begin
        if Jid_To.Is_Empty then
            Session.Send_Message (Message);
        else
            Session.Send_Message (Jid_To, Message);
        end if;
    end Send_Message;

    function To_Path_String (S : Universal_String) return String is
        use Ada.Strings.Unbounded;

        Wws : constant Wide_Wide_String := To_Wide_Wide_String (S);
        Amount : constant Natural := Wws'Length;
        Temp_Str : Unbounded_String;
        Wwc : Wide_Wide_Character;
        C : Character;
    begin
        for I in Natural range 1 .. Amount loop
            Wwc := Wws (I);
            if Wide_Wide_Character'Pos (Wwc) < 256 then
                C := Character'Val (Wide_Wide_Character'Pos (Wwc));
                Append (Temp_Str, C);
            end if;
        end loop;

        return To_String (Temp_Str);
    end To_Path_String;

end Event_Console.Implementations;
