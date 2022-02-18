--  event_console-commands.ads ---

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

with League.Strings;
use League.Strings;
with Event_Sessions;
with Pipe_Manager;

--  Command type objects to store any command inputted.
--
--  Each console command can be stored with its arguments.  The type defined
--  in this package can store arguments and separate them too.
package Event_Console.Commands is

    type Command is tagged private;

    type Name_Type is (Send_Message,
                       --  Bot commands
                       Bot_Is_Connected,
                       Bot_End,
                       --  Misc
                       Unknown, --  An unknown command name (but well formed)
                       Malformed_Command);

    procedure Initialize (Self : in out Command;
                          Command_String : Universal_String);
    --  Initialize the command object from a command string syntax.
    --
    --  Command_String: The string with the whole syntax.

    procedure Initialize (Self : in out Command;
                          Name : Name_Type;
                          Arguments : Universal_String);
    --  Initilize the Command object.
    --
    --  Name: One of the Name_Type.
    --  Arguments: The argument string without the "end command".

    function Get_Name (Self : Command) return Name_Type;
    function Get_Name (Self : Command) return Universal_String;
    function Is_Name (Self : Command; Name : Name_Type) return Boolean;

    function Get_Argument (Self : Command; Name : Wide_Wide_String)
                          return Universal_String;
    function Get_Arguments (Self : Command) return Universal_String;

    function Get_Data_Argument (Self : Command) return Universal_String;

    function Get_Nth_Argument (Self : Command; N : Positive)
                              return Universal_String;
    function Get_Argument_Count (Self : Command) return Natural;

    --  This is not a function per se. Two or more names are mapped to the same
    --  Name_Type element.
    function Namestring_To_Nametype (Name : Universal_String) return Name_Type;
    function Nametype_To_Namestring (Name : Name_Type;
                                     Use_Space : Boolean := True;
                                     Lower_Case : Boolean := True)
                                    return Universal_String;

    procedure Run (Self : Command;
                   Session : not null Event_Sessions.Session_Access;
                   Output_Pipe : in out Pipe_Manager.Pipe_Type);

    function To_Universal_String (Self : Command) return Universal_String;
    function To_Wide_Wide_String (Self : Command) return Wide_Wide_String;
    --  String representation of the command with all its parts.
    --  Useful for debugging purposes.

private
    type Command is tagged record
        Name : Name_Type;
        Arguments : Universal_String;
    end record;

end Event_Console.Commands;
