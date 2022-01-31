--  event_console-implementations.ads ---

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

--  These are the command implementations.
package Event_Console.Implementations is
    procedure Bot_End;
    procedure Bot_Is_Connected
      (Session : not null Event_Sessions.Session_Access;
       Output_Pipe : in out Pipe_Manager.Pipe_Type);

    procedure Send_Message (Session : not null Event_Sessions.Session_Access;
                            Jid_To : Universal_String;
                            Message : Universal_String);

end Event_Console.Implementations;
