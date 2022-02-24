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
with Event_Handlers;
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
    procedure Send_File
      (Session : not null Event_Sessions.Session_Access;
       Handler : not null Event_Handlers.Event_Handler_Access;
       Output_Pipe : in out Pipe_Manager.Pipe_Type;
       Jid_To : Universal_String;
       Path : Universal_String);
    --  Send a file to the given Jid_To XMPP account. Path is the file path.
    --  If the file does not exists, do nothing.
    --
    --  The process implented is:
    --
    --  1. Send the IQ Request to request a slot to upload the file. A slot is
    --     a GET and PUT URL. The request is sent but the answer is handled by
    --     the Event_Handler instance, thus:
    --  2. Register the file data and the JID destination on the Event_Handler.
    --     When the IQ Request answer is received, the Event_Handler can start
    --     the HTTP upload to the provided slot. The Event_Handler has a heap
    --     to store multiple file data.
    --  3. Upload the HTTP File. Once finished, then:
    --  4. Send the GET URL of the slot to the Jid_To account.

end Event_Console.Implementations;
