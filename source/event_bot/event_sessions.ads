--  event_sessions.ads ---

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

with Files;
use Files;
with Configs;
use Configs;

with League.Strings;
use League.Strings;
with XMPP.Sessions;
with XMPP;

package Event_Sessions is

    type Session is limited new XMPP.Sessions.XMPP_Session with private;
    --  Declare XMPP session type, which is derived from XMPP_Sesssion.
    --  Further objects, of Session type will be used for sending requests
    --  to xmpp servers and clients.

    type Session_Access is access all Session'Class;
    --  Define access type

    procedure Send_Message (Self : in out Session;
                            To : Universal_String;
                            Text : Universal_String);
    procedure Send_Message (Self : in out Session; Text : Universal_String);
    procedure Send_Message (Self : in out Session; Text : Wide_Wide_String);
    procedure Send_Presence
      (Self : in out Session;
       To : Universal_String := Empty_Universal_String;
       --  Pres_Type : XMPP.Presence_Type := Unavailable;
       Show : XMPP.Show_Kind := XMPP.Online;
       Status : Universal_String := Empty_Universal_String;
       Priority : XMPP.Priority_Type := 0);
    procedure Send_Get_Url (Self : in out Session;
                            File_Get_Url : Universal_String;
                            To_JID : Universal_String);

    procedure Set_To_JID (Self : in out Session; JID : Universal_String);
    procedure Set_Config (Self : in out Session; Config : Config_Type);

    function Has_Ended (Self : in out Session) return Boolean;
    --  Has the session ended gracefully?

    procedure Ended (Self : in out Session);
    --  Set the session as ended!
    --  This is used by the Event_Handler.

    procedure Send_Upload_IQ_Request (Self : in out Session;
                                      File_Data : File_Information);
    --  Send an IQ Request for a slot (URL) to upload a file.
    --  The Event_Handler will handle the answer.

private

    type Session is limited new XMPP.Sessions.XMPP_Session
       with record
           Ended : Boolean := False;
           To_JID : Universal_String;
           Config : Config_Type;
       end record;

end Event_Sessions;
