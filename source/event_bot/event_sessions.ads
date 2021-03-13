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

with Configs;
use Configs;

with League.Strings;
use League.Strings;
with XMPP.Sessions;

package Event_Sessions is

    type Session is limited new XMPP.Sessions.XMPP_Session with private;
    --  Declare XMPP session type, which is derived from XMPP_Sesssion.
    --  Further objects, of Session type will be used for sending requests
    --  to xmpp servers and clients.

    type Session_Access is access all Session'Class;
    --  Define access type

    procedure Send_Message (Self : in out Session; Text : Universal_String);
    procedure Send_Message (Self : in out Session; Text : Wide_Wide_String);

    procedure Set_To_JID (Self : in out Session; JID : Universal_String);
    procedure Set_Config (Self : in out Session; Config : Config_Type);

private

    type Session is limited new XMPP.Sessions.XMPP_Session
       with record
           To_JID : Universal_String;
           Config : Config_Type;
       end record;

end Event_Sessions;
