--  xmpp-messages-xmpp_file_messages.ads ---

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

with League.Strings;
use League.Strings;

with XMPP.Messages;
use XMPP.Messages;

package XMPP.Messages.XMPP_File_Messages is

    type XMPP_File_Message is new XMPP_Message with private;

    procedure Set_File_Get_Url (Self : in out XMPP_File_Message;
                                Url : Universal_String);

    overriding procedure Custom_Content
      (Self   : XMPP_File_Message;
       Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

private
    type XMPP_File_Message is new XMPP_Message with record
        File_Get_Url : Universal_String;
    end record;

end XMPP.Messages.XMPP_File_Messages;
