--  event_sessions.adb ---

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

with XMPP.Objects;
pragma Warnings (Off, XMPP.Objects);
with XMPP.Messages;
with XMPP.IQ_Requests;

package body Event_Sessions is

    use XMPP.Objects;

    procedure Ended (Self : in out Session) is
    begin
        Self.Ended := True;
    end Ended;

    function Has_Ended (Self : in out Session) return Boolean is
      (Self.Ended);

    procedure Send_Message (Self : in out Session;
                            To : Universal_String;
                            Text : Universal_String) is
        Message : XMPP.Messages.XMPP_Message;
    begin
        Message.Set_Type (XMPP.Chat);
        Message.Set_Body (Text);
        Message.Set_To (To);
        Message.Set_From (Self.Config.JID);

        Self.Send_Object (Message);
    end Send_Message;

    procedure Send_Message (Self : in out Session; Text : Universal_String) is
    begin
        Send_Message (Self, Self.To_JID, Text);
    end Send_Message;

    procedure Send_Message (Self : in out Session; Text : Wide_Wide_String) is
    begin
        Send_Message (Self, To_Universal_String (Text));
    end Send_Message;

    procedure Send_Upload_IQ_Request (Self : in out Session;
                                      File_Data : File_Information) is
        Request : XMPP.IQ_Requests.XMPP_IQ_Request;
    begin
        Request.Set_To (To_Universal_String ("upload.") & Self.Config.Host);
        Request.Set_From (Self.Config.JID);
        Request.Set_Filename (File_Data.Get_Name);
        Request.Set_Size (File_Data.Get_Size);
        Request.Set_Content_Type (File_Data.Get_Content_Type);

        Self.Send_Object (Request);
    end Send_Upload_IQ_Request;

    procedure Set_Config (Self : in out Session; Config : Config_Type) is
    begin
        Self.Config := Config;
    end Set_Config;

    procedure Set_To_JID (Self : in out Session; JID : Universal_String) is
    begin
        Self.To_JID := JID;
    end Set_To_JID;

end Event_Sessions;
