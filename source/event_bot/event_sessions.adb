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

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

with XMPP.Objects;
pragma Warnings (Off, XMPP.Objects);
with XMPP.Messages;
with XMPP.Messages.XMPP_File_Messages;
with XMPP.IQ_Requests;
with XMPP.Presences;

package body Event_Sessions is

    use XMPP.Objects;

    procedure Ended (Self : in out Session) is
    begin
        Self.Ended := True;
    end Ended;

    function Has_Ended (Self : in out Session) return Boolean is
      (Self.Ended);

    procedure Send_Get_Url (Self : in out Session;
                            File_Get_Url : Universal_String;
                            To_JID : Universal_String) is
        use XMPP.Messages.XMPP_File_Messages;
        Message : XMPP_File_Message;
    begin
        Message.Set_Type (XMPP.Chat);
        Message.Set_Body (File_Get_Url);
        Message.Set_To (To_JID);
        Message.Set_From (Self.Config.JID);
        Message.Set_File_Get_Url (File_Get_Url);

        Self.Send_Object (Message);
    end Send_Get_Url;

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

    procedure Send_Presence
      (Self : in out Session;
       To : Universal_String := Empty_Universal_String;
       --  Pres_Type : XMPP.Presence_Type := Unavailable;
       Show : XMPP.Show_Kind := XMPP.Online;
       Status : Universal_String := Empty_Universal_String;
       Priority : XMPP.Priority_Type := 0) is
        P : XMPP.Presences.XMPP_Presence;
        Complete_JID : Universal_String;
    begin
        Complete_JID := Self.Config.JID;
        Complete_JID.Append ("/");
        Complete_JID.Append (Self.Config.Resource_Name);

        Put_Line ("Setting presence...");

        if not To.Is_Empty then
            P.Set_To (To);
        end if;
        P.Set_Show (Show);
        P.Set_From (Complete_JID);
        if not Status.Is_Empty then
            P.Set_Status (Status);
        end if;
        P.Set_Priority (Priority);
        --  Presence type not yet supported...
        --  P.Set_Type (Pres_Type);

        Self.Send_Object (P);
    end Send_Presence;

    procedure Send_Upload_IQ_Request (Self : in out Session;
                                      File_Data : File_Information) is
        Request : XMPP.IQ_Requests.XMPP_IQ_Request;
    begin
        Put ("Sending File Upload IQ request");
        Request.Set_To (To_Universal_String ("upload.") & Self.Config.Host);
        Request.Set_From (Self.Config.JID);
        Request.Set_Filename (File_Data.Get_Name);
        Request.Set_Size (File_Data.Get_Size);
        Request.Set_Content_Type (File_Data.Get_Content_Type);

        Self.Send_Object (Request);
        Put_Line ("File Upload IQ request sent.");
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
