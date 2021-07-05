--  send_file_handlers-logic.adb ---

--  Copyright 2021 cnngimenez
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

--  with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions;

with XMPP.Messages.XMPP_File_Messages;
with XMPP.IQ_Requests;
with Send_File_Client;
with HTTP_Uploader;

package body Send_File_Handlers.Logic is
    procedure Next_File (Self : in out Logic_Data_Type;
                         Client : in out Client_Handler) is
        File_Info : File_Information;
    begin
        if Self.There_Is_Next_File then
            Self.Current_Index := Self.Current_Index + 1;

            File_Info := Self.Send_List.Get_File_Information
              (Self.Current_Index);

            Set_File_Info (Client, File_Info);
        end if;
    end Next_File;

    procedure Send_First (Self : in out Logic_Data_Type;
                          Client : in out Client_Handler) is
    begin
        Self.Next_File (Client);
        Send_Upload_IQ_Request (Client);
    end Send_First;

    procedure Send_Message (Client : in out Client_Handler;
                            File_Get_URL : Universal_String;
                            To_JID : Universal_String) is
        use XMPP.Messages.XMPP_File_Messages;
        Message : XMPP_File_Message;
    begin
        Message.Set_Type (XMPP.Chat);
        Message.Set_Body (File_Get_URL);
        Message.Set_To (To_JID);
        Message.Set_From (Client.Config.JID);
        Message.Set_File_Get_Url (File_Get_URL);

        Client.Object.Send_Object (Message);
    end Send_Message;

    procedure Send_Messages (Self : Logic_Data_Type;
                             Client : in out Client_Handler;
                             File_Get_URL : Universal_String) is

        procedure Send_Get_Url (Filename : String; To_Url : String);

        procedure Send_Get_Url (Filename : String; To_Url : String) is
            Wws_To_Url : constant Wide_Wide_String :=
              Ada.Characters.Conversions.To_Wide_Wide_String (To_Url);
            U_To_Url : constant Universal_String :=
              To_Universal_String (Wws_To_Url);
            Wws_Filename : constant Wide_Wide_String :=
              Ada.Characters.Conversions.To_Wide_Wide_String (Filename);
        begin
            Put_Line ("Sending Get URL of "
                        & Wws_Filename
                        & ":");
            Put_Line (Wws_To_Url);
            Send_Message (Client, File_Get_URL, U_To_Url);
        end Send_Get_Url;

    begin
        Self.Send_List.Iterate_JIDs (Self.Current_Index, Send_Get_Url'Access);
    end Send_Messages;

    procedure Send_Next_File (Self : in out Logic_Data_Type;
                              Client : in out Client_Handler;
                              Put_URL, Get_URL : Universal_String) is
    begin
        Put_Line ("  Uploading file to Put URL.");
        HTTP_Uploader.Upload_File (Put_URL,
                                   Self.File_Info.Get_Filepath,
                                   Self.File_Info.Get_Content_Type);

        Put_Line ("  Sending message to JID.");
        Send_Messages (Client, Get_URL);

        if There_Is_Next_File (Self) then
            Put_Line ("  There is still another file.");
            Next_File (Self, Client);
            Put_Line ("  Current file: "
                        & Ada.Characters.Conversions.To_Wide_Wide_String
                        (Self.File_Info.Get_Filepath));

            Send_Upload_IQ_Request (Client);
        else
            Put_Line ("  There is no other file.");
            Put_Line ("  Closing connection.");
            Client.Object.Close;
        end if;
    end Send_Next_File;

    procedure Send_Upload_IQ_Request (Self : in out Client_Handler) is
        Request : XMPP.IQ_Requests.XMPP_IQ_Request;
    begin
        Put_Line ("Sending File Upload Slot Request...");
        --  TODO: Obtain from Disco discovery request.
        Request.Set_To (To_Universal_String ("upload.") & Self.Config.Host);
        Request.Set_From (Self.Config.JID);

        Request.Set_Filename (Self.File_Info.Get_Name);
        Request.Set_Size (Self.File_Info.Get_Size);
        Request.Set_Content_Type (Self.File_Info.Get_Content_Type);

        Self.Object.Send_Object (Request);
    end Send_Upload_IQ_Request;

    procedure Set_File_Info (Client : in out Client_Handler;
                             File_Info : File_Information) is
    begin
        Client.File_Info := File_Info;
    end Set_File_Info;

    procedure Set_Send_List (Self : in out Logic_Data_Type;
                             Send_List : Send_List_Type) is
    begin
        Self.Send_List := Send_List;
    end Set_Send_List;


    function There_Is_Next_File (Self : in out Logic_Data_Type)
                                return Boolean is
    begin
        return Self.Send_List.Exists_Index (Self.Current_Index + 1);
    end There_Is_Next_File;

end Send_File_Handlers.Logic;
