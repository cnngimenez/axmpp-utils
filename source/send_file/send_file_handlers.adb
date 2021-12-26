------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
--  Copyright © 2011, Alexander Basov <coopht@gmail.com>                    --
--  All rights reserved.                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions      --
--  are met:                                                                --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Alexander Basov, IE nor the names of its      --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision$ $Date$
------------------------------------------------------------------------------
with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions;

with Send_File_Client;

--  with XMPP.IQS;
--  pragma Warnings (Off, XMPP.IQS);
--  XXX : Gnat gpl 2010 bug

with XMPP.Objects;
pragma Warnings (Off, XMPP.Objects);
--  XXX : Gnat gpl 2010 bug

with XMPP.Presences;
--  with XMPP.Messages;
with XMPP.IQ_Requests;

with HTTP_Uploader;
with XMPP.Messages.XMPP_File_Messages;

package body Send_File_Handlers is

    use XMPP.Objects;

    --  function "+" (Item : Wide_Wide_String) return Universal_String
    --    renames League.Strings.To_Universal_String;

    procedure Next_File (Self : in out Client_Handler) is
        File_Info : File_Information;
    begin
        Put_Line ("> Next file. Current_index: "
                    & Self.Data.Current_Index'Wide_Wide_Image);
        if Self.There_Is_Next_File then
            Self.Data.Current_Index := Self.Data.Current_Index + 1;

            File_Info := Self.Data.Send_List.Get_File_Information
              (Self.Data.Current_Index);

            Self.Data.File_Info := File_Info;
        else
            Put_Line ("No more file_info!");
        end if;
    end Next_File;

    procedure Send_First (Self : in out Client_Handler) is
    begin
        Self.Next_File;
        Self.Send_Upload_IQ_Request;
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

    procedure Send_Messages (Self : in out Client_Handler;
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
            Send_Message (Self, File_Get_URL, U_To_Url);
        end Send_Get_Url;

    begin
        Self.Data.Send_List.Iterate_JIDs
          (Self.Data.Current_Index, Send_Get_Url'Access);
    end Send_Messages;

    procedure Send_Next_File (Self : in out Client_Handler;
                              Put_URL, Get_URL : Universal_String) is
    begin
        Put_Line ("  Uploading file to Put URL.");
        --  HTTP_Uploader.Upload_File (Put_URL,
        --                             Self.Data.File_Info.Get_Filepath,
        --                             Self.Data.File_Info.Get_Content_Type);

        Put_Line ("  Sending message to JID.");
        Self.Send_Messages (Get_URL);

        if There_Is_Next_File (Self) then
            Put_Line ("  There is still another file.");
            Self.Next_File;
            Put_Line ("  Current file: "
                        & Ada.Characters.Conversions.To_Wide_Wide_String
                        (Self.Data.File_Info.Get_Filepath));

            Self.Send_Upload_IQ_Request;
        else
            Put_Line ("  There is no other file.");
            Put_Line ("  Closing connection.");
            Self.Object.Close;
        end if;
    end Send_Next_File;

    --------------------
    --  Send_Presence  --
    --------------------
    procedure Send_Presence (Self : in out Client_Handler) is
        P : XMPP.Presences.XMPP_Presence;
        use XMPP;

        Complete_JID : Universal_String;
    begin
        Complete_JID := Self.Config.JID;
        Complete_JID.Append ("/");
        Complete_JID.Append (Self.Config.Resource_Name);

        Put_Line ("Setting presence as online");

        P.Set_Show (Online);
        P.Set_From (Complete_JID);
        P.Set_Priority (50);
        --  P.Set_Type ();
        Self.Object.Send_Object (P);
    end Send_Presence;

    procedure Send_Upload_IQ_Request (Self : in out Client_Handler) is
        Request : XMPP.IQ_Requests.XMPP_IQ_Request;
    begin
        Put_Line ("Sending File Upload Slot Request...");
        --  TODO: Obtain from Disco discovery request.
        Request.Set_To (To_Universal_String ("upload.") & Self.Config.Host);
        Request.Set_From (Self.Config.JID);

        Request.Set_Filename (Self.Data.File_Info.Get_Name);
        Request.Set_Size (Self.Data.File_Info.Get_Size);
        Request.Set_Content_Type (Self.Data.File_Info.Get_Content_Type);

        Self.Object.Send_Object (Request);
    end Send_Upload_IQ_Request;

    procedure Set_Config (Self : in out Client_Handler;
                          Config : Config_Type) is
    begin
        Self.Config := Config;
    end Set_Config;

    procedure Set_File_Info (Self : in out Client_Handler;
                             File_Info : File_Information) is
    begin
        Self.Data.File_Info := File_Info;
    end Set_File_Info;

    procedure Set_Send_List (Self : in out Client_Handler;
                             Send_List : Send_List_Type) is
    begin
        Self.Data.Send_List := Send_List;
    end Set_Send_List;

    --------------------------
    --  Set_Session_Object  --
    --------------------------
    procedure Set_Session_Object
      (Self   : in out Client_Handler;
       Object : not null access Send_File_Client.Session'Class) is
    begin
        Put_Line ("Setting session object");
        Self.Object := Object;
    end Set_Session_Object;

    --  procedure Set_To_JID (Self : in out Client_Handler;
    --                        JID : String) is
    --      use Ada.Characters.Conversions;
    --  begin
    --      Self.To_JID := To_Universal_String (To_Wide_Wide_String (JID));
    --  end Set_To_JID;

    function There_Is_Next_File (Self : in out Client_Handler)
                                return Boolean is
    begin
        return Self.Data.Send_List.Exists_Index (Self.Data.Current_Index + 1);
    end There_Is_Next_File;

end Send_File_Handlers;
