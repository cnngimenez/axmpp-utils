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
with Ada.Task_Identification;
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

with XMPP.Rosters;
use XMPP.Rosters;
with XMPP.Roster_Items;
--  with XMPP.Messages;
with XMPP.IQ_Requests;
with XMPP.Logger;
use XMPP.Logger;

with HTTP_Uploader;
with XMPP.Messages.XMPP_File_Messages;

package body Send_File_Handlers is

    use XMPP.IQS;
    use XMPP.Objects;

    use type XMPP.Bind_State;
    use type XMPP.Session_State;

    --  function "+" (Item : Wide_Wide_String) return Universal_String
    --    renames League.Strings.To_Universal_String;

    procedure Put_Roster (Iq : XMPP_Roster);

    ---------------------------
    --  Bind_Resource_State  --
    ---------------------------
    overriding procedure Bind_Resource_State
      (Self   : in out Client_Handler;
       JID    : League.Strings.Universal_String;
       Status : XMPP.Bind_State) is
    begin
        if Status = XMPP.Success then
            Ada.Wide_Wide_Text_IO.Put_Line
              ("Resource Binded Success: " & JID.To_Wide_Wide_String);

            --  After resource binded successfull establishing session
            Self.Object.Establish_IQ_Session;
        end if;
    end Bind_Resource_State;

    -----------------
    --  Connected  --
    -----------------
    overriding procedure Connected
      (Self    : in out Client_Handler;
       Object  : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
        pragma Unreferenced (Object);
        pragma Unreferenced (Self);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Yeah, we are connected");
    end Connected;

    overriding procedure Disconnected
      (Self : in out Client_Handler) is
        pragma Unreferenced (Self);
        use Ada.Task_Identification;
    begin
        Put_Line ("Disconnected");
        --  Exit from the current task.
        Abort_Task (Current_Task);
    end Disconnected;

    overriding procedure End_Stream
      (Self : in out Client_Handler) is
        pragma Unreferenced (Self);
        use Ada.Task_Identification;
    begin
        Put_Line ("Stream ended");
        --  Exit from the current task.
        Abort_Task (Current_Task);
    end End_Stream;

    overriding procedure Error
      (Self : in out Client_Handler) is
        pragma Unreferenced (Self);
    begin
        Put_Line ("Error!");
    end Error;

    overriding procedure IQ (Self : in out Client_Handler;
                             IQ   : XMPP.IQS.XMPP_IQ'Class) is
        pragma Unreferenced (Self);
        use XMPP;
    begin
        Put_Line ("IQ received");

        case IQ.Get_Kind is
        when Roster =>
            Put_Line ("IQ Is Roster type:");
            Put_Roster (XMPP_Roster (IQ));
        when Message =>
            Put_Line ("IQ Message type");
        when IQ_Upload =>
            declare
                IQu : XMPP.IQ_Uploads.XMPP_IQ_Upload;
            begin
                Put_Line ("IQ_Upload type");
                Put_Line ("GET URL: " &
                            To_Wide_Wide_String (IQu.Get_Get_URL));
                Put_Line ("PUT URL: " &
                            To_Wide_Wide_String (IQu.Get_Put_URL));
            end;
        when others =>
            Put_Line ("IQ Other type?");
        end case;
    end IQ;

    overriding procedure IQ_Upload
      (Self : in out Client_Handler;
       IQ_Upload : XMPP.IQ_Uploads.XMPP_IQ_Upload'Class) is
        --  pragma Unreferenced (Self);

    begin
        Put_Line ("IQ_Upload handler");
        Put_Line ("  Get URL:" & To_Wide_Wide_String (IQ_Upload.Get_Get_URL));
        Put_Line ("  Put URL:" & To_Wide_Wide_String (IQ_Upload.Get_Put_URL));

        Put_Line ("  Uploading file to Put URL.");
        HTTP_Uploader.Upload_File (IQ_Upload.Get_Put_URL,
                                   Self.File_Info.Get_Filepath,
                                   Self.File_Info.Get_Content_Type);

        Put_Line ("  Sending message to JID.");
        Send_Messages (Self, IQ_Upload.Get_Get_URL);

        if There_Is_Next_File (Self) then
            Put_Line ("  There is still another file.");
            Next_File (Self);
            Put_Line ("  Current file: "
                        & Ada.Characters.Conversions.To_Wide_Wide_String
                        (Self.File_Info.Get_Filepath));

            Send_Upload_IQ_Request (Self);
        end if;

    end IQ_Upload;

    overriding procedure Message
      (Self : in out Client_Handler;
       Msg : XMPP.Messages.XMPP_Message'Class) is
        pragma Unreferenced (Self);
        pragma Unreferenced (Msg);
    begin
        Put_Line ("Message received");
    end Message;

    procedure Next_File (Self : in out Client_Handler) is
        File_Info : File_Information;
    begin
        if Self.There_Is_Next_File then
            Self.Current_Index := Self.Current_Index + 1;

            File_Info := Self.Send_List.Get_File_Information
              (Self.Current_Index);

            Self.Set_File_Info (File_Info);
        end if;
    end Next_File;

    ----------------
    --  Presence  --
    ----------------
    overriding procedure Presence
      (Self : in out Client_Handler;
       Data : XMPP.Presences.XMPP_Presence'Class) is
        pragma Unreferenced (Self);

    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Presence Arrived: ");
        Ada.Wide_Wide_Text_IO.Put_Line
          ("User "
             & Data.Get_From.To_Wide_Wide_String
             & " is "
             & XMPP.Show_Kind'Wide_Wide_Image (Data.Get_Show)
             & " (" & Data.Get_Status.To_Wide_Wide_String & ")");
    end Presence;

    procedure Put_Roster (Iq : XMPP_Roster) is
        use XMPP.Roster_Items;

        Amount : constant Natural := Iq.Items_Count - 1;
        Roster_Item : XMPP_Roster_Item_Access;
    begin
        Log ("Client.Handlers.Put_Roster:");
        Put ("Roster received. Items amount: ");
        Put_Line (Natural'Wide_Wide_Image (Amount + 1));
        Put_Line ("--  Roster item list:");
        for I in 0 .. Amount loop
            Roster_Item := Iq.Item_At (I);

            Put (To_Wide_Wide_String
                   (Roster_Item.Get_Name));
            Put_Line (To_Wide_Wide_String
                        (Roster_Item.Get_JID));
        end loop;
        Put_Line ("--  End roster items");

    end Put_Roster;

    overriding procedure Roster
      (Self : in out Client_Handler;
       Data : XMPP.Rosters.XMPP_Roster'Class) is
        pragma Unreferenced (Self);
    begin
        Put_Line ("Client.Roster (handler):");
        Put_Roster (XMPP_Roster (Data));
    end Roster;

    procedure Send_Message (Self : in out Client_Handler;
                            File_Get_URL : Universal_String;
                            To_JID : Universal_String) is
        use XMPP.Messages.XMPP_File_Messages;
        Message : XMPP_File_Message;
    begin
        Message.Set_Type (XMPP.Chat);
        Message.Set_Body (File_Get_URL);
        Message.Set_To (To_JID);
        Message.Set_From (Self.Config.JID);
        Message.Set_File_Get_Url (File_Get_URL);

        Self.Object.Send_Object (Message);
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
        Self.Send_List.Iterate_JIDs (Self.Current_Index, Send_Get_Url'Access);
    end Send_Messages;

    procedure Send_Upload_IQ_Request (Self : in out Client_Handler) is
        Request : XMPP.IQ_Requests.XMPP_IQ_Request;
    begin
        Put_Line ("Sending File Upload Slot Request...");
        --  TODO: Obtain from Disco discovery request.
        Request.Set_To (To_Universal_String ("upload.")
                          & Self.Config.Host);
        Request.Set_From (Self.Config.JID);

        Request.Set_Filename (Self.File_Info.Get_Name);
        Request.Set_Size (Self.File_Info.Get_Size);
        Request.Set_Content_Type (Self.File_Info.Get_Content_Type);

        Self.Object.Send_Object (Request);

        --  Self.Object.Close;
    end Send_Upload_IQ_Request;

    ---------------------
    --  Session_State  --
    ---------------------
    overriding procedure Session_State
      (Self   : in out Client_Handler;
       Status : XMPP.Session_State) is

    begin
        Put_Line ("Session_state:");
        if Status = XMPP.Established then
            Ada.Wide_Wide_Text_IO.Put_Line ("Session established !!!");

            --  After session successfully established,
            --  sending presence
            --  Self.Set_Presence;

            Self.Next_File;
            Self.Send_Upload_IQ_Request;
        end if;
    end Session_State;

    procedure Set_Config (Self : in out Client_Handler;
                          Config : Config_Type) is
    begin
        Self.Config := Config;
    end Set_Config;

    procedure Set_File_Info (Self : in out Client_Handler;
                             File_Info : File_Information) is
    begin
        Self.File_Info := File_Info;
    end Set_File_Info;

    --------------------
    --  Set_Presence  --
    --------------------
    procedure Set_Presence (Self : in out Client_Handler) is
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
    end Set_Presence;

    procedure Set_Send_List (Self : in out Client_Handler;
                             Send_List : Send_List_Type) is
    begin
        Self.Send_List := Send_List;
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

    --------------------
    --  Start_Stream  --
    --------------------
    overriding procedure Start_Stream
      (Self   : in out Client_Handler;
       Object : XMPP.Streams.XMPP_Stream'Class) is
        pragma Unreferenced (Self);
        pragma Unreferenced (Object);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Start_Stream called");
    end Start_Stream;

    -----------------------
    --  Stream_Features  --
    -----------------------
    overriding procedure Stream_Features
      (Self   : in out Client_Handler;
       Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
        pragma Unreferenced (Self);
        pragma Unreferenced (Object);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Stream_Features called");
    end Stream_Features;

    function There_Is_Next_File (Self : in out Client_Handler)
                                return Boolean is
    begin
        return Self.Send_List.Exists_Index (Self.Current_Index + 1);
    end There_Is_Next_File;

end Send_File_Handlers;
