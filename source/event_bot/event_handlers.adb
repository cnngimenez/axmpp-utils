--  event_handlers.adb ---

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

with Ada.Task_Identification;
with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Wide_Wide_Latin_1;

with Event_Sessions;

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
with XMPP.Logger;
use XMPP.Logger;
with HTTP_Uploader;

package body Event_Handlers is

    use XMPP.IQS;
    use XMPP.Objects;

    use type XMPP.Bind_State;
    use type XMPP.Session_State;

    function "+" (Item : Wide_Wide_String) return Universal_String
      renames League.Strings.To_Universal_String;

    procedure Put_Roster (Iq : XMPP_Roster);

    procedure Add_New_Upload_File (Self : in out Event_Handler;
                                   File_Data : Files.File_Information;
                                   Jid_To : Universal_String) is
        Upload_Data : Upload_Type;
    begin
        Upload_Data.File_Data := File_Data;
        Upload_Data.Jid_To := Jid_To;

        Self.Upload_Files.Append (Upload_Data);
    end Add_New_Upload_File;

    ---------------------------
    --  Bind_Resource_State  --
    ---------------------------
    overriding procedure Bind_Resource_State
      (Self   : in out Event_Handler;
       JID    : League.Strings.Universal_String;
       Status : XMPP.Bind_State) is
    begin
        if Status = XMPP.Success then
            Ada.Wide_Wide_Text_IO.Put_Line
              ("Resource Binded Success: " & JID.To_Wide_Wide_String);

            --  After resource binded successfull establishing session
            Self.Session.Establish_IQ_Session;
        end if;
    end Bind_Resource_State;

    -----------------
    --  Connected  --
    -----------------
    overriding procedure Connected
      (Self    : in out Event_Handler;
       Session  : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
        pragma Unreferenced (Session);
        pragma Unreferenced (Self);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Yeah, we are connected");
    end Connected;

    overriding procedure Disconnected
      (Self : in out Event_Handler) is
        pragma Unreferenced (Self);
        use Ada.Task_Identification;
    begin
        Put_Line ("Disconnected");
        --  Exit from the current task.
        Abort_Task (Current_Task);
    end Disconnected;

    overriding procedure End_Stream
      (Self : in out Event_Handler) is
        use Ada.Task_Identification;
    begin
        Put_Line ("Stream ended");
        --  Exit from the current task.
        Self.Session.Ended;
        Abort_Task (Current_Task);
    end End_Stream;

    overriding procedure Error
      (Self : in out Event_Handler) is
        pragma Unreferenced (Self);
    begin
        Put_Line ("Error!");
    end Error;

    overriding procedure IQ (Self : in out Event_Handler;
                             IQ   : XMPP.IQS.XMPP_IQ'Class) is
        pragma Unreferenced (Self);
        use XMPP;
    begin
        Put_Line ("IQ received");
        if Get_Kind (IQ) = Roster then
            Put_Line ("Is Roster type:");
            Put_Roster (XMPP_Roster (IQ));

            --  Send_Message (Iq);
        elsif Get_Kind (IQ) = Message then
            Put_Line ("Message type:");

        end if;

    end IQ;

    overriding procedure IQ_Upload
      (Self : in out Event_Handler;
       IQ_Upload : XMPP.IQ_Uploads.XMPP_IQ_Upload'Class) is
        Upload_Data : Upload_Type;
        --  Put_URL : constant Universal_String := IQ_Upload.Get_Put_URL;
        --  Get_Url : constant Universal_String := IQ_Upload.Get_Put_URL;
    begin
        Put_Line ("IQ_Upload received");
        if Self.Upload_Files.Is_Empty then
            return;
        end if;

        Upload_Data := Self.Upload_Files.First_Element;

        Put_Line ("Uploading file to "
                    & To_Wide_Wide_String (IQ_Upload.Get_Put_URL));
        HTTP_Uploader.Upload_File (IQ_Upload.Get_Put_URL,
                                   Upload_Data.File_Data.Get_Filepath,
                                   Upload_Data.File_Data.Get_Content_Type);
        Put_Line ("Sending the Get URL to: "
                    & To_Wide_Wide_String (Upload_Data.Jid_To));
        Self.Session.Send_Get_Url (IQ_Upload.Get_Get_URL,
                                   Upload_Data.Jid_To);
        Self.Upload_Files.Delete_First;
    end IQ_Upload;

    overriding procedure Message (Self : in out Event_Handler;
                                  Msg : XMPP.Messages.XMPP_Message'Class) is
        Bye_Text : constant Universal_String := +"Bye";
        --  Slash_Position : Natural;
        From : Universal_String;
    begin
        Put_Line ("Message received:");
        Put_Line (To_Wide_Wide_String (Msg.Get_From));
        --  & ": '" & To_Wide_Wide_String (Msg.Get_Body) & "'");

        --  Slash_Position := Msg.Get_From.Index ("/");
        --  if Slash_Position > 0 then
        --      From := Msg.Get_From.Slice (1, Slash_Position - 1);
        --  else
        --      From := Msg.Get_From;
        --  end if;

        --  Put_Line (To_Wide_Wide_String (From));

        if Self.Output_Pipe_Set and then not Msg.Get_Body.Is_Empty then
            Self.Output_Pipe.Write_Message
              ("<!--  > Message: "
                 & " from: """ & To_Wide_Wide_String (Msg.Get_From) & """"
                 & " to: """ & To_Wide_Wide_String (Msg.Get_To) & """"
                 & " id: """ & To_Wide_Wide_String (Msg.Get_Id) & """"
                 & " type: " & Msg.Get_Type'Wide_Wide_Image
                 & " subject: """ & To_Wide_Wide_String (Msg.Get_Subject)
                 & """"
                 & " body: """ & To_Wide_Wide_String (Msg.Get_Body) & """"
                 & " chat-state: " & Msg.Get_Chat_State'Wide_Wide_Image
                 & " -->");
        end if;

        if From = Self.To_JID and then Msg.Get_Body = Bye_Text then
            Self.Session.Send_Message ("Alright! Bye!");
            Self.Session.Close;
        end if;
    end Message;

    ----------------
    --  Presence  --
    ----------------
    overriding procedure Presence
      (Self : in out Event_Handler;
       Data : XMPP.Presences.XMPP_Presence'Class) is

    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Presence Arrived: ");
        --  Ada.Wide_Wide_Text_IO.Put_Line
        --    ("User "
        --       & Data.Get_From.To_Wide_Wide_String
        --       & " is "
        --       & XMPP.Show_Kind'Wide_Wide_Image (Data.Get_Show)
        --       & " (" & Data.Get_Status.To_Wide_Wide_String & ")");

        Self.Output_Pipe.Write_Message
          (Wide_Wide_String'("<!--  > Presence: ")
             & " from: """ & To_Wide_Wide_String (Data.Get_From) & """"
             & " to: """ & To_Wide_Wide_String (Data.Get_To) & """"
             & " type: " & Data.Get_Type'Wide_Wide_Image
             & " show: " & Data.Get_Show'Wide_Wide_Image
             & " priority: " & Data.Get_Priority'Wide_Wide_Image
             & " status:  """ & To_Wide_Wide_String (Data.Get_Status) & """"
             & "-->");
    end Presence;

    overriding procedure Pubsub_Discover_Features
      (Self : in out Event_Handler;
       Supported : Axmpp.Modules.Pubsubs.Pubsub_Support_Type;
       Pubsub_Module : Axmpp.Modules.Pubsubs.Pubsub_Module) is
    begin
        Self.Output_Pipe.Write_Message
          (Wide_Wide_String'("<!--  > Pubsub discovery response: ")
             & Supported'Wide_Wide_Image
             & "-->");
    end Pubsub_Discover_Features;

    overriding procedure Pubsub_Discover_Items
      (Self : in out Event_Handler;
       Pubsub_Nodes : XMPP.XMPP_Pubsub.Node_Vectors.Node_Vector;
       Pubsub_Module : Axmpp.Modules.Pubsubs.Pubsub_Module) is
        LF : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
    begin
        Self.Output_Pipe.Write_Message
          (Wide_Wide_String'
             ("<!--  > Pubsub discovery items, node list received:"));

        for I of Pubsub_Nodes loop
            Self.Output_Pipe.Write_Message
              (Wide_Wide_String'("JID: """)
                 & To_Wide_Wide_String (I.Get_JID) & """"
                 & "Node: """  & To_Wide_Wide_String (I.Get_Node) & """"
                 & "Name: """ & To_Wide_Wide_String (I.Get_Name) & """"
                 & LF);
        end loop;

        Self.Output_Pipe.Write_Message (Wide_Wide_String'("-->"));

    end Pubsub_Discover_Items;

    procedure Put_Roster (Iq : XMPP_Roster) is
        use XMPP.Roster_Items;

        Amount : constant Natural := Iq.Items_Count - 1;
        Roster_Item : XMPP_Roster_Item_Access;
    begin
        Log ("Event_Sessions.Handlers.Put_Roster:");
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
      (Self : in out Event_Handler;
       Data : XMPP.Rosters.XMPP_Roster'Class) is
        pragma Unreferenced (Self);
    begin
        Put_Line ("Event_Sessions.Roster (handler):");
        Put_Roster (XMPP_Roster (Data));
    end Roster;

    ---------------------
    --  Session_State  --
    ---------------------
    overriding procedure Session_State
      (Self   : in out Event_Handler;
       Status : XMPP.Session_State) is

        --  Hello_Text : constant Wide_Wide_String := "Hello! I'm connected!";

    begin
        Put_Line ("Session_state:");
        if Status = XMPP.Established then
            Ada.Wide_Wide_Text_IO.Put_Line ("Session established !!!");

            --  After session successfully established,
            --  sending presence
            --  Self.Set_Presence;
            Self.Session.Send_Presence;

            --  Self.Session.Send_Message (Hello_Text);

            --  Self.Session.Close;
        end if;
    end Session_State;

    procedure Set_Config (Self : in out Event_Handler;
                          Config : Config_Type) is
    begin
        Self.Config := Config;
    end Set_Config;

    procedure Set_Output_Pipe (Self : in out Event_Handler;
                               Pipe : Pipe_Type) is
    begin
        Self.Output_Pipe_Set := True;
        Self.Output_Pipe := Pipe;
    end Set_Output_Pipe;

    --------------------
    --  Set_Presence  --
    --------------------
    procedure Set_Presence (Self : in out Event_Handler) is
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
        P.Set_Priority (0);
        --  P.Set_Type ();
        Self.Session.Send_Object (P);
    end Set_Presence;

    --------------------------
    --  Set_Session_Object  --
    --------------------------
    procedure Set_Session_Object
      (Self   : in out Event_Handler;
       Session : not null access Event_Sessions.Session'Class) is
    begin
        Put_Line ("Setting session object");
        Self.Session := Session;
    end Set_Session_Object;

    procedure Set_Text (Self : in out Event_Handler;
                        Text : Universal_String) is
    begin
        Self.Text := Text;
    end Set_Text;

    procedure Set_To_JID (Self : in out Event_Handler;
                          To_JID : Universal_String) is
    begin
        Self.To_JID := To_JID;
    end Set_To_JID;

    --------------------
    --  Start_Stream  --
    --------------------
    overriding procedure Start_Stream
      (Self   : in out Event_Handler;
       Session : XMPP.Streams.XMPP_Stream'Class) is
        pragma Unreferenced (Self);
        pragma Unreferenced (Session);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Start_Stream called");
    end Start_Stream;

    -----------------------
    --  Stream_Features  --
    -----------------------
    overriding procedure Stream_Features
      (Self   : in out Event_Handler;
       Session : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
        pragma Unreferenced (Self);
        pragma Unreferenced (Session);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Stream_Features called");
    end Stream_Features;

end Event_Handlers;
