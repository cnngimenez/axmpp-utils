--  send_file_handlers-handlers.adb ---

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

with Ada.Task_Identification;
with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

with XMPP.Rosters;
use XMPP.Rosters;
with XMPP.Roster_Items;
with XMPP.Logger;
use XMPP.Logger;
with XMPP.Sessions;

package body Send_File_Handlers.Handlers is

    use XMPP.IQS;
    use type XMPP.Bind_State;
    use type XMPP.Session_State;

    procedure Put_Roster (Iq : XMPP_Roster);

    ---------------------------
    --  Bind_Resource_State  --
    ---------------------------
    overriding procedure Bind_Resource_State
      (Self   : in out Send_File_Handler;
       JID    : League.Strings.Universal_String;
       Status : XMPP.Bind_State) is

        use XMPP.Sessions;

    begin
        if Status = XMPP.Success then
            Ada.Wide_Wide_Text_IO.Put_Line
              ("Resource Binded Success: " & JID.To_Wide_Wide_String);

            --  After resource binded successfull establishing session
            Establish_IQ_Session (XMPP_Session_Access (Self.Object));
        end if;
    end Bind_Resource_State;

    -----------------
    --  Connected  --
    -----------------
    overriding procedure Connected
      (Self    : in out Send_File_Handler;
       Object  : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
        pragma Unreferenced (Object);
        pragma Unreferenced (Self);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Yeah, we are connected");
    end Connected;

    overriding procedure Disconnected
      (Self : in out Send_File_Handler) is
        pragma Unreferenced (Self);
        use Ada.Task_Identification;
    begin
        Put_Line ("Disconnected");
        --  Exit from the current task.
        Abort_Task (Current_Task);
    end Disconnected;

    overriding procedure End_Stream
      (Self : in out Send_File_Handler) is
        pragma Unreferenced (Self);
        use Ada.Task_Identification;
    begin
        Put_Line ("Stream ended");
        --  Exit from the current task.
        Abort_Task (Current_Task);
    end End_Stream;

    overriding procedure Error
      (Self : in out Send_File_Handler) is
        pragma Unreferenced (Self);
    begin
        Put_Line ("Error!");
    end Error;

    overriding procedure IQ (Self : in out Send_File_Handler;
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
      (Self : in out Send_File_Handler;
       IQ_Upload : XMPP.IQ_Uploads.XMPP_IQ_Upload'Class) is
        --  pragma Unreferenced (Self);

    begin
        Put_Line ("IQ_Upload handler");
        Put_Line ("  Get URL:" & To_Wide_Wide_String (IQ_Upload.Get_Get_URL));
        Put_Line ("  Put URL:" & To_Wide_Wide_String (IQ_Upload.Get_Put_URL));

        Send_Next_File (Self,
                        IQ_Upload.Get_Put_URL, IQ_Upload.Get_Get_URL);

    end IQ_Upload;

    overriding procedure Message
      (Self : in out Send_File_Handler;
       Msg : XMPP.Messages.XMPP_Message'Class) is
        pragma Unreferenced (Self);
        pragma Unreferenced (Msg);
    begin
        Put_Line ("Message received");
    end Message;

    ----------------
    --  Presence  --
    ----------------
    overriding procedure Presence
      (Self : in out Send_File_Handler;
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
      (Self : in out Send_File_Handler;
       Data : XMPP.Rosters.XMPP_Roster'Class) is
        pragma Unreferenced (Self);
    begin
        --  Put_Line ("Client.Roster (handler):");
        --  Put_Roster (XMPP_Roster (Data));
        null;
    end Roster;

    ---------------------
    --  Session_State  --
    ---------------------
    overriding procedure Session_State
      (Self   : in out Send_File_Handler;
       Status : XMPP.Session_State) is

    begin
        Put_Line ("Session_state:");
        if Status = XMPP.Established then
            Ada.Wide_Wide_Text_IO.Put_Line ("Session established !!!");

            --  After session successfully established,
            --  sending presence
            --  Self.Set_Presence;

            --  Self.Data.Next_File (Self);
            Self.Send_First;
        end if;
    end Session_State;

    --------------------
    --  Start_Stream  --
    --------------------
    overriding procedure Start_Stream
      (Self   : in out Send_File_Handler;
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
      (Self   : in out Send_File_Handler;
       Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
        pragma Unreferenced (Self);
        pragma Unreferenced (Object);
    begin
        Ada.Wide_Wide_Text_IO.Put_Line ("Stream_Features called");
    end Stream_Features;
end Send_File_Handlers.Handlers;
