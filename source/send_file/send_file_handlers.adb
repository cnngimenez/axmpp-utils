------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
--  Copyright © 2011, Alexander Basov <coopht@gmail.com>                     --
--  All rights reserved.                                                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without       --
--  modification, are permitted provided that the following conditions       --
--  are met:                                                                 --
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
       Put_Line ("IQ Upload handler");
       Put_Line ("Get URL:" & To_Wide_Wide_String (IQ_Upload.Get_Get_URL));
       Put_Line ("Put URL:" & To_Wide_Wide_String (IQ_Upload.Get_Put_URL));

       HTTP_Uploader.Upload_File (IQ_Upload.Get_Put_URL,
                                  Self.File_Info.Get_Filepath);
   end IQ_Upload;

   overriding procedure Message
     (Self : in out Client_Handler;
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

   ---------------------
   --  Session_State  --
   ---------------------
   overriding procedure Session_State
     (Self   : in out Client_Handler;
      Status : XMPP.Session_State) is

       Request : XMPP.IQ_Requests.XMPP_IQ_Request;
   begin
       Put_Line ("Session_state:");
       if Status = XMPP.Established then
           Ada.Wide_Wide_Text_IO.Put_Line ("Session established !!!");

           --  After session successfully established,
           --  sending presence
           --  Self.Set_Presence;

           Put_Line ("Sending Slot Request...");
           --  TODO: Obtain from Disco discovery request.
           Request.Set_To (To_Universal_String ("upload.") & Self.Config.Host);
           Request.Set_From (Self.Config.JID);

           Request.Set_Filename (Self.File_Info.Get_Name);
           Request.Set_Size (Self.File_Info.Get_Size);
           Request.Set_Content_Type (Self.File_Info.Get_Content_Type);

           Self.Object.Send_Object (Request);

           --  Self.Object.Close;
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

end Send_File_Handlers;
