--  send_file_handlers-handlers.ads ---

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

with XMPP.Presences;
with XMPP.Streams;
with XMPP.Stream_Features;
with XMPP.Messages;
with XMPP.Rosters;
with XMPP.IQ_Uploads;
with XMPP.IQS;
with Send_File_Handlers;

package Send_File_Handlers.Handlers is

    type Send_File_Handler is new Client_Handler with null record;

    overriding procedure Connected
      (Self   : in out Send_File_Handler;
       Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class);
    --  We whant to handle connected event.

    overriding procedure Disconnected
      (Self : in out Send_File_Handler);

    overriding procedure End_Stream
      (Self : in out Send_File_Handler);

    overriding procedure Start_Stream
      (Self   : in out Send_File_Handler;
       Object : XMPP.Streams.XMPP_Stream'Class);
    --  We whant to handle Start_Stream event.

    overriding procedure Stream_Features
      (Self   : in out Send_File_Handler;
       Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class);
    --  We whant to get stream features.

    overriding procedure Presence
      (Self : in out Send_File_Handler;
       Data : XMPP.Presences.XMPP_Presence'Class);
    --  We whant to receive presence events.

    overriding procedure Bind_Resource_State
      (Self   : in out Send_File_Handler;
       JID    : League.Strings.Universal_String;
       Status : XMPP.Bind_State);
    --  We whant to know, what resource was binded, and if it was successfull.

    overriding procedure Session_State
      (Self   : in out Send_File_Handler;
       Status : XMPP.Session_State);
    --  We whant to get information about session state.

    overriding procedure IQ
      (Self : in out Send_File_Handler;
       IQ   : XMPP.IQS.XMPP_IQ'Class);

    overriding procedure IQ_Upload
      (Self : in out Send_File_Handler;
       IQ_Upload : XMPP.IQ_Uploads.XMPP_IQ_Upload'Class);

    overriding procedure Roster
      (Self : in out Send_File_Handler;
       Data : XMPP.Rosters.XMPP_Roster'Class);

    overriding procedure Message
      (Self : in out Send_File_Handler;
       Msg : XMPP.Messages.XMPP_Message'Class);

    overriding procedure Error
      (Self : in out Send_File_Handler);

end Send_File_Handlers.Handlers;
