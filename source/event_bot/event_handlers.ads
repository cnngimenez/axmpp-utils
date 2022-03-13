--  event_handlers.ads ---

--  Copyright 2022 cnngimenez
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

with Ada.Containers.Vectors;

with League.Strings;
use League.Strings;

with XMPP.Presences;
with XMPP.Stream_Handlers;
with XMPP.Streams;
with XMPP.Stream_Features;
with XMPP.IQS;
with XMPP.Messages;
with XMPP.Rosters;
with XMPP.IQ_Uploads;
with Axmpp.Modules.Pubsubs;
with XMPP.XMPP_Pubsub.Node_Vectors;

with Configs;
use Configs;
with Pipe_Manager;
use Pipe_Manager;
with Files;
use Files;

limited with Event_Sessions;

package Event_Handlers is
    type Event_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
      with private;

    type Event_Handler_Access is access all Event_Handler'Class;

    overriding procedure Connected
      (Self   : in out Event_Handler;
       Session : XMPP.Stream_Features.XMPP_Stream_Feature'Class);
    --  We whant to handle connected event.

    overriding procedure Disconnected
      (Self : in out Event_Handler);

    overriding procedure End_Stream
      (Self : in out Event_Handler);

    overriding procedure Start_Stream
      (Self   : in out Event_Handler;
       Session : XMPP.Streams.XMPP_Stream'Class);
    --  We whant to handle Start_Stream event.

    overriding procedure Stream_Features
      (Self   : in out Event_Handler;
       Session : XMPP.Stream_Features.XMPP_Stream_Feature'Class);
    --  We whant to get stream features.

    overriding procedure Presence
      (Self : in out Event_Handler;
       Data : XMPP.Presences.XMPP_Presence'Class);
    --  We whant to receive presence events.

    overriding procedure Bind_Resource_State
      (Self   : in out Event_Handler;
       JID    : League.Strings.Universal_String;
       Status : XMPP.Bind_State);
    --  We whant to know, what resource was binded, and if it was successfull.

    overriding procedure Session_State
      (Self   : in out Event_Handler;
       Status : XMPP.Session_State);
    --  We whant to get information about session state.

    overriding procedure IQ
      (Self : in out Event_Handler;
       IQ   : XMPP.IQS.XMPP_IQ'Class);

    overriding procedure IQ_Upload
      (Self : in out Event_Handler;
       IQ_Upload : XMPP.IQ_Uploads.XMPP_IQ_Upload'Class);

    overriding procedure Roster
      (Self : in out Event_Handler;
       Data : XMPP.Rosters.XMPP_Roster'Class);

    overriding procedure Message
      (Self : in out Event_Handler;
       Msg : XMPP.Messages.XMPP_Message'Class);

    overriding procedure Error
      (Self : in out Event_Handler);

    --  Pubsub managmenent

    overriding procedure Pubsub_Discover_Features
      (Self : in out Event_Handler;
       Supported : Axmpp.Modules.Pubsubs.Pubsub_Support_Type;
       Pubsub_Module : Axmpp.Modules.Pubsubs.Pubsub_Module);

    overriding procedure Pubsub_Discover_Items
      (Self : in out Event_Handler;
       Pubsub_Nodes : XMPP.XMPP_Pubsub.Node_Vectors.Node_Vector;
       Pubsub_Module : Axmpp.Modules.Pubsubs.Pubsub_Module);

    procedure Set_Session_Object
      (Self   : in out Event_Handler;
       Session : not null access Event_Sessions.Session'Class);
    --  Function to set session object in handler.

    procedure Set_Presence (Self : in out Event_Handler);
    --  Declaring function to set presence.

    procedure Set_Config (Self : in out Event_Handler;
                          Config : Config_Type);

    procedure Set_Text (Self : in out Event_Handler;
                        Text : Universal_String);

    procedure Set_To_JID (Self : in out Event_Handler;
                          To_JID : Universal_String);

    procedure Set_Output_Pipe (Self : in out Event_Handler;
                               Pipe : Pipe_Type);

    procedure Add_New_Upload_File (Self : in out Event_Handler;
                                   File_Data : Files.File_Information;
                                   Jid_To : Universal_String);
private
    type Upload_Type is record
        File_Data : File_Information;
        Jid_To : Universal_String;
        --  Put_Url : Universal_String;
        --  Get_Url : Universal_String;
    end record;

    package File_Package is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Upload_Type);

    subtype File_Vector is File_Package.Vector;

    type Event_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
      with
       record
           Session : access Event_Sessions.Session;

           Text : Universal_String;
           To_JID : Universal_String;
           Config : Config_Type;
           Output_Pipe : Pipe_Type;
           Output_Pipe_Set : Boolean := False;

           Upload_Files : File_Vector;
       end record;

end Event_Handlers;
