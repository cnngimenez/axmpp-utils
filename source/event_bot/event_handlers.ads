with League.Strings;
use League.Strings;

with XMPP.Presences;
with XMPP.Stream_Handlers;
with XMPP.Streams;
with XMPP.Stream_Features;
with XMPP.IQS;
with XMPP.Messages;
with XMPP.Rosters;

with Configs;
use Configs;
with Pipe_Manager;
use Pipe_Manager;

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

    overriding procedure Roster
      (Self : in out Event_Handler;
       Data : XMPP.Rosters.XMPP_Roster'Class);

    overriding procedure Message
      (Self : in out Event_Handler;
       Msg : XMPP.Messages.XMPP_Message'Class);

    overriding procedure Error
      (Self : in out Event_Handler);

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

private

    type Event_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
       with
       record
           Session : access Event_Sessions.Session;

           Text : Universal_String;
           To_JID : Universal_String;
           Config : Config_Type;
           Output_Pipe : Pipe_Type;
           Output_Pipe_Set : Boolean := False;
       end record;

end Event_Handlers;
