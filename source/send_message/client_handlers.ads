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

limited with Client;

--  Client_Handlers - package, contains Client_Handler type, inherited from
--  XMPP.Stream_Handlers.XMPP_Stream_Handler.
--  XMPP_Stream_Handler should be inherited and some handlers
--  should be overriden, if we whant to react for some events, issued
--  in AXMPP library.

package Client_Handlers is

    type Client_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
      with private;

    type Client_Handler_Access is access all Client_Handler'Class;

    overriding procedure Connected
      (Self   : in out Client_Handler;
       Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class);
    --  We whant to handle connected event.

    overriding procedure Disconnected
      (Self : in out Client_Handler);

    overriding procedure End_Stream
      (Self : in out Client_Handler);

    overriding procedure Start_Stream
      (Self   : in out Client_Handler;
       Object : XMPP.Streams.XMPP_Stream'Class);
    --  We whant to handle Start_Stream event.

    overriding procedure Stream_Features
      (Self   : in out Client_Handler;
       Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class);
    --  We whant to get stream features.

    overriding procedure Presence
      (Self : in out Client_Handler;
       Data : XMPP.Presences.XMPP_Presence'Class);
    --  We whant to receive presence events.

    overriding procedure Bind_Resource_State
      (Self   : in out Client_Handler;
       JID    : League.Strings.Universal_String;
       Status : XMPP.Bind_State);
    --  We whant to know, what resource was binded, and if it was successfull.

    overriding procedure Session_State
      (Self   : in out Client_Handler;
       Status : XMPP.Session_State);
    --  We whant to get information about session state.

    overriding procedure IQ
      (Self : in out Client_Handler;
       IQ   : XMPP.IQS.XMPP_IQ'Class);

    overriding procedure Roster
      (Self : in out Client_Handler;
       Data : XMPP.Rosters.XMPP_Roster'Class);

    overriding procedure Message
      (Self : in out Client_Handler;
       Msg : XMPP.Messages.XMPP_Message'Class);

    overriding procedure Error
      (Self : in out Client_Handler);

    procedure Set_Session_Object
      (Self   : in out Client_Handler;
       Object : not null access Client.Session'Class);
    --  Function to set session object in handler.

    procedure Set_Presence (Self : in out Client_Handler);
    --  Declaring function to set presence.

    procedure Set_Config (Self : in out Client_Handler;
                          Config : Config_Type);

    procedure Set_Text (Self : in out Client_Handler;
                        Text : Universal_String);

    procedure Set_To_JID (Self : in out Client_Handler;
                          To_JID : Universal_String);

private

    type Client_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
      with
       record
           Object : access Client.Session;

           Text : Universal_String;
           To_JID : Universal_String;
           Config : Config_Type;
       end record;

end Client_Handlers;
