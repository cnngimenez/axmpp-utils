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
with League.Strings;
use League.Strings;

with XMPP.Stream_Handlers;
with XMPP.Stream_Features;

with Files;
use Files;
with Configs;
use Configs;
with Send_Lists;
use Send_Lists;

limited with Send_File_Client;

--  Client_Handlers - package, contains Client_Handler type, inherited from
--  XMPP.Stream_Handlers.XMPP_Stream_Handler.
--  XMPP_Stream_Handler should be inherited and some handlers
--  should be overriden, if we whant to react for some events, issued
--  in AXMPP library.

package Send_File_Handlers is

    type Client_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
      with private;

    type Client_Handler_Access is access all Client_Handler'Class;

    procedure Set_Config (Self : in out Client_Handler;
                          Config : Config_Type);
    procedure Set_Send_List (Self : in out Client_Handler;
                             Send_List : Send_List_Type);

    procedure Set_Session_Object
      (Self   : in out Client_Handler;
       Object : not null access Send_File_Client.Session'Class);
    --  Function to set session object in handler.

    overriding procedure Connected
      (Self   : in out Client_Handler;
       Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is null;
    --  We whant to handle connected event.

private

    type Logic_Data_Type is tagged record
        Send_List : Send_List_Type;

        --  Current file to send
        File_Info : File_Information;
        Current_Index : Natural := 0;
    end record;

    type Client_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
       with
       record
           Object : access Send_File_Client.Session;
           Config : Config_Type;

           Data : Logic_Data_Type;
       end record;

    procedure Send_First (Self : in out Client_Handler);
    --  Send the first IQ Upload request. This must be used before
    --  Send_Next_File.

    procedure Send_Next_File (Self : in out Client_Handler;
                              Put_URL, Get_URL : Universal_String);
    --  Start the overal process sending all the files from the Send List to
    --  all users. Send List provides a list of files to upload and a list
    --  of users who want to receive them. It will be uploaded and sent one
    --  file per turn.
    --
    --  Call this procedure each time the IQ Request ansewrs or at the
    --  begining of the session.

    procedure Send_Presence (Self : in out Client_Handler);
    --  Declaring function to set presence.

    procedure Send_Upload_IQ_Request (Self : in out Client_Handler);
    --  Send the File Upload IQ Request (ask for a new slot) to the server.
    --  This will retrieve the GET and PUT URL.

    procedure Next_File (Self : in out Client_Handler);
    --  If there is another file, set it as current file to process.

    procedure Send_Message (Client : in out Client_Handler;
                            File_Get_URL : Universal_String;
                            To_JID : Universal_String);
    --  Send message with the File GET URL to one JID.

    procedure Send_Messages (Self : in out Client_Handler;
                             File_Get_URL : Universal_String);
    --  Send messages with the File Get URL to all users. Use the hash and the
    --  current index to retrieve the user lists.

    function There_Is_Next_File (Self : in out Client_Handler) return Boolean;
    --  Is there another file to send?

    procedure Set_File_Info (Self : in out Client_Handler;
                             File_Info : File_Information);

end Send_File_Handlers;
