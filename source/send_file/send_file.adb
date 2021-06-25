------------------------------------------------------------------------------
--                                                                           --
--                              AXMPP Project                                --
--                                                                           --
--                           XMPP Library for Ada                            --
--                                                                           --
------------------------------------------------------------------------------
--                                                                           --
--  Copyright © 2011, Alexander Basov <coopht@gmail.com>                    --
--  All rights reserved.                                                     --
--                                                                           --
--  Redistribution and use in source and binary forms, with or without       --
--  modification, are permitted provided that the following conditions       --
--  are met:                                                                 --
--                                                                           --
--  * Redistributions of source code must retain the above copyright         --
--    notice, this list of conditions and the following disclaimer.          --
--                                                                           --
--  * Redistributions in binary form must reproduce the above copyright      --
--    notice, this list of conditions and the following disclaimer in the    --
--    documentation and/or other materials provided with the distribution.   --
--                                                                           --
--  * Neither the name of the Alexander Basov, IE nor the names of its       --
--    contributors may be used to endorse or promote products derived from   --
--    this software without specific prior written permission.               --
--                                                                           --
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
--                                                                           --
------------------------------------------------------------------------------
--  $Revision$ $Date$
------------------------------------------------------------------------------
with Send_File_Client;
with Send_File_Handlers;
with XMPP.Sessions;
with XMPP.Logger;

with Ada.Task_Identification;
use Ada.Task_Identification;
with Ada.Environment_Variables;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
--  with Ada.Characters.Conversions;
--  use Ada.Characters.Conversions;

--  with League.Strings;
--  use League.Strings;

with Configs;
with Files;
use Files;

procedure Send_File is

    --  function S2Us (S : String) return Universal_String;

    --  function S2Us (S : String) return Universal_String is
    --  begin
    --      return To_Universal_String
    --        (To_Wide_Wide_String (S));
    --  end S2Us;

    Home_Path : constant String := Ada.Environment_Variables.Value ("HOME");
    Config : Configs.Config_Type;
    S : constant not null Send_File_Client.Session_Access
      := new Send_File_Client.Session;
    H : constant not null Send_File_Handlers.Client_Handler_Access
      := new Send_File_Handlers.Client_Handler;
    File_Info : File_Information;
begin
    if Argument_Count /= 3 then
        Put_Line ("Synopsis:");
        Put_Line ("    bin/send_message JID FilePath Mime-type");
        New_Line;
        Put_Line ("Send a message to the given JID");
        --  For some reason the return does not work.
        --  GNAT.OS_Lib.OS_Exit (0);
        Set_Exit_Status (Success);
        return;
    end if;

    Put_Line ("Loading config.");
    Config.Load (Home_Path & "/.config/axmpp-utils/connection.cfg");
    H.Set_Config (Config);
    Put_Line ("Config loaded.");

    H.Set_To_JID (Argument (1));

    Put_Line ("Creating fileinfo");
    File_Info := Create (Argument (2));
    File_Info.Set_Content_Type (Argument (3));

    Put_Line ("checking existence");
    if not File_Info.File_Exists then
        Put_Line ("Provided file does not exists!");
        Set_Exit_Status (Failure);
        return;
    end if;

    Put_Line ("File information:");
    Put_Line (File_Info.To_String);

    XMPP.Logger.Enable_Debug;
    XMPP.Sessions.Initialize;

    S.Set_Stream_Handler (H);
    H.Set_Session_Object (S);
    H.Set_File_Info (File_Info);

    S.Set_Host (Config.Host);
    S.Set_JID (Config.JID);
    S.Set_Password (Config.Password);
    S.Set_Resource (Config.Resource_Name);

    Put_Line ("Opening...");
    S.Open;

    --  S.Close;
    --  Put_Line ("Ended");
end Send_File;
