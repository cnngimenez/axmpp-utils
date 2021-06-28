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

with Ada.Environment_Variables;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
--  with Ada.Characters.Conversions;
--  use Ada.Characters.Conversions;
with Ada.Directories;

--  with League.Strings;
--  use League.Strings;

with Configs;
with Send_Lists;
use Send_Lists;

procedure Send_File is

    --  function S2Us (S : String) return Universal_String;
    function Load_Send_List return Boolean;

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
    Send_List : Send_List_Type;

    function Load_Send_List return Boolean is
    begin
        if Ada.Directories.Exists (Argument (1)) then
            Send_List.Fill_From_File (Argument (1));
            return True;
        else
            Put_Line ("The file does not exists!");
            return False;
        end if;
    end Load_Send_List;

begin
    if Argument_Count /= 3 and then Argument_Count /= 1 then
        Put_Line ("Synopsis:");
        Put_Line ("    bin/send_file JID FilePath Mime-type");
        Put_Line ("    bin/send_file ListFile");
        New_Line;
        Put_Line ("Send a message to the given JID.");
        Put_Line ("The second syntax use a list of files and JID in a file");
        Put_Line ("with the following format:");
        New_Line;
        Put_Line ("Path to file-1");
        Put_Line ("MIME-Type-1");
        Put_Line ("JID_1_1@xmpp.org");
        Put_Line ("JID_1_2@xmpp.org");
        Put_Line ("...");
        Put_Line ("--");
        Put_Line ("...");
        Put_Line ("--");
        Put_Line ("Path to file-n");
        Put_Line ("MIME-Type-n");
        Put_Line ("JID_n_1@xmpp.org");
        Put_Line ("JID_n_2@xmpp.org");
        Put_Line ("...");
        --  For some reason the return does not work.
        --  GNAT.OS_Lib.OS_Exit (0);
        Set_Exit_Status (Success);
        return;
    end if;

    if Argument_Count = 1 then
        if not Load_Send_List then
            Set_Exit_Status (Failure);
            return;
        end if;
    else
        Send_List.Add (Argument (2), Argument (3), Argument (1));
    end if;

    Put_Line ("Files to send loaded:");
    Put_Line (To_String (Send_List));

    --  TODO Check file existence...

    Put_Line ("Loading config.");
    Config.Load (Home_Path & "/.config/axmpp-utils/connection.cfg");
    H.Set_Config (Config);
    Put_Line ("Config loaded.");

    H.Set_Send_List (Send_List);

    XMPP.Logger.Enable_Debug;
    XMPP.Sessions.Initialize;

    S.Set_Stream_Handler (H);
    H.Set_Session_Object (S);
    --  H.Set_File_Info (File_Info);

    S.Set_Host (Config.Host);
    S.Set_JID (Config.JID);
    S.Set_Password (Config.Password);
    S.Set_Resource (Config.Resource_Name);

    Put_Line ("Opening...");
    S.Open;

    --  S.Close;
    --  Put_Line ("Ended");
end Send_File;
