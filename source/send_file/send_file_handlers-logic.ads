--  send_file_handlers-logic.ads ---

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

package Send_File_Handlers.Logic is

    type Sclient_Handler is limited new Client_Handler with private;
    type Logic_Data_Type is tagged private;

    procedure Send_Next_File (Self : in out SClient_Handler;
                              Put_URL, Get_URL : Universal_String);
    --  Start the overal process sending all the files from the Send List to
    --  all users. Send List provides a list of files to upload and a list
    --  of users who want to receive them. It will be uploaded and sent one
    --  file per turn.
    --
    --  Call this procedure each time the IQ Request ansewrs or at the
    --  begining of the session.

private

    type Sclient_Handler is limited new Client_Handler with record
        Data : Logic_Data_Type;
    end record;

    type Logic_Data_Type is tagged record
        Send_List : Send_List_Type;

        --  Current file to send
        File_Info : File_Information;
        Current_Index : Natural := 0;
    end record;

    procedure Send_Upload_IQ_Request (Self : in out Client_Handler);
    --  Send the File Upload IQ Request (ask for a new slot) to the server.
    --  This will retrieve the GET and PUT URL.

    procedure Next_File (Self : in out Logic_Data_Type;
                         Client : in out Client_Handler);
    --  If there is another file, set it as current file to process.

    procedure Send_Message (Client : in out Client_Handler;
                            File_Get_URL : Universal_String;
                            To_JID : Universal_String);
    --  Send message with the File GET URL to one JID.

    procedure Send_Messages (Self : Logic_Data_Type;
                             Client : in out Client_Handler;
                             File_Get_URL : Universal_String);
    --  Send messages with the File Get URL to all users. Use the hash and the
    --  current index to retrieve the user lists.

    function There_Is_Next_File (Self : in out Logic_Data_Type) return Boolean;
    --  Is there another file to send?

    procedure Set_File_Info (Client : in out Client_Handler;
                             File_Info : File_Information);

    procedure Set_Send_List (Self : in out Logic_Data_Type;
                             Send_List : Send_List_Type);

end Send_File_Handlers.Logic;
