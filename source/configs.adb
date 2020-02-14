--  configs.adb ---

--  Copyright 2020 cnngimenez
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
with Ada.Exceptions;
with Ada.Characters.Conversions;
use Ada.Characters.Conversions;
with Ada.Text_IO;
use Ada.Text_IO;

package body Configs is

    function S2Us (S : String) return Universal_String is
    begin
        return To_Universal_String
          (To_Wide_Wide_String (S));
    end S2Us;

    procedure Load (Config : in out Config_Type; 
                    Filepath : String) is
        File : File_Type;
    begin
        Open (File, In_File, Filepath);

        Config.JID := S2Us (Get_Line (File));
        Config.Username := S2Us (Get_Line (File));
        Config.Host := S2Us (Get_Line (File));
        Config.Password := S2Us (Get_Line (File));
        Config.Resource_Name := S2Us (Get_Line (File));

        Close (File);
        
    exception
       when E : Name_Error =>
           Put_Line ("Name error (possible no file founded)");
           Put_Line (Ada.Exceptions.Exception_Information
                       (E));

           Ada.Task_Identification.Abort_Task
             (Ada.Task_Identification.Current_Task);
       when E : others =>
           Put_Line (Ada.Exceptions.Exception_Information (E));
           
           Ada.Task_Identification.Abort_Task
             (Ada.Task_Identification.Current_Task);
    end Load;

end Configs;
