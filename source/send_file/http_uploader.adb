--  http_uploader.adb ---

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

with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body HTTP_Uploader is

    function Get_File_Data (Path : String) return String;

    function Get_File_Data (Path : String) return String is
        use Ada.Text_IO;
        use Ada.Strings.Unbounded;

        File : File_Type;
        Data : Unbounded_String;
    begin
        Open (File, In_File, Path);

        while not End_Of_File (File) loop
            declare
                Line : constant String := Get_Line (File);
            begin
                Append (Data, Line);
            end;
        end loop;

        Close (File);

        return To_String (Data);
    end Get_File_Data;

    procedure Upload_File (Put_Url : Universal_String;
                           Path : String) is
    begin
        null;
    end Upload_File;

    procedure Upload_File (Put_Url : String;
                           Path : String) is

        Http_Client : Util.Http.Clients.Client;
        Response : Util.Http.Clients.Response;

        Data : constant String := Get_File_Data (Path);
    begin
        Http_Client.Put (Put_Url, Data, Response);
    end Upload_File;

begin
    Util.Http.Clients.Curl.Register;

end HTTP_Uploader;
