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

package body HTTP_Uploader is
    
    --  Upload a file using HTTP PUT.
    procedure Upload_File (Path : String; 
                           Put_Url : String) is

        Http_Client : Util.Http.Clients.Client;
        Response : Util.Http.Clients.Response;
        
        Data : String := Get_File_Data (Path);
    begin
        Http.Put (Put_Url, Data, Response);
    end Upload_File;
    
begin
    Util.Http.Clients.Curl.Register;
    
end HTTP_Uploader;
