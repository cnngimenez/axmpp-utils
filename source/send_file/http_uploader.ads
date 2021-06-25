--  http_uploader.ads ---

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

with League.Strings;
use League.Strings;

package HTTP_Uploader is

    --  Upload a file using HTTP PUT.
    procedure Upload_File (Put_Url : Universal_String;
                           Path : String;
                           Mime_Type : String);
    procedure Upload_File (Put_Url : String;
                           Path : String;
                           Mime_Type : String);

end HTTP_Uploader;
