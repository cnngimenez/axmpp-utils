--  files.ads ---

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

package Files is
    type File_Information is tagged record;

    function Create (Filepath : Wide_Wide_String) return File_Information;

    function Get_Filepath (Self : File_Information) return Wide_Wide_String;
    function Get_Name (Self : File_Information) return Universal_String;
    function Get_Size (Self : File_Information) return Universal_String;
    function Get_Size (Self : File_Information) return Integer;
    function Get_Content_Type (Self : File_Information) return Universal_String;

    function Get_Base64 (Self : File_Information) return String;
    function Get_Base64 (Filepath : String) return String;

private

    type File_Information is tagged record
        Filepath : Wide_Wide_String;
        Name : Universal_String;
        Size : Integer;
        Content_Type : Universal_String;
    end record;

end Files;

