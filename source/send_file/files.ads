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

with League.Strings;
use League.Strings;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Directories;
use Ada.Directories;

package Files is
    --  Store the file information.
    type File_Information is tagged private;

    --  Take the file information from the given existent file.
    function Create (Filepath : String) return File_Information;

    function Get_Filepath (Self : File_Information) return Unbounded_String;
    function Get_Filepath (Self : File_Information) return String;
    function Get_Name (Self : File_Information) return Universal_String;
    function Get_Size (Self : File_Information) return Universal_String;
    function Get_Size (Self : File_Information) return Integer;
    function Get_Size (Self : File_Information) return File_Size;
    procedure Set_Content_Type (Self : in out File_Information;
                                Content_Type : Universal_String);
    function Get_Content_Type (Self : File_Information)
                              return Universal_String;

    --  function Get_Base64 (Self : File_Information) return String;
    --  function Get_Base64 (Filepath : String) return String;

private

    type File_Information is tagged record
        Filepath : Unbounded_String;
        Name : Universal_String;
        Size : File_Size;
        Content_Type : Universal_String;
    end record;

end Files;
