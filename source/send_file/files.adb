--  files.adb ---

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

with Ada.Characters.Conversions;

package body Files is

    function Create (Filepath : String) return File_Information is
        use Ada.Characters.Conversions;

        File_Info : File_Information;
    begin
        File_Info.Filepath := To_Unbounded_String (Filepath);
        File_Info.Size := Ada.Directories.Size (Filepath);
        File_Info.Name := To_Universal_String
          (To_Wide_Wide_String
             (Ada.Directories.Simple_Name (Filepath)));
        --  File Info.Content_Type :=
        return File_Info;
    end Create;

    function Get_Content_Type (Self : File_Information)
                              return Universal_String is
    begin
        return Self.Content_Type;
    end Get_Content_Type;

    function Get_Filepath (Self : File_Information)
                          return Unbounded_String is
    begin
        return Self.Filepath;
    end Get_Filepath;

    function Get_Filepath (Self : File_Information)
                          return String is
    begin
        return To_String (Self.Filepath);
    end Get_Filepath;

    function Get_Name (Self : File_Information) return Universal_String is
    begin
        return Self.Name;
    end Get_Name;

    function Get_Size (Self : File_Information) return Universal_String is
    begin
        return League.Strings.To_Universal_String
          (Self.Size'Wide_Wide_Image);
    end Get_Size;

    function Get_Size (Self : File_Information) return Integer is
    begin
        return Integer (Self.Size);
    end Get_Size;

    function Get_Size (Self : File_Information) return File_Size is
    begin
        return Self.Size;
    end Get_Size;

    --  function Get_Base64 (Self : File_Information) return String;
    --  function Get_Base64 (Filepath : String) return String;

    procedure Set_Content_Type (Self : in out File_Information;
                                Content_Type : Universal_String) is
    begin
        Self.Content_Type := Content_Type;
    end Set_Content_Type;

end Files;
