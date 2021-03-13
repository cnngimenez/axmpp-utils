--  pipe_manager.adb ---

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

with Ada.Directories;
with Ada.Characters.Conversions;
use Ada.Characters.Conversions;

with Interfaces.C;

package body Pipe_Manager is

    use Interfaces.C;

    function Mkfifo (Filename : char_array; Mode : int) return int
    with Import, Convention => C, External_Name => "mkfifo";

    function Attend_Pipe (Pipe : in out Pipe_Type) return String is
      (To_String (Attend_Pipe (Pipe)));

    function Attend_Pipe (Pipe : in out Pipe_Type) return Universal_String is
      (S2Us (Attend_Pipe (Pipe)));

    function Attend_Pipe (Pipe : in out Pipe_Type) return Unbounded_String is
        File : File_Type;
    begin
        Open (File, In_File, To_String (Pipe.Path));
        Pipe.Last_Message := Read_Message (File);
        Close (File);

        return Pipe.Last_Message;
    end Attend_Pipe;

    function Get_Last_Message (Pipe : Pipe_Type) return Unbounded_String is
    begin
        return Pipe.Last_Message;
    end Get_Last_Message;

    function Get_Path (Pipe : Pipe_Type) return Unbounded_String is
    begin
        return Pipe.Path;
    end Get_Path;

    procedure Initialize (Pipe : in out Pipe_Type; Path : String) is
        Pipe_Cname : constant char_array := To_C (Path, True);
    begin
        Pipe.Path := To_Unbounded_String (Path);
        Pipe.Last_Message := To_Unbounded_String ("");

        if not Ada.Directories.Exists (To_String (Pipe.Path)) then
            if Mkfifo (Pipe_Cname, 8#777#) /= 0 then
                raise Pipe_Creation_Error;
            end if;
        end if;
    end Initialize;

    function Read_Message (File : File_Type) return Unbounded_String is
        Buffer : Unbounded_String;
        C : Character;
    begin
        Buffer := To_Unbounded_String ("");

        loop
            Get (File, C);
            Append (Buffer, C);
            exit when End_Of_File (File);
        end loop;

        return Buffer;
    end Read_Message;

    function S2Us (S : String) return Universal_String is
      (To_Universal_String  (To_Wide_Wide_String (S)));

end Pipe_Manager;
