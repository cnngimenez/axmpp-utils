--  pipe_manager.ads ---

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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO;
use Ada.Text_IO;

with League.Strings;
use League.Strings;

package Pipe_Manager is

    type Pipe_Type is tagged private;
    type Direction_Type is (Input_Only, Output_Only);

    procedure Initialize (Pipe : in out Pipe_Type;
                          Path : String;
                          Direction : Direction_Type := Input_Only);

    function Attend_Pipe (Pipe : in out Pipe_Type) return String;
    function Attend_Pipe (Pipe : in out Pipe_Type) return Unbounded_String;
    function Attend_Pipe (Pipe : in out Pipe_Type) return Universal_String;

    procedure Write_Message (Pipe : in out Pipe_Type; Message : String);
    procedure Write_Message (Pipe : in out Pipe_Type;
                             Message : Universal_String);
    procedure Write_Message (Pipe : in out Pipe_Type;
                             Message : Wide_Wide_String);
    procedure Write_Message (Pipe : in out Pipe_Type;
                             Message : Unbounded_String);

    function Get_Last_Message (Pipe : Pipe_Type) return Unbounded_String;
    function Get_Path (Pipe : Pipe_Type) return Unbounded_String;
    function Get_Direction (Pipe : Pipe_Type) return Direction_Type;

    Pipe_Creation_Error : exception;

private

    type Pipe_Type is tagged record
        Direction : Direction_Type;
        Path : Unbounded_String;
        Last_Message : Unbounded_String;
    end record;

    --  Read a message until EOF and return it.
    function Read_Message (File : File_Type) return Unbounded_String;

    function Date_String return String;
    function Date_String return Wide_Wide_String;

    function S2Us (S : String) return Universal_String;

end Pipe_Manager;
