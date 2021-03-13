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

package Pipe_Manager is
   
   type Pipe_Type is tagged private;
   
   procedure Create (Pipe : in out Pipe_Type; Path : String);
   function Get_Last_Message(Pipe : Pipe_Type) return Unbounded_String;
   function Get_Path(Pipe : Pipe_Type) return Unbounded_String;
   
private
   type Pipe_Type is tagged record
      Path : Unbounded_String;
      Last_Message : Unbounded_String;
   end record;
end Pipe_Manager;
