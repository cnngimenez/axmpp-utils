--  remove_host.adb ---

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
use Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;

with XMPP.Utils;
use XMPP.Utils;

procedure Remove_Host is
begin
    if Argument_Count < 1 then
        Put_Line ("Synopsis:");
        Put_Line ("  bin/remove_host JID");
        New_Line;
        Put_Line ("Remove the host part from a JID.");
        return;
    end if;
    
    Put_Line (Remove_Host (To_Wide_Wide_String (Argument (1))));
end Remove_Host;
