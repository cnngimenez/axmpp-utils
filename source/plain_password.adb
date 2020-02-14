--  plain_password.adb ---

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
with Ada.Wide_Wide_Text_Io;
use Ada.Wide_Wide_Text_Io;
with Ada.Command_Line;
use Ada.Command_Line;

with XMPP.PLAIN_Auth;
use XMPP.PLAIN_Auth;
with Xmpp.Logger;

procedure Plain_Password is    
begin 
    if Argument_Count < 2 then
        Put_Line ("Synopsis:");
        Put_Line ("    plain_password USERNAME PASSWORD");
        New_Line;
        Put_Line ("Create a Base64 convertion for the PLAIN "
                    & "authentication method");
        return;
    end if;
    Xmpp.Logger.Enable_Debug;

    Put_Line (Plain_Password 
                (To_Wide_Wide_String (Argument (1)),
                 To_Wide_Wide_String (Argument (2))));
end Plain_Password;
