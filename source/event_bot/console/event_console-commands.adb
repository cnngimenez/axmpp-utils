--  event_console-commands.adb ---

--  Copyright 2022 cnngimenez
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

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with League.String_Vectors;
with Event_Console.Implementations;

package body Event_Console.Commands is

    function Get_Argument (Self : Command; Name : Wide_Wide_String)
                          return Universal_String is
        use League.String_Vectors;

        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
        Splitted_Arguments : constant Universal_String_Vector :=
          Self.Arguments.Split (Lf);
        I : Positive := 1;
        Length : constant Natural := Splitted_Arguments.Length;
        Current_Argument : Universal_String;
    begin
        Current_Argument := Splitted_Arguments.Element (I);
        while not Current_Argument.Starts_With (Name) and then I <= Length loop
            I := I + 1;
            Current_Argument := Splitted_Arguments.Element (I);
        end loop;

        if I > Length then
            return Empty_Universal_String;
        end if;

        --  Return only the argument value
        return Current_Argument.Tail_From (Name'Length + 1);
    end Get_Argument;

    function Get_Arguments (Self : Command) return Universal_String is
    begin
        return Self.Arguments;
    end Get_Arguments;

    function Get_Data_Argument (Self : Command) return Universal_String is
        use League.String_Vectors;

        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
        Splitted_Arguments : constant Universal_String_Vector :=
          Self.Arguments.Split (Lf);
        I : Positive := 1;
        Length : constant Natural := Splitted_Arguments.Length;
        Current_Argument : Universal_String;
        Data_Argument : constant Universal_String :=
          To_Universal_String ("data=");
    begin
        --  goto data= parameter
        Current_Argument := Splitted_Arguments.Element (I);
        while I <= Length and then Current_Argument /= Data_Argument loop
            I := I + 1;
            Current_Argument := Splitted_Arguments.Element (I);
        end loop;

        --  Return the rest of the strings joined with LF again.
        return Splitted_Arguments.Slice (I, Length).Join (Lf);
    end Get_Data_Argument;

    function Get_Name (Self : Command) return Name_Type is
    begin
        return Self.Name;
    end Get_Name;

    function Get_Name (Self : Command) return Universal_String is
        use Ada.Characters.Conversions;
    begin
        return To_Universal_String (To_Wide_Wide_String (Self.Name'Image));
    end Get_Name;

    function Get_Nth_Argument (Self : Command; N : Positive)
                              return Universal_String is
        Starting, Ending : Natural;
        I : Natural := N;
        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
    begin
        --  Search for the starting position.

        --  I = 0 means no more new lines!
        Starting := Index (Self.Arguments, 1, Lf);

        while I > 0 and then Starting > 0 loop
            Starting := Self.Arguments.Index (Starting, Lf);
            I := I - 1;
        end loop;

        if Starting = 0 then
            --  The nth parameter has not been found, return "".
            return Empty_Universal_String;
        end if;

        --  Search for the ending position.

        Ending := Self.Arguments.Index (Starting, Lf);
        if Ending = 0 then
            --  No ending new line, just return the last character.
            Ending := Self.Arguments.Length;
        end if;

        return Self.Arguments.Slice (Starting, Ending);
    end Get_Nth_Argument;

    procedure Initialize (Self : in out Command;
                          Command_String : Universal_String) is
        Name, Arguments : Universal_String;
        Name_Ending : Positive;
        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
    begin
        Name_Ending := Command_String.Index (Lf);
        Name := Command_String.Head_To (Name_Ending);
        Arguments := Command_String.Tail_From (Name_Ending + 1);
        --  Remove "end command" & LF string.
        Arguments.Slice (1, Arguments.Length - 12);

        Self.Name := Namestring_To_Nametype (Name);
        Self.Arguments := Arguments;
    end Initialize;

    procedure Initialize (Self : in out Command;
                          Name : Name_Type;
                          Arguments : Universal_String) is
    begin
        Self.Name := Name;
        Self.Arguments := Arguments;
    end Initialize;

    function Is_Name (Self : Command; Name : Name_Type) return Boolean is
    begin
        return Self.Name = Name;
    end Is_Name;

    function Namestring_To_Nametype (Name : Universal_String)
                                    return Name_Type is
        use Ada.Strings.Wide_Wide_Maps;
        use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
        use Ada.Strings.Wide_Wide_Fixed;

        Elt : Name_Type := Name_Type'First;
        Search_Name : Wide_Wide_String := To_Wide_Wide_String (Name);
        Standard_Name : Universal_String;
        --  The Name in lowercase and spaces instead of symbols.

        Underscore_To_Space_Map : constant Wide_Wide_Character_Mapping :=
          To_Mapping ("_.:", "   ");
    begin
        --  Prepare the name string: convert to lowercase and replace symbols
        Translate (Search_Name, Lower_Case_Map);
        Translate (Search_Name, Underscore_To_Space_Map);
        Standard_Name := To_Universal_String (Search_Name);

        --  Search which Name_Type enumerate has the same string representation
        while Elt < Name_Type'Last and then
          Nametype_To_Namestring (Elt) /= Standard_Name
        loop
            Elt := Name_Type'Succ (Elt);
        end loop;

        return Elt;
    end Namestring_To_Nametype;

    function Nametype_To_Namestring (Name : Name_Type;
                                     Use_Space : Boolean := True;
                                     Lower_Case : Boolean := True)
                                    return Universal_String is
        use Ada.Strings.Wide_Wide_Maps;
        use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
        use Ada.Strings.Wide_Wide_Fixed;

        Namestr : Wide_Wide_String := Name'Wide_Wide_Image;
        Underscore_To_Space_Map : constant Wide_Wide_Character_Mapping :=
          To_Mapping ("_", " ");
    begin
        if Use_Space then
            Translate (Namestr, Underscore_To_Space_Map);
        end if;

        if Lower_Case then
            Translate (Namestr, Lower_Case_Map);
        end if;

        return To_Universal_String (Namestr);
    end Nametype_To_Namestring;

    procedure Run (Self : Command;
                   Session : not null Event_Sessions.Session_Access) is
        use Event_Console.Implementations;
    begin
        if Self.Get_Name = End_Bot then
            End_Bot;
        elsif Self.Get_Name = Send_Message then
            Send_Message (Session,
                          Self.Get_Argument ("to"),
                          Self.Get_Data_Argument);
        end if;
    end Run;

end Event_Console.Commands;
