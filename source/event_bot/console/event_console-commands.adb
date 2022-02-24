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
    begin
        return Self.Arguments.Element (To_Universal_String (Name));
    end Get_Argument;

    function Get_Argument_Count (Self : Command) return Natural is
    begin
        return Natural (Self.Arguments.Length);
    end Get_Argument_Count;

    function Get_Arguments (Self : Command) return Universal_String is
        Argument_String : Universal_String;
        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
        use Argument_Map_Package;
    begin
        for Cursor in Self.Arguments.Iterate loop
            Argument_String := Key (Cursor) & "=" & Element (Cursor) & Lf;
        end loop;
        return Argument_String;
    end Get_Arguments;

    function Get_Data_Argument (Self : Command) return Universal_String is
    begin
        return Self.Arguments.Element (To_Universal_String ("data"));
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

    procedure Initialize (Self : in out Command;
                          Command_String : Universal_String) is
        use Argument_Map_Package;

        Name, Arguments : Universal_String;
        Name_Ending : Natural;
        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
    begin
        Self.Name := Unknown;
        Clear (Self.Arguments);

        --  Retrive the name from the command string.
        Name_Ending := Command_String.Index (Lf);

        if Name_Ending = 0 then
            --  Malformed command.
            Self.Name := Malformed_Command;
            return;
        end if;

        Name := Command_String.Head_To (Name_Ending - 1);

        --  Retrieve the Argument from the command string.
        Arguments := Command_String.Slice (Name_Ending + 1,
                                           Command_String.Length);
        --  Remove "end command" & LF string.
        if Arguments.Length < 12 then
            --  It does not end in "end command"!
            Self.Name := Malformed_Command;
            Clear (Self.Arguments);
            return;
        elsif Arguments.Length = 12 then
            Arguments := Empty_Universal_String;
        else
            Arguments.Slice (1, Arguments.Length - 12);
        end if;

        Self.Name := Namestring_To_Nametype (Name);
        Parse_Argument_String (Self, Arguments);
    end Initialize;

    procedure Initialize (Self : in out Command;
                          Name : Name_Type;
                          Arguments : Universal_String) is
        --  Arguments has no "end command" string.
    begin
        Self.Name := Name;
        Parse_Argument_String (Self, Arguments);
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

    procedure Parse_Argument_String (Self : in out Command;
                                     Arguments : Universal_String) is
        use League.String_Vectors;
        use Argument_Map_Package;

        procedure Insert_Oneline_Argument (Line : Universal_String);

        procedure Insert_Oneline_Argument (Line : Universal_String) is
            Separator_Position : constant Natural := Line.Index ('=');
            Key, Element : Universal_String;
        begin
            if Separator_Position < 1 then
                return;
            end if;
            Key := Line.Head (Separator_Position - 1);
            Element := Line.Tail_From (Separator_Position + 1);

            if not Key.Is_Empty then
                Include (Self.Arguments, Key, Element);
            end if;
        end Insert_Oneline_Argument;

        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
        Splitted_Arguments : constant Universal_String_Vector :=
          Arguments.Split (Lf);
        Is_Data : Boolean := False;
        Line, Data_Element : Universal_String;
        Data_Argument : constant Universal_String :=
          To_Universal_String ("data=");
    begin
        for I in 1 .. Splitted_Arguments.Length loop
            Line := Splitted_Arguments.Element (I);

            if Line = Data_Argument then
                --  Starts the "data=" line...
                Is_Data := True;
                Line := Empty_Universal_String;
            end if;

            if not Line.Is_Empty then
                if Is_Data then
                    Data_Element.Append (Line);
                    Data_Element.Append (Lf);
                else
                    Insert_Oneline_Argument (Line);
                end if;
            end if;
        end loop;

        if not Data_Element.Is_Empty then
            Include (Self.Arguments,
                     To_Universal_String ("data"),
                     Data_Element);
        end if;
    end Parse_Argument_String;

    procedure Run (Self : Command;
                   Session : not null Event_Sessions.Session_Access;
                   Handler : not null Event_Handlers.Event_Handler_Access;
                   Output_Pipe : in out Pipe_Manager.Pipe_Type) is
        use Event_Console.Implementations;
    begin
        if Self.Get_Name = Bot_End then
            Bot_End;
        elsif Self.Get_Name = Bot_Is_Connected then
            Bot_Is_Connected (Session, Output_Pipe);
        elsif Self.Get_Name = Send_Message then
            Send_Message (Session,
                          Self.Get_Argument ("to"),
                          Self.Get_Data_Argument);
        elsif Self.Get_Name = Send_File then
            Send_File (Session, Handler, Output_Pipe,
                       Self.Get_Argument ("to"),
                       Self.Get_Argument ("file"));
        end if;
    end Run;

    function To_Universal_String (Arguments : Argument_Map)
                                 return Universal_String is
        use Argument_Map_Package;

        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
        Parsed_Arguments : Universal_String;
    begin
        for I in Arguments.Iterate loop
            Parsed_Arguments := Parsed_Arguments &
              "    Arguments (""" & Key (I) &  """) = " &
              """" & Element (I) & """" & Lf;
        end loop;
        return Parsed_Arguments;
    end To_Universal_String;

    function To_Universal_String (Self : Command) return Universal_String is
        Lf : constant Wide_Wide_Character :=
          Ada.Characters.Wide_Wide_Latin_1.LF;
    begin
        return
          "--  Command: " & Lf &
          --  Name
          "  Name: " & Self.Get_Name & Lf &
          --  Argument information
          "  Arguments: " & Lf &
          "    Count:" & Self.Get_Argument_Count'Wide_Wide_Image & Lf &
          To_Universal_String (Self.Arguments) &
          --  Data argument
          "  Data argument parsed: " & Lf &
          """" & Self.Get_Data_Argument & """" & Lf;
    end To_Universal_String;

    function To_Wide_Wide_String (Self : Command) return Wide_Wide_String is
    begin
        return To_Wide_Wide_String (Self.To_Universal_String);
    end To_Wide_Wide_String;

end Event_Console.Commands;
