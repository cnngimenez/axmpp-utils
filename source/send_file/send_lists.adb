--  send_lists.adb ---

--  Copyright 2021 cnngimenez
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Containers;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

package body Send_Lists is

    procedure Add (Send_List : in out Send_List_Type;
                   Filepath : Unbounded_String;
                   Mime_Type : Unbounded_String;
                   JID : Unbounded_String) is

        procedure Add_New_Vector;

        procedure Add_New_Vector is
            File_Data : File_Data_Type;
        begin
            File_Data.Filepath := Filepath;
            File_Data.Mime_Type := Mime_Type;
            File_Data.Jid_List.Append (JID);

            Send_List.Insert (Filepath, File_Data);
        end Add_New_Vector;

    begin
        if Send_List.Contains (Filepath) then
            declare
                File_Data : File_Data_Type :=
                  Send_List.Element (Filepath);
            begin
                --  File_Data.Mime_Type := Mime_Type;
                File_Data.Jid_List.Append (JID);
                Send_List.Replace (Filepath, File_Data);
            end;
        else
            Add_New_Vector;
        end if;
    end Add;

    procedure Add (Send_List : in out Send_List_Type;
                   Filepath : String;
                   Mime_Type : String;
                   JID : String) is

        Un_Filepath : constant Unbounded_String :=
          To_Unbounded_String (Filepath);
        Un_Jid : constant Unbounded_String :=
          To_Unbounded_String (JID);
        Un_Mime : constant Unbounded_String :=
          To_Unbounded_String (Mime_Type);

    begin
        Send_List.Add (Un_Filepath, Un_Mime, Un_Jid);
    end Add;

    function Exists_Index (Self : Send_List_Type;
                           Index : Natural) return Boolean is
        use Ada.Containers;
    begin
        return Count_Type (Index) <= Length (Self);
    end Exists_Index;

    procedure Fill_From_File (Send_List : in out Send_List_Type;
                              Load_File_Path : String) is
        use Ada.Text_IO;

        File : File_Type;
        Un_Mime, Un_Jid, Un_Filepath : Unbounded_String;
    begin
        Open (File, In_File, Load_File_Path);

        loop
            Un_Filepath := To_Unbounded_String (Get_Line (File));
            Un_Mime := To_Unbounded_String (Get_Line (File));

            loop
                Un_Jid := To_Unbounded_String (Get_Line (File));
                if Un_Jid /= "--" then
                    Send_List.Add (Un_Filepath, Un_Mime, Un_Jid);
                end if;
                exit when Un_Jid = "--" or else End_Of_File (File);
            end loop;

            exit when End_Of_File (File);
        end loop;

        Close (File);
    exception
    when Error : End_Error =>
        Ada.Text_IO.Put_Line
          ("Fill_From_File, file exception:"
             & Ada.Exceptions.Exception_Information (Error));
        Close (File);
        return;
    end Fill_From_File;

    function Get_File_Information (Self : Send_List_Type;
                                   Index : Natural) return File_Information is
        File_Data : constant File_Data_Type := Self.Get_JIDs (Index);
    begin
        return Get_File_Information (File_Data);
    end Get_File_Information;

    function Get_File_Information (Self : File_Data_Type)
                                  return File_Information is
        File_Info : File_Information;
    begin
        File_Info := Create (To_String (Self.Filepath));
        File_Info.Set_Content_Type (To_String (Self.Mime_Type));
        return File_Info;
    end Get_File_Information;

    function Get_JIDs (Self : Send_List_Type'Class;
                       Index : Natural) return File_Data_Type is
        use Send_File_Package;
        C : Cursor := First (Self);
    begin
        for I in 1 .. (Index - 1) loop
            Next (C);
        end loop;

        return Self (C);
    end Get_JIDs;

    procedure Iterate_JIDs (Self : Send_List_Type;
                            Index : Natural;
                            Process : not null access
                              procedure (Filepath : String; Jid : String)) is
        use JID_List_Package;

        procedure Iterate_Process (Position : Cursor);

        File_Data : constant File_Data_Type :=
          Self.Get_JIDs (Index);

        procedure Iterate_Process (Position : Cursor) is
            JID : constant String := To_String (Element (Position));
        begin
            Process (To_String (File_Data.Filepath), JID);
        end Iterate_Process;

    begin
        File_Data.Jid_List.Iterate (Iterate_Process'Access);
    end Iterate_JIDs;

    --  procedure Remove_All_JID (Send_File : in out Send_List_Type;
    --                            Filepath : String);

    --  procedure Remove_One (Send_File : in out Send_List_Type;
    --                        Filepath : String;
    --                        JID : string);

    function To_String (Self : File_Data_Type) return String is
        use JID_List_Package;
        procedure Vector_Process (Position : Cursor);

        Output : Unbounded_String;

        procedure Vector_Process (Position : Cursor) is
        begin
            Append (Output, Element (Position));
            Append (Output, ", ");
        end Vector_Process;
    begin
        Append (Output, "- Filepath: ");
        Append (Output, Self.Filepath);
        Append (Output, LF);
        Append (Output, "- Mime-type: ");
        Append (Output, Self.Mime_Type);
        Append (Output, LF);
        Append (Output, "- JID: ");
        Self.Jid_List.Iterate (Vector_Process'Access);
        return To_String (Output);
    end To_String;

    function To_String (Self : Send_List_Type) return String is
        use Send_File_Package;
        procedure Hash_Process (Position : Cursor);

        Output : Unbounded_String;

        procedure Hash_Process (Position : Cursor) is
            File_Data : constant File_Data_Type := Element (Position);
        begin
            Append (Output, LF);
            Append (Output, "- Key:");
            Append (Output, Key (Position));
            Append (Output, LF);
            Append (Output, "- Element:");
            Append (Output, LF);
            Append (Output, To_String (File_Data));
        end Hash_Process;
    begin
        Append (Output, "Files:");
        Self.Iterate (Hash_Process'Access);
        return To_String (Output);
    end To_String;

end Send_Lists;
