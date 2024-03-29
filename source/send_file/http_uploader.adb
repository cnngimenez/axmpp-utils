--  http_uploader.adb ---

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

with Ada.Exceptions;
use Ada.Exceptions;
with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings;
--  with Ada.Strings.Fixed;

with AWS.Client;
with AWS.Response;
with AWS.Net.SSL.Certificate;
with AWS.Containers.Tables;

package body HTTP_Uploader is

    function Get_File_Data (Path : String) return String;
    function Wws2s (Wws : Wide_Wide_String) return String;

    function Get_File_Data (Path : String) return String is
        use Ada.Text_IO;
        use Ada.Strings.Unbounded;

        File : File_Type;
        Data : Unbounded_String;
        Ch : Character;
    begin
        Open (File, In_File, Path);

        while not End_Of_File (File) loop
            Get (File, Ch);
            Append (Data, Ch);
        end loop;

        Close (File);

        return To_String (Data);
    end Get_File_Data;

    procedure Upload_File (Put_Url : Universal_String;
                           Path : String;
                           Mime_Type : String) is
        Url : constant String := Wws2s (To_Wide_Wide_String (Put_Url));
        use Ada.Text_IO;
    begin
        Upload_File (Url, Path, Mime_Type);
    exception
    when Errors : others =>
        Put_Line ("Error in HTTP_Upload.Upload_File 1:");
        Put_Line (Exception_Information (Errors));
        raise;
    end Upload_File;

    procedure Upload_File (Put_Url : String;
                           Path : String;
                           Mime_Type : String) is
        use Ada.Text_IO;
        use Ada.Streams;
        use Ada.Strings.Unbounded;
        use AWS;

        procedure Check_Certificate
          (Certificate : AWS.Net.SSL.Certificate.Object);
        --  procedure Show_Progress (Total, Sent : Stream_Element_Offset);
        function Read_File (Path : String) return Stream_Element_Array;
        procedure Show_Stream (Sea : Stream_Element_Array);

        procedure Check_Certificate
          (Certificate : AWS.Net.SSL.Certificate.Object) is
            use AWS.Net.SSL.Certificate;
        begin
            if Verified (Certificate) then
                Put_Line ("Certificate verified.");
                return;
            end if;

            Put_Line ("WARNING: SSL Certificate has not been verified!");
            Put_Line ("--  HTTPS Certificate information:");
            Put_Line ("Common_Name: " & Common_Name (Certificate));
            Put_Line ("Subject: " & Subject (Certificate));
            Put_Line ("Issuer: " & Issuer (Certificate));
            Put_Line ("Serial number: " & Serial_Number (Certificate));
            Put_Line ("Verified: " & Verified (Certificate)'Image);
            Put_Line ("Status: " & Status (Certificate)'Image);
            Put_Line ("Status message: " & Status_Message (Certificate));
            Put_Line ("--");
        end Check_Certificate;

        function Read_File (Path : String) return Stream_Element_Array is
            use Ada.Streams.Stream_IO;

            Filesize : Stream_Element_Offset :=
              Stream_Element_Offset (Ada.Directories.Size (Path));
            Stream_Array : Stream_Element_Array (1 .. Filesize);
            File : Stream_IO.File_Type;
        begin
            Put_Line ("Path: " & Path);
            Stream_IO.Open (File, In_File, Path);
            Stream_IO.Read (File, Stream_Array, Filesize);
            Put_Line ("Stream offset: " & Filesize'Image);
            --  Put_Line ("Stream last: " & Stream_Array'Last'Image);
            --  Show_Stream (Stream_Array);
            Close (File);

            return Stream_Array;
        end Read_File;

        --  procedure Show_Progress (Total, Sent : Stream_Element_Offset) is
        --  begin
        --      Put_Line ("Uploading " & Sent'Image & " of " & Total'Image);
        --  end Show_Progress;

        procedure Show_Stream (Sea : Stream_Element_Array) is
            Element : Stream_Element;
            Last : constant Stream_Element_Offset := Sea'Last;
        begin
            for C in Sea'Range loop
                Element := Sea (C);
                Put (C'Image & "/" & Last'Image & ":" & Element'Image & "|");
            end loop;
        end Show_Stream;

        Connection : Client.HTTP_Connection;
        Rdata : Response.Data;
        Header_List : Client.Header_List;

    begin

        Put_Line ("Uploading to " & Put_Url
                    & "(" & Mime_Type & ")");

        AWS.Containers.Tables.Add
          (AWS.Containers.Tables.Table_Type (Header_List),
           "Content-Type", Mime_Type);
        --  AWS.Containers.Tables.Add
        --    (AWS.Containers.Tables.Table_Type (Header_List),
        --     "Content-Length",
        --     Ada.Strings.Fixed.Trim (Filesize'Image, Ada.Strings.Both));

        Client.Create (Connection => Connection,
                       Host => Put_Url,
                       Persistent => False);
        --  Client.Upload (Connection => Connection,
        --                 Result => Rdata,
        --                 Filename => Path,
        --                 URI => Put_Url,
        --                 Headers => Header_List);
        --  --  Progress => Show_Progress'Access);
        Client.Put (Connection => Connection,
                    Result => Rdata,
                    --  Data => Get_File_Data (Path),
                    Data => Read_File (Path),
                    URI => Put_Url,
                    Headers => Header_List);

        --  --  Non Keep-Alive (No Connection instance).
        --  Rdata := Client.Put (Data => Get_File_Data (Path),
        --                       --  Data => Stream_Array,
        --                       URL => Put_Url,
        --                       Headers => Header_List);

        Put_Line ("Status code: " &
                    Response.Status_Code (Rdata)'Image);
        Put_Line ("Message body: " &
                    To_String (Response.Message_Body (Rdata)));

        Put_Line ("Cipher description: "
                    & Client.Cipher_Description (Connection));
        Check_Certificate (Client.Get_Certificate (Connection));

        Client.Close (Connection);

        --  Show_Stream (Stream_Array);
    exception
    when Errors : others =>
        Put_Line ("Error in HTTP_Upload:");
        Put_Line (Exception_Information (Errors));
        Put_Line ("Attempting to close connection...");
        Client.Close (Connection);
        raise;
    end Upload_File;

    --  Convert from Wide_Wide_String to String
    --
    --  WARNING: Information loss. All Wide_Wide_Character values greater
    --  than 256 (8-bits) will be ignored.
    function Wws2s (Wws : Wide_Wide_String) return String is
        use Ada.Strings.Unbounded;
        use Ada.Text_IO;

        Amount : constant Natural := Wws'Length;
        Temp_Str : Unbounded_String;
        Wwc : Wide_Wide_Character;
        C : Character;
    begin
        for I in Natural range 1 .. Amount loop
            Wwc := Wws (I);
            if Wide_Wide_Character'Pos (Wwc) < 256 then
                C := Character'Val (Wide_Wide_Character'Pos (Wwc));
                Append (Temp_Str, C);
            else
                Put_Line ("Warning: character in Wide Wide String cannot be "
                            & "transformed into String");
            end if;
        end loop;

        return To_String (Temp_Str);
    end Wws2s;

end HTTP_Uploader;
