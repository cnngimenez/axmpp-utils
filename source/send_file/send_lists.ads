--  send_lists.ads ---

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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

with Files;
use Files;

--  Declares types for storing a list of files associated with their JIDs.
--
--  This only stores this information, it will not send them.
--
--  # File format
--  A file can be loaded with all the associations. This file has the
--  following syntax:
--
--  ```
--  Path to file
--  MIME-Type1
--  JID1@xmpp.org
--  JID2@xmpp.org
--  --
--  Path to file2
--  MIME-Type2
--  JID1@xmpp.org
--  JID3@xmpp.org
--  JID4@xmpp.org
--  --
--  Path to file3
--  MIME-Type3
--  JID5@xmpp.org
--  ```
--
package Send_Lists is

    type Send_List_Type is tagged private;
    type File_Data_Type is tagged private;

    procedure Fill_From_File (Send_List : in out Send_List_Type;
                              Load_File_Path : String);
    --  Load a file with the file to send associated to several JIDs.

    procedure Add (Send_List : in out Send_List_Type;
                   Filepath : Unbounded_String;
                   Mime_Type : Unbounded_String;
                   JID : Unbounded_String);

    procedure Add (Send_List : in out Send_List_Type;
                   Filepath : String;
                   Mime_Type : String;
                   JID : String);
    --  Add an association between a Filepath to send with its JID
    --  destinatary.
    --  Creates and add everything required to store this association.

    --  procedure Remove_All_JID (Send_File : in out Send_List_Type;
    --                            Filepath : String);
    --  Remove all JID associated to Filepath. Filepath is also removed.

    --  procedure Remove_One (Send_File : in out Send_List_Type;
    --                        Filepath : String;
    --                        JID : string);
    --  Remove one association: Filepath to JID.

    function Get_JIDs (Self : Send_List_Type'Class;
                       Index : Natural) return File_Data_Type;
    function Get_File_Information (Self : Send_List_Type;
                                   Index : Natural) return File_Information;

    procedure Iterate_JIDs (Self : Send_List_Type;
                            Index : Natural;
                            Process : not null access
                              procedure (Filepath : String; Jid : String));

    function Get_File_Information (Self : File_Data_Type)
                                  return File_Information;

    function Exists_Index (Self : Send_List_Type;
                           Index : Natural) return Boolean;

    function To_String (Self : File_Data_Type) return String;
    function To_String (Self : Send_List_Type) return String;

    --  -------------------------
private
    --  -------------------------

    package JID_List_Package is new Ada.Containers.Vectors
      (Element_Type => Unbounded_String,
       Index_Type => Positive);

    type File_Data_Type is tagged record
        Filepath : Unbounded_String;
        Mime_Type : Unbounded_String;
        Jid_List : JID_List_Package.Vector;
    end record;

    package Send_File_Package is new Ada.Containers.Hashed_Maps
      (Key_Type => Unbounded_String,
       Element_Type => File_Data_Type,
       Hash => Ada.Strings.Unbounded.Hash,
       Equivalent_Keys => Ada.Strings.Unbounded."=");
       --  "=" => Jid_List_Package."=");

    type Send_List_Type is new Send_File_Package.Map with null record;

end Send_Lists;
