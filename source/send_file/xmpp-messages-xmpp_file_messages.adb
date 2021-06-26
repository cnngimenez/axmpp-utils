--  xmpp-messages-xmpp_file_messages.adb ---

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

with XML.SAX.Attributes;

package body XMPP.Messages.XMPP_File_Messages is

    overriding procedure Custom_Content
      (Self   : XMPP_File_Message;
       Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

        X_Element : constant Universal_String :=
          To_Universal_String ("x");
        X_Uri : constant Universal_String :=
          To_Universal_String ("jabber:x:oob");
        XMLNS_Name : constant Universal_String :=
          To_Universal_String ("xmlns");
        URL_Element : constant Universal_String :=
          To_Universal_String ("url");
        Attrs : XML.SAX.Attributes.SAX_Attributes;

    begin
        Attrs.Set_Value (Qualified_Name => XMLNS_Name,
                         Value => X_Uri);

        Writer.Start_Element (Qualified_Name => X_Element,
                              Attributes => Attrs);

        Writer.Start_Element (Qualified_Name => URL_Element);
        Writer.Characters (Self.File_Get_Url);
        Writer.End_Element (Qualified_Name => URL_Element);

        Writer.End_Element (Qualified_Name => X_Element);
    end Custom_Content;

    procedure Set_File_Get_Url (Self : in out XMPP_File_Message;
                                Url : Universal_String) is
    begin
        Self.File_Get_Url := Url;
    end Set_File_Get_Url;

end XMPP.Messages.XMPP_File_Messages;
