with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;

with HTTP_Uploader;
with Files;
use Files;

procedure Uploader is
    File_Info : File_Information;
begin
    if Argument_Count /= 3 then
        Put_Line ("Synopsis:");
        Put_Line ("    bin/uploader URL File_Path Mime-Type");
        New_Line;
        Put_Line ("Send a file to the provided HTTP Server.");

        Set_Exit_Status (Success);
        return;
    end if;

    File_Info := Create (Argument (2));
    File_Info.Set_Content_Type (Argument (3));

    if not File_Info.File_Exists then
        Put_Line ("The file given by parameter does not exists!");

        Set_Exit_Status (Failure);
        return;
    end if;

    Put_Line ("File information:");
    Put_Line (File_Info.To_String);

    Put_Line ("Sending data...");
    HTTP_Uploader.Upload_File (Argument (1),
                               File_Info.Get_Filepath,
                               File_Info.Get_Content_Type);

    Put_Line ("Upload finished.");
    Set_Exit_Status (Success);
end Uploader;
