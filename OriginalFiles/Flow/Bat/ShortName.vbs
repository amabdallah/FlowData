Set fso=CreateObject("Scripting.FileSystemObject")

' Is object a file or folder?
If fso.FolderExists(WScript.Arguments(0)) Then
   'It's a folder
   Set objFolder = fso.GetFolder(WScript.Arguments(0))
   rtrn = InputBox("Here's your short path:", "SHORT PATH", objFolder.ShortPath)
End If

If fso.FileExists(WScript.Arguments(0)) Then
   'It's a file
   Set objFile = fso.GetFile(WScript.Arguments(0))
   rtrn = InputBox("Here's your short path:", "SHORT PATH", objFile.ShortPath)
End If