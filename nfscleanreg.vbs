' VBScript
' $Id: nfscleanreg.vbs,v 1.2.4.2 2005/10/31 19:49:27 layer Exp $

' from http://mikesalsbury.com/mambo/content/view/134/
'
' This function returns a "true/false" response if a
' given Windows Registry key exists.
'
' Since a script will generate an error if it attempts
' to read a non-existent Windows Registry key, we use
' a local "on error resume next" to keep executing
' normally if the error occurs.
'
Function RegEntryExists(theEntry)
  On error resume next
  set shell = CreateObject("WScript.Shell")
  entry = shell.RegRead(theEntry)
  If Err.Number <> 0 then
    Err.Clear
    RegEntryExists = FALSE
  else
    Err.Clear
    RegEntryExists = TRUE
  end if
  set shell = Nothing
End Function

Function RegDeleteEntry(key)
  set Sh = CreateObject("WScript.Shell")
  Sh.RegDelete
End Function

theEntry = "HKCR\CLSID\{D7DDF8D4-92DE-4e76-9326-8746C446AAC4}\"
if regEntryExists(theEntry) then
   RegDeleteEntry(theEntry)
   msgbox "Key " & theEntry & " was deleted."
else
   msgbox "Key " & theEntry & " does not exist."
end if

theEntry = "HKCR\CLSID\{D9AD2502-2F93-4c0b-BC3C-20689232C3B0}\"
if regEntryExists(theEntry) then
   RegDeleteEntry(theEntry)
   msgbox "Key " & theEntry & " was deleted."
else
   msgbox "Key " & theEntry & " does not exist."
end if
