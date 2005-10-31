' VBScript
' $Id: nfscleanreg.vbs,v 1.2.4.4 2005/10/31 21:11:49 layer Exp $

' adapted from code found here:
'    http://mikesalsbury.com/mambo/content/view/134/
'
' This function returns a "true/false" response if a
' given Windows Registry key exists.
'
' Since a script will generate an error if it attempts
' to read a non-existent Windows Registry key, we use
' a local "on error resume next" to keep executing
' normally if the error occurs.
'
Function regEntryExists(theEntry)
  On error resume next
  Set shell = WScript.CreateObject("WScript.Shell")
  entry = shell.RegRead(theEntry)
  If Err.Number <> 0 then
    'msgbox "FALSE: " & Err.Description
    Err.Clear
    regEntryExists = FALSE
  else
    Err.Clear
    'msgbox "TRUE"
    regEntryExists = TRUE
  end if
  Set shell = Nothing
End Function

Function RegDeleteEntry(key)
  Set Sh = CreateObject("WScript.Shell")
  if regEntryExists(key) then
     Sh.RegDelete key
     msgbox "Key " & key & " was deleted."
  else
     msgbox "Key " & key & " does not exist."
  end if
End Function

RegDeleteEntry "HKCR\CLSID\{D9AD2502-2F93-4c0b-BC3C-20689232C3B0}\X0"
RegDeleteEntry "HKCR\CLSID\{D7DDF8D4-92DE-4e76-9326-8746C446AAC4}\X0"
