' VBScript
Set Sh = CreateObject("WScript.Shell")
key =  "HKEY_CLASSES_ROOT\"
Sh.RegDelete key & "CLSID\{D7DDF8D4-92DE-4e76-9326-8746C446AAC4}\"
Sh.RegDelete key & "CLSID\{D9AD2502-2F93-4c0b-BC3C-20689232C3B0}\"
