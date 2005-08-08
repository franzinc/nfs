' VBScript
Set Sh = CreateObject("WScript.Shell")
key =  "HKEY_CLASSES_ROOT\"
Sh.RegDelete key & "CLSID\{D7DDF8D4-92DE-4e76-9326-8746C446AAC4}\"
