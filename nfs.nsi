;; Allegro NFS NSIS installer script

;; Disable compression when developing (severely speeds up the debug
;; cycle)
;;SetCompress off
SetCompressor /SOLID lzma

!include WinMessages.nsh
!include servicelib.nsh
!include LogicLib.nsh

;------------------------------------------------------------------------------

; Author: Lilla (lilla@earthlink.net) 2003-06-13
; function IsUserAdmin uses plugin \NSIS\PlusgIns\UserInfo.dll
; This function is based upon code in \NSIS\Contrib\UserInfo\UserInfo.nsi
; This function was tested under NSIS 2 beta 4 (latest CVS as of this writing).
;
; Usage:
;   Call IsUserAdmin
;   Pop $R0   ; at this point $R0 is "true" or "false"
;
Function IsUserAdmin
Push $R0
Push $R1
Push $R2

ClearErrors
UserInfo::GetName
IfErrors Win9x
Pop $R1
UserInfo::GetAccountType
Pop $R2

StrCmp $R2 "Admin" 0 Continue
; Observation: I get here when running Win98SE. (Lilla)
; The functions UserInfo.dll looks for are there on Win98 too, 
; but just don't work. So UserInfo.dll, knowing that admin isn't required
; on Win98, returns admin anyway. (per kichik)
StrCpy $R0 "true"
Goto Done

Continue:
; You should still check for an empty string because the functions
; UserInfo.dll looks for may not be present on Windows 95. (per kichik)
StrCmp $R2 "" Win9x
StrCpy $R0 "false"
Goto Done

Win9x:
; we don't work on win9x...
StrCpy $R0 "false"

Done:

Pop $R2
Pop $R1
Exch $R0
FunctionEnd

!macro StopAndDeleteService UN
Function ${UN}StopAndDeleteService
  Push running
  Push nfs
  Push ""
  Call ${UN}Service
  Pop $0 ;response
  StrCmp $0 "true" 0 ServiceStopped
     ; Not using servicelib.nsh to stop the service because it
     ; does not wait until the service is fully stopped.
     ;Push "stop"
     ;Push "nfs"
     ;Push ""
     ;Call Service
     ;Pop $0 ;response
     DetailPrint "Stopping NFS service..."      
     ExecWait '"$WINDIR\system32\net.exe" stop nfs'

 ServiceStopped:
  ;; now delete
  DetailPrint "Removing NFS service..."
  Push delete
  Push nfs
  Push ""
  Call ${UN}Service
  Pop $0 ;response
  ;; Allow time for console's to exit, etc.
  Sleep 5000 

FunctionEnd
!macroend

!insertmacro StopAndDeleteService ""
!insertmacro StopAndDeleteService un.

;; Not used anymore, but leaving the code here for
;; educational purposes.
!macro StopConsole UN
Function ${UN}StopConsole
  ;; This probably won't work for consoles running in other
  ;; windows stations/desktops.

  FindWindowLoop:
 
  FindWindow $0 "" "Allegro NFS Console"
  IsWindow $0 0 NoMoreConsoles
    DetailPrint "Terminating Allegro NFS Console..."
    SendMessage $0 ${WM_CLOSE} 1 0
    Sleep 3000 ;; Allow time for it to die
    Goto FindWindowLoop

  NoMoreConsoles:
  
FunctionEnd
!macroend

!insertmacro StopConsole ""
!insertmacro StopConsole un.


;------------------------------------------------------------------------------

!define REGKEY "Software\Franz Inc.\Allegro NFS ${VERSION}"
!define VERBOSE_PROD "Allegro NFS ${VERSION} Server for Windows"
!define SHORT_PROD "Allegro NFS ${VERSION}"
; for DEP workaround
!define APPCOMPATLAYERS "SOFTWARE\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers"

Name "${VERBOSE_PROD}"

; The installer program that will be created
OutFile "dists\setup-nfs-${VERSION2}.exe"

; The default installation directory
InstallDir "c:\AllegroNFS"

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "${REGKEY}" "Install_Dir"

!ifdef NFSDEMO
LicenseData license-demo.txt
!else
LicenseData license-paid.txt
!endif

;--------------------------------

; IsWin9x
;
; Base on GetWindowsVersion from
;     http://nsis.sourceforge.net/wiki/Get_Windows_version
;
; Based on Yazno's function, http://yazno.tripod.com/powerpimpit/
; Updated by Joost Verburg
;
; Returns on top of stack
;
; Windows Version (95, 98, ME, NT x.x, 2000, XP, 2003)
; or
; '' (Unknown Windows Version)
;
; Usage:
;   Call IsWin9x
;   Pop $R0
;   ; at this point $R0 is "true" or "false"
 
Function IsWin9x

  Push $R0
  Push $R1

  ClearErrors

  ReadRegStr $R0 HKLM \
  "SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion

  IfErrors 0 lbl_winnt
    ; we are not NT
    StrCpy $R0 "true"
    Goto lbl_done

 lbl_winnt:
  StrCpy $R0 "false"

 lbl_done:
  Pop $R1
  Exch $R0

FunctionEnd

;--------------------------------
; definitions for Registry access

!define HKEY_LOCAL_MACHINE       0x80000002

!define KEY_QUERY_VALUE          0x0001
!define KEY_ENUMERATE_SUB_KEYS   0x0008

!define REG_MULTI_SZ             7

!define RegOpenKeyEx     "Advapi32::RegOpenKeyEx(i, t, i, i, *i) i"
!define RegQueryValueEx  "Advapi32::RegQueryValueEx(i, t, i, *i, i, *i) i"
!define RegCloseKey      "Advapi32::RegCloseKey(i) i"

;--------------------------------
Function .onInit

  Call IsWin9x
  Pop $R0   ; at this point $R0 is "true" or "false"
  StrCmp $R0 "true" 0 IsWinNT
     MessageBox MB_OK|MB_ICONSTOP \
        'Allegro NFS Server does not work on Windows 9x.'
     Abort
 IsWinNT:

  Call IsUserAdmin
  Pop $R0   ; at this point $R0 is "true" or "false"
  StrCmp $R0 "false" 0 IsAdmin
     MessageBox MB_OK|MB_ICONSTOP \
        'You must be a member of the Administrators group to install.'
     Abort
 IsAdmin:

  System::Call 'kernel32::CreateMutexA(i 0, i 0, t "Global\AllegroNFSInstallMutex") i .r1 ?e'
  Pop $R0
 
  StrCmp $R0 0 +3
    MessageBox MB_OK|MB_ICONSTOP "The installer is already running."
    Abort

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Check to see if there are any file renames on reboot are pending,
  ; and if they are then abort the installation.

  ; Make sure our registry access isn't redirected because we're a 32-bit
  ; installer.  We need to see the unredirected registry.
  SetRegView 64

  ; Will hold the handle to the "Session Manager" key:
  StrCpy $0 ""
  ; Will hold the PendingFileRenameOperations registry value type code
  ; (e.g. REG_SZ, REG_MULTI_SZ, etc):
  StrCpy $1 ""
  ; Will hold the size of the  PendingFileRenameOperations registry value:
  StrCpy $2 ""
  ; Used to hold the return value of registry system calls:
  StrCpy $3 ""
  ; From http://nsis.sourceforge.net/System_plug-in_readme
  ; "The System plug-in will not be able to process the callback calls
  ; right if it's unloaded" ... so we set it to `alwaysoff' and back to
  ; `manual' at the end.
  SetPluginUnload alwaysoff

  System::Call "${RegOpenKeyEx}(${HKEY_LOCAL_MACHINE}, \
    'SYSTEM\CurrentControlSet\Control\Session Manager', \
    0, ${KEY_QUERY_VALUE}|${KEY_ENUMERATE_SUB_KEYS}, .r0) .r3"

  StrCmp $3 0 checkValue
    ;MessageBox MB_OK "DEBUG: Can't open registry key! ($3)"
    Goto installGreenLight

checkValue:
  System::Call "${RegQueryValueEx}(r0, 'PendingFileRenameOperations', \
    0, .r1, 0, .r2) .r3"
  StrCmp $3 0 checkType
    ;MessageBox MB_OK "DEBUG: Can't query registry value size! ($3)"
    Goto installGreenLight

checkType:
  StrCmp $1 ${REG_MULTI_SZ} rebootNeeded
    ;MessageBox MB_OK "DEBUG: Registry value not REG_MULTI_SZ! ($3)"
    Goto installGreenLight

rebootNeeded:
  MessageBox MB_OK|MB_ICONSTOP "A reboot is needed before the installation can proceed"
  Abort
 
installGreenLight:
  StrCmp $0 0 noCloseRegKey
    System::Call "${RegCloseKey}(r0)"
noCloseRegKey:
  ; Restore state changed above:
  SetPluginUnload manual
  ; Set back to the 32-bit view:
  SetRegView 32

FunctionEnd

;--------------------------------

; Pages

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The main, required install section
Section "${VERBOSE_PROD}"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath "$INSTDIR"

  ; Perform all operations within the 'All Users' context.
  SetShellVarContext all 

  ;;Call StopConsole
  Call StopAndDeleteService

;;;;;;; now install the files...

  File /r "nfs\*"
!ifdef NFSDEMO
  File /oname=license.txt "license-demo.txt"
!else
  File /oname=license.txt "license-paid.txt"
!endif
  File "nfs.cfg.default"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nfs.cfg 

  ; If nfs.cfg is already there, don't overwrite it.
  IfFileExists "$INSTDIR\nfs.cfg" HasExistingConfig

  ;; Check for configuration files from previous versions, and copy
  ;; it into the newly named installation directory.
  IfFileExists "C:\Program Files (x86)\Allegro NFS\nfs.cfg" 0 CheckFor32Bit
	StrCpy $0 "C:\Program Files (x86)\Allegro NFS\nfs.cfg"
	StrCpy $1 "$INSTDIR\nfs.cfg"
	StrCpy $2 1        ; 0 to overwrite file if it already exists
	System::Call 'kernel32::CopyFile(t r0, t r1, b r2) l'
	Pop $0 ; pops a bool.  if overwrite is off and there is a file then error will be 1
	StrCmp $0 "true" 0 HasExistingConfig
     	    MessageBox MB_OK|MB_ICONSTOP 'Error creating nfs.cfg [1]'
     	    Abort

CheckFor32Bit:
  IfFileExists "C:\Program Files\Allegro NFS\nfs.cfg" 0 DefaultConfig
	StrCpy $0 "C:\Program Files\Allegro NFS\nfs.cfg"
	StrCpy $1 "$INSTDIR\nfs.cfg"
	StrCpy $2 1        ; 0 to overwrite file if it already exists
	System::Call 'kernel32::CopyFile(t r0, t r1, b r2) l'
	Pop $0 ; pops a bool.  if overwrite is off and there is a file then error will be 1
	StrCmp $0 "true" 0 HasExistingConfig
     	    MessageBox MB_OK|MB_ICONSTOP 'Error creating nfs.cfg [2]'
     	    Abort
	Goto HasExistingConfig

DefaultConfig:
  File /oname=nfs.cfg nfs.cfg.default

HasExistingConfig:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Write the installation path into the registry
  WriteRegStr HKLM "${REGKEY}" "Install_Dir" "$INSTDIR"

  ; See if we need to work around DEP
  Push $1
  System::Call "apphelp::ShimFlushCache(i 0, i 0, i 0, i 0) i .r1"
  ; $1 will be "error" if there is no apphelp on this system.
  ; $1 will be 1 if the call succeeded, which means we need to
  ;    work around DEP.
  IntCmp $1 1 0 noDEP noDEP	
	DetailPrint "Installing DEP workarounds"
	; Turn off DEP for Allegro NFS programs
	; Add registry entries
	WriteRegStr HKLM "${APPCOMPATLAYERS}" "$INSTDIR\nfs.exe"  "DisableNXShowUI"
	WriteRegStr HKLM "${APPCOMPATLAYERS}" "$INSTDIR\configure\configure.exe"  "DisableNXShowUI"
	System::Call "apphelp::ShimFlushCache(i 0, i 0, i 0, i 0)"	
  noDEP:
  Pop $1

!define UNINSTMAIN "Software\Microsoft\Windows\CurrentVersion\Uninstall"
!define UNINSTKEY "${UNINSTMAIN}\${SHORT_PROD}"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "${UNINSTKEY}" "DisplayName" "${VERBOSE_PROD}"
  WriteRegStr HKLM "${UNINSTKEY}" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "${UNINSTKEY}" "NoModify" 1
  WriteRegDWORD HKLM "${UNINSTKEY}" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

  ; MSVC runtime support for ACL
  ifFileExists $SYSDIR\vcruntime140.dll vcinstalled 0
    DetailPrint "Running vcredist_x86.exe"
    Execwait '"$INSTDIR/vcredist_x86.exe" /passive' $0
    DetailPrint "vcredist installer returned: $0"
    ${If} $0 <> 0
       MessageBox MB_OK "VCREDIST failed ($0).  Please report this to Franz Inc. at support@franz.com."
    ${Endif}
 vcinstalled:

  ExecWait '"$INSTDIR\nfs.exe" /install /quiet'  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

!define SMDIR "$SMPROGRAMS\${VERBOSE_PROD}"

  SetShellVarContext current
  ; Delete any old shortcuts that were installed in the current user area
  RMDir /r "${SMDIR}"  
  SetShellVarContext all

  ; Start with a clean state
  RMDir /r "${SMDIR}"
	
  CreateDirectory "${SMDIR}"
  CreateShortCut "${SMDIR}\Uninstall.lnk" "$INSTDIR\uninstall.exe"
  ;; These don't work on Vista.  They're not really necessary anyway.
  ;;CreateShortCut "${SMDIR}\Start NFS Service.lnk" "%windir%\system32\net.exe" "start nfs"
  ;;CreateShortCut "${SMDIR}\Stop NFS Service.lnk" "%windir%\system32\net.exe" "stop nfs"
  CreateShortCut "${SMDIR}\Configure ${VERBOSE_PROD}.lnk" \
		"$INSTDIR\configure\configure.exe"
  CreateShortCut "${SMDIR}\${SHORT_PROD} Console.lnk" \
		"$INSTDIR\nfs.exe" "/console"
  CreateShortCut "${SMDIR}\Check for program update.lnk" \
		"http://nfsforwindows.com/updatecheck?version=${VERSION}"
SectionEnd

Section "Start service after install"
  ExecWait '"$INSTDIR\nfs.exe" /start /quiet'
SectionEnd

Section "System tray icon"
  CreateShortCut "$SMSTARTUP\${SHORT_PROD} Console.lnk" \
		"$INSTDIR\nfs.exe" "/console /quiet"
  Exec '"$INSTDIR\nfs.exe" /console /quiet'
SectionEnd

Section "Run configuration program after install"
  IfSilent +2
      Exec '"$INSTDIR\configure\configure.exe"'
SectionEnd

;--------------------------------

; Uninstaller

!ifdef NFSDEMO
Function un.onUninstSuccess
    ExecShell "open" "http://nfsforwindows.com/uninstall"
FunctionEnd
!endif

Section Uninstall
  SetShellVarContext all 

  ClearErrors
  
  ;;Call un.StopConsole
  Call un.StopAndDeleteService

  DetailPrint "Removing registry keys..."
  DeleteRegKey HKLM "${UNINSTKEY}"
  DeleteRegKey HKLM "${REGKEY}"
  DeleteRegValue HKLM "${APPCOMPATLAYERS}" "$INSTDIR\nfs.exe"
  DeleteRegValue HKLM "${APPCOMPATLAYERS}" "$INSTDIR\configure\configure.exe"

  DetailPrint "Removing files and uninstaller..."
  RMDir /r "$INSTDIR\system-dlls"
  ; would have used RMDir /r $INSTDIR but the config
  ; file is there too and we might want to preserve it.  Bleh
  Delete /rebootok "$INSTDIR\files.bu"
  Delete /rebootok "$INSTDIR\*.txt"
  Delete /rebootok "$INSTDIR\*.dll"
  Delete /rebootok "$INSTDIR\*.dxl"
  Delete /rebootok "$INSTDIR\*.exe"
  Delete /rebootok "$INSTDIR\*.lic"
  Delete /rebootok "$INSTDIR\nfs.cfg.default"
  Delete /rebootok "$INSTDIR\nsm-state"
  RMDir /r "$INSTDIR\configure"
  RMDir /r "$INSTDIR\locales"

  IfFileExists "$INSTDIR\nfs.cfg" 0 leave_nfs_cfg
    MessageBox MB_YESNO|MB_ICONQUESTION \
   "Would you like to preserve $INSTDIR\nfs.cfg?" \
     IDYES leave_nfs_cfg IDNO remove_nfs_cfg
 
 remove_nfs_cfg:
    Delete /rebootok "$INSTDIR\nfs.cfg"
    ; User said they don't want nfs.cfg, so we can blow away the main
    ; program directory, too.  We don't specify /rebootok, since we don't
    ; care if the directory is not removed if they added their own files
    ; to the directory.
    RMDir "$INSTDIR" 

 leave_nfs_cfg:	

  RMDir /r "${SMDIR}"
  ; DO NOT remove "$INSTDIR" here!  It will fail to remove the directory
  ; because "nfs.cfg" is still there, and this causes the reboot flag
  ; to be set, since we give /rebootok.
;;; DEBUG: uncomment the following to force a "reboot needed" situation
  ;;; RMDir /rebootok "$INSTDIR"

  Delete /rebootok "$SMSTARTUP\${SHORT_PROD} Console.lnk"

  IfRebootFlag 0 noreboot
  MessageBox MB_YESNO|MB_ICONQUESTION "A reboot is required to finish the uninstall. Reboot now?" IDNO noreboot
  Reboot
  # the above will never return
 noreboot:

SectionEnd
