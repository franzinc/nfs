;; TODO:

;; option to install service
;; option to start service
;; install/uninstall: stop and remove service first.

!define REGKEY "Software\Franz Inc.\Allegro NFS"
!define VERBOSE_PROD "Allegro NFS Server for Windows"
!define SHORT_PROD "Allegro NFS"

SetCompressor bzip2

Name "${VERBOSE_PROD}"

; The installer program that will be created
OutFile "dists\setup-nfs-${VERSION}.exe"

; The default installation directory
InstallDir "$PROGRAMFILES\${SHORT_PROD}"

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "${REGKEY}" "Install_Dir"

LicenseData binary-license.txt

;--------------------------------

; Pages

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

var nfs_cfg_existed


; The stuff to install
Section "${VERBOSE_PROD}"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath "$INSTDIR"

  ; In case we're installing over an existing setup  
  ExecWait '"$INSTDIR\nfs.exe" /stop /quiet'
  ExecWait '"$INSTDIR\nfs.exe" /remove /quiet'

  ; binaries
  File /r "nfs\*"
  File  "readme.txt"
  File "binary-license.txt"
  File "access-control.txt"

; if nfs.cfg already exists, keep it (copy in nfs.cfg.sample instead)
  IfFileExists  "$INSTDIR\nfs.cfg" 0 make_fresh_nfs_cfg
   strcpy $nfs_cfg_existed "yes"
   File "nfs.cfg.sample"
   Goto nfs_cfg_done
  make_fresh_nfs_cfg:
   strcpy $nfs_cfg_existed "no"
   File /oname=nfs.cfg nfs.cfg.sample
  
  nfs_cfg_done:

  ; Write the installation path into the registry
  WriteRegStr HKLM "${REGKEY}" "Install_Dir" "$INSTDIR"

!define UNINSTMAIN "Software\Microsoft\Windows\CurrentVersion\Uninstall"
!define UNINSTKEY "${UNINSTMAIN}\${SHORT_PROD}"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "${UNINSTKEY}" "DisplayName" "${VERBOSE_PROD}"
  WriteRegStr HKLM "${UNINSTKEY}" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "${UNINSTKEY}" "NoModify" 1
  WriteRegDWORD HKLM "${UNINSTKEY}" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

!define SMDIR "$SMPROGRAMS\${VERBOSE_PROD}"

  CreateDirectory "${SMDIR}"
  CreateShortCut "${SMDIR}\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "" ""
  CreateShortCut "${SMDIR}\${VERBOSE_PROD}.lnk" "$INSTDIR\nfs.exe" "" "" ""
  CreateShortCut "${SMDIR}\Start NFS Service.lnk" "$INSTDIR\nfs.exe" \
                                                   "/start" "" ""
  CreateShortCut "${SMDIR}\Stop NFS Service.lnk" "$INSTDIR\nfs.exe" \
                                                   "/stop" "" ""
  CreateShortCut "${SMDIR}\Edit nfs.cfg.lnk" "notepad" "$INSTDIR\nfs.cfg" "" ""
SectionEnd

Section "Install as a service"
  ;; just in case
  ExecWait '"$INSTDIR\nfs.exe" /stop /quiet'
  ExecWait '"$INSTDIR\nfs.exe" /remove /quiet'
  ExecWait '"$INSTDIR\nfs.exe" /install /quiet'
  strcmp $nfs_cfg_existed "yes" 0 do_not_start_service
      ExecWait '"$INSTDIR\nfs.exe" /start /quiet'

  do_not_start_service:
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  ; Stop and remove service 
  ExecWait '"$INSTDIR\nfs.exe" /stop /quiet'
  ExecWait '"$INSTDIR\nfs.exe" /remove /quiet'
  
  ; Remove registry keys
  DeleteRegKey HKLM "${UNINSTKEY}"
  DeleteRegKey HKLM "${REGKEY}"

  ; Remove files and uninstaller
  Rmdir /r "$INSTDIR\system-dlls"
  ; would have used rmdir /r $INSTDIR but the config
  ; file is there too and we might want to preserve it.  Bleh
  Delete /rebootok "$INSTDIR\*.txt"
  Delete /rebootok "$INSTDIR\*.dll"
  Delete /rebootok "$INSTDIR\*.dxl"
  Delete /rebootok "$INSTDIR\*.exe"
  Delete /rebootok "$INSTDIR\*.lic"
  Delete /rebootok "$INSTDIR\nfs.cfg.sample"


  IfFileExists "$INSTDIR\nfs.cfg" 0 no_nfs_cfg
    MessageBox MB_YESNO|MB_ICONQUESTION \
   "Would you like to preserve $INSTDIR\nfs.cfg?" \
     IDYES no_nfs_cfg IDNO remove_nfs_cfg
 
 remove_nfs_cfg:
    Delete /rebootok "$INSTDIR\nfs.cfg"

 no_nfs_cfg:	

  ; may not work if nfs.cfg was preserved.
  rmdir "$INSTDIR" 

  ; Remove directories used
  RMDir /r "${SMDIR}"
  RMDir /rebootok "$INSTDIR"

SectionEnd
