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

; The stuff to install
Section "${VERBOSE_PROD}"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath "$INSTDIR"

  IfFileExists "$INSTDIR\nfs.exe" 0 +3
      ExecWait '"$INSTDIR\nfs.exe" /stop /remove /quiet'
      Sleep 3000 ;; allow time for Windows to let go


  File /r "nfs\*"
  File "binary-license.txt"
  File "nfs.cfg.default"

  ; If nfs.cfg is already there, don't overwrite it.
  IfFileExists "$INSTDIR\nfs.cfg" +2
	File /oname=nfs.cfg nfs.cfg.default

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
  CreateShortCut "${SMDIR}\Uninstall.lnk" "$INSTDIR\uninstall.exe"
  CreateShortCut "${SMDIR}\${VERBOSE_PROD}.lnk" "$INSTDIR\nfs.exe"
  CreateShortCut "${SMDIR}\Start NFS Service.lnk" "$INSTDIR\nfs.exe" \
                                                   "/start /quiet"
  CreateShortCut "${SMDIR}\Stop NFS Service.lnk" "$INSTDIR\nfs.exe" \
                                                   "/stop /quiet"
  CreateShortCut "${SMDIR}\Configure ${VERBOSE_PROD}.lnk" \
		"$INSTDIR\configure\configure.exe"
SectionEnd

Section "Install as a service"
  ExecWait '"$INSTDIR\nfs.exe" /stop /remove /install /start /quiet'
SectionEnd

Section "Run configuration program after install"
  Exec '"$INSTDIR\configure\configure.exe"'
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  ; Stop and remove service 
  ExecWait '"$INSTDIR\nfs.exe" /stop /remove /quiet'
  Sleep 3000 ;; allow time for Windows to let go
  
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
  Delete /rebootok "$INSTDIR\nfs.cfg.default"
  Rmdir /r "$INSTDIR\configure"

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
