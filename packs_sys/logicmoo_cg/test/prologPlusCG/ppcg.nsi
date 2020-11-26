;NSIS Modern User Interface version 1.70
;InstallOptions Example Script
;Written by Joost Verburg
;This file modified by Ulrik Petersen for PPCG

;---------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;General

  SetCompressor lzma

  ;Name and file
  Name "Prolog+CG 2.0.10"
  OutFile "..\PPCG-2.0.10-setup-win32.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\PPCG-2.0.10"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\PrologPlusCG\2.0.10" "Install_Dir"

;--------------------------------
;Interface Settings

  ;!define MUI_HEADERIMAGE
  ;!define MUI_HEADERIMAGE_BITMAP "emdros-logo-installer.bmp" 
  ;!define MUI_HEADERIMAGE_BITMAP_NOSTRETCH
  !define MUI_ABORTWARNING
  

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "COPYING.txt"
  ; Page custom CustomPageA
  ;!insertmacro MUI_PAGE_COMPONENTS
  ; Page custom CustomPageB
  !insertmacro MUI_PAGE_DIRECTORY
  ; Page custom CustomPageC
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Reserve Files
  
  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for solid compression (by default, solid compression is enabled for BZIP2 and LZMA)
  
  ;ReserveFile "ioA.ini"
  ;ReserveFile "ioB.ini"
  ;ReserveFile "emdros-logo-installer.bmp"
  ;!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;--------------------------------
;Variables

  ;Var INI_VALUE

;--------------------------------
;Installer Sections

Section "Programs and documentation" SecFiles

  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...

  File "AUTHORS.txt"
  File "ChangeLog.txt"
  File "COPYING.txt"
  File "Makefile"
  File "NEWS.txt"
  File "PPCGApplet.jar"
  File "ppcg.jsmooth"
  File "PrologPlusCG.exe"
  File "README.txt"
  File "start.bat"

  SetOutPath "$INSTDIR\classes"
  File /r "classes\*"

  SetOutPath "$INSTDIR\manual"
  File /r "manual\*"

  SetOutPath "$INSTDIR\Samples"
  File /r "Samples\*"

  SetOutPath "$INSTDIR\src"
  File /r "src\*"


  ;Store installation folder
  WriteRegStr HKCU "Software\PrologPlusCG\2.0.10" "Install_Dir" $INSTDIR
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrologPlusCG-2.0.10" "DisplayName" "Prolog+CG 2.0.10"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrologPlusCG-2.0.10" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrologPlusCG-2.0.10" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrologPlusCG-2.0.10" "NoRepair" 1


  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  ;Read a value from an InstallOptions INI file
  ;!insertmacro MUI_INSTALLOPTIONS_READ $INI_VALUE "ioC.ini" "Field 2" "State"
  
  ;Display a messagebox if check box was checked
  ;StrCmp $INI_VALUE "1" "" +2
  ;  MessageBox MB_OK "You checked the check box, here is the MessageBox..."

SectionEnd

Section "Start Menu Shortcuts" SecShortcuts

  ; Main programs + Uninstall
  CreateDirectory "$SMPROGRAMS\PrologPlusCG-2.0.10"

  ; Set OutPath in order to set working directory
  SetOutPath "$INSTDIR"
  CreateShortCut "$SMPROGRAMS\PrologPlusCG-2.0.10\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

  ; Set OutPath in order to set working directory
  SetOutPath "$INSTDIR\Samples"

  CreateShortCut "$SMPROGRAMS\PrologPlusCG-2.0.10\PrologPlusCG 2.0.10.lnk" "$INSTDIR\PrologPlusCG.exe" "" "$INSTDIR\PrologPlusCG.exe" 0

  ; Manual
  CreateShortCut "$SMPROGRAMS\PrologPlusCG-2.0.10\manual (HTML).lnk" "$INSTDIR\manual\index.html "" "$INSTDIR\manual\index.html" 0

  ; PDF
  CreateShortCut "$SMPROGRAMS\PrologPlusCG-2.0.10\manual (PDF).lnk" "$INSTDIR\manual\index.pdf "" "$INSTDIR\manual\index.pdf" 0


SectionEnd


;--------------------------------
;Installer Functions

;Function .onInit

  ;Extract InstallOptions INI files
  ;!insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioA.ini"
  ;!insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioB.ini"
  ;!insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioC.ini"
  
;FunctionEnd

;LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
;LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "This is a page created using the InstallOptions plug-in."

;Function CustomPageA
;
;  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
;  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioA.ini"
;
;FunctionEnd
;
;Function CustomPageB
;
;  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
;  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioB.ini"
;
;FunctionEnd
;
;Function CustomPageC
;
;  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
;  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioC.ini"
;
;FunctionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecFiles ${LANG_ENGLISH} "Emdros programs and documentation."
  LangString DESC_SecShortcuts ${LANG_ENGLISH} "Shortcuts in the Programs menu."

;  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecFiles} $(DESC_SecFiles)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecShortcuts} $(DESC_SecShortcuts)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...

  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrologPlusCG-2.0.10"

  RMDir /r /REBOOTOK "$INSTDIR"

  ; Start Menu programs
  Delete "$SMPROGRAMS\PrologPlusCG-2.0.10\*"
  RMdir  "$SMPROGRAMS\PrologPlusCG-2.0.10"
   
 

  ;DeleteRegKey /ifempty HKCU "Software\Emdros\Emdros%BACKEND%\2.0.10"
  DeleteRegKey HKCU "Software\PrologPlusCG\2.0.10"

SectionEnd
