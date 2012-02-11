# Microsoft Developer Studio Project File - Name="coco" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=coco - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "coco.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "coco.mak" CFG="coco - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "coco - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "coco - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "coco - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x804 /d "NDEBUG"
# ADD RSC /l 0x804 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "coco - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ  /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ  /c
# ADD BASE RSC /l 0x804 /d "_DEBUG"
# ADD RSC /l 0x804 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "coco - Win32 Release"
# Name "coco - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\Chi2.f90
NODEP_F90_CHI2_=\
	".\Debug\constants.mod"\
	".\Debug\cosmoSetUp.mod"\
	".\Debug\IO.mod"\
	".\Debug\ReadData.mod"\
	".\Debug\subroutines.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Coco.f90
NODEP_F90_COCO_=\
	".\Debug\cosmoSetUp.mod"\
	".\Debug\GenerateIDL.mod"\
	".\Debug\IO.mod"\
	".\Debug\paramScan.mod"\
	".\Debug\subroutines.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Constants.f90
# End Source File
# Begin Source File

SOURCE=.\GenIDL.f90
NODEP_F90_GENID=\
	".\Debug\constants.mod"\
	".\Debug\cosmoSetUp.mod"\
	".\Debug\IO.mod"\
	".\Debug\Precision.mod"\
	".\Debug\ReadData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\io.f90
NODEP_F90_IO_F9=\
	".\Debug\constants.mod"\
	".\Debug\Precision.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MathTool.f90
NODEP_F90_MATHT=\
	".\Debug\Precision.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MCMC.f90
NODEP_F90_MCMC_=\
	".\Debug\Chi2.mod"\
	".\Debug\constants.mod"\
	".\Debug\cosmoSetUp.mod"\
	".\Debug\IO.mod"\
	".\Debug\Precision.mod"\
	".\Debug\propose.mod"\
	".\Debug\Random.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ParamScan.f90
NODEP_F90_PARAM=\
	".\Debug\Chi2.mod"\
	".\Debug\constants.mod"\
	".\Debug\cosmoSetUp.mod"\
	".\Debug\IO.mod"\
	".\Debug\MCMC.mod"\
	".\Debug\Precision.mod"\
	".\Debug\ReadData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\propose.f90
NODEP_F90_PROPO=\
	".\Debug\cosmoSetUp.mod"\
	".\Debug\Random.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\RandGen.f90
# End Source File
# Begin Source File

SOURCE=.\ReadInData.f90
NODEP_F90_READI=\
	".\Debug\constants.mod"\
	".\Debug\IO.mod"\
	".\Debug\Precision.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Settings.f90
NODEP_F90_SETTI=\
	".\Debug\constants.mod"\
	".\Debug\Precision.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Subroutines.f90
NODEP_F90_SUBRO=\
	".\Debug\constants.mod"\
	".\Debug\cosmoSetUp.mod"\
	".\Debug\Precision.mod"\
	".\Debug\ReadData.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
