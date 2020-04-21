@echo off
rem
rem usage:
rem
rem    c:> win32/build c:\gnat\2014
rem

rem The followinng variable can be set to std, openssl or gnutls
set SOCKET=std

set ROOTDIR=%CD%
set GPROPTS=-XPRJ_BUILD=Release -XPRJ_TARGET=Windows_NT -XTARGET=win -XPRJ_XMLADA=Installed -XPRJ_LDAP=Installed -XPRJ_LAL=Disabled -XPRJ_SOCKLIB=gnat -XSOCKET=%SOCKET%

if .%1==. goto dusage

path %1\bin;%path%

rem ----------------------------------------------- SETUP
:setup
mkdir .build\win\setup\src
copy config\setup\aws-os_lib-tmplt.c .build\win\setup\src
gprbuild -p -XPRJ_BUILD=Debug -XLIBRARY_TYPE=static -XPRJ_TARGET=Windows_NT -XTARGET=win  -Pconfig\setup xoscons
if errorlevel 1 goto error
cd .build\win\setup\src
gcc -C -E -DTARGET=\"windows\" aws-os_lib-tmplt.c > aws-os_lib-tmplt.i
gcc -S aws-os_lib-tmplt.i
..\xoscons aws-os_lib
if errorlevel 1 goto error
del aws-os_lib-tmplt*
cd ..
mkdir tsrc
cd tsrc

echo with Ada2WSDL.Parser; > ada2wsdl-options.adb
echo package body Ada2WSDL.Options is >> ada2wsdl-options.adb
echo procedure Set_Default is >> ada2wsdl-options.adb
echo begin >> ada2wsdl-options.adb
echo pragma Style_Checks (Off); >> ada2wsdl-options.adb
echo Parser.Add_Option ("-I%1\include\aws"); >> ada2wsdl-options.adb
echo end Set_Default; >> ada2wsdl-options.adb
echo end Ada2WSDL.Options; >> ada2wsdl-options.adb

cd ..\..\..

mkdir projects
cd projects

echo abstract project AWS_Lib_Shared is > aws_lib_shared.gpr
echo for Source_Files use (); >> aws_lib_shared.gpr
echo type SSL_Library_Kind is ("relocatable", "static"); >> aws_lib_shared.gpr
echo SSL_Library_Type : SSL_Library_Kind := external ("SSL_LIBRARY_TYPE", "relocatable"); >> aws_lib_shared.gpr
echo LIB_Path := "./";  >> aws_lib_shared.gpr
echo S_SSL_Lib := "ssl"; >> aws_lib_shared.gpr
echo R_SSL_Lib := "ssl32"; >> aws_lib_shared.gpr
echo S_CRY_Lib := "crypto"; >> aws_lib_shared.gpr
echo R_CRY_Lib := "eay32"; >> aws_lib_shared.gpr
echo S_TLS_Lib := "gnutls"; >> aws_lib_shared.gpr
echo R_TLS_Lib := "gnutls"; >> aws_lib_shared.gpr
echo LIBZ_Path := Project'Project_Dir & "..\..\..\lib\aws\static"; >> aws_lib_shared.gpr
echo end AWS_Lib_Shared; >> aws_lib_shared.gpr

echo project aws_lal is > aws_lal.gpr
echo for Source_Files use (); >> aws_lal.gpr
echo end aws_lal; >> aws_lal.gpr

echo with "xmlada"; > aws_xmlada.gpr
echo project aws_xmlada is >> aws_xmlada.gpr
echo for Source_Files use (); >> aws_xmlada.gpr
echo end aws_xmlada; >> aws_xmlada.gpr

echo abstract project AWS_Config is > aws_config.gpr
echo for Source_Dirs use (); >> aws_config.gpr
echo type Boolean_Type is ("true", "false"); >> aws_config.gpr
echo Zlib_Exists : Boolean_Type := "false"; >> aws_config.gpr
echo type SOCKET_Type is ("std", "openssl", "gnutls"); >> aws_config.gpr
echo SOCKET : SOCKET_Type := "%SOCKET%"; >> aws_config.gpr
echo end AWS_Config; >> aws_config.gpr

cd ..\..

rem ----------------------------------------------- BUILD
:build
gprbuild -p %GPROPTS% -XLIBRARY_TYPE=static -XXMLADA_BUILD=static tools/tools.gpr
if errorlevel 1 goto error
gprbuild -p %GPROPTS% -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable aws.gpr
if errorlevel 1 goto error

rem ----------------------------------------------- UNINSTALL
:uninstall
gprinstall --prefix=%1 -f --uninstall %GPROPTS% aws

rem ----------------------------------------------- INSTALL
:install
gprinstall --prefix=%1 -p -f %GPROPTS% -XLIBRARY_TYPE=static -XXMLADA_BUILD=static aws.gpr
if errorlevel 1 goto error
gprinstall --prefix=%1 -p -f %GPROPTS% -XLIBRARY_TYPE=static -XXMLADA_BUILD=static --mode=usage --install-name=aws tools/tools.gpr
if errorlevel 1 goto error
gprinstall --prefix=%1 -p -f %GPROPTS% -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable --build-name=relocatable aws.gpr
if errorlevel 1 goto error

goto exit

rem ----------------------------------------------- USAGE
:dusage
echo usage: build gnat-prefix
goto exit

rem ----------------------------------------------- ERROR
:error
echo Couldn't build or install AWS
chdir /d %ROOTDIR%
rmdir /S /Q .build
exit /b 1

rem ----------------------------------------------- EXIT
:exit
exit /b 0
