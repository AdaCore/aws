@echo off
rem
rem usage: win32\build <install directory>
rem
rem    c:> win32/build c:\gnatpro\20.2
rem

rem If SOCKET is set to openssl or gnutls you need to copy the ssl &
rem crypto libraries and the include files into the GNAT Pro installation :
rem
rem   The libraries (libssl.dll & libcrypto.dll):
rem      c:\gnatpro\20.2\bin
rem   The openssl directory containing headers:
rem      c:\gnatpro\20.2\<TARGET>\include

set SOCKET=std
set TARGET=x86_64-pc-mingw32

rem ~dp to get the directory only
FOR /F %%G IN ('"where gnatls"') DO SET GNATDIR=%%~dpG

if .%GNATDIR%==. goto nognat

set SSLDIR=%GNATDIR%/%TARGET%
set C_INCLUDE_PATH=%SSLDIR%/include

set ROOTDIR=%CD%
set BDIR=%ROOTDIR%\%TARGET%
set PDIR=%ROOTDIR%\%TARGET%\projects
set CDIR=%ROOTDIR%\%TARGET%\common

set GPROPTS=-XPRJ_BUILD=Release -XPRJ_TARGET=Windows_NT -XTARGET=%TARGET% -XPRJ_XMLADA=Installed -XPRJ_LDAP=Installed -XPRJ_ASIS=Disabled -XPRJ_LAL=Disabled -XPRJ_SOCKLIB=gnat -XSOCKET=%SOCKET% -XTGT_DIR=%BDIR% -aP%BDIR%\projects

if .%1==. goto dusage

path %1\bin;%path%

rem ----------------------------------------------- SETUP SRC
echo Setup...

mkdir %BDIR%\setup\src
copy config\setup\aws-os_lib-tmplt.c %BDIR%\setup\src
gprbuild -p -XPRJ_BUILD=Debug -XLIBRARY_TYPE=static -XPRJ_TARGET=Windows_NT -XTARGET=%TARGET% -XTGT_DIR=%BDIR% -Pconfig\setup xoscons
if errorlevel 1 goto error
cd %BDIR%\setup\src
gcc -C -E -DTARGET=\"windows\" aws-os_lib-tmplt.c > aws-os_lib-tmplt.i
gcc -S aws-os_lib-tmplt.i
..\bin\xoscons aws-os_lib
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

cd ..\..

echo Create projects...

mkdir %PDIR%
cd %PDIR%

echo abstract project AWS_Lib_Shared is > aws_lib_shared.gpr
echo for Source_Files use (); >> aws_lib_shared.gpr
echo type SSL_Library_Kind is ("relocatable", "static"); >> aws_lib_shared.gpr
echo SSL_Library_Type : SSL_Library_Kind := external ("SSL_LIBRARY_TYPE", "relocatable"); >> aws_lib_shared.gpr

if .%SOCKET%==.std goto std

rem With SSL
echo LIB_Path  := "%GNATDIR%";  >> aws_lib_shared.gpr
echo S_SSL_Lib := "ssl";        >> aws_lib_shared.gpr
echo R_SSL_Lib := "ssl";        >> aws_lib_shared.gpr
echo S_CRY_Lib := "crypto";     >> aws_lib_shared.gpr
echo R_CRY_Lib := "crypto";     >> aws_lib_shared.gpr
echo S_TLS_Lib := "gnutls";     >> aws_lib_shared.gpr
echo R_TLS_Lib := "gnutls";     >> aws_lib_shared.gpr

goto endcom

rem Without SSL
:std
echo LIB_Path  := ""; >> aws_lib_shared.gpr
echo S_SSL_Lib := ""; >> aws_lib_shared.gpr
echo R_SSL_Lib := ""; >> aws_lib_shared.gpr
echo S_CRY_Lib := ""; >> aws_lib_shared.gpr
echo R_CRY_Lib := ""; >> aws_lib_shared.gpr
echo S_TLS_Lib := ""; >> aws_lib_shared.gpr
echo R_TLS_Lib := ""; >> aws_lib_shared.gpr

:endcom
echo LIBZ_Path := Project'Project_Dir ^& "..\..\..\lib\aws\static"; >> aws_lib_shared.gpr
echo end AWS_Lib_Shared; >> aws_lib_shared.gpr

rem XML/Ada comes with GNAT, always available
echo with "xmlada";            > aws_xmlada.gpr
echo project aws_xmlada is    >> aws_xmlada.gpr
echo for Source_Files use (); >> aws_xmlada.gpr
echo end aws_xmlada;          >> aws_xmlada.gpr

rem No LAL
echo project aws_lal is        > aws_lal.gpr
echo for Source_Files use (); >> aws_lal.gpr
echo end aws_lal;             >> aws_lal.gpr

rem The main config project
echo abstract project AWS_Config is                     > aws_config.gpr
echo for Source_Dirs use ();                           >> aws_config.gpr
echo type Boolean_Type is ("true", "false");           >> aws_config.gpr
echo Zlib_Exists : Boolean_Type := "false";            >> aws_config.gpr
echo type SOCKET_Type is ("std", "openssl", "gnutls"); >> aws_config.gpr
echo SOCKET : SOCKET_Type := "%SOCKET%";               >> aws_config.gpr
echo end AWS_Config;                                   >> aws_config.gpr

cd %ROOTDIR%

rem ----------------------------------------------- MINIMAL BUILD (awsres)
:build
echo Build step 1 (minimal)

mkdir %CDIR%\src

gprbuild -p %GPROPTS% -XLIBRARY_TYPE=static -XXMLADA_BUILD=static -XTO_BUILD=awsres.adb tools/tools.gpr
if errorlevel 1 goto error

rem ----------------------------------------------- SETUP TEMPLATES
:setup
echo Generate templates

cd %ROOTDIR%\tools\wsdl2aws-templates

%BDIR%\release\static\tools\awsres -r wsdl2aws_templates -o %CDIR%\src *.tads *.tadb *.macros

rem ----------------------------------------------- FULL BUILD
echo build step 2 (full)

cd %ROOTDIR%

gprbuild -p %GPROPTS% -XLIBRARY_TYPE=static -XXMLADA_BUILD=static tools/tools.gpr
if errorlevel 1 goto error

gprbuild -p %GPROPTS% -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable aws.gpr
if errorlevel 1 goto error

rem ----------------------------------------------- INSTALL
echo Install in %1

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
echo usage: build install-dir
goto exit

rem ----------------------------------------------- NO GNAT Pro
:nognat
echo No GNAT Pro compiler found
goto exit

rem ----------------------------------------------- ERROR
:error
echo Couldn't build or install AWS
chdir /d %ROOTDIR%
rmdir /S /Q %TARGET%
exit /b 1

rem ----------------------------------------------- EXIT
:exit
exit /b 0
