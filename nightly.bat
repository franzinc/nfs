rem $Id: nightly.bat,v 1.1 2004/02/20 23:27:37 layer Exp $
d:
cd \nightly-builds\nfs
make dist-demo update_demo_cobweb
if errorlevel 1 goto errorp
goto done
:errorp
pause
:done
