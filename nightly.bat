rem $Id: nightly.bat,v 1.2 2004/02/20 23:44:11 layer Exp $
make dist-demo update_demo_cobweb
if errorlevel 1 goto errorp
goto done
:errorp
pause
:done
