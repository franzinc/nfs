#include <windows.h>

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER -1
#endif

/* Extra stuff that I can't do in lisp */

_declspec(dllexport) int truncate(char *filename, int size) {
	HANDLE hFile; 
	int res;
 
	hFile = CreateFile(filename,           // open MYFILE.TXT 
                GENERIC_WRITE,              
               	0,
                NULL,                      // no security 
                OPEN_EXISTING,             // existing file only 
                FILE_ATTRIBUTE_NORMAL,     // normal file 
                NULL);                     // no attr. template 
 
	if (hFile == INVALID_HANDLE_VALUE) 
	{ 
	        return GetLastError();
	} 
	
	if (SetFilePointer(hFile, size, 0, FILE_BEGIN) == INVALID_SET_FILE_POINTER) {
		res=GetLastError();
		CloseHandle(hFile);
		return res;
	}
	
	if (!SetEndOfFile(hFile)) {
		res=GetLastError();
		CloseHandle(hFile);
		return res;
	}		

	CloseHandle(hFile);
	return 0;
}

