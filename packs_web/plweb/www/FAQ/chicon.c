/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Code found on ExpertExchange. Written by Alex Cohn:

http://www.experts-exchange.com/Developer/Programming/Programming_Languages/Cplusplus/Q_20108576.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>
#include <string.h>
#include <windows.h>
#include <sys/stat.h>

//#define DEBUG_PRINT
#pragma pack(1)

typedef struct ICONRESDIR
{ BYTE Width;
  BYTE Height;
  BYTE ColorCount;
  BYTE reserved;
} ICONRESDIR;

typedef struct tagRESDIR
{
#if 0
  union
  { ICONRESDIR Icon;
    CURSORDIR  Cursor;
  } ResInfo;
#else
  ICONRESDIR Icon;
#endif
  WORD Planes;
  WORD BitCount;
  DWORD BytesInRes;
  WORD IconCursorId;
} RESDIR;

// source: http://msdn.microsoft.com/library/techart/msdn_icons.htm

typedef struct tagRESDIRONDISK
{
#if 0
  union
  { ICONRESDIR Icon;
    CURSORDIR  Cursor;
  } ResInfo;
#else
  ICONRESDIR Icon;
#endif
  WORD Planes;
  WORD BitCount;
  DWORD BytesInRes;
  DWORD ImageOffset;
} RESDIRONDISK;

typedef struct NEWHEADER
{ WORD Reserved;
  WORD ResType;
  WORD ResCount;
} NEWHEADER, *PNEWHEADER;

HMODULE hExe;
BYTE *buf = NULL;
int buflen = 0;
BYTE *ico = NULL;
int icolen = 0;

int
print_usage()
{ char my_name[MAX_PATH] = "abcdefghijklmnop";
  char *my_shortname;

  GetModuleFileName(0, my_name, sizeof(my_name));
  my_shortname = strrchr(my_name, '\\') + 1;
  fprintf(stderr, "\nUsage:\n"
	  "%s old.exe new.ico new.exe \n"
	  "\tto replace shell icon of old executable with the new one\n",
	  my_shortname);
  exit(1);
}

int
replace_icon(char *pIconBuf, int BytesInRes, int Width, int Height,
	     int ColorCount)
{ NEWHEADER *pDirHeader;
  RESDIRONDISK *pResDir;
  int k;

  pDirHeader = (NEWHEADER *) ico;
  pResDir = (RESDIRONDISK *) (pDirHeader + 1);

#ifdef DEBUG_PRINT
  fprintf(stderr, "Ico file contains %d icons\n", pDirHeader->ResCount);
#endif	/* DEBUG_PRINT */
  for (k = 0; k < pDirHeader->ResCount; k++)
  { int i;
    if (pResDir[k].BytesInRes == BytesInRes &&
	pResDir[k].Icon.Width == Width &&
	pResDir[k].Icon.Height == Height &&
	pResDir[k].Icon.ColorCount == ColorCount)
    {
#ifdef DEBUG_PRINT
      fprintf(stderr, "match! : %d[%d bytes] %dx%dx%d.\n",
	      k, pResDir[k].BytesInRes,
	      pResDir[k].Icon.Width, pResDir[k].Icon.Height,
	      pResDir[k].Icon.ColorCount);
#endif	/* DEBUG_PRINT */

      memcpy(pIconBuf, ico + pResDir[k].ImageOffset, BytesInRes);
      return TRUE;
    }
#ifdef DEBUG_PRINT
    else
    { fprintf(stderr, "loose! : %d[%d bytes] %dx%dx%d.\n",
	      k, pResDir[k].BytesInRes,
	      pResDir[k].Icon.Width, pResDir[k].Icon.Height,
	      pResDir[k].Icon.ColorCount);

    }
#endif	/* DEBUG_PRINT */
  }
  return FALSE;			// match not found
}

BOOL CALLBACK
my_enum_res_callback(HMODULE hExe,	// module handle
		     LPCTSTR lpszType,	// resource type
		     LPTSTR lpszName,	// resource name
		     LPARAM lParam	// application-defined parameter
  )
{ HRSRC hRsrc = 0;
  HGLOBAL hMem;
  DWORD nDataLen;
  NEWHEADER *pDirHeader;
  RESDIR *pResDir;
  BYTE *pData;
  unsigned int i, k;

  hRsrc = FindResource(hExe, lpszName, RT_GROUP_ICON);
  hMem = LoadResource(hExe, hRsrc);
  pDirHeader = (NEWHEADER *) LockResource(hMem);
  pResDir = (RESDIR *) (pDirHeader + 1);

  for (k = 0; k < pDirHeader->ResCount; k++)
  { hRsrc =
      FindResource(hExe, MAKEINTRESOURCE(pResDir[k].IconCursorId), RT_ICON);
    hMem = LoadResource(hExe, hRsrc);

    nDataLen = SizeofResource(hExe, hRsrc);
    pData = LockResource(hMem);

#ifdef DEBUG_PRINT
    fprintf(stderr,
	    "Icon found: %d.%d[%d bytes] %dx%dx%d (%d bytes loaded).\n",
	    lpszName, pResDir[k].IconCursorId, pResDir[k].BytesInRes,
	    pResDir[k].Icon.Width, pResDir[k].Icon.Height,
	    pResDir[k].Icon.ColorCount, nDataLen);
#endif	/* DEBUG_PRINT */

    for (i = 0; i <= buflen - nDataLen; i++)
    { int j;
      for (j = 0; j < nDataLen; j++)
      { if (buf[i + j] != pData[j])
	  break;
      }

      if (j == nDataLen)
      { if (replace_icon
	    (buf + i, pResDir[k].BytesInRes, pResDir[k].Icon.Width,
	     pResDir[k].Icon.Height, pResDir[k].Icon.ColorCount))
	  *(int *) lParam += 1;
	break;
      }
    }

  }
  return FALSE;			// stop enumeration after first icon group
}

void
list_icons(char *exename)
{ int cnt = 0;

  hExe = LoadLibraryEx(exename, NULL, LOAD_LIBRARY_AS_DATAFILE);

  if (hExe == 0)
  { fprintf(stderr, "cannot load file \'%s\' to find an icon\n", exename);
    print_usage();
  }

  if (EnumResourceNames
      (hExe, RT_GROUP_ICON, my_enum_res_callback, (LPARAM) & cnt) == 0
      && cnt == 0)
  { fprintf(stderr, "cannot find or replace icons in file \'%s\'\n", exename);
    print_usage();
  }

  FreeLibrary(hExe);
}

int
read_file(const char *filename, char **pbuf)
{ FILE *in;
  int bytesin;
  struct stat ST;

  in = fopen(filename, "rb");
  if (in == NULL)
  { fprintf(stderr, "Error opening file \'%s\'\n", filename);
    print_usage();
  }

  fstat(fileno(in), &ST);
  if (ST.st_size == 0)
  { fprintf(stderr, "Error opening file \'%s\'\n", filename);
    print_usage();
  }

  *pbuf = malloc(ST.st_size);
  if (pbuf == NULL)
  { fprintf(stderr, "Error allocating buffer for file \'%s\'\n", filename);
    print_usage();
  }

  if (ST.st_size != (bytesin = fread(*pbuf, 1, ST.st_size, in)))
  { fprintf(stderr, "Error reading file \'%s\'\n", filename);
    print_usage();
  }
  fclose(in);

  return ST.st_size;
}

int
main(int argc, char *argv[])
{ FILE *out;

  if (argc != 4)
    print_usage();

  buflen = read_file(argv[1], &buf);
  icolen = read_file(argv[2], &ico);
  list_icons(argv[1]);

  free(ico);

  out = fopen(argv[argc - 1], "wb");
  if (out == NULL)
  { fprintf(stderr, "Error writing to file \'%s\'\n", argv[argc - 1]);
    print_usage();
  }
  fwrite(buf, 1, buflen, out);
  fclose(out);

  free(buf);

/* printf("Done\n");
   Sleep(5000);
*/

  exit(0);
}
