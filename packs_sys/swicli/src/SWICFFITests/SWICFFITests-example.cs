using System;
using Swicli.Library;
using System.Runtime.InteropServices;
using System.Drawing;
using System.Security;
namespace SWICFFITests
{
    public static class SWICFFITestsProgram
    {
        static SWICFFITestsProgram()
        {
            Console.WriteLine("SWICLITestDLL::SWICLITestClass.<clinit>()");
        }
        
        public static void Main(String[] args)
        {
            PrologCLR.ClientReady = true;
            PrologCLR.Main(args);
            return;
            SWICFFITestsWindows.WinMain(args);
        #if NET40
            dynamic d = new PInvokeMetaObject("glibc");
        #endif
            PrologCLR.cliDynTest_1();
           // PrologCLR.cliDynTest_3<String>();
            PrologCLR.cliDynTest_2();
            SWICFFITestsWindows.install();
        }
        public static void install()
        {
            Console.WriteLine("SWICLITestDLL::SWICLITestClass.install()");
            //Console.WriteLine("SWICLITestClass::install press ctrol-D to leave CSharp");
            //System.Reflection.Assembly.Load("csharp").EntryPoint.DeclaringType.GetMethod("Main", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static).Invoke(null, new object[] { new String[0] });
        }
    }
    [StructLayout(LayoutKind.Sequential)]
    public struct WINHTTP_CURRENT_USER_IE_PROXY_CONFIG
    {
        [MarshalAs(UnmanagedType.Bool)]
        bool AutoDetect;
        [MarshalAs(UnmanagedType.LPWStr)]
        string AutoConfigUrl;
        [MarshalAs(UnmanagedType.LPWStr)]
        string Proxy;
        [MarshalAs(UnmanagedType.LPWStr)]
        string ProxyBypass;

    }
    [SuppressUnmanagedCodeSecurity]
    public static class SWICFFITestsWindows
    {
        [DllImport("user32.dll", CharSet = CharSet.Unicode)]
        public static extern int MessageBox(IntPtr hWnd, String text, String caption, uint type);
 
        static SWICFFITestsWindows()
        {
            Console.WriteLine(typeof(SWICFFITestsWindows).ToString() + ".<clinit>()");
        }

        public static void WinMain(String[] args)
        {
            PInvokeMetaObject pi = PrologCLR.cliGetDll("user32.dll");
            pi.Invoke<int>("MessageBox",
                new Type[] { typeof(IntPtr), typeof(String), typeof(String), typeof(uint) },
                 typeof(int),
                 (object)null, new IntPtr(0), "Hello World!", "Hello Dialog", (uint)0);

            //pi.InvokeDLL<int>("MessageBox", new IntPtr(0), "Hello World!", "Hello Dialog", 0);

            MessageBox(new IntPtr(0), "Hello World!", "Hello Dialog", 0);
        }

        /*
            HANDLE CreateEvent(
              LPSECURITY_ATTRIBUTES lpEventAttributes,
              BOOL bManualReset,
              BOOL bInitialState,
              LPCTSTR lpName
            );
        */
        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr CreateEvent(IntPtr lpEventAttributes,
            bool bManualReset, bool bInitialState, [System.Runtime.InteropServices.MarshalAs(UnmanagedType.LPStr)] string lpName);

        /*
              HANDLE CreateFileMapping(
                  HANDLE hFile,
                  LPSECURITY_ATTRIBUTES lpAttributes,
                  DWORD flProtect,
                  DWORD dwMaximumSizeHigh,
                  DWORD dwMaximumSizeLow,
                  LPCTSTR lpName
                  ); 
			
              */

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr CreateFileMapping(IntPtr hFile,
           IntPtr lpFileMappingAttributes, uint flProtect,
           uint dwMaximumSizeHigh,
           uint dwMaximumSizeLow, string lpName);


        [DllImport("shell32.dll", EntryPoint = "ShellExecute", SetLastError = true)]
        public static extern string ShellExecute(IntPtr HWND, string operation, string file, string parameters, string directory, int showcmd);

        public static void install()
        {
            Console.WriteLine(typeof(SWICFFITestsWindows).ToString() + ".install()");
        }

        [DllImport("dwmapi.dll")]
        unsafe static extern int DwmGetWindowAttribute(
            IntPtr hwnd,
            uint dwAttribute,
            void* pvAttribute,
            uint cbAttribute);

        [DllImport("kernel32.dll", SetLastError = false)]
        public static extern IntPtr GetCurrentThread();

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetThreadPriority(
            IntPtr hThread,
            int nPriority);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr CreateFileMappingW(
            IntPtr hFile,
            IntPtr lpFileMappingAttributes,
            uint flProtect,
            uint dwMaximumSizeHigh,
            uint dwMaximumSizeLow,
            [MarshalAs(UnmanagedType.LPTStr)] string lpName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr MapViewOfFile(
            IntPtr hFileMappingObject,
            uint dwDesiredAccess,
            uint dwFileOffsetHigh,
            uint dwFileOffsetLow,
            UIntPtr dwNumberOfBytesToMap);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool UnmapViewOfFile(IntPtr lpBaseAddress);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetProcessDPIAware();

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool ShowScrollBar(
            IntPtr hWnd,
            int wBar,
            [MarshalAs(UnmanagedType.Bool)] bool bShow);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetVersionEx(ref OSVERSIONINFOEX lpVersionInfo);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetLayeredWindowAttributes(
            IntPtr hwnd,
            out uint pcrKey,
            out byte pbAlpha,
            out uint pdwFlags);

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Portability", "CA1901:PInvokeDeclarationsShouldBePortable", MessageId = "2")]
        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetLayeredWindowAttributes(
            IntPtr hwnd,
            uint crKey,
            byte bAlpha,
            uint dwFlags);

        [DllImport("gdi32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr CreateFontW(
            int nHeight,
            int nWidth,
            int nEscapement,
            int nOrientation,
            int fnWeight,
            uint fdwItalic,
            uint fdwUnderline,
            uint fdwStrikeOut,
            uint fdwCharSet,
            uint fdwOutputPrecision,
            uint fdwClipPrecision,
            uint fdwQuality,
            uint fdwPitchAndFamily,
            string lpszFace);
        [StructLayout(LayoutKind.Sequential)]
        public struct RECT
        {
            public int Left, Top, Right, Bottom;

            public RECT(int left, int top, int right, int bottom)
            {
                Left = left;
                Top = top;
                Right = right;
                Bottom = bottom;
            }

            public RECT(System.Drawing.Rectangle r) : this(r.Left, r.Top, r.Right, r.Bottom) { }

            public int X
            {
                get { return Left; }
                set { Right -= (Left - value); Left = value; }
            }

            public int Y
            {
                get { return Top; }
                set { Bottom -= (Top - value); Top = value; }
            }

            public int Height
            {
                get { return Bottom - Top; }
                set { Bottom = value + Top; }
            }

            public int Width
            {
                get { return Right - Left; }
                set { Right = value + Left; }
            }

            public System.Drawing.Point Location
            {
                get { return new System.Drawing.Point(Left, Top); }
                set { X = value.X; Y = value.Y; }
            }

            public System.Drawing.Size Size
            {
                get { return new System.Drawing.Size(Width, Height); }
                set { Width = value.Width; Height = value.Height; }
            }

            public static implicit operator System.Drawing.Rectangle(RECT r)
            {
                return new System.Drawing.Rectangle(r.Left, r.Top, r.Width, r.Height);
            }

            public static implicit operator RECT(System.Drawing.Rectangle r)
            {
                return new RECT(r);
            }

            public static bool operator ==(RECT r1, RECT r2)
            {
                return r1.Equals(r2);
            }

            public static bool operator !=(RECT r1, RECT r2)
            {
                return !r1.Equals(r2);
            }

            public bool Equals(RECT r)
            {
                return r.Left == Left && r.Top == Top && r.Right == Right && r.Bottom == Bottom;
            }

            public override bool Equals(object obj)
            {
                if (obj is RECT)
                    return Equals((RECT)obj);
                else if (obj is System.Drawing.Rectangle)
                    return Equals(new RECT((System.Drawing.Rectangle)obj));
                return false;
            }

            public override int GetHashCode()
            {
                return ((System.Drawing.Rectangle)this).GetHashCode();
            }

            public override string ToString()
            {
                return string.Format(System.Globalization.CultureInfo.CurrentCulture, "{{Left={0},Top={1},Right={2},Bottom={3}}}", Left, Top, Right, Bottom);
            }
        }
        [StructLayoutAttribute(LayoutKind.Sequential)]
        public struct BITMAPINFO
        {
            /// <summary>
            /// A BITMAPINFOHEADER structure that contains information about the dimensions of color format.
            /// </summary>
            public BITMAPINFOHEADER bmiHeader;

            /// <summary>
            /// An array of RGBQUAD. The elements of the array that make up the color table.
            /// </summary>
            [MarshalAsAttribute(UnmanagedType.ByValArray, SizeConst = 1, ArraySubType = UnmanagedType.Struct)]
            public RGBQUAD[] bmiColors;
        }
        [StructLayout(LayoutKind.Sequential)]
        public struct RGBQUAD
        {
            public byte rgbBlue;
            public byte rgbGreen;
            public byte rgbRed;
            public byte rgbReserved;
        }
        [DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern int DrawTextW(
            IntPtr hdc,
            string lpString,
            int nCount,
            ref RECT lpRect,
            uint uFormat);

        [DllImport("gdi32.dll", SetLastError = true)]
        public static extern IntPtr CreateDIBSection(
            IntPtr hdc,
            ref BITMAPINFO pbmi,
            uint iUsage,
            out IntPtr ppvBits,
            IntPtr hSection,
            uint dwOffset);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr CreateFileW(
            string lpFileName,
            uint dwDesiredAccess,
            uint dwShareMode,
            IntPtr lpSecurityAttributes,
            uint dwCreationDisposition,
            uint dwFlagsAndAttributes,
            IntPtr hTemplateFile);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public unsafe static extern bool WriteFile(
            IntPtr hFile,
            void* lpBuffer,
            uint nNumberOfBytesToWrite,
            out uint lpNumberOfBytesWritten,
            IntPtr lpOverlapped);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public unsafe static extern bool ReadFile(
            Microsoft.Win32.SafeHandles.SafeFileHandle sfhFile,
            void* lpBuffer,
            uint nNumberOfBytesToRead,
            out uint lpNumberOfBytesRead,
            IntPtr lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool CloseHandle(IntPtr hObject);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetHandleInformation(
            IntPtr hObject,
            uint dwMask,
            uint dwFlags);

        [DllImport("user32.dll", SetLastError = false)]
        public static extern int GetUpdateRgn(
            IntPtr hWnd,
            IntPtr hRgn,
            [MarshalAs(UnmanagedType.Bool)] bool bErase);

        [DllImport("uxtheme.dll", PreserveSig = false, SetLastError = false)]
        public static extern void DrawThemeBackground(
            IntPtr hTheme,
            IntPtr hdc,
            int iPartId,
            int iStateId,
            ref RECT pRect,
            ref RECT pClipRect);

        [DllImport("uxtheme.dll", CharSet = CharSet.Unicode, SetLastError = false)]
        public static extern IntPtr OpenThemeData(
            IntPtr hwnd,
            [MarshalAs(UnmanagedType.LPWStr)] string pszClassList);

        [DllImport("uxtheme.dll", PreserveSig = false)]
        public static extern void CloseThemeData(IntPtr hTheme);

        [DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr FindWindowExW(
            IntPtr hwndParent,
            IntPtr hwndChildAfter,
            [MarshalAs(UnmanagedType.LPWStr)] string lpszClass,
            [MarshalAs(UnmanagedType.LPWStr)] string lpszWindow);

        [DllImport("user32.dll", SetLastError = false)]
        public static extern IntPtr SendMessageW(
            IntPtr hWnd,
            uint msg,
            IntPtr wParam,
            IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public extern static bool PostMessageW(
            IntPtr handle,
            uint msg,
            IntPtr wParam,
            IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        public static extern uint GetWindowLongW(
            IntPtr hWnd,
            int nIndex);

        [DllImport("user32.dll", SetLastError = true)]
        public static extern uint SetWindowLongW(
            IntPtr hWnd,
            int nIndex,
            uint dwNewLong);


        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool QueryPerformanceCounter(out ulong lpPerformanceCount);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool QueryPerformanceFrequency(out ulong lpFrequency);

        [DllImport("msvcrt.dll", CallingConvention = CallingConvention.Cdecl, SetLastError = false)]
        public static extern unsafe void memcpy(void* dst, void* src, UIntPtr length);

        [DllImport("msvcrt.dll", CallingConvention = CallingConvention.Cdecl, SetLastError = false)]
        public static extern unsafe void memset(void* dst, int c, UIntPtr length);

        [DllImport("User32.dll", SetLastError = false)]
        public static extern int GetSystemMetrics(int nIndex);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern uint WaitForSingleObject(
            IntPtr hHandle,
            uint dwMilliseconds);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern uint WaitForMultipleObjects(
            uint nCount,
            IntPtr[] lpHandles,
            [MarshalAs(UnmanagedType.Bool)] bool bWaitAll,
            uint dwMilliseconds);

        public static uint WaitForMultipleObjects(IntPtr[] lpHandles, bool bWaitAll, uint dwMilliseconds)
        {
            return WaitForMultipleObjects((uint)lpHandles.Length, lpHandles, bWaitAll, dwMilliseconds);
        }

        [DllImport("wtsapi32.dll", SetLastError = true)]
        public static extern uint WTSRegisterSessionNotification(IntPtr hWnd, uint dwFlags);

        [DllImport("wtsapi32.dll", SetLastError = true)]
        public static extern uint WTSUnRegisterSessionNotification(IntPtr hWnd);

        [DllImport("Gdi32.dll", SetLastError = true)]
        public static extern uint GetRegionData_Borked(
            IntPtr hRgn,
            uint dwCount,
            IntPtr lpRgnData);

        [DllImport("Gdi32.dll", SetLastError = true)]
        public unsafe static extern IntPtr CreateRectRgn(
            int nLeftRect,
            int nTopRect,
            int nRightRect,
            int nBottomRect);

        [DllImport("Gdi32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public extern static bool MoveToEx(
            IntPtr hdc,
            int X,
            int Y,
            out POINT lpPoint);

        [DllImport("Gdi32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public extern static bool LineTo(
            IntPtr hdc,
            int nXEnd,
            int nYEnd);

        [DllImport("User32.dll", SetLastError = true)]
        public extern static int FillRect(
            IntPtr hDC,
            ref RECT lprc,
            IntPtr hbr);

        [DllImport("Gdi32.dll", SetLastError = true)]
        public extern static IntPtr CreatePen(
            int fnPenStyle,
            int nWidth,
            uint crColor);

        [DllImport("Gdi32.dll", SetLastError = true)]
        public extern static IntPtr CreateSolidBrush(uint crColor);

        [DllImport("Gdi32.dll", SetLastError = false)]
        public extern static IntPtr SelectObject(
            IntPtr hdc,
            IntPtr hgdiobj);

        [DllImport("Gdi32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public extern static bool DeleteObject(IntPtr hObject);

        [DllImport("Gdi32.dll", SetLastError = true)]
        public extern static uint DeleteDC(IntPtr hdc);

        [DllImport("Gdi32.Dll", SetLastError = true)]
        public extern static IntPtr CreateCompatibleDC(IntPtr hdc);

        [DllImport("Gdi32.Dll", SetLastError = true)]
        public extern static uint BitBlt(
            IntPtr hdcDest,
            int nXDest,
            int nYDest,
            int nWidth,
            int nHeight,
            IntPtr hdcSrc,
            int nXSrc,
            int nYSrc,
            uint dwRop);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern IntPtr VirtualAlloc(
            IntPtr lpAddress,
            UIntPtr dwSize,
            uint flAllocationType,
            uint flProtect);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool VirtualFree(
            IntPtr lpAddress,
            UIntPtr dwSize,
            uint dwFreeType);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool VirtualProtect(
            IntPtr lpAddress,
            UIntPtr dwSize,
            uint flNewProtect,
            out uint lpflOldProtect);

        [DllImport("Kernel32.dll", SetLastError = false)]
        public static extern IntPtr HeapAlloc(IntPtr hHeap, uint dwFlags, UIntPtr dwBytes);

        [DllImport("Kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool HeapFree(IntPtr hHeap, uint dwFlags, IntPtr lpMem);

        [DllImport("Kernel32.dll", SetLastError = false)]
        public static extern UIntPtr HeapSize(IntPtr hHeap, uint dwFlags, IntPtr lpMem);

        [DllImport("Kernel32.dll", SetLastError = true)]
        public static extern IntPtr HeapCreate(
            uint flOptions,
            [MarshalAs(UnmanagedType.SysUInt)] IntPtr dwInitialSize,
            [MarshalAs(UnmanagedType.SysUInt)] IntPtr dwMaximumSize
            );

        [DllImport("Kernel32.dll", SetLastError = true)]
        public static extern uint HeapDestroy(IntPtr hHeap);

        [DllImport("Kernel32.Dll", SetLastError = true)]
        public unsafe static extern uint HeapSetInformation(
            IntPtr HeapHandle,
            int HeapInformationClass,
            void* HeapInformation,
            uint HeapInformationLength
            );

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr LoadLibraryW(
            [MarshalAs(UnmanagedType.LPWStr)] string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool FreeLibrary(IntPtr hModule);

        [DllImport("winhttp.dll", CharSet = CharSet.Unicode)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WinHttpGetIEProxyConfigForCurrentUser(ref WINHTTP_CURRENT_USER_IE_PROXY_CONFIG pProxyConfig);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern IntPtr GlobalFree(IntPtr hMem);

        [DllImport("user32.dll", SetLastError = false)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetForegroundWindow(IntPtr hWnd);

        [DllImport("user32.dll", SetLastError = false)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool IsIconic(IntPtr hWnd);

        [DllImport("user32.dll", SetLastError = false)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);

        public const int ERROR_SERVICE_NOT_ACTIVE = 1062;
        public const int VER_PLATFORM_WIN32s = 0;
        public const int VER_PLATFORM_WIN32_WINDOWS = 1;
        public const int VER_PLATFORM_WIN32_NT = 2;
        public const int VER_PLATFORM_WIN32_HH = 3;
        public const int VER_PLATFORM_WIN32_CE = 3;

        public const int VER_BUILDNUMBER = 0x0000004; // dwBuildNumber 
        public const int VER_MAJORVERSION = 0x0000002; // dwMajorVersion
        // If you are testing the major version, you must also test the minor version and the service pack major and minor versions.
        public const int VER_MINORVERSION = 0x0000001; // dwMinorVersion 
        public const int VER_PLATFORMID = 0x0000008; // dwPlatformId 
        public const int VER_SERVICEPACKMAJOR = 0x0000020; // wServicePackMajor 
        public const int VER_SERVICEPACKMINOR = 0x0000010; // wServicePackMinor 
        public const int VER_SUITENAME = 0x0000040; // wSuiteMask 
        public const int VER_PRODUCT_TYPE = 0x0000080; // dwProductType

        public const int VER_EQUAL = 1; // The current value must be equal to the specified value. 
        public const int VER_GREATER = 2; // The current value must be greater than the specified value. 
        public const int VER_GREATER_EQUAL = 3; // The current value must be greater than or equal to the specified value. 
        public const int VER_LESS = 4; // The current value must be less than the specified value. 
        public const int VER_LESS_EQUAL = 5; // The current value must be less than or equal to the specified value. 

        public const int VER_AND = 6;
        // All product suites specified in the wSuiteMask member must be present in the current system. 

        public const int VER_OR = 7; // At least one of the specified product suites must be present in the current system. 

        [StructLayout(LayoutKind.Sequential)]
        public struct OSVERSIONINFOEX
        {
            public int OSVersionInfoSize;
            public int MajorVersion;
            public int MinorVersion;
            public int BuildNumber;
            public int PlatformId;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)]
            public string CSDVersion;
            public UInt16 ServicePackMajor;
            public UInt16 ServicePackMinor;
            public UInt16 SuiteMask;
            public byte ProductType;
            public byte Reserved;
        }

        //		[DllImport("kernel32")]
        //		static extern bool GetVersionEx(ref OSVERSIONINFOEX versionInfo);

        [DllImport("kernel32")]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool VerifyVersionInfo(ref OSVERSIONINFOEX versionInfo,
                                                      int typeMask,
                                                      ulong conditionMask);

        [DllImport("kernel32")]
        public static extern ulong VerSetConditionMask(ulong conditionMask, int typeBitMask, int operatorMask);

        #region Native WiFi API

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public struct WLAN_INTERFACE_INFO_LIST
        {
            public WLAN_INTERFACE_INFO_LIST(IntPtr pList)
            {
                NumberOfItems = Marshal.ReadInt32(pList, 0);
                Index = Marshal.ReadInt32(pList, 4);
                InterfaceInfo = new WLAN_INTERFACE_INFO[NumberOfItems];

                for (int i = 0; i < NumberOfItems; i++)
                {
                    IntPtr pItemList = new IntPtr(pList.ToInt32() + (i * 284));
                    WLAN_INTERFACE_INFO wii = new WLAN_INTERFACE_INFO();

                    byte[] intGuid = new byte[16];
                    for (int j = 0; j < 16; j++)
                    {
                        intGuid[j] = Marshal.ReadByte(pItemList, 8 + j);
                    }
                    wii.InterfaceGuid = new Guid(intGuid);
                    //wii.InterfacePtr = new IntPtr(pItemList.ToInt32() + 8);
                    wii.InterfaceDescription =
                        Marshal.PtrToStringUni(new IntPtr(pItemList.ToInt32() + 24), 256).Replace("\0", "");
                    wii.State = (WLAN_INTERFACE_STATE)Marshal.ReadInt32(pItemList, 280);

                    InterfaceInfo[i] = wii;
                }
            }


            public int NumberOfItems; // Contains the number of items in the InterfaceInfo member.
            public int Index; // The index of the current item. The index of the first item is 0. 

            [MarshalAs(UnmanagedType.ByValArray)]
            public WLAN_INTERFACE_INFO[] InterfaceInfo;
            // Pointer to an array of WLAN_INTERFACE_INFO structures containing interface information. 
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public struct WLAN_INTERFACE_INFO
        {
            public Guid InterfaceGuid;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 256)]
            public string InterfaceDescription;
            public WLAN_INTERFACE_STATE State;
        }

        public enum WLAN_INTERFACE_STATE
        {
            wlan_interface_state_not_ready = 0,
            wlan_interface_state_connected = 1,
            wlan_interface_state_ad_hoc_network_formed = 2,
            wlan_interface_state_disconnecting = 3,
            wlan_interface_state_disconnected = 4,
            wlan_interface_state_associating = 5,
            wlan_interface_state_discovering = 6,
            wlan_interface_state_authenticating = 7
        }

        [DllImport("wlanapi", SetLastError = true)]
        public static extern uint WlanOpenHandle(
            [In] uint dwClientVersion,
            [In, Out] IntPtr pReserved,
            [Out] out uint pdwNegotiatedVersion,
            [Out] out uint phClientHandle);

        [DllImport("wlanapi", SetLastError = true)]
        public static extern uint WlanCloseHandle(
            [In] uint hClientHandle,
            [In] IntPtr pReserved);

        [DllImport("wlanapi", SetLastError = true)]
        public static extern uint WlanEnumInterfaces(
            [In] uint hClientHandle,
            [In] IntPtr pReserved,
            [Out] out IntPtr ppInterfaceList);

        [DllImport("wlanapi", SetLastError = true)]
        public static extern void WlanFreeMemory([In] IntPtr pMemory);

        #endregion

        #region Windows Zero Configuration API

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public struct INTFS_KEY_TABLE
        {
            public uint dwNumIntfs;
            [MarshalAs(UnmanagedType.ByValArray)]
            public uint[] pIntfs;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public struct INTF_KEY_ENTRY
        {
            public string wszGuid;
        }

        [DllImport("wzcsapi.dll", SetLastError = true)]
        public static extern int WZCEnumInterfaces(
            [In] IntPtr pSvrAddr,
            [Out] out INTFS_KEY_TABLE pIntfs);

        #endregion
        /// <summary>Enumeration of the different ways of showing a window using
        /// ShowWindow</summary>
        [DllImport("user32.dll")]
        static extern bool ShowWindow(IntPtr hWnd, WindowShowStyle nCmdShow);

    }
    public enum WindowShowStyle : uint
    {
        /// <summary>Hides the window and activates another window.</summary>
        /// <remarks>See SW_HIDE</remarks>
        Hide = 0,
        /// <summary>Activates and displays a window. If the window is minimized
        /// or maximized, the system restores it to its original size and
        /// position. An application should specify this flag when displaying
        /// the window for the first time.</summary>
        /// <remarks>See SW_SHOWNORMAL</remarks>
        ShowNormal = 1,
        /// <summary>Activates the window and displays it as a minimized window.</summary>
        /// <remarks>See SW_SHOWMINIMIZED</remarks>
        ShowMinimized = 2,
        /// <summary>Activates the window and displays it as a maximized window.</summary>
        /// <remarks>See SW_SHOWMAXIMIZED</remarks>
        ShowMaximized = 3,
        /// <summary>Maximizes the specified window.</summary>
        /// <remarks>See SW_MAXIMIZE</remarks>
        Maximize = 3,
        /// <summary>Displays a window in its most recent size and position.
        /// This value is similar to "ShowNormal", except the window is not
        /// actived.</summary>
        /// <remarks>See SW_SHOWNOACTIVATE</remarks>
        ShowNormalNoActivate = 4,
        /// <summary>Activates the window and displays it in its current size
        /// and position.</summary>
        /// <remarks>See SW_SHOW</remarks>
        Show = 5,
        /// <summary>Minimizes the specified window and activates the next
        /// top-level window in the Z order.</summary>
        /// <remarks>See SW_MINIMIZE</remarks>
        Minimize = 6,
        /// <summary>Displays the window as a minimized window. This value is
        /// similar to "ShowMinimized", except the window is not activated.</summary>
        /// <remarks>See SW_SHOWMINNOACTIVE</remarks>
        ShowMinNoActivate = 7,
        /// <summary>Displays the window in its current size and position. This
        /// value is similar to "Show", except the window is not activated.</summary>
        /// <remarks>See SW_SHOWNA</remarks>
        ShowNoActivate = 8,
        /// <summary>Activates and displays the window. If the window is
        /// minimized or maximized, the system restores it to its original size
        /// and position. An application should specify this flag when restoring
        /// a minimized window.</summary>
        /// <remarks>See SW_RESTORE</remarks>
        Restore = 9,
        /// <summary>Sets the show state based on the SW_ value specified in the
        /// STARTUPINFO structure passed to the CreateProcess function by the
        /// program that started the application.</summary>
        /// <remarks>See SW_SHOWDEFAULT</remarks>
        ShowDefault = 10,
        /// <summary>Windows 2000/XP: Minimizes a window, even if the thread
        /// that owns the window is hung. This flag should only be used when
        /// minimizing windows from a different thread.</summary>
        /// <remarks>See SW_FORCEMINIMIZE</remarks>
        ForceMinimized = 11
    }


    [StructLayout(LayoutKind.Sequential)]
    public struct RECT
    {
        public int Left, Top, Right, Bottom;

        public RECT(int left, int top, int right, int bottom)
        {
            Left = left;
            Top = top;
            Right = right;
            Bottom = bottom;
        }

        public RECT(System.Drawing.Rectangle r)
            : this(r.Left, r.Top, r.Right, r.Bottom)
        {
        }

        public int X
        {
            get { return Left; }
            set { Right -= (Left - value); Left = value; }
        }

        public int Y
        {
            get { return Top; }
            set { Bottom -= (Top - value); Top = value; }
        }

        public int Width
        {
            get { return Right - Left; }
            set { Right = value + Left; }
        }

        public int Height
        {
            get { return Bottom - Top; }
            set { Bottom = value + Top; }
        }

        public Point Location
        {
            get { return new Point(Left, Top); }
            set { X = value.X; Y = value.Y; }
        }

        public Size Size
        {
            get { return new Size(Width, Height); }
            set { Width = value.Width; Height = value.Height; }
        }

        public static implicit operator Rectangle(RECT r)
        {
            return new Rectangle(r.Left, r.Top, r.Width, r.Height);
        }

        public static implicit operator RECT(Rectangle r)
        {
            return new RECT(r);
        }

        public static bool operator ==(RECT r1, RECT r2)
        {
            return r1.Equals(r2);
        }

        public static bool operator !=(RECT r1, RECT r2)
        {
            return !r1.Equals(r2);
        }

        public bool Equals(RECT r)
        {
            return r.Left == Left && r.Top == Top && r.Right == Right && r.Bottom == Bottom;
        }

        public override bool Equals(object obj)
        {
            if (obj is RECT)
            {
                return Equals((RECT)obj);
            }

            if (obj is Rectangle)
            {
                return Equals(new RECT((Rectangle)obj));
            }

            return false;
        }

        public override int GetHashCode()
        {
            return ((Rectangle)this).GetHashCode();
        }

        public override string ToString()
        {
            return string.Format(System.Globalization.CultureInfo.CurrentCulture, "{{Left={0},Top={1},Right={2},Bottom={3}}}", Left, Top, Right, Bottom);
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct SIZE
    {
        public int Width;
        public int Height;

        public SIZE(int width, int height)
        {
            Width = width;
            Height = height;
        }

        public static explicit operator Size(SIZE s)
        {
            return new Size(s.Width, s.Height);
        }

        public static explicit operator SIZE(Size s)
        {
            return new SIZE(s.Width, s.Height);
        }

        public override string ToString()
        {
            return string.Format("{0}x{1}", Width, Height);
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct POINT
    {
        public int X;
        public int Y;

        public POINT(int x, int y)
        {
            X = x;
            Y = y;
        }

        public static explicit operator Point(POINT p)
        {
            return new Point(p.X, p.Y);
        }

        public static explicit operator POINT(Point p)
        {
            return new POINT(p.X, p.Y);
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct WINDOWINFO
    {
        public uint cbSize;
        public RECT rcWindow;
        public RECT rcClient;
        public uint dwStyle;
        public uint dwExStyle;
        public uint dwWindowStatus;
        public uint cxWindowBorders;
        public uint cyWindowBorders;
        public ushort atomWindowType;
        public ushort wCreatorVersion;

        public WINDOWINFO(Boolean? filler)
            : this() // Allows automatic initialization of "cbSize" with "new WINDOWINFO(null/true/false)".
        {
            cbSize = (UInt32)(Marshal.SizeOf(typeof(WINDOWINFO)));
        }
    }

    public struct WINDOWPLACEMENT
    {
        public int length;
        public int flags;
        public WindowShowStyle showCmd;
        public POINT ptMinPosition;
        public POINT ptMaxPosition;
        public RECT rcNormalPosition;
    }


    public struct BLENDFUNCTION
    {
        public byte BlendOp;
        public byte BlendFlags;
        public byte SourceConstantAlpha;
        public byte AlphaFormat;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct APPBARDATA
    {
        public int cbSize;
        public IntPtr hWnd;
        public int uCallbackMessage;
        public int uEdge;
        public RECT rc;
        public IntPtr lParam;

        public static APPBARDATA NewAPPBARDATA()
        {
            APPBARDATA abd = new APPBARDATA();
            abd.cbSize = Marshal.SizeOf(typeof(APPBARDATA));
            return abd;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct DWM_BLURBEHIND
    {
        public DWM_BB dwFlags;
        public bool fEnable;
        public IntPtr hRgnBlur;
        public bool fTransitionOnMaximized;
    }
    [Flags]
    public enum DWM_BB : uint
    {
        ENABLE = 0x00000001,

        BLURREGION = 0x00000002,

        TRANSITIONONMAXIMIZED = 0x00000004,
    }
    [StructLayout(LayoutKind.Sequential)]
    public struct MARGINS
    {
        public int leftWidth;
        public int rightWidth;
        public int topHeight;
        public int bottomHeight;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct DWM_THUMBNAIL_PROPERTIES
    {
        public int dwFlags;
        public RECT rcDestination;
        public RECT rcSource;
        public byte opacity;
        public bool fVisible;
        public bool fSourceClientAreaOnly;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct CursorInfo
    {
        public Int32 cbSize; // Specifies the size, in bytes, of the structure.
        public Int32 flags; // Specifies the cursor state. This parameter can be one of the following values:
        public IntPtr hCursor; // Handle to the cursor.
        public Point ptScreenPos; // A POINT structure that receives the screen coordinates of the cursor.
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct IconInfo
    {
        public bool fIcon; // Specifies whether this structure defines an icon or a cursor. A value of TRUE specifies
        public Int32 xHotspot; // Specifies the x-coordinate of a cursor's hot spot. If this structure defines an icon, the hot
        public Int32 yHotspot; // Specifies the y-coordinate of the cursor's hot spot. If this structure defines an icon, the hot
        public IntPtr hbmMask; // (HBITMAP) Specifies the icon bitmask bitmap. If this structure defines a black and white icon,
        public IntPtr hbmColor; // (HBITMAP) Handle to the icon color bitmap. This member can be optional if this
    }

    /// <summary>
    /// Structure, which contains information for a single stream .
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode, Pack = 1)]
    public struct AVISTREAMINFO
    {
        /// <summary>
        /// Four-character code indicating the stream type.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int type;

        /// <summary>
        /// Four-character code of the compressor handler that will compress this video stream when it is saved.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int handler;

        /// <summary>
        /// Applicable flags for the stream.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int flags;

        /// <summary>
        /// Capability flags; currently unused.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int Capabilities;

        /// <summary>
        /// Priority of the stream.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I2)]
        public short priority;

        /// <summary>
        /// Language of the stream.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I2)]
        public short language;

        /// <summary>
        /// Time scale applicable for the stream.
        /// </summary>
        ///
        /// <remarks>Dividing <b>rate</b> by <b>scale</b> gives the playback rate in number of samples per second.</remarks>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int scale;

        /// <summary>
        /// Rate in an integer format.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int rate;

        /// <summary>
        /// Sample number of the first frame of the AVI file.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int start;

        /// <summary>
        /// Length of this stream.
        /// </summary>
        ///
        /// <remarks>The units are defined by <b>rate</b> and <b>scale</b>.</remarks>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int length;

        /// <summary>
        /// Audio skew. This member specifies how much to skew the audio data ahead of the video frames in interleaved files.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int initialFrames;

        /// <summary>
        /// Recommended buffer size, in bytes, for the stream.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int suggestedBufferSize;

        /// <summary>
        /// Quality indicator of the video data in the stream.
        /// </summary>
        ///
        /// <remarks>Quality is represented as a number between 0 and 10,000.</remarks>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int quality;

        /// <summary>
        /// Size, in bytes, of a single data sample.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int sampleSize;

        /// <summary>
        /// Dimensions of the video destination rectangle.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.Struct, SizeConst = 16)]
        public RECT rectFrame;

        /// <summary>
        /// Number of times the stream has been edited.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int editCount;

        /// <summary>
        /// Number of times the stream format has changed.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int formatChangeCount;

        /// <summary>
        /// Description of the stream.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 64)]
        public string name;
    }

    /// <summary>
    /// Structure, which contains information about a stream and how it is compressed and saved.
    /// </summary>
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct AVICOMPRESSOPTIONS
    {
        /// <summary>
        /// Four-character code indicating the stream type.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int type;

        /// <summary>
        /// Four-character code for the compressor handler that will compress this video stream when it is saved.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int handler;

        /// <summary>
        /// Maximum period between video key frames.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int keyFrameEvery;

        /// <summary>
        /// Quality value passed to a video compressor.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int quality;

        /// <summary>
        /// Video compressor data rate.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int bytesPerSecond;

        /// <summary>
        /// Flags used for compression.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int flags;

        /// <summary>
        /// Pointer to a structure defining the data format.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int format;

        /// <summary>
        /// Size, in bytes, of the data referenced by <b>format</b>.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int formatSize;

        /// <summary>
        /// Video-compressor-specific data; used internally.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int parameters;

        /// <summary>
        /// Size, in bytes, of the data referenced by <b>parameters</b>.
        /// </summary>
        [MarshalAs(UnmanagedType.I4)]
        public int parametersSize;

        /// <summary>
        /// Interleave factor for interspersing stream data with data from the first stream.
        /// </summary>
        ///
        [MarshalAs(UnmanagedType.I4)]
        public int interleaveEvery;
    }

    public enum BitmapCompressionMode : uint
    {
        BI_RGB = 0,
        BI_RLE8 = 1,
        BI_RLE4 = 2,
        BI_BITFIELDS = 3,
        BI_JPEG = 4,
        BI_PNG = 5
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct BITMAPINFOHEADER
    {
        public uint biSize;
        public int biWidth;
        public int biHeight;
        public ushort biPlanes;
        public ushort biBitCount;
        public BitmapCompressionMode biCompression;
        public uint biSizeImage;
        public int biXPelsPerMeter;
        public int biYPelsPerMeter;
        public uint biClrUsed;
        public uint biClrImportant;

        public BITMAPINFOHEADER(int width, int height, ushort bitCount)
        {
            biSize = (uint)Marshal.SizeOf(typeof(BITMAPINFOHEADER));
            biWidth = width;
            biHeight = height;
            biPlanes = 1;
            biBitCount = bitCount;
            biCompression = BitmapCompressionMode.BI_RGB;
            biSizeImage = 0;
            biXPelsPerMeter = 0;
            biYPelsPerMeter = 0;
            biClrUsed = 0;
            biClrImportant = 0;
        }
    }
}
