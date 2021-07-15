// This file is a part of the helm-w32-launcher's C# helper.
//
// Copyright (c) 2014, Fanael Linithien
// See ../license.txt for licensing information.
namespace HelmW32Launcher
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Runtime.InteropServices;
    using System.Text;

    internal class ItemLister : ICommand
    {
        public void Run(string[] args)
        {
            List<string> shortcuts = new List<string>(100);
            foreach (string path in GetStartMenuPaths())
            {
                shortcuts.AddRange(Directory.GetFiles(
                    path, "*.lnk", SearchOption.AllDirectories));
            }

            Console.Write(LispPrinter.PrintStartMenuEntries(shortcuts));
        }

        private static string[] GetStartMenuPaths()
        {
            return new string[]
            {
                Environment.GetFolderPath(Environment.SpecialFolder.StartMenu),
                GetCommonStartMenu()
            };
        }

        private static string GetCommonStartMenu()
        {
            const int CSIDL_COMMON_STARTMENU = 0x16;
            const int MAX_PATH = 260;
            StringBuilder result = new StringBuilder(MAX_PATH);
            Marshal.ThrowExceptionForHR(
                NativeMethods.SHGetFolderPathW(
                    IntPtr.Zero,
                    CSIDL_COMMON_STARTMENU,
                    IntPtr.Zero,
                    0,
                    result));

            return result.ToString();
        }
    }
}
