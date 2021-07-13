// This file is a part of the helm-w32-launcher's C# helper.
//
// Copyright (c) 2014, Fanael Linithien
// See ../license.txt for licensing information.
namespace HelmW32Launcher
{
    using System;
    using System.Runtime.InteropServices;
    using System.Text;

    internal static class NativeMethods
    {
        [DllImport("shell32.dll", CharSet = CharSet.Unicode, ExactSpelling = true)]
        public static extern int SHGetFolderPathW(
            IntPtr owner,
            int folder,
            IntPtr token,
            uint flags,
            StringBuilder path);

        [DllImport("shell32.dll", ExactSpelling = true)]
        public static extern void ILFree(IntPtr pidlList);

        [DllImport("shell32.dll", CharSet = CharSet.Unicode, ExactSpelling = true)]
        public static extern IntPtr ILCreateFromPathW(string pszPath);

        [DllImport("shell32.dll", ExactSpelling = true)]
        public static extern int SHOpenFolderAndSelectItems(
            IntPtr pidlList,
            uint cild,
            IntPtr children,
            uint flags);
    }
}
