// This file is a part of the helm-w32-launcher's C# helper.
//
// Copyright (c) 2014, Fanael Linithien
// See ../license.txt for licensing information.
namespace HelmW32Launcher
{
    using System;
    using System.Diagnostics;
    using System.Runtime.InteropServices;
    using System.Text;

    internal class ProcessStarter : ICommand
    {
        public void Run(string[] args)
        {
            string verb = DecodeArg(args[1]);
            string fileName = DecodeArg(args[2]);
            if (string.Equals(verb, "--explore--"))
            {
                OpenExplorerOnFile(fileName);
            }
            else
            {
                ShellExecute(verb, fileName);
            }
        }

        private static void ShellExecute(string verb, string fileName)
        {
            ProcessStartInfo processStartInfo = new ProcessStartInfo();
            processStartInfo.UseShellExecute = true;
            processStartInfo.Verb = verb;
            processStartInfo.FileName = fileName;
            Process.Start(processStartInfo);
        }

        private static void OpenExplorerOnFile(string fileName)
        {
            IntPtr pidlList = NativeMethods.ILCreateFromPathW(fileName);
            if (pidlList == IntPtr.Zero)
            {
                throw new ExternalException("ILCreateFromPathW call failed");
            }

            try
            {
                Marshal.ThrowExceptionForHR(
                    NativeMethods.SHOpenFolderAndSelectItems(
                        pidlList, 0, IntPtr.Zero, 0));
            }
            finally
            {
                NativeMethods.ILFree(pidlList);
            }
        }

        private static string DecodeArg(string arg)
        {
            return Encoding.UTF8.GetString(Convert.FromBase64String(arg));
        }
    }
}
