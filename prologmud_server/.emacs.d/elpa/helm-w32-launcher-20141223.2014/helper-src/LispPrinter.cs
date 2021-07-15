// This file is a part of the helm-w32-launcher's C# helper.
//
// Copyright (c) 2014, Fanael Linithien
// See ../license.txt for licensing information.
namespace HelmW32Launcher
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Text;

    internal static class LispPrinter
    {
        public static string PrintStartMenuEntries(
            IEnumerable<string> entryPaths)
        {
            Impl impl = new Impl();
            impl.PrintStartMenuEntries(entryPaths);
            return impl.Result;
        }

        private class Impl
        {
            private StringBuilder result = new StringBuilder(65536);

            public Impl()
            {
            }

            public string Result
            {
                get { return this.result.ToString(); }
            }

            public void PrintStartMenuEntries(IEnumerable<string> entryPaths)
            {
                this.result.Append('(');
                foreach (string entryPath in entryPaths)
                {
                    this.result.Append('(');
                    this.PrintString(
                        Path.GetFileNameWithoutExtension(entryPath));
                    this.result.Append(" . ");
                    this.PrintString(entryPath);
                    this.result.Append(')');
                }

                this.result.Append(')');
            }

            private void PrintString(string str)
            {
                this.result.Append('"');
                this.result.Append(
                    str.Replace(@"\", @"\\").Replace("\"", "\\\""));
                this.result.Append('"');
            }
        }
    }
}
