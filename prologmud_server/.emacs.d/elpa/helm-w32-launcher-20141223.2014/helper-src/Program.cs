// This file is a part of the helm-w32-launcher's C# helper.
//
// Copyright (c) 2014, Fanael Linithien
// See ../license.txt for licensing information.
namespace HelmW32Launcher
{
    using System;
    using System.Reflection;
    using System.Text;

    internal class Program
    {
        [STAThread]
        private static int Main(string[] args)
        {
            try
            {
                Console.OutputEncoding = Encoding.UTF8;
                object obj = Activator.CreateInstance(
                    null, GetCurrentNamespace() + "." + args[0]).Unwrap();
                ICommand command = (ICommand)obj;
                command.Run(args);
                return 0;
            }
            catch (Exception e)
            {
                Console.Error.WriteLine(e);
                return 255;
            }
        }

        private static string GetCurrentNamespace()
        {
            return typeof(Program).Namespace;
        }
    }
}
