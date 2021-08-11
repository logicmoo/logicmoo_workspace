using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using SbsSW.SwiPlCs;
using Swicli.Library;

namespace PlConsole
{
    class Program
    {
        static void Main(string[] args)
        {
           // libpl.PL_initialise(args.Length, args);
            try
            {
                Type.GetType("org.armedbear.lisp.Main").GetMethod("main").Invoke(null, args);
                //org.armedbear.lisp.Main.main(args);
            }
            catch (Exception exception )
            {
                Embedded.WriteException( exception);
                //throw;
            }
            return;
        }
    }
}
