using System;
using System.Threading;
using SbsSW.SwiPlCs;

namespace Swicli.Library
{
    public static class Embedded
    {


        public const string ExportModule = "swicli";

        /// <summary>
        /// the .Net process (Not OS)
        /// </summary>
        /// <returns></returns>
        internal static bool Is64BitRuntime()
        {
            int bits = IntPtr.Size*8;
            return bits == 64;
        }

        /// <summary>
        /// The OS and not the .Net process
        ///  therefore "Program Files" are either for 64bit or 32bit apps
        /// </summary>
        /// <returns></returns>
        internal static bool Is64BitComputer()
        {
            return Is64BitRuntime() || !String.IsNullOrEmpty(Environment.GetEnvironmentVariable("ProgramFiles(x86)"));
        }

        public static int VMStringsAsAtoms = libpl.CVT_STRING;
        private static string CLASSPATH = null;
        public static bool VerboseStartup = false;
        public static bool JplDisabled = true;
        public static bool PlCsDisabled = false;
        public static bool IsHalted = false;
        public static bool IsPLWin = false;
        public static bool RedirectStreams = false;
        public static bool JplSafeNativeMethodsDisabled = false;
        public static bool JplSafeNativeMethodsCalled = false;
        public static bool FailOnMissingInsteadOfError = true;


        public static bool IsLinux
        {
            get
            {
                int p = (int) Environment.OSVersion.Platform;
                return (p == 4) || (p == 6) || (p == 128);
            }
        }


        public static int install()
        {
            try
            {

                IsPLWin = Type.GetType("Mono.Runtime") == null;
                RedirectStreams = false;
                Embedded.IsEmbeddedFromProlog = true;
                PrologCLR.SetupProlog();
                ConsoleWriteLine(typeof (Embedded).Name + ".install suceeded");
                (new Thread(PrologCLR.cliStartJmx)).Start();
                (new Thread(PrologCLR.cliStartDbg)).Start();
                PrologCLR.ClientReady = true;
                return libpl.PL_succeed;
            }
            catch (Exception e)
            {
                WriteException(e);
                ConsoleWriteLine(typeof (Embedded).Name + ".install failed");
                return libpl.PL_fail;
            }
        }

        public static bool IsEmbeddedFromProlog { get; set; }

        public static void WriteException(Exception exception)
        {
#if USE_IKVM
            java.lang.Exception ex = exception as java.lang.Exception;
            if (ex != null)
            {
                ex.printStackTrace();

            }
#endif
            //else
            {
                Exception inner = exception.InnerException;
                if (inner != null && inner != exception)
                {
                    WriteException(inner);
                }
                ConsoleWriteLine("ST: " + exception.StackTrace);
            }

            ConsoleWriteLine("PrologCLR: " + exception);
        }

        public static void ConsoleWriteLine(string text)
        {
			try {
			    // System.Console.Error.WriteLine(text);
			    // System.Windows.Forms.MessageBox.Show(text);
				System.Console.WriteLine(text);
			} catch (System.TypeInitializationException e) {
				// @TODO
			} catch (Exception e) {
				// @TODO
			}
        }

        public static unsafe bool Warn(string text, params object[] ps)
        {
            text = PlStringFormat(text, ps);
            return libpl.PL_warning(text) != 0;
        }

        public static unsafe bool Error(string text, params object[] ps)
        {
            text = PlStringFormat(text, ps);
            text = text.Replace("\\", "/");
            //text = text.Replace("\\", "\\\\");
            return libpl.PL_warning(text) != 0;
        }

        public static bool WarnMissing(string text, params object[] ps)
        {
            text = PlStringFormat(text, ps);
            if (true)
            {
                Debug(text);
                return false;
            }
            return Warn(text);
        }

        public static void Debug(string text, params object[] ps)
        {
            text = PlStringFormat(text, ps);
            ConsoleWriteLine(text);
        }

        public static string PlStringFormat(string text, params object[] ps)
        {
            PrologCLR.RegisterCurrentThread();
            try
            {
                if (ps != null && ps.Length > 0)
                {
                    for (int i = 0; i < ps.Length; i++)
                    {
                        var o = ps[i];
                        if (o == null)
                        {
                            ps[i] = "NULL";
                        }
                        else if (o is Exception)
                        {
                            ps[i] = PrologCLR.ExceptionString((Exception) o);
                        }
                    }
                    text = String.Format(text, ps);
                }
            }
            catch (Exception)
            {
            }
            PrologCLR.DeregisterThread(Thread.CurrentThread);
            return text;
        }

    }
}