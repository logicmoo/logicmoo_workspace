using System.Runtime.InteropServices;

namespace Swicli.Library
{
    [System.Security.SuppressUnmanagedCodeSecurity]
    public static class JplSafeNativeMethods
    {
        private const string DllFileName = @"jpl.dll";//"libjpl.dll" for 5.7.8; //was 

        public static string DllFileName1
        {
            get { return DllFileName; }
        }
        [DllImport(DllFileName)]
        public static extern void install();

        //[DllImport(DllFileName)]
        //public static extern java.lang.Thread jni_env();

        //[DllImport(DllFileName)]
        //public static extern int jpl_c_lib_version_1_plc(uint term_t);
    }
}