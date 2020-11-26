using System;
namespace ExampleCallsProlog
{
    public static class ExampleCallsPrologClass
    {
        static ExampleCallsPrologClass()
        {
            Message("ExampleCallsProlog::SWICLITestClass.<clinit>()");
        }
        public static void install()
        {
            Message("ExampleCallsProlog::SWICLITestClass.install()");
            var a = System.Reflection.Assembly.Load("csharp");
            if (a == null) return;
            var e = a.EntryPoint;
            var dt = e.DeclaringType;
            if (dt == null) return;
            Message("ExampleCallsProlog::install press ctrol-D to leave CSharp");
            var m = dt.GetMethod("Main", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static);
            m.Invoke(null, new object[] { new String[0] });
        }
        public static void Main(string[] args0)
        {
            Message("ExampleCallsProlog::SWICLITestClass.install()");
        }

        public static void Message(string p)
        {
            System.Windows.Forms.MessageBox.Show(p);
            Console.WriteLine(p);
        }

    }
}
