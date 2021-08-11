using System;
using Swicli.Library;
using SbsSW.SwiPlCs;
namespace SWICLITestDLL
{
    public static class SWICLITestClass
    {
        static SWICLITestClass()
        {
            Console.WriteLine("SWICLITestDLL::SWICLITestClass.<clinit>()");
        }
        public static void install()
        {
            Console.WriteLine("SWICLITestDLL::SWICLITestClass.install()");
            //NonDetExample.LoadNonDetExamples();
            Console.WriteLine("SWICLITestClass::install press ctrol-D to leave CSharp");
            System.Reflection.Assembly.Load("csharp").EntryPoint.DeclaringType.GetMethod("Main", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static).Invoke(null, new object[] { new String[0] });
        }
        public static void Main(string[] args0)
        {
            PrologCLR.PingThreadFactories();
            bool demo = args0.Length >0;
            PrologCLR.SetupProlog();
			if (demo)
			{
				MainDemo();
			}
		}

		public static void MainDemo() {

			PrologCLR.DoQuery("asserta(fff(1))");
			PrologCLR.DoQuery("asserta(fff(9))");
			PrologCLR.DoQuery("nl");
			PrologCLR.DoQuery("flush");

			PrologCLR.PlAssert("father(martin, inka)");
			if (!Embedded.PlCsDisabled)
			{
				PlQuery.PlCall("assert(father(uwe, gloria))");
				PlQuery.PlCall("assert(father(uwe, melanie))");
				PlQuery.PlCall("assert(father(uwe, ayala))");
				using (PlQuery q = new PlQuery("father(P, C), atomic_list_concat([P,' is_father_of ',C], L)"))
				{
					foreach (PlTermV v in q.Solutions)
						PrologCLR.ConsoleTrace(PrologCLR.ToCSString(v));

					foreach (PlQueryVariables v in q.SolutionVariables)
						PrologCLR.ConsoleTrace(v["L"].ToString());


					PrologCLR.ConsoleTrace("all child's from uwe:");
					q.Variables["P"].Unify("uwe");
					foreach (PlQueryVariables v in q.SolutionVariables)
						PrologCLR.ConsoleTrace(v["C"].ToString());
				}
				//PlQuery.PlCall("ensure_loaded(library(thread_util))");
				//Warning: [Thread 2] Thread running "thread_run_interactor" died on exception: thread_util:attach_console/0: Undefined procedure: thread_util:win_open_console/5
				//PlQuery.PlCall("interactor");
				//Delegate Foo0 = foo0;
				PrologCLR.RegisterPLCSForeigns();
			}

			PrologCLR.PlAssert("tc2:-foo2(X,Y),writeq(f(X,Y)),nl,X=5");
			PrologCLR.PlAssert("tc3:-foo3(X,Y,Z),Z,writeln(f(X,Y,Z)),X=5");
		}
        

    }


    public class OpImplTest
    {
        private OpImplTest()
        {

        }
        //static public operator String
    }
}