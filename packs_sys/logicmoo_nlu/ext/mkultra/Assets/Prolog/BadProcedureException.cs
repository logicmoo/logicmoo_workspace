using System;

namespace Prolog
{
    class BadProcedureException : Exception
    {
        public object Procedure { get; private set; }

        public BadProcedureException(object proc)
        {
            Procedure = proc;
        }

        public BadProcedureException(Symbol functor, int arity)
        {
            Procedure = new Structure(Symbol.Intern("/"), functor, arity);
        }

        public override string Message
        {
            get
            {
                var s = Procedure as Structure;
                if (s != null)
                    return String.Format("Unknown arithmetic function: {0}/{1}", s.Arguments[0], s.Arguments[1]);
                return String.Format("Bad function or procedure: {0}", this.Procedure);
            }
        }

    }
}
