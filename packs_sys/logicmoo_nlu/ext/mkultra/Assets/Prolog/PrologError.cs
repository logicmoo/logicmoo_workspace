using System;

namespace Prolog
{
    /// <summary>
    /// Lets Prolog specify a Prolog StackTrace for an exception, so that the correct
    /// information and file name will be displayed in the Unity console.
    /// </summary>
    public class PrologError : Exception
    {
        private readonly string prologStackTrace;
        public PrologError(Exception innerException, string prologStackTrace)
            : base(innerException.Message)
        {
            this.prologStackTrace = prologStackTrace;
        }

        public override string StackTrace
        { get { return prologStackTrace; } }
    }
}
