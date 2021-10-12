using System;

namespace Prolog
{
    /// <summary>
    /// Under Unity, we need to signal warnings in the log as exceptions because that's
    /// the only mechanism we have for overriding the stack trace.  So this is a wrapper
    /// based on PrologError that lets us log the warning with an appropriate source file
    /// and line number.
    /// </summary>
    class PrologWarning : Exception
    {
        private readonly string prologStackTrace;
        public PrologWarning(string message, string prologStackTrace) 
            : base(message)
        {
            this.prologStackTrace = prologStackTrace;
        }

        public override string StackTrace
        { get { return prologStackTrace; } }
    }
}
