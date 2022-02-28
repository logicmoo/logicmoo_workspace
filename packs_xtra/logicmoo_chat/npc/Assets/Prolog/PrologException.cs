using System;

namespace Prolog
{
    /// <summary>
    /// General class for prolog-specific exceptions mandated by the ISO standard.
    /// </summary>
    public class PrologException : Exception
    {
        /// <summary>
        /// The ISO-standard exception term describing the exception
        /// </summary>
        // ReSharper disable once InconsistentNaming
        public object ISOException { get; private set; }
        /// <summary>
        /// Creates a .NET Exception object to represent an ISO-Prolog exception
        /// </summary>
        public PrologException(object isoException)
        {
            ISOException = isoException;
        }

        /// <summary>
        /// Human-readable message describing the exception
        /// </summary>
        public override string Message
        {
            get
            {
                return String.Format("Prolog exception: {0}", ISOException);
            }
        }
    }
}
