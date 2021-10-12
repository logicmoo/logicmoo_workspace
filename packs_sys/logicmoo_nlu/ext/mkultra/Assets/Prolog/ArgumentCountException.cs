using System;
using System.Text;

namespace Prolog
{
    /// <summary>
    /// Indicates a procedure or predicate was called with the wrong number of arguments.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2237:MarkISerializableTypesWithSerializable"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1032:ImplementStandardExceptionConstructors")]
    public class ArgumentCountException : ArgumentException
    {
        /// <summary>
        /// Indicates a variable that should be bound isn't
        /// </summary>
        public ArgumentCountException(string procName, object[] actualArguments, params object[] expectedArguments)
        {
            procedureName = procName;
            ActualArguments = actualArguments;
            ExpectedArguments = expectedArguments;
        }

        private readonly string procedureName;

        /// <summary>
        /// Arguments provided in the call
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        public readonly object[] ActualArguments;
        /// <summary>
        /// Arguments the procedure or predicate expected.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        public readonly object[] ExpectedArguments;

        /// <summary>
        /// Message describing the problem.
        /// </summary>
        public override string Message
        {
            get
            {
                var b = new StringBuilder();
                b.AppendFormat("Wrong number of arguments to {0} '{1}'.\nReceived: ", "predicate", procedureName);
                bool firstOne = true;
                foreach (var e in ActualArguments)
                {
                    if (firstOne)
                        firstOne = false;
                    else
                        b.Append(", ");

                    b.Append(Term.ToStringInPrologFormat(e));

                }
                b.Append("\nExpected: ");
                firstOne = true;
                foreach (var e in ExpectedArguments)
                {
                    if (firstOne)
                        firstOne = false;
                    else
                        b.Append(", ");

                    b.Append(e);
                }
                return b.ToString();
            }
        }
    }
}
