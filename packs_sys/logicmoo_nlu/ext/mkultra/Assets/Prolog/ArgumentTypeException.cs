using System;

namespace Prolog
{
    /// <summary>
    /// Indicates a procedure or predicate was called with the wrong number of arguments.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2237:MarkISerializableTypesWithSerializable"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1032:ImplementStandardExceptionConstructors")]
    public class ArgumentTypeException : ArgumentException
    {
        /// <summary>
        /// Indicates a variable that should be bound isn't
        /// </summary>
        public ArgumentTypeException(string procName, string argumentName, object value, Type expectedType)
        {
            procedureName = procName;
            ArgumentName = argumentName;
            Value = value;
            ExpectedType = expectedType;
        }

        private readonly string procedureName;
        /// <summary>
        /// Name of the argument that was passed an incorrect value.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        public readonly string ArgumentName;
        /// <summary>
        /// Value passed to the argument
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        public readonly object Value;
        /// <summary>
        /// Type of value that should have been passed
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        public readonly Type ExpectedType;

        /// <summary>
        /// Message describing the problem.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1305:SpecifyIFormatProvider", MessageId = "System.String.Format(System.String,System.Object[])")]
        public override string Message
        {
            get
            {
                return string.Format("The {0} argument to {1} should have been of type {2} but was passed {3}.", ArgumentName, procedureName,
                                     ExpectedType.Name,
                                     Term.ToStringInPrologFormat(Value));
            }
        }
    }
}
