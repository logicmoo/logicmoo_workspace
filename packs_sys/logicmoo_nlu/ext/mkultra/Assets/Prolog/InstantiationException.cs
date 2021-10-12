using System;

namespace Prolog
{
    /// <summary>
    /// Thrown when a variable that should/shouldn't be bound isn't/is bound
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2237:MarkISerializableTypesWithSerializable"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1032:ImplementStandardExceptionConstructors")]
    public class InstantiationException : Exception
    {
        /// <summary>
        /// Indicates a variable that should/shouldn't be bound isn't/is bound
        /// </summary>
        public InstantiationException(LogicVariable offendingVariable, string message)
            : base(message)
        {
            Variable = offendingVariable;
        }

        /// <summary>
        /// The variable that should/shouldn't have been bound.
        /// </summary>
        public LogicVariable Variable { get; private set; }
    }

    /// <summary>
    /// Thrown when a variable that should be bound isn't
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2237:MarkISerializableTypesWithSerializable"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1032:ImplementStandardExceptionConstructors")]
    public class UninstantiatedVariableException : InstantiationException
    {
        /// <summary>
        /// Indicates a variable that should be bound isn't
        /// </summary>
        public UninstantiatedVariableException(LogicVariable offendingVariable, string message)
            : base(offendingVariable, message)
        { }
    }

    /// <summary>
    /// Thrown when a variable that shouldn't be bound is bound
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2237:MarkISerializableTypesWithSerializable"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1032:ImplementStandardExceptionConstructors")]
    public class InstantiatedVariableException : InstantiationException
    {
        /// <summary>
        /// Indicates a variable that shouldn't be bound is bound
        /// </summary>
        public InstantiatedVariableException(LogicVariable offendingVariable, string message)
            : base(offendingVariable, message)
        { }
    }
}
