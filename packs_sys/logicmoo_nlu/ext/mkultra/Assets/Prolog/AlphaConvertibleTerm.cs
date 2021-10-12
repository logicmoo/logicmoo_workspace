using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// A Term that requires processing during alpha conversion.
    /// </summary>
    public abstract class AlphaConvertibleTerm : Term
    {
        /// <summary>
        /// Recopy the term to replace variables.  If term contains no variables, no recopying is done.
        /// </summary>
        /// <param name="oldVars">Variables to be replaced</param>
        /// <param name="newVars">The corresponding variables that are replacing the oldVars</param>
        /// <param name="context">PrologContext to evaluating indexicals</param>
        /// <param name="evalIndexicals">If true, any indexicals will be replaced with their values.</param>
        /// <returns>Converted term or original term if not conversion necessary</returns>
        public abstract object AlphaConvert(
            List<LogicVariable> oldVars,
            LogicVariable[] newVars,
            PrologContext context,
            bool evalIndexicals);
    }
}
