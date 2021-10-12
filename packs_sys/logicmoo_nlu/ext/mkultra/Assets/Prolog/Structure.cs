using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

namespace Prolog
{
    /// <summary>
    /// Represents a function or predicate expression in the logic programming system.
    /// </summary>
    [SuppressMessage("Microsoft.Naming", "CA1716:IdentifiersShouldNotMatchKeywords", MessageId = "Structure")]
    public sealed class Structure : AlphaConvertibleTerm
    {
        #region Constructor
        /// <summary>
        /// Creates a new term with the specified functor (predicate or function name) and arguments.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "functor")]
        public Structure(Symbol functor, params object[] functorArguments)
        {
            if (functorArguments == null) 
                throw new ArgumentNullException("functorArguments");
            Functor = functor;
            Arguments = functorArguments;
        }

        /// <summary>
        /// Creates a new term with the specified functor (predicate or function name) and arguments.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "functor")]
        public Structure(string functor, params object[] args)
            : this(Symbol.Intern(functor), args)
        {
        }

        /// <summary>
        /// Returns the Term whose functor is the first element of the list and whose arguments are the other elements of the list.
        /// Functional version of =..
        /// </summary>
        public static Structure FromList(Structure listExpression)
        {
            if (listExpression == null || !listExpression.IsFunctor(Symbol.PrologListConstructor, 2)) throw new ArgumentException("Argument must be a prolog list");
            object functorArg = listExpression.Argument(0);
            if (functorArg == null) throw new ArgumentException("First element of list (functor) must be a symbol.");
            var functor = functorArg as Symbol;
            if (functor == null) throw new ArgumentException("First element of list (functor) must be a symbol.");
            return new Structure(functor, Prolog.PrologListToArray(listExpression.Arguments[1]));
        }
        #endregion

        /// <summary>
        /// Returns a Prolog list whose first element is the functor of the term and whose tail is its arguments.
        /// Functional version of =..
        /// </summary>
        public Structure ToPrologList()
        {
            return new Structure(Symbol.PrologListConstructor, Functor, Prolog.ArrayToPrologList(Arguments));
        }

        #region Instance fields and properties
        /// <summary>
        /// Returns the predicate indicator for this structure.
        /// </summary>
        public PredicateIndicator PredicateIndicator
        {
            get
            {
                return new PredicateIndicator(this);
            }
        }

        /// <summary>
        /// The name of the function or predicate.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Functor")]
        public Symbol Functor { get; private set; }
        /// <summary>
        /// Arguments to the function or predicate.
        /// </summary>
        [SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")]
        public object[] Arguments { get; private set; }

        /// <summary>
        /// Returns the canonicalized version of the ith argument
        /// </summary>
        public object Argument(int argumentIndex)
        {
            return Deref(Arguments[argumentIndex]);
        }

        /// <summary>
        /// Returns the canonicalized version of the ith argument
        /// </summary>
        public T Argument<T>(int argumentIndex)
        {
            return (T)Deref(Arguments[argumentIndex]);
        }

        /// <summary>
        /// The arity (number of arguments) of this structure.
        /// </summary>
        public int Arity
        {
            get { return Arguments.Length; }
        }
        #endregion

        #region Derived properties and predicates
        /// <summary>
        /// True if neither this term nor its subterms contain and LogicVariables
        /// </summary>
        public bool IsGroundInstance
        {
            get
            {
                foreach (var o in Arguments)
                {
                    var t = o as Structure;
                    if (t != null && !t.IsGroundInstance)
                        return false;
                }
                return true;
            }
        }

        /// <summary>
        /// True if the term has the specified functor and arity
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "arity"), SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Functor")]
        public bool IsFunctor(Symbol name, int arity)
        {
            return Functor == name && Arguments.Length == arity;
        }

        /// <summary>
        /// True if the term has the specified functor and arity
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "arity"), SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Functor")]
        public static bool IsFunctor(object term, Symbol name, int arity)
        {
            var s = term as Structure;
            return s != null && s.IsFunctor(name, arity);
        }
        #endregion

        #region Unification
        internal override IEnumerable<bool> UnifyWithTerm(Term term)
        {
            return term.UnifyWithStructure(this);
        }

        internal override IEnumerable<bool> UnifyWithStructure(Structure value)
        {
            if (value == null || value.Functor != Functor || value.Arguments.Length != Arguments.Length)
                return FailEnumerator;
            return UnifyArrays(Arguments, value.Arguments);
        }

        internal override bool UnifyWithTerm(Term term, PrologContext context)
        {
            return term.UnifyWithStructure(this, context);
        }

        internal override bool UnifyWithStructure(Structure value, PrologContext context)
        {
            if (value == null || value.Functor != Functor || value.Arguments.Length != Arguments.Length)
                return false;
            return UnifyArrays(Arguments, value.Arguments, context);
        }
        #endregion

        #region Alpha conversion and copying
        /// <summary>
        /// Appends arguments to the end of Term, returning a new Term.
        /// </summary>
        public Structure AddArguments(params object[] args)
        {
            if (args == null) throw new ArgumentNullException("args", "Argument list to add to term is null.");
            var newArgs = new object[Arguments.Length + args.Length];
            Arguments.CopyTo(newArgs, 0);
            args.CopyTo(newArgs, Arguments.Length);
            return new Structure(Functor, newArgs);
        }

        /// <summary>
        /// Returns a Term identical to this one except for replacing any variables appearing in oldVars
        /// with their corresponding variables from newVars.  Returns the original Term object if no oldVars
        /// appear within it (e.g. if it's a ground instance).
        /// </summary>
        [SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists")]
        public override object AlphaConvert(List<LogicVariable> oldVars, LogicVariable[] newVars, PrologContext context, bool evalIndexicals)
        {
            var newArgs = AlphaConvertArglist(Arguments, oldVars, newVars, context, evalIndexicals);
            return newArgs == this.Arguments ? this : new Structure(this.Functor, newArgs);
        }
        #endregion

        #region Macroexpansion
        /// <summary>
        /// Macroexpands term (e.g. for definite clause grammars
        /// </summary>
        /// <returns>Expanded version of term (or original object if no expansion necessary)</returns>
        public Structure Expand()
        {
            if (IsFunctor(Symbol.GrammarRule, 2))
                return ExpandDCGRule();
            return this;
        }

        // ReSharper disable InconsistentNaming
        private Structure ExpandDCGRule()
        {
            var start = new LogicVariable(StartSym);
            if (Argument(1) == null) // name --> [].
                return ExpandDCGRuleHead(Argument(0), start, start);
            LogicVariable rest;
            Structure body = ExpandDCGRuleBody(Argument(1), start, out rest);
            return new Structure(Symbol.Implication,
                                 ExpandDCGRuleHead(Argument(0), start, rest),
                                 body);
        }
        private static readonly Symbol StartSym = Symbol.Intern("Start");
        private static readonly Symbol RestSym = Symbol.Intern("Rest");

        private static readonly Symbol LSym = Symbol.Intern("L");

        private static Structure ExpandDCGRuleBody(object goal, LogicVariable start, out LogicVariable rest)
        {
            var t = goal as Structure;
            if (t != null && t.IsFunctor(Symbol.Comma, 2))
            {
                var t2 = t.Argument(0) as Structure;
                if (t2 != null && t2.IsFunctor(Symbol.PrologListConstructor, 2))
                    return ExpandDCGLiteral(t2, t.Argument(1), start, out rest);
                if (t2 != null && t2.IsFunctor(Symbol.CurlyBrackets, 1))
                    return ExpandDCGCurlyBracketExpression(t2.Argument(0), t.Argument(1), start, out rest);
                var intermediate = new LogicVariable(LSym);
                return new Structure(Symbol.Comma,
                                     ExpandDCGRuleGoal(t.Argument(0), start, intermediate),
                                     ExpandDCGRuleBody(t.Argument(1), intermediate, out rest));
            }
            if (t != null && t.IsFunctor(Symbol.CurlyBrackets, 1))
            {
                Structure cg = t.Argument(0) as Structure ?? new Structure(Symbol.Call, t.Argument(0));
                rest = start;
                return cg;
            }
            return ExpandDCGRuleTrailingGoal(goal, start, out rest);
        }

        private static readonly Symbol DefiniteClauseCapitalC = Symbol.Intern("C");

        private static Structure ExpandDCGLiteral(Structure literal, object afterward, LogicVariable start, out LogicVariable rest)
        {
            if (literal == null)
                return ExpandDCGRuleBody(afterward, start, out rest);
            if (literal.Argument(1) == null && afterward == null)
            {
                rest = new LogicVariable(RestSym);
                return new Structure(DefiniteClauseCapitalC, start, literal.Argument(0), rest);
            }
            var intermediate = new LogicVariable(LSym);
            return new Structure(Symbol.Comma,
                                 new Structure(DefiniteClauseCapitalC, start, literal.Argument(0), intermediate),
                                 ExpandDCGLiteral((Structure)literal.Argument(1), afterward, intermediate, out rest));
        }

        private static Structure ExpandDCGCurlyBracketExpression(object goalExpression, object afterward, LogicVariable start, out LogicVariable rest)
        {
            var t = goalExpression as Structure;
            if (afterward == null)
            {
                if (t == null)
                    t = new Structure(Symbol.Call, goalExpression);
                rest = start;
                return t;
            }
            if (t != null && t.IsFunctor(Symbol.Comma, 2))
                return new Structure(Symbol.Comma,
                                     t.Argument(0),
                                     ExpandDCGCurlyBracketExpression(t.Argument(1), afterward, start, out rest));
            return new Structure(Symbol.Comma,
                                 goalExpression,
                                 ExpandDCGRuleBody(afterward, start, out rest));
        }

        private static Structure ExpandDCGRuleTrailingGoal(object expression, LogicVariable start, out LogicVariable rest)
        {
            var t = expression as Structure;
            var s = expression as Symbol;
            if (t != null)
            {
                if (t.IsFunctor(Symbol.PrologListConstructor, 2))
                    return ExpandDCGLiteral(t, null, start, out rest);
                rest = new LogicVariable(RestSym);
                return t.AddArguments(start, rest);
            }
            if (s != null)
            {
                rest = new LogicVariable(RestSym);
                return new Structure(s, start, rest);
            }
            throw new ArgumentException("Invalid expression in grammar rule: "+ToStringInPrologFormat(expression), "expression");
        }

        private static Structure ExpandDCGRuleGoal(object expression, LogicVariable start, LogicVariable rest)
        {
            var t = expression as Structure;
            var s = expression as Symbol;
            if (t != null)
            {
                if (t.IsFunctor(Symbol.PrologListConstructor, 2))
                    return ExpandDCGLiteral(t, null, start, out rest);
                return t.AddArguments(start, rest);
            }
            if (s != null)
                return new Structure(s, start, rest);
            throw new ArgumentException("Invalid expression in grammar rule: " + ToStringInPrologFormat(expression), "expression");
        }

        private static Structure ExpandDCGRuleHead(object expression, LogicVariable start, LogicVariable rest)
        {
            var t = expression as Structure;
            var s = expression as Symbol;
            if (t != null)
                return t.AddArguments(start, rest);
            if (s != null)
                return new Structure(s, start, rest);
            throw new ArgumentException("Invalid expression in grammar rule: " + ToStringInPrologFormat(expression), "expression");
        }
        #endregion

        #region Prolog-format output
#if !OldPrologWriter
        /// <summary>
        /// Renders term in Prolog format
        /// </summary>
        public override string ToString()
        {
            return ISOPrologWriter.WriteToString(this);
        }
#else
        /// <summary>
        /// Renders term in Prolog format
        /// </summary>
        public override string ToString()
        {
            StringBuilder s = new StringBuilder();
            ToStringBuilder(s);
            return s.ToString();
        }

        internal void ToStringBuilder(StringBuilder s)
        {
            if (IsFunctor(Symbol.PrologListConstructor, 2))
                WritePrologList(s);
            else if (IsFunctor(Symbol.Comma, 2) || IsFunctor(Symbol.Implication, 2))
            {
                Write(s, Argument(0));
                s.Append(Functor.Name);
                s.Append(' ');
                Write(s, Argument(1));
            }
            else if (Arguments.Length == 2 && PrologReader.IsBinaryOperator(Functor))
            {
                Write(s, Argument(0));
                //s.Append(' ');
                s.Append(Functor.Name);
                //s.Append(' ');
                Write(s, Argument(1));
            }
            else
                WriteNormalPrologTerm(s);
        }

        private void WriteNormalPrologTerm(StringBuilder s)
        {
            Write(s, Functor);
            s.Append('(');
            bool firstOne = true;
            foreach (var arg in Arguments)
            {
                if (firstOne)
                    firstOne = false;
                else
                    s.Append(", ");

                WriteAndPossiblyParenthesize(s, arg);
            }
            s.Append(')');
        }

        private void WritePrologList(StringBuilder s)
        {
            s.Append('[');
            bool first = true;
            object current = Canonicalize(this);
            LogicVariable l = current as LogicVariable;
            if (l != null && l.IsBound)
                current = l.Value;
            while (current != null)
            {
                Structure t = current as Structure;

                if (first)
                    first = false;
                else if (t == null)
                    s.Append(" | ");
                else
                    s.Append(", ");
                if (t == null)
                {
                    Write(s, current);
                    current = null;
                }
                else
                {
                    if (t.IsFunctor(Symbol.PrologListConstructor, 2))
                    {
                        Write(s, t.Argument(0));
                        current = t.Argument(1);
                    }
                    else
                    {
                        WriteAndPossiblyParenthesize(s, current);
                        current = null;
                    }
                }
            }
            s.Append(']');
        }
#endif
        #endregion
    }
}
