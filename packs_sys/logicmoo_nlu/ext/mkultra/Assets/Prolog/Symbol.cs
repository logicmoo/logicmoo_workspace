// Copyright 2007, 2008, 2009, 2010, 2011 Ian Horswill
// This file is part of Twig.
//
// Twig is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as 
// published by the Free Software Foundation, either version 3 of
//  the License, or (at your option) any later version.
//
// Twig is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Twig.  If not, see <http://www.gnu.org/licenses/>.

using System.Collections.Generic;

namespace Prolog

{
    /// <summary>
    /// A Lisp Symbol
    /// </summary>
    [Documentation("The class of LISP symbols.")]
    public sealed class Symbol : Term
    {
        #region Fields and Properties
        /// <summary>
        /// The printed representation of the symbol
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        [Documentation("Printed representation of the symbol.")]
        public readonly string Name;

        /// <summary>
        /// The keyword name without the trailing colon.
        /// </summary>
        [Documentation("The keyword name without the trailing colon.")]
        public string KeywordName
        {
            get
            {
                return Name.TrimEnd(':');
            }
        }

        /// <summary>
        /// True if this is a keyword symbol.  Keywords are self-evaluating.
        /// </summary>
        [Documentation("True if this is a keyword symbol, i.e. ends with a colon.  Keywords are self-evaluating.")]
        public bool IsKeyword
        {
            get
            {
                return Name.EndsWith(":");
            }
        }

        /// <summary>
        /// True if this is a pattern variable.
        /// </summary>
        [Documentation("True if this is a pattern variable, either in traditional lisp format (starts with a '?') or Prolog format (starts with a captial letter or '_').")]
        public bool IsPatternVariable
        {
            get
            {
                return Name.StartsWith("?") || Name[0] == '_' || char.IsUpper(Name[0]);
            }
        }
        #endregion

        #region Printing
        /// <summary>
        /// Prints the name of the symbol.
        /// </summary>
        public override string ToString()
        {
            return Name;
        }
        #endregion

        #region Interning and symbol table
        static readonly Dictionary<string, Symbol> SymbolTable = new Dictionary<string, Symbol>();

        /// <summary>
        /// Returns the symbol with the specified name.
        /// Creates a new symbol if necessary, but always returns the same object
        /// when called with the same string.
        /// </summary>
        [Documentation("Returns the unique symbol iwth the specified name.")]
        public static Symbol Intern(string name)
        {
            Symbol s;
            SymbolTable.TryGetValue(name, out s);
            if (s != null)
                return s;
            // Constructor adds to symbol table automatically
            return new Symbol(name);
        }

        /// <summary>
        /// True if the string names a symbol in the symbol table.  Does not intern a new symbol.
        /// </summary>
        /// <param name="name">Name of the symbol to search for</param>
        /// <returns>True if there is a symbol by that name.</returns>
        public static bool IsInterned(string name)
        {
            return SymbolTable.ContainsKey(name);
        }
        #endregion

        #region Constructor - private -
        Symbol(string name)
        {
            //Prolog allows null atoms, alas.
            //System.Diagnostics.Debug.Assert(!string.IsNullOrEmpty(name));
            Name = name;
            SymbolTable[name] = this;
        }
        #endregion

        #region Unification
        internal override IEnumerable<bool> UnifyWithTerm(Term value)
        {
            return value.UnifyWithAtomicConstant(this);
        }
        internal override bool UnifyWithTerm(Term value, PrologContext context)
        {
            return value.UnifyWithAtomicConstant(this, context);
        }
        
        #endregion

        #region Symbol constants
        /// <summary>
        /// The symbol 'quote
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Quote = Intern("quote");
        /// <summary>
        /// The symbol 'quasiquote
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Quasiquote")]
        public static readonly Symbol Quasiquote = Intern("quasiquote");
        /// <summary>
        /// The symbol 'unquote
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Unquote = Intern("unquote");
        /// <summary>
        /// The symbol 'unquote-splicing
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol UnquoteSplicing = Intern("unquote-splicing");

        /// <summary>
        /// The symbol 'lambda
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Lambda = Intern("lambda");
        /// <summary>
        /// The symbol 'member
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Member = Intern("member");

        /// <summary>
        /// The symbol 'macroexpand
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Macroexpand")]
        public static readonly Symbol Macroexpand = Intern("macroexpand");

        /// <summary>
        /// The symbol 'else
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Else = Intern("else");
        /// <summary>
        /// The symbol 'if
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol If = Intern("if");
        /// <summary>
        /// The symbol 'begin
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Begin = Intern("begin");
        /// <summary>
        /// The symbol 'set!
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol SetBang = Intern("set!");
        /// <summary>
        /// The symbol 'define
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Define = Intern("define");
        /// <summary>
        /// The symbol 'let
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Let = Intern("let");
        
        /// <summary>
        /// The symbol 'new
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol New = Intern("new");
        /// <summary>
        /// The symbol 'not
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Not = Intern("not");
        /// <summary>
        /// The symbol 'list
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol List = Intern("list");
        /// <summary>
        /// The symbol 'append
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Append = Intern("append");

        /// <summary>
        /// The symbol 'this
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol This = Intern("this");

        /// <summary>
        /// The symbol 'stack
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Stack = Intern("stack");

        /// <summary>
        /// The symbol '...
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Ellipsis = Intern("...");

        /// <summary>
        /// The symbol 'Object
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Object = Intern("Object");

        /// <summary>
        /// The Prolog symbol ','
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Comma = Intern(",");

        /// <summary>
        /// The symbol '='
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol EqualSign = Intern("=");
        /// <summary>
        /// The Prolog symbol '|'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol VerticalBar = Intern("|");
        /// <summary>
        /// The Prolog symbol ':-'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Implication = Intern(":-");
        /// <summary>
        /// The Prolog symbol '.'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol PrologListConstructor = Intern("cons");
        /// <summary>
        /// The Prolog symbol '.'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Dot = Intern(".");
        /// <summary>
        /// The Prolog symbol '!'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Cut = Intern("!");

        /// <summary>
        /// The Prolog symbol 'call'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Call = Intern("call");

        /// <summary>
        /// The Prolog symbol 'fail'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Fail = Intern("fail");

        /// <summary>
        /// The Prolog symbol 'true'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol True = Intern("true");

        /// <summary>
        /// The Prolog symbol '{}'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol CurlyBrackets = Intern("{}");

        /// <summary>
        /// The Prolog symbol '-->'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol GrammarRule = Intern("-->");

        /// <summary>
        /// The Prolog symbol '$'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol DollarSign = Intern("$");

        /// <summary>
        /// The Prolog symbol 'end_of_file'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol EndOfFile = Intern("end_of_file");

        /// <summary>
        /// The division symbol, '/'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Slash = Intern("/");
        /// <summary>
        /// The grammar rule predicate indicator symbol, '//'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol SlashSlash = Intern("//");
        /// <summary>
        /// The symbol 'maplist'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol MapList = Intern("maplist");
        /// <summary>
        /// The Prolog symbol ';'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Semicolon = Intern(";");
        /// <summary>
        /// The Prolog symbol '->'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol IfThenArrow = Intern("->");
        /// <summary>
        /// The Prolog symbol ':'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Colon = Intern(":");
        /// <summary>
        /// The Prolog symbol 'global'
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Symbol Global = Intern("global");

        /// <summary>
        /// The Prolog symbol '-'
        /// </summary>
        public static readonly Symbol Minus = Intern("-");
        /// <summary>
        /// The Prolog symbol '::'
        /// </summary>
        public static readonly Symbol ColonColon = Intern("::");

        #endregion
    }
}
