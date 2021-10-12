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

using System;
using System.Text;

namespace Prolog
{
    /// <summary>
    /// Signals that a Lisp expression had incorrect syntax.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2237:MarkISerializableTypesWithSerializable")
    ,
     System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1032:ImplementStandardExceptionConstructors"
         )]
    public class SyntaxErrorException : Exception
    {
        /// <summary>
        /// Expression producing the error
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming",
            "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "sexp")]
        public object SExp { get; private set; }

        /// <summary>
        /// The TextReader from which this was read.
        /// </summary>
        public object TextReader { get; private set; }

        /// <summary>
        /// Represents an instance of a syntax error
        /// </summary>
        /// <param name="exp">The probelmatic expression</param>
        /// <param name="message">Descriptive message for user</param>
        public SyntaxErrorException(object exp, string message)
            : base(message)
        {
            SExp = exp;
            TextReader = null;
        }

        /// <summary>
        /// Represents an instance of a syntax error
        /// </summary>
        public SyntaxErrorException(object exp, string message, System.IO.TextReader reader)
            : base(AnnotateMessage(message, reader))
        {
            SExp = exp;
            TextReader = reader;
        }

        private static string AnnotateMessage(string mes, System.IO.TextReader reader)
        {
            var tracker = reader as PositionTrackingTextReader;
            if (tracker != null)
            {
                var sb = new StringBuilder();
                sb.Append(mes);
                sb.Append(" in line:        (at ");
                sb.Append(tracker.File);
                sb.Append(":");
                sb.Append(tracker.Line);
                sb.Append(")\n");

                sb.Append(tracker.ErrorLine);
                sb.Append('\n');
                for (int i = 0; i < tracker.Column-2; i++)
                    sb.Append(' ');
                sb.Append("^");
                return sb.ToString();
            }
            return mes;
        }
    }
}
