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
using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// Extension methods and other utilities for working with delegates
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Utils")]
    public static class DelegateUtils
    {
        /// <summary>
        /// Assigns d the printed name s
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "s"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "d")]
        public static void NameProcedure(Delegate d, string s)
        {
            NamedProcedureTable[d] = s;
        }

        /// <summary>
        /// Name of procedure for debugging purposes
        /// </summary>
        /// <param name="procedure"></param>
        /// <returns></returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "d")]
        public static string DelegateName(Delegate procedure)
        {
            if (NamedProcedureTable.ContainsKey(procedure))
                return NamedProcedureTable[procedure];
            return procedure.Method.Name;
        }

        /// <summary>
        /// The arguments of the specified procedure.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "d"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Arglist")]
        public static IList<object> Arglist(this Delegate d)
        {
            return Arglists[d];
        }

        /// <summary>
        /// The documentation for the specified procedure.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "d")]
        public static string Documentation(this Delegate d)
        {
            return Docstrings[d];
        }

        internal static Dictionary<Delegate, IList<object>> Arglists = new Dictionary<Delegate, IList<object>>();
        internal static Dictionary<Delegate, string> Docstrings = new Dictionary<Delegate, string>();
        internal static Dictionary<Delegate, string> NamedProcedureTable = new Dictionary<Delegate, string>();
    }
}
