// // --------------------------------------------------------------------------------------------------------------------
// // <copyright file="KBExclusionException.cs" company="Ian Horswill">
// //   Copyright (C) 2013 Ian Horswill
// //   
// //   Permission is hereby granted, free of charge, to any person obtaining a copy of
// //   this software and associated documentation files (the "Software"), to deal in the
// //   Software without restriction, including without limitation the rights to use, copy,
// //   modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
// //   and to permit persons to whom the Software is furnished to do so, subject to the
// //   following conditions:
// //   
// //   The above copyright notice and this permission notice shall be included in all
// //   copies or substantial portions of the Software.
// //   
// //   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// //   INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// //   PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// //   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// //   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// //   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// // </copyright>
// // --------------------------------------------------------------------------------------------------------------------
using System;

namespace Prolog
{
    /// <summary>
    /// Thrown when attempting to write a non-exclusive value to an exclusive value or vice-versa.
    /// </summary>
    [Serializable]
    public class ELNodeExclusionException : InvalidOperationException
    {
        /// <summary>
        /// Thrown when attempting to write a non-exclusive value to an exclusive value or vice-versa.
        /// </summary>
        public ELNodeExclusionException(string message, ELNode node, object key)
            : base(string.Format("{0}: {1}{2}{3}", 
                                 message, 
                                 node,
                                 node.IsExclusive?ELProlog.ExclusiveOperator:ELProlog.NonExclusiveOperator,
                                 ISOPrologWriter.WriteToString(key))) {}
    }
}
