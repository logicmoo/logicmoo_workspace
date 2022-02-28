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
using System.Reflection;
namespace Prolog
{
    /// <summary>
    /// Describes in which versions of the manual this item should be included.
    /// </summary>
    [Flags]
    public enum DocumentationTypes
    {
        /// <summary>
        /// LISP version of the manual
        /// </summary>
        Lisp = 1,
        /// <summary>
        /// C# version of the manual
        /// </summary>
        // ReSharper disable once InconsistentNaming
        CS = 2,
        /// <summary>
        /// Both versions of the manual
        /// </summary>
        All = 3
    };

    /// <summary>
    /// Tags a class or member with a documentation string for use in
    /// online help and automatic generation of the manual.
    /// </summary>
    [AttributeUsage(AttributeTargets.All), Documentation("Attaches a textual description to the type or member, for incorporation into the Twig manual and use in the online documentation.")]
    public sealed class DocumentationAttribute : Attribute
    {
        /// <summary>
        /// English text describing the attached Type or member.
        /// </summary>
        public string Description { get; private set; }

        /// <summary>
        /// Specifies which version(s) the manual this item should be included in.
        /// </summary>
        public DocumentationTypes ManualVersion { get; private set; }

        /// <summary>
        /// Tag a class or member with a documentation string for use in
        /// online help and automatic generation of the manual.
        /// </summary>
        public DocumentationAttribute(string description, DocumentationTypes manualVersion)  // url is a positional parameter
        {
            Description = description;
            ManualVersion = manualVersion;
        }

        /// <summary>
        /// Tag a class or member with a documentation string for use in
        /// online help and automatic generation of the manual.
        /// </summary>
        public DocumentationAttribute(string description)
            : this(description, DocumentationTypes.All)
        { }
    }

    /// <summary>
    /// Adds extension methods for reading documentation annotations of Types and MemberInfo objects.
    /// </summary>
    public static class DocumentationExtensions
    {
        /// <summary>
        /// Get documentation string of member, if any.
        /// </summary>
        public static string GetDocumentation(this MemberInfo memberInfo)
        {
            if (memberInfo == null)
                throw new ArgumentNullException("memberInfo");

            object[] a = memberInfo.GetCustomAttributes(typeof(DocumentationAttribute), false);
            if (a.Length == 0)
                return null;
            return ((DocumentationAttribute)a[0]).Description;
        }
    }
}