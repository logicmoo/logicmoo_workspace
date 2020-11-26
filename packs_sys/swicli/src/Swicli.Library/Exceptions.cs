/*  $Id$
*  
*  Project: Swicli.Library - Two Way Interface for .NET and MONO to SWI-Prolog
*  Author:        Douglas R. Miles
*                 Uwe Lesta (SbsSW.SwiPlCs classes)
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.com
*  Copyright (C): 2008, Uwe Lesta SBS-Softwaresysteme GmbH, 
*     2010-2012 LogicMOO Developement
*
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*********************************************************/

using System;
using System.Runtime.Serialization;
using System.Security.Permissions;
using SbsSW.DesignByContract;
using Swicli.Library;
// Exception implementation 
    // SecurityPermissionAttribute for GetObjectData

namespace SbsSW.SwiPlCs.Exceptions
{



    #region namspace documentation
    /// <summary>
    /// <para>These are the namespace comments for <b>SbsSW.SwiPlCs.Exceptions</b>.</para>
    /// <para>The namespace SbsSW.SwiPlCs.Exceptions provides the Exception classes to catch a prolog exception 
    /// see <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/exception.html">SWI-Prolog Manual - 4.9 ISO compliant Exception handling</see>
    /// </para>
    /// <para>Prolog exceptions are mapped to C# exceptions using the subclass PlException of <see cref="Exception"/>
    /// to represent the Prolog exception term.</para>
    /// <para>All type-conversion functions of the interface raise Prolog-compliant exceptions, 
    /// providing decent error-handling support at no extra work for the programmer.</para>
    /// <para>For some commonly used exceptions, subclasses of PlException have been created to exploit 
    /// both their constructors for easy creation of these exceptions as well as selective trapping in C#.</para>
    /// <para>Currently, these are <see cref="PlTypeException"/> and <see cref="PlDomainException"/>.</para>
    /// </summary>
    /// <remarks>
    /// </remarks>
    /// <todo>To throw an exception, create an instance of PlException and use throw() or cppThrow(). The latter refines the C# exception class according to the represented Prolog exception before calling throw(). 
    /// </todo>
    [System.Runtime.CompilerServices.CompilerGenerated()]
    class NamespaceDoc
    {
    }
    #endregion namspace documentation



	#region class PlLibException
	/// <summary>This exception is thrown if something in the interface went wrong.</summary>
    [Serializable]
	public class PlLibException : Exception, ISerializable
	{



        /// <inheritdoc />
        public PlLibException()
			: base()
		{
            BP();
		}

	    static private void BP()
	    {
            libpl.PL_open_foreign_frame();
            PrologCLR.BP();
            return;
	    }

	    /// <inheritdoc />
        public PlLibException(string message)
			: base(message)
		{
            BP();
        }
        /// <inheritdoc />
        public PlLibException(string message, Exception innerException)
			: base(message, innerException)
		{
            BP();

		}
		#region implementation of ISerializable

		// ISerializable Constructor
        /// <inheritdoc />
        protected PlLibException(SerializationInfo info, StreamingContext context)
			: base(info, context)
		{
            BP();
        }

		// see http://msdnwiki.microsoft.com/en-us/mtpswiki/f1d0010b-14fb-402f-974f-16318f0bc19f.aspx
        /// <inheritdoc />
        [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			if (info == null)
				throw new ArgumentNullException("info");
			base.GetObjectData(info, context);
		}
		#endregion implementation of ISerializable
	}
	#endregion class PlLibException


	#region class PlException
    /// <inheritdoc />
	/// <summary>
    /// <para>This class is the base class to catch exceptions thrown by prolog in C#.</para>
	/// </summary>
    /// <example>
    ///     <code source="..\..\TestSwiPl\PlException.cs" region="prolog_exception_sample_doc" />
    /// </example>
    /// <seealso cref="PlTypeException"/>
    /// <seealso href="http://gollem.science.uva.nl/SWI-Prolog/Manual/exception.html">SWI-Prolog Manual - 4.9 ISO compliant Exception handling</seealso>
    [Serializable]
	public class PlException : Exception, ISerializable
	{
        private string _messagePl;
        private PlTerm _exTerm;

        /// <summary>provide somtimes some additional information about the exceptions reason.</summary>
        public string MessagePl { get { return _messagePl; } }


        /// <inheritdoc />
        public PlException()
			: base()
		{
            EnsureExFrame();
            _exTerm = PlTerm.PlVar();
		}
        /// <inheritdoc />
        public PlException(string message)
			: base(message)
		{
            EnsureExFrame();
            _messagePl = message;
            _exTerm = PlTerm.PlAtom(message);
            //_exTerm = new PlTerm(message);
		}
        /// <inheritdoc />
        public PlException(string message, Exception innerException)
			: base(message, innerException)
		{
            EnsureExFrame();
            _messagePl = message + "; innerExeption:" + innerException.Message;
            _exTerm = PlTerm.PlAtom(message);
            //_exTerm = new PlTerm(message);
		}

		#region implementation of ISerializable

		// ISerializable Constructor
        /// <inheritdoc />
        protected PlException(SerializationInfo info, StreamingContext context)
			: base(info, context)
		{
            EnsureExFrame();
			if (info == null)
				throw new ArgumentNullException("info");
            _messagePl = (string)info.GetValue("_messagePl", typeof(string));
			_exTerm = (PlTerm)info.GetValue("_exTerm", typeof(PlTerm));
		}

		// see http://msdnwiki.microsoft.com/en-us/mtpswiki/f1d0010b-14fb-402f-974f-16318f0bc19f.aspx
        /// <inheritdoc />
        [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			if (info == null)
				throw new ArgumentNullException("info");
			base.GetObjectData(info, context);
            info.AddValue("_messagePl", _messagePl);
			info.AddValue("_exTerm", this._exTerm);
		}
		#endregion implementation of ISerializable


        /// <summary>
        /// <para>To catch a exception thrown by prolog</para>
        /// <para>For a example see <see cref="PlException"/>.</para>
        /// </summary>
        /// <param name="term">A PlTerm containing the Prolog exception</param>
        /// <see cref="PlException"/>
        public PlException(PlTerm term)
		{
            EnsureExFrame();
            Check.Require(term.TermRefIntern != 0);
            _exTerm = new PlTerm(term.TermRef);  // If this line is deleted -> update comment in PlTern(term_ref)
            _messagePl = "" + _exTerm;
		}

        private void EnsureExFrame()
        {
            libpl.PL_open_foreign_frame();
            PrologCLR.BP();
        }

        /// <summary>
        /// Get the <see cref="PlTerm"/> of this exception.
        /// </summary>
        public PlTerm Term { get { return _exTerm; } }

        /// <inheritdoc />
		public override string Message
		{
			get { return this.ToString(); }
		}

		//operator char *(void);
        /// <inheritdoc />
        /// <summary>
        /// The exception is translated into a message as produced by print_message/2. The character data is stored in a ring.
        /// </summary>
        /// <returns></returns>
		override public string ToString()
		{
            if (_messagePl != null) return GetType() + ": " + _messagePl;

            if (!PlEngine.IsInitialized)
                return "A PlException was thrown but it can't formatted because PlEngine is not Initialized.";

			string strRet = "[ERROR: Failed to generate message.  Internal error]\n";
            if (libpl.NoToString) return "[ERROR: Failed to generate message.  NoToString presently]\n";
            using (PlFrame fr = new PlFrame())
            {

#if USE_PRINT_MESSAGE
				PlTermV av = new PlTermV(2);

                av[0] = PlTerm.PlCompound("print_message", new PlTermV(new PlTerm("error"), new PlTerm( _exTerm.TermRef)));
				PlQuery q = new PlQuery("$write_on_string", av);
				if ( q.NextSolution() )
					strRet = (string)av[1];
				q.Free();
#else
                PlTermV av = new PlTermV(2);
                av[0] = new PlTerm(_exTerm.TermRef);
                PlQuery q = new PlQuery("$messages", "message_to_string", av);
                if (q.NextSolution())
                    strRet = av[1].ToString();
                //q.Free();
                return strRet;
                q.Dispose();
#endif
            }
			return strRet;
		}

        /// <summary>
        /// TODO
        /// </summary>
        /// <remarks>Used in the PREDICATE() wrapper to pass the exception to Prolog. See PL_raise_exeption().</remarks>
        /// <returns></returns>
		public int PlThrow()
		{
			return libpl.PL_raise_exception(_exTerm.TermRef);
		}

        /// <summary>
        /// TODO
        /// </summary>
        /// <remarks>see <see href="http://www.swi-prolog.org/packages/pl2cpp.html#cppThrow()"/></remarks>
		public void Throw()
		{
			// term_t
			uint a = libpl.PL_new_term_ref();
			// atom_t 
			uint name = 0;
			int arity = 0;

			if (0 != libpl.PL_get_arg(1, _exTerm.TermRef, a) && 0 != libpl.PL_get_name_arity(a, ref name, ref arity))
			{
				string str = libpl.PL_atom_chars(name);

				if (str == "type_error")
					throw new PlTypeException(_exTerm);
				else if (str == "domain_error")
					throw new PlDomainException(_exTerm);
			}
            this._messagePl = this.Message;
			throw this;
		}

	} // class PlException 
	#endregion


	#region class PlTypeException
    /// <inheritdoc />
    /// <summary>
    /// A type error expresses that a term does not satisfy the expected basic Prolog type.
    /// </summary>
    /// <example>
    /// This sample demonstrate how to catch a PlTypeException in C# that is thrown somewhere int the prolog code.
    ///     <code source="..\..\TestSwiPl\PlException.cs" region="prolog_type_exception_sample_doc" />
    /// </example>
	[Serializable]
	public class PlTypeException : PlException
	{
        /// <inheritdoc />
		public PlTypeException()
			: base()
		{
		}
        /// <inheritdoc />
        public PlTypeException(string message)
            : base(message) 
		{
		}
        /// <inheritdoc />
        public PlTypeException(string message, Exception innerException)
            : base(message, innerException)
        {
        }
        /// <inheritdoc />
        protected PlTypeException(SerializationInfo info, StreamingContext context)
			: base(info, context)
		{
		}


        /// <inheritdoc />
        public PlTypeException(PlTerm term)
			: base(term)
		{
		}

        /// <summary>
        /// Creates an ISO standard Prolog error term expressing the expected type and actual term that does not satisfy this type.
        /// </summary>
        /// <param name="expected">The type which was expected</param>
        /// <param name="actual">The actual term</param>
		public PlTypeException(string expected, PlTerm actual)
			: base(
			PlTerm.PlCompound("error",
            new PlTermV(PlTerm.PlCompound("type_error",
			new PlTermV(new PlTerm(expected), actual)),
			PlTerm.PlVar())
			))
		{
		}
	} // class PlTypeException
	#endregion class PlTypeException


	#region class PlDomainException
    /// <summary>
    /// A domain exception expresses that a term satisfies the basic Prolog type expected, but is unacceptable
    /// to the restricted domain expected by some operation. 
    /// </summary>
    /// <example>
    /// For example, the standard Prolog open/3 call expect an IO-Mode (read, write, append, ...).
    /// If an integer is provided, this is a type error, if an atom other than one of the defined IO-modes is provided it is a domain error.
    ///     <code source="..\..\TestSwiPl\PlException.cs" region="prolog_domain_exception_sample_doc" />
    /// </example>
	[Serializable]
    internal class PlDomainException : PlException
	{

        /// <inheritdoc cref="PlException" />
        public PlDomainException()
			: base()
		{ }
		/*
		public PlDomainException(string message)
			: base(message)
		{
		}
		public PlDomainException(string message, Exception innerException)
			: base(message, innerException)
		{
		}
         */
        /// <inheritdoc cref="PlException" />
        protected PlDomainException(SerializationInfo info, StreamingContext context)
			: base(info, context)
		{
		}

        /// <inheritdoc cref="PlException" />
        public PlDomainException(PlTerm t)
			: base(t)
		{ }
		/*
		PlDomainException(string expected, PlTerm actual)
			:
			base(new PlCompound("error",
			new PlTermV(new PlCompound("domain_error",
			new PlTermV(new PlTerm(expected), actual)),
			PlTerm.PlVar())
			)
			)
		{ }
		 */
	}
	#endregion class PlDomainException


} // namespace SbsSW.SwiPlCs
