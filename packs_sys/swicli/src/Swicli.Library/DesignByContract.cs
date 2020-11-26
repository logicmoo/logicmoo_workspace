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
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;
// Exception implementation 

// SecurityPermissionAttribute for GetObjectData

namespace SbsSW.DesignByContract
{
    /// <summary>
    /// Design By Contract Checks.
    /// 
    /// Each method generates an exception or
    /// a trace assertion statement if the contract is broken.
    /// </summary>
    /// <remarks>
    /// This example shows how to call the Require method.
    /// <code>
    /// public void Test(int x)
    /// {
    /// 	try
    /// 	{
    ///			Check.Require(x > 1, "x must be > 1");
    ///		}
    ///		catch (System.Exception ex)
    ///		{
    ///			PrologCLR.ConsoleTrace(ex.ToString());
    ///		}
    ///	}
    /// </code>
    ///
    /// You can direct output to a Trace listener. For example, you could insert
    /// <code>
    /// Trace.Cogbot.Clear();
    /// Trace.Cogbot.Add(new TextWriterTraceListener(Console.Out));
    /// </code>
    /// 
    /// or direct output to a file or the Event Log.
    /// 
    /// (Note: For ASP.NET clients use the Cogbot collection
    /// of the Debug, not the Trace, object and, for a Release build, only exception-handling
    /// is possible.)
    /// </remarks>
    /// 
    public sealed class Check
    {
        #region Interface

        /// <summary>
        /// Precondition check - should run regardless of preprocessor directives.
        /// </summary>
        public static void Require(bool assertion, string message) {
            if (UseExceptions) {
                if (!assertion) 
					throw new PreconditionException(message);
            }
            else {
                Trace.Assert(assertion, "Precondition: " + message);
            }
        }

        /// <summary>
        /// Precondition check - should run regardless of preprocessor directives.
        /// </summary>
        public static void Require(bool assertion, string message, Exception inner) {
            if (UseExceptions) {
                if (!assertion) throw new PreconditionException(message, inner);
            }
            else {
                Trace.Assert(assertion, "Precondition: " + message);
            }
        }

        /// <summary>
        /// Precondition check - should run regardless of preprocessor directives.
        /// </summary>
        public static void Require(bool assertion) {
            if (UseExceptions) {
                if (!assertion) throw new PreconditionException("Precondition failed.");
            }
            else {
                Trace.Assert(assertion, "Precondition failed.");
            }
        }

        /// <summary>
        /// Postcondition check.
        /// </summary>
        public static void Ensure(bool assertion, string message) {
            if (UseExceptions) {
                if (!assertion) throw new PostConditionException(message);
            }
            else {
                Trace.Assert(assertion, "Postcondition: " + message);
            }
        }

        /// <summary>
        /// Postcondition check.
        /// </summary>
        public static void Ensure(bool assertion, string message, Exception inner) {
            if (UseExceptions) {
                if (!assertion) throw new PostConditionException(message, inner);
            }
            else {
                Trace.Assert(assertion, "Postcondition: " + message);
            }
        }

        /// <summary>
        /// Postcondition check.
        /// </summary>
        public static void Ensure(bool assertion) {
            if (UseExceptions) {
                if (!assertion) throw new PostConditionException("Postcondition failed.");
            }
            else {
                Trace.Assert(assertion, "Postcondition failed.");
            }
        }

        /// <summary>
        /// Invariant check.
        /// </summary>
        public static void Invariant(bool assertion, string message) {
            if (UseExceptions) {
                if (!assertion) throw new InvariantException(message);
            }
            else {
                Trace.Assert(assertion, "Invariant: " + message);
            }
        }

        /// <summary>
        /// Invariant check.
        /// </summary>
        public static void Invariant(bool assertion, string message, Exception inner) {
            if (UseExceptions) {
                if (!assertion) throw new InvariantException(message, inner);
            }
            else {
                Trace.Assert(assertion, "Invariant: " + message);
            }
        }

        /// <summary>
        /// Invariant check.
        /// </summary>
        public static void Invariant(bool assertion) {
            if (UseExceptions) {
                if (!assertion) throw new InvariantException("Invariant failed.");
            }
            else {
                Trace.Assert(assertion, "Invariant failed.");
            }
        }

        /// <summary>
        /// Assertion check.
        /// </summary>
        public static void Assert(bool assertion, string message) {
            if (UseExceptions) {
                if (!assertion) throw new AssertionException(message);
            }
            else {
                Trace.Assert(assertion, "Assertion: " + message);
            }
        }

        /// <summary>
        /// Assertion check.
        /// </summary>
        public static void Assert(bool assertion, string message, Exception inner) {
            if (UseExceptions) {
                if (!assertion) throw new AssertionException(message, inner);
            }
            else {
                Trace.Assert(assertion, "Assertion: " + message);
            }
        }

        /// <summary>
        /// Assertion check.
        /// </summary>
        public static void Assert(bool assertion) {
            if (UseExceptions) {
                if (!assertion) throw new AssertionException("Assertion failed.");
            }
            else {
                Trace.Assert(assertion, "Assertion failed.");
            }
        }

        /// <summary>
        /// Set this if you wish to use Trace Assert statements 
        /// instead of exception handling. 
        /// (The Check class uses exception handling by default.)
        /// </summary>
        public static bool UseAssertions {
            get {
                return useAssertions;
            }
            set {
                useAssertions = value;
            }
        }

        #endregion // Interface

        #region Implementation

        // No creation
        private Check() { }

        /// <summary>
        /// Is exception handling being used?
        /// </summary>
        private static bool UseExceptions {
            get {
                return !useAssertions;
            }
        }

        // Are trace assertion statements being used? 
        // Default is to use exception handling.
        private static bool useAssertions; // init by default with = false;

        #endregion // Implementation

    } // End Check

    #region Exceptions

    /// <summary>
    /// Exception raised when a contract is broken.
    /// Catch this exception type if you wish to differentiate between 
    /// any DesignByContract exception and other runtime exceptions.
    ///  
    /// </summary>
    [Serializable] 
    public class DesignByContractException : Exception
    {
        /// <inheritdoc />
        public DesignByContractException() { }
        /// <inheritdoc />
        public DesignByContractException(string message) : base(message) { }
        /// <inheritdoc />
        public DesignByContractException(string message, Exception inner) : base(message, inner) { }

		#region implementation of ISerializable

		// ISerializable Constructor
        /// <inheritdoc />
        protected DesignByContractException(SerializationInfo info, StreamingContext context)
			: base(info, context)
		{
			if (info == null)
				throw new ArgumentNullException("info");
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

    /// <summary>
    /// Exception raised when a precondition fails.
    /// </summary>
    [Serializable]
    public class PreconditionException : DesignByContractException
    {
        /// <summary>
        /// Precondition Exception.
        /// </summary>
        public PreconditionException() { }
        /// <summary>
        /// Precondition Exception.
        /// </summary>
        public PreconditionException(string message) : base(message) { }
        /// <summary>
        /// Precondition Exception.
        /// </summary>
        public PreconditionException(string message, Exception inner) : base(message, inner) { }

		#region implementation of ISerializable
        /// <inheritdoc />
        protected PreconditionException(SerializationInfo info, StreamingContext context) : base(info, context) { }

        /// <inheritdoc />
        [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			base.GetObjectData(info, context);
		}
		#endregion implementation of ISerializable
    }

    /// <summary>
    /// Exception raised when a postcondition fails.
    /// </summary>
    [Serializable]
    public class PostConditionException : DesignByContractException
    {
        /// <summary>
        /// Postcondition Exception.
        /// </summary>
        public PostConditionException() { }
        /// <summary>
        /// Postcondition Exception.
        /// </summary>
        public PostConditionException(string message) : base(message) { }
        /// <summary>
        /// Postcondition Exception.
        /// </summary>
        public PostConditionException(string message, Exception inner) : base(message, inner) { }
		
        #region implementation of ISerializable
        /// <inheritdoc />
        protected PostConditionException(SerializationInfo info, StreamingContext context) : base(info, context) { }

        /// <inheritdoc />
        [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			base.GetObjectData(info, context);
		}
		#endregion implementation of ISerializable
    }

    /// <summary>
    /// Exception raised when an invariant fails.
    /// </summary>
    [Serializable]
    public class InvariantException : DesignByContractException
    {
        /// <summary>
        /// Invariant Exception.
        /// </summary>
        public InvariantException() { }
        /// <summary>
        /// Invariant Exception.
        /// </summary>
        public InvariantException(string message) : base(message) { }
        /// <summary>
        /// Invariant Exception.
        /// </summary>
        public InvariantException(string message, Exception inner) : base(message, inner) { }

        #region implementation of ISerializable
        /// <inheritdoc />
        protected InvariantException(SerializationInfo info, StreamingContext context) : base(info, context) { }

        /// <inheritdoc />
        [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			base.GetObjectData(info, context);
		}
		#endregion implementation of ISerializable
    }

    /// <summary>
    /// Exception raised when an assertion fails.
    /// </summary>
    [Serializable]
    public class AssertionException : DesignByContractException
    {
        /// <summary>
        /// Assertion Exception.
        /// </summary>
        public AssertionException() { }
        /// <summary>
        /// Assertion Exception.
        /// </summary>
        public AssertionException(string message) : base(message) { }
        /// <summary>
        /// Assertion Exception.
        /// </summary>
        public AssertionException(string message, Exception inner) : base(message, inner) { }

        #region implementation of ISerializable
        /// <inheritdoc />
        protected AssertionException(SerializationInfo info, StreamingContext context) : base(info, context) { }

        /// <inheritdoc />
        [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			base.GetObjectData(info, context);
		}
		#endregion implementation of ISerializable
    }

    #endregion // Exception classes
}
