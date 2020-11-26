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

using System.Windows.Forms;
#if USE_IKVM
using Type = System.Type;
#else
using JClass = System.Type;
using Type = System.Type;
#endif
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;
using SbsSW.DesignByContract;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Streams;
using Swicli.Library;
/********************************
*	       TYPES	Comment     *
********************************/
#region used type from SWI-Prolog.h

/*
<!-- 
#ifdef _PL_INCLUDE_H
	typedef module		module_t;	// atom module 
	typedef Procedure	predicate_t;// atom predicate handle
	typedef Record		record_t;	// handle to atom recorded term
#else
typedef	unsigned long	atom_t;		// Prolog atom
typedef void *		    module_t;		// Prolog module
typedef void *		    predicate_t;	// Prolog procedure
typedef void *		    record_t;		// Prolog recorded term
typedef unsigned long	term_t;		// opaque term handle
typedef unsigned long	qid_t;		// opaque query handle
typedef unsigned long	PL_fid_t;	// opaque foreign context handle
#endif
typedef unsigned long	functor_t;	// Name/Arity pair
typedef unsigned long	atomic_t;	// same atom word
typedef unsigned long	control_t;	// non-deterministic control arg
typedef void *		    PL_engine_t;	// opaque engine handle 
typedef unsigned long	foreign_t;	// return type of foreign functions

unsigned long	-  uint
-->
*/

#endregion

// The namespace summary is above class NamespaceDoc
namespace SbsSW.SwiPlCs.Callback
{

    #region namspace documentation
    /// <summary>
    /// <para>The namespace SbsSW.SwiPlCs.Callback provides the delegates to register .NET methods to be 
    /// called from <a href='http://www.swi-prolog.org' >SWI-Prolog</a>
    /// </para>
    /// </summary>
    /// <remarks>
    /// <note>It is only possible to call <see langword="static"/> methods</note>
    /// </remarks>
    /// <seealso cref="SbsSW.SwiPlCs.PlEngine.RegisterForeign(string, System.Delegate)"/>
    [System.Runtime.CompilerServices.CompilerGenerated()]
    class NamespaceDoc
    {
    }
    #endregion namspace documentation


    #region enum PlForeignSwitches
    /// <summary>
    /// Flags that are responsible for the foreign predicate parameters 
    /// </summary>
    [Flags]
    public enum PlForeignSwitches:uint 
    {
        /// <summary>0 - PL_FA_NOTHING: no flags. </summary>
        None = 0,
        /// <summary>1 - PL_FA_NOTRACE: Predicate cannot be seen in the tracer. </summary>
        NoTrace = 1,
        /// <summary>2 - PL_FA_TRANSPARENT: Predicate is module transparent.</summary>
        Transparent = 2,
        /// <summary>4 - PL_FA_NONDETERMINISTIC: Predicate is non-deterministic. See also PL_retry().</summary>
        /// <seealso href="http://gollem.science.uva.nl/SWI-Prolog/Manual/foreigninclude.html#PL_retry()">PL_retry()</seealso>
        Nondeterministic = 4,
        /// <summary>8 - PL_FA_VARARGS: (Default) Use alternative calling convention.   call using t0, ac, ctx </summary>
        VarArgs = 8,
        /// <summary>16 - PL_FA_CREF:  /* call using t0, ac, ctx */.</summary>
        CRef = 16,
        /// <summary>
        /// /* Internal: ISO core predicate */
        /// </summary>
        ISO = 32,
    }
    #endregion enum PlForeignSwitches


    #region delagates for C# callbacks

    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate Int32 PL_agc_hook_t(uint t_atom);

    /// <summary>
    /// PL_EXPORT(void) PL_on_halt(void (*)(int, void *), void *);
    /// </summary>
    /// <param name="t_atom"></param>
    /// <returns></returns>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public unsafe delegate void SwiOnHalt(int i, void* closureObj);

    /// <summary>
    /// typedef void (*PL_abort_hook_t)(void);
    /// </summary>
    /// <param name="t_atom"></param>
    /// <returns></returns>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public unsafe delegate void PL_abort_hook_t();


    /// <inheritdoc cref="DelegateParameter2" />
    /// 
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate bool DelegateParameter0();


    /// <inheritdoc cref="DelegateParameter2" />
    /// <example>
    /// <para>See also the example in <see cref="DelegateParameter2" />.</para>
    ///     <code source="..\..\TestSwiPl\CallbackForeigenPredicate.cs" region="t_creating_a_list_doc" />
    /// </example>
    /// <param name="term"></param>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate bool DelegateParameter1(PlTerm term);


    /// <summary>
    /// <para>Provide a predefined Delegate to register a C# method to be called from SWI-Prolog</para>
    /// </summary>
    /// <example>
    /// <para>This example is for <see cref="DelegateParameter2" /> and shows how o call a C# method with two parameter.</para>
    /// <para>For other samples see the source file CallbackForeigenPredicate.cs in the TestSwiPl VS2008 test project.</para>
    ///     <code source="..\..\TestSwiPl\CallbackForeigenPredicate.cs" region="t_in_out_doc" />
    /// </example>
    /// <param name="term1"></param>
    /// <param name="term2"></param>
    /// <returns>true for succeeding otherwise false for fail</returns>
    /// <seealso cref="M:SbsSW.SwiPlCs.PlEngine.RegisterForeign(System.Delegate)"/>
    /// <seealso cref="M:SbsSW.SwiPlCs.PlEngine"/>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate bool DelegateParameter2(PlTerm term1, PlTerm term2);

    /// <inheritdoc cref="DelegateParameter2" />
    /// <param name="term1"></param>
    /// <param name="term2"></param>
    /// <param name="term3"></param>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate bool DelegateParameter3(PlTerm term1, PlTerm term2, PlTerm term3);

    /// <inheritdoc cref="DelegateParameter2" />
    /// <param name="term1"></param>
    /// <param name="term2"></param>
    /// <param name="term3"></param>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate bool DelegateParameter4(PlTerm term1, PlTerm term2, PlTerm term3, PlTerm term4);

    /// <inheritdoc cref="DelegateParameter2" />
    /// <param name="term1"></param>
    /// <param name="term2"></param>
    /// <param name="term3"></param>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate bool DelegateParameter5(PlTerm term1, PlTerm term2, PlTerm term3, PlTerm term4, PlTerm term5);
    /// <summary>
    /// <para>With this delegate you can build a call-back predicate with a variable amount of parameters.</para>
    /// </summary>
    /// <example>
    ///     <code source="..\..\TestSwiPl\CallbackForeigenPredicate.cs" region="t_varargs_doc" />
    /// </example>
    /// <param name="termVector">The termVector representing the arguments which can be accessed by the 
    /// indexer of PlTermV see <see cref="PlTermV"/>. The amount of parameters is in <see cref="PlTermV.Size"/>
    /// </param>
    /// <returns>True for succeeding otherwise false for fail</returns>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate bool DelegateParameterVarArgs(PlTermV termVector);


    /// <summary>
    /// <para><b>NOT IMPLEMENTED YET</b></para>
    /// <para>For details to implement see <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/foreigninclude.html#PL_register_foreign_in_module()">9.6.17 Registering Foreign Predicates</see></para>
    /// see also <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/foreigninclude.html#PL_foreign_control()">PL_foreign_control</see>
    /// </summary>
    /// <param name="term1">TODO</param>
    /// <param name="term2">TODO</param>
    /// <param name="control">TODO</param>
    /// <returns>TODO</returns>
    /// <example>TODO
    /// <para>See "t_backtrack" in TestSwiPl.CallbackForeigenPredicate.cs</para>
    /// </example>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate int DelegateParameterBacktrack1(PlTerm term1, IntPtr control);

    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate int DelegateParameterBacktrack2(PlTerm term1, PlTerm term2, IntPtr control);

    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate int DelegateParameterBacktrackVarArgs(PlTerm term1, int arity, IntPtr control);

    #endregion delagates for C# callbacks

} // namespace SbsSW.SwiPlCs.Callback


namespace SbsSW.SwiPlCs.Streams
{

    #region namspace documentation
    /// <summary>
    /// <para>The namespace SbsSW.SwiPlCs.Streams provides the delegates to redirect the read 
    /// and write functions of the <a href='http://www.swi-prolog.org' >SWI-Prolog</a> IO Streams.</para>
    /// <para>When <see cref="PlEngine.Initialize"/> is called the *Sinput->functions.read is 
    /// replaced by the .NET method 'Sread_function' and *Sinput->functions.write by 'Swrite_funktion'.</para>
    /// <para>For further examples see the methods <see cref="PlEngine.SetStreamFunctionWrite"/> and <see cref="PlEngine.SetStreamFunctionRead"/></para>
    /// </summary>
    /// <remarks>
    /// <note>The reason for this is debugging.</note>
    /// </remarks>
    /// <example>
    /// <code source="..\swi-cs.cs" region="default_io_doc" />
    /// </example>
    /// <seealso cref="PlEngine.SetStreamFunctionRead"/>
    [System.Runtime.CompilerServices.CompilerGenerated()]
    class NamespaceDoc
    {
    }
    #endregion namspace documentation



    /// <summary>
    /// The standard SWI-Prolog streams ( input output error )
    /// </summary>
    public enum PlStreamType
    {
        /// <summary>0 - The standard input stream.</summary>
        Input = 0,
        /// <summary>1 - The standard input stream.</summary>
        Output = 1,
        /// <summary>1 - The standard error stream.</summary>
        Error = 2
    }


    /// <summary>
    /// See <see cref="PlEngine.SetStreamFunctionRead"/>
    /// </summary>
    /// <param name="handle">A C stream handle. simple ignore it.</param>
    /// <param name="buffer">A pointer to a string buffer</param>
    /// <param name="bufferSize">The size of the string buffer</param>
    /// <returns>A <see cref="System.Delegate"/></returns>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate long DelegateStreamReadFunction(IntPtr handle, System.IntPtr buffer, long bufferSize);

    /// <summary>
    /// See <see cref="PlEngine.SetStreamFunctionWrite"/>
    /// </summary>
    /// <param name="handle">A C stream handle. simple ignore it.</param>
    /// <param name="buffer">A pointer to a string buffer</param>
    /// <param name="bufferSize">The size of the string buffer</param>
    /// <returns>A <see cref="System.Delegate"/></returns>
    [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    public delegate long DelegateStreamWriteFunction(IntPtr handle, string buffer, long bufferSize);

    /*
    <!--
    [System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    private delegate long Sseek_function(IntPtr handle, long pos, int whence);
    [System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    private delegate Int64 Sseek64_function(IntPtr handle, Int64 pos, int whence);
    [System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    private delegate int Sclose_function(IntPtr handle);
    [System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute(System.Runtime.InteropServices.CallingConvention.Cdecl)]
    private delegate int Scontrol_function(IntPtr handle, int action, IntPtr arg);


    [StructLayout(LayoutKind.Sequential, Pack = 8)]
    public struct MIOFUNCTIONS
    {
        Sread_function read;		//* fill the buffer
        DelegateStreamWriteFunction write;		//* empty the buffer 
        Sseek_function seek;		//* seek to position 
        Sclose_function close;		//* close stream 
        Scontrol_function control;	//* Info/control 
        Sseek64_function seek64;		//* seek to position (intptr_t files) 
    };
    
     -->
     */

}


// The namespace summary is above class NamespaceDoc
namespace SbsSW.SwiPlCs
{

    #region namspace documentation
    /// <summary>
    /// <para>The online documentation home is <a href='http://www.lesta.de/prolog/swiplcs/Generated/Index.aspx'>here</a>.</para>
    /// <para>This namespace SbsSW.SwiPlCs provides an .NET interface to <a href='http://www.swi-prolog.org' >SWI-Prolog</a></para>
    /// 
    /// 
    /// <para><h4>Overview</h4></para>
    /// <para>Prolog variables are dynamically typed and all information is passed around using the 
    /// C-interface type term_t witch is an int. In C#, term_t is embedded in the lightweight struct <see cref="PlTerm"/>. 
    /// Constructors and operator definitions provide flexible operations and integration with important C#-types (<c>string, int and double</c>).
    /// </para>
    /// 
    /// <para>The list below summarises the important classes / struct defined in the C# interface.</para>
    /// <list type="table">  
    /// <listheader><term>class / struct</term><description>Short description </description></listheader>  
    /// <item><term><see cref="PlEngine"/></term><description>A static class represents the prolog engine.</description></item>  
    /// <item><term><see cref="PlTerm"/></term><description>A struct representing prolog data.</description></item>  
    /// <item><term><see cref="PlTermV"/></term><description>A vector of <see cref="PlTerm"/>.</description></item>  
    /// <item><term><see cref="PlQuery"/></term><description>A class to query Prolog.</description></item>  
    /// </list>   
    /// </summary>
    /// 
    /// <example>
    /// <para>Before going into a detailed description of the CSharp classes let me present a few examples 
    /// illustrating the `feel' of the interface. The Assert class in the sample is from the test framework 
    /// and has nothing to do with the interface. It shows only which return values are expected.
    /// </para>
    /// <h4>Creating terms</h4>
    /// <para>This very simple example shows the basic creation of a Prolog term and how a Prolog term is converted to C#-data:</para>
    /// <code>
    ///     PlTerm t1 = new PlTerm("x(A)"); 
    ///     PlTerm t2 = new PlTerm("x(1)"); 
    ///     Assert.IsTrue(t1.Unify(t2));
    ///     Assert.AreEqual("x(1)", t1.ToString());
    /// </code>
    /// 
    /// <h4>Calling Prolog</h4>
    /// <para>This example shows how to make a simple call to prolog.</para>
    /// <code>
    ///     PlTerm l1 = new PlTerm("[a,b,c,d]");
    ///     Assert.IsTrue(PlQuery.PlCall("is_list", l1));
    /// </code>
    /// 
    /// <h4>Getting the solutions of a query</h4>
    /// <para>This example shows how to obtain all solutions of a prolog query.</para>
    /// <para><see cref="PlQuery"/> takes the name of a predicate and the goal-argument vector as arguments.
    /// From this information it deduces the arity and locates the predicate. the member-function 
    /// NextSolution() yields true if there was a solution and false otherwise. 
    /// If the goal yielded a Prolog exception it is mapped into a C# exception.</para>
    /// <code>
    ///     PlQuery q = new PlQuery("member", new PlTermV(new PlTerm("A"), new PlTerm("[a,b,c]")));
    ///     while (q.NextSolution())
    ///         PrologCLR.ConsoleTrace(s[0].ToString());
    /// </code>
    /// <para>There is an other constructor of <see cref="PlQuery"/> which simplify the sample above.</para>
    /// <code>
    ///     PlQuery q = new PlQuery("member(A, [a,b,c])");
    ///     foreach (PlTermV s in q.Solutions)
    ///         PrologCLR.ConsoleTrace(s[0].ToString());
    /// </code>
    /// <para>An other way to get the results is to use <see cref="PlQuery.SolutionVariables"/> to iterate over <see cref="PlQueryVariables"/>.</para>
    /// <code>
    ///     PlQuery q = new PlQuery("member(A, [a,b,c])");
    ///     foreach (PlQueryVariables vars in q.SolutionVariables)
    ///         PrologCLR.ConsoleTrace(vars["A"].ToString());
    /// </code>
    /// <para>It is also possible to get all solutions in a list by <see cref="PlQuery.ToList()"/>. 
    /// This could be used to work with LinQ to objects which is really nice. <see cref="PlQuery"/> and <see cref="PlQuery.ToList()"/> for further samples.</para>
    /// <code>
    ///     var results = from n in new PlQuery("member(A, [a,b,c])").ToList() select new {A = n["A"].ToString()};
    ///     foreach (var s in results)
    ///         PrologCLR.ConsoleTrace(s.A);
    /// </code>
    /// </example>
    [System.Runtime.CompilerServices.CompilerGenerated()]
    class NamespaceDoc
    {
    }
    #endregion namspace documentation

    
    /// <summary>
    /// Obtain the type of a term, which should be a term returned by one of the other 
    /// interface predicates or passed as an argument. The function returns the type of 
    /// the Prolog term. The type identifiers are listed below. 
    /// </summary>
    /// <remarks>see <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/foreigninclude.html#PL_term_type()">PL_term_type(term_t)</see> in the SWI-Prolog Manual.</remarks>
    /// <seealso cref="P:SbsSW.SwiPlCs.PlTerm.PlType"/>
    /// <example>
    /// In this sample a Prolog variable is created in <see cref="PlTerm">PlTerm t</see> and the <see cref="P:SbsSW.SwiPlCs.PlTerm.PlType"/> 
    /// is checked by his integer representation and his name.
    /// <code>
    ///     PlTerm t = PlTerm.PlVar();
    ///     Assert.AreEqual(1, (int)t.PlType);
    ///     Assert.AreEqual(PlType.PlVariable, t.PlType);
    /// </code>
    /// </example>
    public enum PlType
    {
        /// <summary>0 - PL_UNKNOWN: Undefined </summary>
        PlUnknown = 0,
        /// <summary>1 - PL_VARIABLE: An unbound variable. The value of term as such is a unique identifier for the variable.</summary>
        PlVariable = 1,
        /// <summary>2 - PL_ATOM: A Prolog atom.</summary>
        PlAtom = 2,
        /// <summary>3 - PL_INTEGER: A Prolog integer.</summary>
        PlInteger = 3,
        /// <summary>4 - PL_FLOAT: A Prolog floating point number.</summary>
        PlFloat = 4,
        /// <summary>5 - PL_STRING: A Prolog string.</summary>
        PlString = 5,
        /// <summary>6 - PL_TERM: A compound term. Note that a list is a compound term ./2.</summary>
        PlTerm = 6,
        PlNil = (7),		/* The constant [] */
        PlBlob = (8),		/* non-atom blob */
        PlListPair = (9),	/* [_|_] term */

         	PL_VARIABLE	 =(1),		/* nothing */
  PL_ATOM		 =(2),		/* const char * */
  PL_INTEGER	 =(3),		/* int */
  PL_FLOAT	 =(4),		/* double */
  PL_STRING	 =(5),		/* const char * */
  PL_TERM		 =(6),

  PL_NIL		 =(7),		/* The constant [] */
  PL_BLOB		 =(8),		/* non-atom blob */
  PL_LIST_PAIR	 =(9),		/* [_|_] term */

        		/* PL_unify_term(), */
  PL_FUNCTOR	 =(10),		/* functor_t, arg ... */
  PL_LIST		 =(11),		/* length, arg ... */
  PL_CHARS	 =(12),		/* const char * */
  PL_POINTER	 =(13),		/* void * */
					/* PlArg::PlArg=(text, type), */
  PL_CODE_LIST	 =(14),		/* [ascii...] */
  PL_CHAR_LIST	 =(15),		/* [h,e,l,l,o] */
  PL_BOOL		 =(16),		/* PL_set_prolog_flag=(), */
  PL_FUNCTOR_CHARS =(17),		/* PL_unify_term=(), */
  _PL_PREDICATE_INDICATOR =(18),	/* predicate_t =(Procedure), */
  PL_SHORT	 =(19),		/* short */
  PL_INT		 =(20),		/* int */
  PL_LONG		 =(21),		/* long */
  PL_DOUBLE	 =(22),		/* double */
  PL_NCHARS	 =(23),		/* size_t, const char * */
  PL_UTF8_CHARS	 =(24),		/* const char * */
  PL_UTF8_STRING	 =(25),		/* const char * */
  PL_INT64	 =(26),		/* int64_t */
  PL_NUTF8_CHARS	 =(27),		/* size_t, const char * */
  PL_NUTF8_CODES	 =(29),		/* size_t, const char * */
  PL_NUTF8_STRING	 =(30),		/* size_t, const char * */
  PL_NWCHARS	 =(31),		/* size_t, const wchar_t * */
  PL_NWCODES	 =(32),		/* size_t, const wchar_t * */
  PL_NWSTRING	 =(33),		/* size_t, const wchar_t * */
  PL_MBCHARS	 =(34),		/* const char * */
  PL_MBCODES	 =(35),		/* const char * */
  PL_MBSTRING	 =(36),		/* const char * */
  PL_INTPTR	 =(37),		/* intptr_t */
  PL_CHAR		 =(38),		/* int */
  PL_CODE		 =(39),		/* int */
  PL_BYTE		 =(40),		/* int */
					/* PL_skip_list=(), */
  PL_PARTIAL_LIST	 =(41),		/* a partial list */
  PL_CYCLIC_TERM	 =(42),		/* a cyclic list/term */
  PL_NOT_A_LIST	 =(43),		/* Object is not a list */
					/* dicts */
  PL_DICT		 =(44),

    }


    /*
    /// <summary>
    /// Extends PlTerm
    /// </summary>
    public static class PlTermList 
    {
        /// <summary>
        /// extension "doit"
        /// </summary>
        /// <param name="plTerm">how ?</param>
        public static void DoIt(this PlTerm plTerm)
        {
            PrologCLR.ConsoleTrace(plTerm.ToString());
        }
    }
    */

    public class PlArrayEnumerator: IEnumerator<PlTerm>
    {
        PlTerm orig;
        int index = 0;
        public PlArrayEnumerator(PlTerm compound, int startElement)
        {
            orig = compound;
            index = startElement - 1;
        }

        #region IEnumerator<PlTerm> Members

        PlTerm IEnumerator<PlTerm>.Current
        {
            get { return orig[index]; }
        }

        #endregion

        #region IDisposable Members

        void IDisposable.Dispose()
        {
            //orig = null;
        }

        #endregion

        #region IEnumerator Members

        object IEnumerator.Current
        {
            get { return orig[index]; }
        }

        bool IEnumerator.MoveNext()
        {
            return (index++ < orig.Arity);
        }

        void IEnumerator.Reset()
        {
            index = 0;
        }

        #endregion
    }

    public class PlTermVEnumerator : IEnumerator<PlTerm>
    {
        PlTermV orig;
        int index = -1;
        public PlTermVEnumerator(PlTermV compound, int startElement)
        {
            orig = compound;
            index = startElement - 1;
        }

        #region IEnumerator<PlTerm> Members

        PlTerm IEnumerator<PlTerm>.Current
        {
            get
            {
                try
                {
                    return orig[index];
                }
                catch (IndexOutOfRangeException)
                {
                    throw new InvalidOperationException();
                }
            }
        }

        #endregion

        #region IDisposable Members

        void IDisposable.Dispose()
        {
            //orig = null;
        }

        #endregion

        #region IEnumerator Members

        object IEnumerator.Current
        {
            get
            {
                try
                {
                    return orig[index];
                }
                catch (IndexOutOfRangeException)
                {
                    throw new InvalidOperationException();
                } 
            }
        }

        bool IEnumerator.MoveNext()
        {
            return (++index < orig.Size);
        }

        void IEnumerator.Reset()
        {
            index = -1;
        }

        #endregion
    }
    public interface IndexableWithLength<T>
    {
        T this[int index] { get; }
        int Length { get; }
    }
    /********************************
    *     GENERIC PROLOG TERM		*
    ********************************/
    #region public struct PlTerm
    /// <summary>
    ///  <para>The PlTerm <see langword="struct"/> plays a central role in conversion and operating on Prolog data.</para>
    ///  <para>PlTerm implements <see cref="System.IComparable"/> to support ordering in <see cref="System.Linq"/> queries if PlTerm is a List.</para>
    ///  <para>Creating a PlTerm can be done by the <see href="Overload_SbsSW_SwiPlCs_PlTerm__ctor.htm">Constructors</see> or by the following static methods:</para>
    ///  <para>PlVar(), PlTail(), PlCompound, PlString(), PlCodeList(), PlCharList() (see remarks)</para>
    /// </summary>
    /// <remarks>
    /// <list type="table">  
    /// <listheader><term>static method</term><description>Description </description></listheader>  
    /// <item><term><see cref="PlVar()"/></term><description>Creates a new initialised term (holding a Prolog variable).</description></item>  
    /// <item><term><see cref="PlTail(PlTerm)"/></term><description>PlTail is for analysing and constructing lists.</description></item>  
    /// <item><term><see href="Overload_SbsSW_SwiPlCs_PlTerm_PlCompound.htm">PlCompound(string)</see></term><description>Create compound terms. E.g. by parsing (as read/1) the given text.</description></item>  
    /// <item><term><see cref="PlString(string)"/></term><description>Create a SWI-Prolog string.</description></item>  
    /// <item><term><see cref="PlCodeList(string)"/></term><description>Create a Prolog list of ASCII codes from a 0-terminated C-string.</description></item>  
    /// <item><term><see cref="PlCharList(string)"/></term><description>Create a Prolog list of one-character atoms from a 0-terminated C-string.</description></item>  
    /// </list>   
    /// </remarks>
    public struct PlTerm : IComparable, IEnumerable<PlTerm>, IndexableWithLength<PlTerm>// TODO, IList<PlTerm> // LISTS
    {

        ///<summary>
        ///</summary>
        ///<param name="t"></param>
        ///<returns></returns>
        //public static explicit operator uint(PlTerm t)
       // {
         //   return t.TermRef;
       // }

        private uint _termRef; // term_t
        
        // Properties
        internal uint TermRef
        {
            get
            {
                //if (0 == _termRef) {
                //    _termRef = libpl.PL_new_term_ref();
                //}
                Check.Require(_termRef != 0, "use of an uninitialised plTerm. If you need a variable use PlTerm.PlVar() instead");
                return _termRef;
            }
            //set { _termRef = (uint)value; }
        }

        /// <summary>
        /// This one should be for checks only e.g.
        /// Check.Require(arg1.TermRefIntern != 0);
        /// </summary>
        internal uint TermRefIntern
        {
            get { return _termRef; }
        }

        #region implementing IComparable CompareTo

        ///<inheritdoc />
        public int CompareTo(object obj)
        {
            if (obj is PlTerm)
            {
                return libpl.PL_compare(this.TermRef, ((PlTerm)obj).TermRef);
            }
            else
            {
                throw new ArgumentException("object is not a PlTerm");
            }
        }

        #endregion

        /// <summary>
        /// <para>If PlTerm is compound and index is between 0 and Arity (including), the nth PlTerm is returned.</para>
        /// <para>If pos is 0 the functor of the term is returned (For a list '.').</para>
        /// <para>See: <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual//foreigninclude.html#PL_get_arg()">PL_get_arg/3</see></para>
        /// </summary>
        /// <param name="pos">To Get the nth PlTerm</param>
        /// <returns>a PlTerm</returns>
        /// <example>
        ///     <code source="..\..\TestSwiPl\PlTerm.cs" region="PlTerm_indexer_doc" />
        /// </example>
        /// <exception cref="NotSupportedException">Is thrown if PlTerm is not of Type PlCompound see <see cref="IsCompound"/></exception>
        /// <exception cref="ArgumentOutOfRangeException">Is thrown if (pos &lt;  0 || pos >= Arity)</exception>
        /// <exception cref="InvalidOperationException">Is thrown if PL_get_arg returns 0.</exception>
        public PlTerm this[int pos]
        {
            get
            {
                if(!this.IsCompound)
                    throw new NotSupportedException("Work only for compound terms!");

                if (pos < 0 || pos > this.Arity)
                    throw new ArgumentOutOfRangeException("pos", "Must be greater than 0 and lesser then the arity of the term " + "this=" + this + " pos=" + pos);

                if (0 == pos)
                {
                    if (this.IsList)
                        return PlTerm.PlAtom(PlTerm.LIST_FUNCTOR_NAME);
                    else
                        return PlTerm.PlAtom(this.Name);
                }
                else
                {
                    uint a = libpl.PL_new_term_ref();
                    if (0 != libpl.PL_get_arg(pos, this.TermRef, a))
                        return new PlTerm(a);
                    else
                        throw new InvalidOperationException("PlTerm indexer: PL_get_arg return 0");

                }
            }
            //set
            //{
            //    myData[pos] = value;
            //}
        }


        #region constructors


        // Creates a new initialised term (holding a Prolog variable). 
        //public PlTerm()
        //{
        //    _termRef = libpl.PL_new_term_ref();
        //}


        // NOTE : Be Careful you *can* delete this constructor or make it private 
        //        it compiles but the tests will fail
        /// <summary>
        /// Create a PlTerm but *no* new term_ref it only copies the term_ref into the new object
        /// Used Intern by
        /// - PlException constructor
        /// - PlQueryQ.GetSolutions()
        /// - PlTermV this[int index] indexer
        /// </summary>
        /// <param name="termRef"></param>
        internal PlTerm(uint termRef)// PlTerm(term_t termRef)
        {
            _termRef = termRef;
        }


        /// <overloads>
        /// <summary>
        /// A new PlTerm can be also created by the static methods:
        /// <list type="table">  
        /// <listheader><term>static method</term><description>Description </description></listheader>  
        /// <item><term><see cref="PlVar()"/></term><description>Creates a new initialised term (holding a Prolog variable).</description></item>  
        /// <item><term><see cref="PlTail(PlTerm)"/></term><description>PlTail is for analysing and constructing lists.</description></item>  
        /// <item><term><see href="Overload_SbsSW_SwiPlCs_PlTerm_PlCompound.htm">PlCompound(string)</see></term><description>Create compound terms. E.g. by parsing (as read/1) the given text.</description></item>  
        /// <item><term><see cref="PlString(string)"/></term><description>Create a SWI-Prolog string.</description></item>  
        /// <item><term><see cref="PlCodeList(string)"/></term><description>Create a Prolog list of ASCII codes from a 0-terminated C-string.</description></item>  
        /// <item><term><see cref="PlCharList(string)"/></term><description>Create a Prolog list of one-character atoms from a 0-terminated C-string.</description></item>  
        /// </list>   
        /// </summary>
        /// </overloads>
        /// <summary>
        /// Creates a term-references holding a Prolog term representing text.
        /// </summary>
        /// <param name="text">the text</param>
        public PlTerm(string text)
        {
            // old
            //_termRef = libpl.PL_new_term_ref();
            //if (0 == libpl.PL_chars_to_term(text, this.TermRef))
            //    throw new PlException("Term creation failed : " + text);

            // term_t 
            uint t = libpl.PL_new_term_ref();
            
            if (0 == libpl.PL_chars_to_term(text, t))
                throw new PlException("PlTerm creation failed : " + text);

            this._termRef = libpl.PL_new_term_ref();
            libpl.PL_put_term(this.TermRef, t);
        }
        /// <summary>
        /// Creates a term-references holding a Prolog integer representing value.
        /// </summary>
        /// <param name="value">a integer value</param>
        public PlTerm(int value)
        {
            _termRef = libpl.PL_new_term_ref();
            libpl.PL_put_integer(this.TermRef, value);
        }
        public PlTerm(long value)
        {
            _termRef = libpl.PL_new_term_ref();
            libpl.PL_put_integer(this.TermRef, value);
        }
        /// <summary>
        /// Creates a term-references holding a Prolog float representing value.
        /// </summary>
        /// <param name="value">a double value</param>
        public PlTerm(double value)
        {
            _termRef = libpl.PL_new_term_ref();
            libpl.PL_put_float(this.TermRef, value);
        }

        #endregion


     /***************************************
	 *	    SPECIALISED TERM CREATION		*
     *	    as static methods               *
	 ***************************************/

        #region PlVar Creation

        /// <summary>
        /// Creates a new initialised term (holding a Prolog variable).
        /// </summary>
        /// <returns>a PlTerm</returns>
        static public PlTerm PlVar()
        {
            PlTerm term = new PlTerm();
            term._termRef = libpl.PL_new_term_ref();
            return term;
        }
        #endregion

        #region PlList Creation
        /// <summary>
        /// <para>
        /// PlTail is for analysing and constructing lists. 
        /// It is called PlTail as enumeration-steps make the term-reference follow the `tail' of the list.
        /// </para>
        /// <para>
        /// A PlTail is created by making a new term-reference pointing to the same object. 
        /// As PlTail is used to enumerate or build a Prolog list, the initial list 
        /// term-reference keeps pointing to the head of the list.
        /// </para>
        /// </summary>
        /// <inheritdoc cref="Append(PlTerm)" select="example"/>
        /// <param name="list">The initial PlTerm</param>
        /// <returns>A PlTerm for which is_list/1 succeed.</returns>
        /// <seealso cref="Append(PlTerm)"/>
        /// <seealso cref="Add(PlTerm)"/>
        /// <seealso cref="AddList(PlTerm)"/>
        /// <seealso cref="Close()"/>
        /// <seealso cref="NextValue()"/>
        static public PlTerm PlTail(PlTerm list)
        {
            Check.Require((object)list != null);
            Check.Require(list.IsList || list.IsVar);

            PlTerm term = new PlTerm();
            if (0 != libpl.PL_is_variable(list.TermRef) || 0 != libpl.PL_is_list(list.TermRef))
                term._termRef = libpl.PL_copy_term_ref(list.TermRef);
            else
                throw new PlTypeException("list", list);

            return term;
        }
        #endregion


        #region PlCompound Creation

        /// <overloads>
        /// <summary>
        /// <para>These static methods creates a new compound <see cref="PlTerm"/>.</para>
        /// <para>For an example <see cref="PlCompound(string, PlTermV)"/></para>
        /// </summary>
        /// </overloads>
        /// <summary>
        /// Create a term by parsing (as read/1) the text.
        /// </summary>
        /// <exception cref="PlException">If the text is not valid Prolog syntax, a syntax error exception is raised.
        /// <para>Otherwise a new term-reference holding the parsed text is created.</para>
        /// </exception>
        /// <param name="text">The string representing the compound term  parsed by read/1.</param>
        /// <returns>a new <see cref="PlTerm"/></returns>
        static internal PlTerm PlCompound(string text)
        {
            // term_t 
            uint t = libpl.PL_new_term_ref();

            if (0 == libpl.PL_chars_to_term(text, t))
                throw new PlException("PlCompound creation failed : " + text);

            PlTerm term = PlVar();
            libpl.PL_put_term(term.TermRef, t);
            return term;
        }

        /// <summary>
        /// <para>Create a compound term with the given name from the given vector of arguments. See <see cref="PlTermV"/> for details.</para>
        /// </summary>
        /// <example>
        /// <para>The example below creates the Prolog term hello(world).</para>
        /// <code>
        ///  PlTerm t = PlTerm.PlCompound("hello", new PlTermv("world"));
        /// </code>
        /// </example>
        /// <param name="functor">The functor (name) of the compound term</param>
        /// <param name="args">the arguments as a <see cref="PlTermV"/></param>
        /// <returns>a new <see cref="PlTerm"/></returns>
        static public PlTerm PlCompound(string functor, PlTermV args)
        {
            Check.Require((object)args != null);
            //if (args == null)
            //    throw new ArgumentNullException("args");
            int arity = args.Size;
            if (arity == 2 && (functor == "." || functor == "[|]"))
            {
                return PlList(args);
            }
            else
            {
                PlTerm term = new PlTerm();
                term._termRef = libpl.PL_new_term_ref();
                functor = checkForNewList(functor, arity);
                libpl.PL_cons_functor_v(term.TermRef, libpl.PL_new_functor(libpl.PL_new_atom(functor), args.Size), args.A0);
                return term;
            }
        }

        public static PlTerm PlList(PlTermV args)
        {
            PlTerm term = new PlTerm();
            term._termRef = libpl.PL_new_term_ref();
            libpl.PL_cons_functor_v(term.TermRef, libpl.PL_new_functor(libpl.PL_new_atom(LIST_FUNCTOR_NAME), args.Size), args.A0);
            //libpl.PL_put_list(term._termRef, args.A0, args.A0 + 1);
            return term;
        }

        public static PlTerm PlList(PlTerm h, PlTerm t)
        {
            return PlTerm.PlList(new PlTermV(h, t));
        }

        

        public static String OLD_LIST_FUNCTOR_NAME = ".";
        public static String NEW_LIST_FUNCTOR_NAME = "[|]";
        static bool _wasTraditional;
        public static String LIST_FUNCTOR_NAME = IsTraditional ? OLD_LIST_FUNCTOR_NAME : NEW_LIST_FUNCTOR_NAME;
        public static bool IsTraditional {
            get
            {
                
                return _wasTraditional;
            }
            set
            {
                if (_wasTraditional == value) return;
                _wasTraditional = value;
                LIST_FUNCTOR_NAME = _wasTraditional ? OLD_LIST_FUNCTOR_NAME : NEW_LIST_FUNCTOR_NAME;                 
            }
        }

        private static string checkForNewList(string functor, int arity)
        {
            if (arity == 2 && ((functor == ".") || functor == "[|]")) return LIST_FUNCTOR_NAME;
            return functor;
        }

        /// <summary>
        /// <para>Create a compound term with the given name ant the arguments</para>
        /// </summary>
        /// <param name="functor">The functor (name) of the compound term</param>
        /// <param name="arg1">The first Argument as a <see cref="PlTerm"/></param>
        /// <returns>a new <see cref="PlTerm"/></returns>
        static public PlTerm PlCompound(string functor, params PlTerm[] arg1)
        {
            PlTermV args = new PlTermV(arg1);
            return PlTerm.PlCompound(functor, args);
        }

        public static PlTerm PlNewAtom(string name)
        {
            uint termRef = libpl.PL_new_term_refs(1);
            PlTerm term = new PlTerm();
            term._termRef = termRef;
            libpl.PL_put_atom(termRef, libpl.PL_new_atom(name));
            return term;
        }

        public static PlTerm PlAtom(string name)
        {
            uint termRef = libpl.PL_new_term_refs(1);
            PlTerm term = new PlTerm();
            term._termRef = termRef;
            libpl.PL_put_atom_chars(termRef, name);
            return term;
        }

#pragma warning disable 1573
        ///<inheritdoc cref="PlCompound(string, PlTerm)" />
        /// <param name="arg2">The second Argument as a <see cref="PlTerm"/></param>
        static public PlTerm PlCompound(string functor, PlTerm arg1, PlTerm arg2)
        {
            PlTermV args = new PlTermV(arg1, arg2);
            return PlTerm.PlCompound(functor, args);
        }
        ///<inheritdoc cref="PlCompound(string, PlTerm, PlTerm)" />
        /// <param name="arg3">The third Argument as a <see cref="PlTerm"/></param>
        static public PlTerm PlCompound(string functor, PlTerm arg1, PlTerm arg2, PlTerm arg3)
        {
            PlTermV args = new PlTermV(arg1, arg2, arg3);
            return PlTerm.PlCompound(functor, args);
        }
        static public PlTerm PlCompound(string functor, PlTerm arg1, PlTerm arg2, PlTerm arg3, PlTerm arg4)
        {
            PlTermV args = new PlTermV(arg1, arg2, arg3, arg4);
            return PlTerm.PlCompound(functor, args);
        }
#pragma warning restore 1573
        #endregion PlCompound Creation
        
        #region PlString Creation

        /// <overloads>
        /// <para>
        /// A SWI-Prolog string represents a byte-string on the global stack.
        /// It's lifetime is the same as for compound terms and other data living on the global stack.
        /// Strings are not only a compound representation of text that is garbage-collected,
        /// but as they can contain 0-bytes, they can be used to contain arbitrary C-data structures.
        /// </para>
        ///</overloads>
        /// <param name="text">the string</param>
        /// <returns>a new PlTerm</returns>
        static public PlTerm PlString(string text)
        {
            PlTerm t = new PlTerm();
            t._termRef = libpl.PL_new_term_ref();

            libpl.PL_put_string_chars(t.TermRef, text);
            return t;
        }
#pragma warning disable 1573
        /// <inheritdoc cref="PlString(string)" />
        /// <param name="len">the length of the string</param>
        static public PlTerm PlString(string text, int len)
        {
            PlTerm t = new PlTerm();
            t._termRef = libpl.PL_new_term_ref();
            libpl.PL_put_string_nchars(t.TermRef, len, text);
            return t;
        }
#pragma warning restore 1573
        #endregion PlString Creation

        #region PlCodeList Creation
        /// <summary>
        /// Create a Prolog list of ASCII codes from a 0-terminated C-string.
        /// </summary>
        /// <param name="text">The text</param>
        /// <returns>a new <see cref="PlTerm"/></returns>
        static public PlTerm PlCodeList(string text)
        {
            PlTerm term = new PlTerm();
            term._termRef = libpl.PL_new_term_ref();
            libpl.PL_put_list_codes(term.TermRef, text);
            return term;
        }
        #endregion

        #region PlCharList Creation
        /// <overloads>
        /// <summary>
        /// <para>These static methods creates a new PlCharList TODO TODO <see cref="PlTerm"/>.</para>
        /// </summary>
        /// </overloads>
        /// <summary>Create a Prolog list of one-character atoms from a C#-string.</summary>
        /// <remarks>Character lists are compliant to Prolog's <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/manipatom.html#atom_chars/2">atom_chars/2</see> predicate.</remarks>
        /// <param name="text">a string</param>
        /// <returns>A new PlTerm containing a prolog list of character</returns>
        static public PlTerm PlCharList(string text)
        {
            PlTerm term = new PlTerm();
            term._termRef = libpl.PL_new_term_ref();
            libpl.PL_put_list_chars(term.TermRef, text);
            return term;
        }
        #endregion



     /***************************************
	 *	                            		*
	 ***************************************/


        #region Testing the type of a term ( IsVar, IsList, .... )

        /// <summary>Get the <see cref="T:SbsSW.SwiPlCs.PlType"/> of a <see cref="PlTerm"/>.</summary>
        public PlType PlType
        {
            get { return (PlType)libpl.PL_term_type(this.TermRef); }
        }

        // all return non zero if condition succeed

        /// <summary>Return true if <see cref="PlTerm"/> is a variable</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsVar { get { return 0 != libpl.PL_is_variable(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is a ground term. See also ground/1. This function is cycle-safe.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsGround { get { return 0 != libpl.PL_is_ground(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is an atom.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsAtom
        {
            get
            {
                return 0 != libpl.PL_is_atom(this.TermRef);
            }
        }
        /// <summary>Return true if <see cref="PlTerm"/> is an atom.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsBlob
        {
            get
            {
                UIntPtr type = new UIntPtr();
                return 0 != libpl.PL_is_blob(this.TermRef, ref type);
            }
        }        /// <summary>Return true if <see cref="PlTerm"/> is an atom.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsAttVar
        {
            get
            {
                return 0 != libpl.PL_is_attvar(this.TermRef);
            }
        }

        /// <summary>Return true if <see cref="PlTerm"/> is an atom.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsAtomOrNil
        {
            get
            {

                bool wasAtom = 0 != libpl.PL_is_atom(this.TermRef);
                if (!wasAtom) return IsNil;
                return wasAtom;

            }
        }

        /// <summary>Return true if <see cref="PlTerm"/> is an atom.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsAtomOrString
        {
            get
            {

                bool wasAtom = 0 != libpl.PL_is_atom(this.TermRef);
                if (!wasAtom) return IsString;
                return wasAtom;

            }
        }
        /// <summary>Return true if <see cref="PlTerm"/> is a string.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsString { get { return 0 != libpl.PL_is_string(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is an integer.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsInteger { get { return 0 != libpl.PL_is_integer(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is a float.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsFloat { get { return 0 != libpl.PL_is_float(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is a compound term. Note that a list is a compound term ./2</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsCompound { get { return 0 != libpl.PL_is_compound(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is a compound term with functor ./2 or the atom [].</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsList { get { return 0 != libpl.PL_is_list(this.TermRef) || IsNewList; } }

        /// <summary>Return true if <see cref="PlTerm"/> is a compound term with functor ./2 or the atom [].</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsNewList { get
        {
            return (PlType == PlType.PlListPair) || (IsCompound && Arity == 2 && Name == LIST_FUNCTOR_NAME);
        } }

        /// <summary>Return true if <see cref="PlTerm"/> is atomic (not variable or compound).</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsAtomic { get { return 0 != libpl.PL_is_atomic(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is an integer or float.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsNumber { get { return 0 != libpl.PL_is_number(this.TermRef); } }

        /// <summary>Return true if <see cref="PlTerm"/> is an empty list or nil.</summary>
        /// <seealso cref="T:SbsSW.SwiPlCs.PlType"/>
        public bool IsNil { get { return  0 != libpl.PL_get_nil(this.TermRef); } }

        #endregion



     /***************************************
	 *	LIST ( PlTerm ) implementation      *
	 ***************************************/

        #region list ( PlTail ) Methods

        // building
        /// <summary>
        /// Appends element to the list and make the PlTail reference point to the new variable tail. 
        /// If A is a variable, and this method is called on it using the argument "gnat", 
        /// a list of the form [gnat|B] is created and the PlTail object now points to the new variable B.
        /// 
        /// This method returns TRUE if the unification succeeded and FALSE otherwise. No exceptions are generated.
        /// </summary>
        /// <example>
        ///     <code source="..\..\TestSwiPl\PlLTail.cs" region="List_Append_from_doc" />
        /// </example>
        /// <param name="term">The PlTerm to append on the list.</param>
        /// <returns>true if successful otherwise false</returns>
        public bool Append(PlTerm term)
        {
            Check.Require(this.IsList || this.IsVar, "" + this + " must be a list or var for Append " + term);
            Check.Require((object)term != null);

            uint tmp = libpl.PL_new_term_ref();
            if (0 != libpl.PL_unify_list(TermRef, tmp, TermRef) && 0 != libpl.PL_unify(tmp, term.TermRef))
                return true;

            return false;
        }

        /// <summary>
        /// Appends an element to a list by creating a new one and copy all elements
        /// Note This is a slow version
        /// see my mail from Jan from 2007.11.06 14:44
        /// </summary>
        /// <param name="term">a closed list</param>
        /// <returns>True if Succeed</returns>
        public bool Add(PlTerm term)
        {
            Check.Require(this.IsList, "" + this + " must be a list for Add");
            Check.Require((object)term != null);

            uint list, head, tail;
            BuildOpenList(out list, out head, out tail);

            if (0 == libpl.PL_unify_list(tail, head, tail))	// extend the list with a variable
                return false;
            if (0 == libpl.PL_unify(term.TermRef, head))	// Unify this variable with the new list
                return false;
            libpl.PL_unify_nil(tail);

            this._termRef = list;
            return true;
        }


        /// <summary>
        /// Appends a list ( PlTail ) to a list by creating a new one and copy all elements
        /// </summary>
        /// <example>
        /// <code source="..\..\TestSwiPl\PlLTail.cs" region="List_Add_list_doc" />
        /// </example>
        /// <param name="listToAppend">a closed list</param>
        /// <returns>True if Succeed</returns>
        public bool AddList(PlTerm listToAppend)
        {
            Check.Require(this.IsList, "" + this + " must be a list for AddList " + listToAppend);
            Check.Require((object)listToAppend != null);
            Check.Require(listToAppend.IsList);

            uint list, head, tail;
            BuildOpenList(out list, out head, out tail);

            uint list2 = libpl.PL_copy_term_ref(listToAppend.TermRef);
            uint elem = libpl.PL_new_term_ref(); 			// 'elem' for iterating the old list
            while (0 != libpl.PL_get_list(list2, elem, list2))
            {
                libpl.PL_unify_list(tail, head, tail);	// extend the list with a variable
                libpl.PL_unify(elem, head);				// Unify this variable with the new list
            }

            libpl.PL_unify_nil(tail);

            this._termRef = list;
            return true;
        }

        /// <summary>
        /// Unifies the term with [] and returns the result of the unification.
        /// </summary>
        /// <returns></returns>
        public int Close()
        {
            Check.Require(this.IsList || this.IsVar, "" + this + " must be a list or var for Close ");
            return libpl.PL_unify_nil(TermRef);
        }

        /// <summary>
        /// return a PlTerm bound to the next element of the list PlTail and advance PlTail. 
        /// Returns the element on success or a free PlTerm (Variable) if PlTail represents the empty list. 
        /// If PlTail is neither a list nor the empty list, a PlTypeException (type_error) is thrown. 
        /// </summary>
        /// <inheritdoc cref="AddList(PlTerm)" select="example"/>
        /// <returns>The Next element in the list as a PlTerm which is a variable for the last element or an empty list</returns>
        public PlTerm NextValue()
        {
            Check.Require(this.IsList, "" + this + " must be a list for NextValue");
            // PlTerm term = new PlTerm();
            PlTerm term = PlTerm.PlVar();
            if (0 != libpl.PL_get_list(this.TermRef, term.TermRef, this.TermRef))
            {
                return term;
            }
            if (0 != libpl.PL_get_nil(this.TermRef))
            {
                return term;
            }
            throw new PlTypeException("list", this);
        }

        /// <summary>
        /// Converts to a strongly typed ReadOnlyCollection of PlTerm objects that can be accessed by index
        /// </summary>
        /// <returns>A strongly typed ReadOnlyCollection of PlTerm objects</returns>
        public ReadOnlyCollection<PlTerm> ToList()
        {
            Check.Require(this.IsList, "" + this + " must be a list for ToList");
            List<PlTerm> l = new List<PlTerm>();
            foreach (PlTerm t in this)
            {
                l.Add(t);
            }
            return new ReadOnlyCollection<PlTerm>(l);
        }

        /// <summary>
        /// Converts to a strongly typed Collection of strings that can be accessed by index
        /// </summary>
        /// <returns>A strongly typed string Collection</returns>
        public Collection<string> ToListString() 
        {
            Check.Require(this.IsList, " must be a list for ToListString");
            List<string> l = new List<string>();
            foreach (PlTerm t in this.Copy())
            {
                l.Add(t.ToString());
            }
            return new Collection<string>(l);
        }


        #region IEnumerable<T> Members
        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>A System.Collections.Generic.IEnumerator&lt;T that can be used to iterate through the collection.</returns>
        public IEnumerator<PlTerm> GetEnumerator()
        {
            if (IsList)
            {
                // list is destrctive!?!
                return Copy().GetEnumeratorL();
            }
            return new PlArrayEnumerator(this, 1);
        }
        private IEnumerator<PlTerm> GetEnumeratorL()
        {
            Check.Require(this.IsList, " must be a list for GetEnumeratorL");
            PlTerm t = default(PlTerm);// new PlTerm(); //null;
            while (this.Next(ref t))
            {
                yield return (PlTerm)t;
            }
        }
        #endregion IEnumerable<T> Members


        // private list helper methods

        // enumerating
        // see: http://www.mycsharp.de/wbb2/thread.php?threadid=53241

        #region IEnumerable Members
        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>An System.Collections.IEnumerator object that can be used to iterate through the collection.</returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator(); // Looks recursive but it is *not*.
        }
        #endregion


        /// <summary>
        /// Bind termRef to the next element of the list PlTail and advance PlTail. 
        /// Returns TRUE on success and FALSE if PlTail represents the empty list. 
        /// If PlTail is neither a list nor the empty list, a type_error is thrown. 
        /// </summary>
        /// <param name="termRef"></param>
        /// <returns></returns>
        private bool Next(ref PlTerm termRef)
        {
            Check.Require(this.IsList, "must be a list of Next ");
//            termRef = new PlTerm();
            termRef = PlTerm.PlVar();
            if (0 != libpl.PL_get_list(this.TermRef, termRef._termRef, this.TermRef))
            {
                return true;
            }
            if (0 != libpl.PL_get_nil(this.TermRef))
            {
                return false;
            }
            throw new PlTypeException("list", this);
        }

        private void BuildOpenList(out uint list, out uint head, out uint tail)
        {
            list = libpl.PL_new_term_ref();			    // our list (starts unbound)
            tail = libpl.PL_copy_term_ref(list);	    // the tail of it (starts as the whole)
            head = libpl.PL_new_term_ref(); 			// placeholder for the element
            uint elem = libpl.PL_new_term_ref(); 		// for iterating the old list
            while (0 != libpl.PL_get_list(this.TermRef, elem, this.TermRef))
            {
                libpl.PL_unify_list(tail, head, tail);	// extend the list with a variable
                libpl.PL_unify(elem, head);				// Unify this variable with the new list
            }
        }

        #endregion list ( PlTail ) Methods


        #region ToString and helpers

        private string ToStringAsListFormat()
        {
            StringBuilder sb = new StringBuilder(""); ;
            PlTerm list = PlTerm.PlTail(this);
            //list.GetEnumerator();
            foreach (PlTerm t in list)
            {
                if (0 < sb.Length)
                    sb.Append(',');
                sb.Append(t.ToString());
            }
            sb.Insert(0, '[');
            sb.Append(']');
            return sb.ToString();
        }

        /// <inheritdoc />
        /// <summary>
        /// <para>If PlTerm is a list the string is build by calling ToString() for each element in the list 
        /// separated by ',' and put the brackets around '[' ']'.</para>
        /// <para></para>
        /// </summary>
        /// <seealso cref="O:string"/>
        /// <returns>A string representing the PlTerm.</returns>
        override public string ToString()
        {
            string s = "";
            if (this.IsList)    //switch (this.PlType)
                s = ToStringAsListFormat();
            else
                s = (string)this;
            return s;
        }



        static string string_buffer;

        static long SwriteStringCanonical(IntPtr handle, string buffer, long buffersize)
        {
            string s = buffer.Substring(0, (int)buffersize);
            string_buffer = s;
            return buffersize;
        }

        /// <summary>
        /// Convert a PlTerm to a string by <see href="http://www.swi-prolog.org/pldoc/man?predicate=write_canonical%2f1">write_canonical/1</see>
        /// <para>This Method use <see cref="DelegateStreamWriteFunction"/> and is slow.</para>
        /// </summary>
        /// <returns>return the string of a PlTerm</returns>
        public string ToStringCanonical()
        {
            // redirect write stream
            DelegateStreamWriteFunction old_write_function = PlEngine._function_write;
            DelegateStreamWriteFunction wf = new DelegateStreamWriteFunction(SwriteStringCanonical);
            PlEngine.SetStreamFunctionWrite(PlStreamType.Output, wf);

            PlQuery.PlCall("write_canonical", new PlTermV(this));
            PlQuery.PlCall("flush_output");

            // restore stream function
            PlEngine.SetStreamFunctionWrite(PlStreamType.Output, old_write_function);

            return string_buffer;
        }

        #endregion

        #region unification
        /// <overloads>
        /// This methods performs Prolog unification and returns true if successful and false otherwise.
        /// It is equal to the prolog =/2 operator.
        /// <para>See <see cref="Put(PlTerm)"/> for an example.</para>
        /// <remarks>
        /// This methods are introduced for clear separation between the destructive assignment in C# using =
        /// and prolog unification.
        /// </remarks>
        /// </overloads>
        /// <summary>Put a PlTerm with a PlTerm</summary>
        /// <example>
        /// <code source="..\..\TestSwiPl\PlTerm.cs" region="UnifyTermVar_doc" />
        /// </example>
        /// <param name="term">the second term for unification</param>
        /// <returns>true or false</returns>
        public void Put(PlTerm term)
        {
             libpl.PL_put_term(this.TermRef, term.TermRef);
        }


        /// <inheritdoc cref="Put(PlTerm)"/>
        /// <param name="atom">A string to Put with</param>
        public void Put(string atom)
        {
             libpl.PL_put_atom_chars(this.TermRef, atom);
        }

        public void Put(long integer)
        {
            libpl.PL_put_integer(this.TermRef, integer);
        }

        public void Put(ulong integer)
        {
            if (integer > long.MaxValue)
            {
                // slow but works
                Put(new PlTerm("" + integer));
                return;
            }
            libpl.PL_put_integer(this.TermRef, (long)integer);
        }

        public void Put(double atom)
        {
             libpl.PL_put_float(this.TermRef, atom);
        }

        /*
        <!--
        int operator =(const PlTerm &t2)	// term 
        {
            return PL_put(_termRef, t2._termRef);
        }
        int operator =(const PlAtom &atom)	// atom
        {
            return PL_put_atom(TermRef, atom._handle);
        }
        int operator =(const char *v)		// atom (from char *)
        {
            return PL_put_atom_chars(_termRef, v);
        }
        int operator =(long v)		// integer
        {
            return PL_put_integer(_termRef, v);
        }
        int operator =(int v)			// integer
        {
            return PL_put_integer(_termRef, v);
        }
        int operator =(double v)		// float
        {
            return PL_put_float(_termRef, v);
        }
        int operator =(const PlFunctor &f)	// functor
        {
            return PL_put_functor(_termRef, f.functor);
        }
        -->
        */
        #endregion unification

        #region unification
        /// <overloads>
        /// This methods performs Prolog unification and returns true if successful and false otherwise.
        /// It is equal to the prolog =/2 operator.
        /// <para>See <see cref="Unify(PlTerm)"/> for an example.</para>
        /// <remarks>
        /// This methods are introduced for clear separation between the destructive assignment in C# using =
        /// and prolog unification.
        /// </remarks>
        /// </overloads>
        /// <summary>Unify a PlTerm with a PlTerm</summary>
        /// <example>
        /// <code source="..\..\TestSwiPl\PlTerm.cs" region="UnifyTermVar_doc" />
        /// </example>
        /// <param name="term">the second term for unification</param>
        /// <returns>true or false</returns>
        public bool Unify(PlTerm term)
        {
            return 0 != libpl.PL_unify(this.TermRef, term.TermRef);
        }

        public bool CanUnify(PlTerm term)
        {

            return 0 != libpl.PL_unify(this.TermRef, term.TermRef);
        }

        /// <inheritdoc cref="Unify(PlTerm)"/>
        /// <param name="atom">A string to unify with</param>
        public bool Unify(string atom)
        {
            return 0 != libpl.PL_unify_atom_chars(this.TermRef, atom);
        }
        public bool UnifyAtom(string atom)
        {
            return 0 != libpl.PL_unify_atom_chars(this.TermRef, atom);
        }

        public bool Unify(long atom)
        {
            return 0 != libpl.PL_unify_integer(this.TermRef, atom);
        }

        public bool Unify(ulong atom)
        {
            if (atom > long.MaxValue)
            {
                // slow but works
                return Unify(new PlTerm("" + atom));
            }
            return 0 != libpl.PL_unify_integer(this.TermRef, (long)atom);
        }

        public bool Unify(double atom)
        {
            return 0 != libpl.PL_unify_float(this.TermRef, atom);
        }

        /// <summary>
        /// Useful e.g. for lists list.Copy().ToList(); list.ToString();
        /// </summary>
        /// <returns>Return a unifies PlTerm.PlVar of this term</returns>
        internal PlTerm Copy()
        {
            PlTerm tc = PlTerm.PlVar();
            if (!this.Unify(tc))
                throw new PlLibException("Copy term fails (Unification return false)");
            return tc;
        }
        /*
        <!--
        int operator =(const PlTerm &t2)	// term 
        {
            return PL_unify(_termRef, t2._termRef);
        }
        int operator =(const PlAtom &atom)	// atom
        {
            return PL_unify_atom(TermRef, atom._handle);
        }
        int operator =(const char *v)		// atom (from char *)
        {
            return PL_unify_atom_chars(_termRef, v);
        }
        int operator =(long v)		// integer
        {
            return PL_unify_integer(_termRef, v);
        }
        int operator =(int v)			// integer
        {
            return PL_unify_integer(_termRef, v);
        }
        int operator =(double v)		// float
        {
            return PL_unify_float(_termRef, v);
        }
        int operator =(const PlFunctor &f)	// functor
        {
            return PL_unify_functor(_termRef, f.functor);
        }
        -->
        */
        #endregion unification


        #region Arity and Name
        /// <summary><para>Get the arity of the functor if <see cref="PlTerm"/> is a compound term.</para></summary>
        /// <remarks><para><see cref="Arity"/> and <see cref="Name"/> are for compound terms only</para></remarks>
        /// <exception cref="NotSupportedException">Is thrown if the term isn't compound</exception>
        public int Arity
        {
            get
            {
                uint name = 0; // atom_t 
                int arity = 0;
                if (0 != libpl.PL_get_name_arity(this.TermRef, ref name, ref arity))
                    return arity;

                if (IsString) return 0;
                Embedded.Error(" arity called on a " + this.PlType);
                throw new NotSupportedException("Only possible for compound or atoms");
                //throw new PlTypeException("compound", this);   // FxCop Don't like this type of exception
            }
        }

        /// <summary>
        /// <para>Get a holding the name of the functor if <see cref="PlTerm"/> is a compound term.</para>
        /// </summary>
        /// <inheritdoc cref="Arity" />
        public string Name
        {
            get
            {

                uint name = 0; // atom_t 
                int arity = 0;
                var pltype = this.PlType;
                if (pltype == PlType.PlNil) return "[]";
                if (pltype == PlType.PlListPair) return LIST_FUNCTOR_NAME;
                if (0 != libpl.PL_get_name_arity(this.TermRef, ref name, ref arity))
                {
                    return libpl.PL_atom_chars(name);
                }
                    

                if (IsString) return ToString();


                Embedded.Debug("pltype = "+pltype);
                throw new NotSupportedException("Only possible for compound or atoms or string");
                //throw new PlTypeException("compound", this);   // FyCop Don't like this type of exception
            }
        }

        #endregion Arity and Name

        public long longValue()
        {
            return (long)this;
        }

        public int intValue()
        {
            return (int)this;
        }

        public double doubleValue()
        {
            return (double)this;
        }

        #region cast oprators
        /// <summary>
        /// Converts the Prolog argument into a string which implies Prolog atoms and strings
        /// are converted to the represented text or throw a PlTypeException. 
        /// </summary>
        /// <remarks>
        /// <para>Converts the Prolog argument using PL_get_chars() using the 
        /// flags CVT_ALL|CVT_WRITE|BUF_RING, which implies Prolog atoms and strings
        /// are converted to the represented text or throw a PlTypeException. 
        /// </para>
        /// <para>If the above call return 0 <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/foreigninclude.html#PL_get_chars()">PL_get_chars</see> is called a second time with the flags CVT_ALL|CVT_WRITE|BUF_RING|REP_UTF8.</para>
        /// <para>All other data is handed to write/1.</para>
        /// </remarks>
        /// <param name="term">A PlTerm that can be converted to a string</param>
        /// <returns>A C# string</returns>
        /// <exception cref="PlTypeException">Throws a PlTypeException exception</exception>
        /// <exception cref="SbsSW.DesignByContract.PreconditionException">Is thrown if the operator is used on an uninitialized PlTerm</exception>
        public static explicit operator string(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            return CastToString(term.TermRef);
        }
        public static string CastToString(uint TermRef)
        {
            return CastToString0(TermRef);
            return PrologCLR.InvokeFromC(() => CastToString0(TermRef), true);
        }
        public static string CastToString0(uint TermRef)
        {
            String s = "";
            if (0 != libpl.PL_get_chars(TermRef, ref s, libpl.CVT_ALL | libpl.CVT_WRITE | libpl.BUF_RING))
                return s;
            else if (0 != libpl.PL_get_chars(TermRef, ref s, libpl.CVT_ALL | libpl.CVT_WRITE | libpl.BUF_RING | libpl.REP_UTF8)) // libpl.REP_MB -> convertiert nach BestFitMapping
            {
                return s;
            }
            if (0 != libpl.PL_get_chars(TermRef, ref s, libpl.CVT_ALL | libpl.CVT_WRITE | libpl.REP_UTF8 | libpl.BUF_RING | libpl.CVT_VARIABLE))
                return s;
            throw new PlTypeException("text", new PlTerm(TermRef));
        }

        /// <summary>
        /// Yields a int if the PlTerm is a Prolog integer or float that can be converted 
        /// without loss to a int. Throws a PlTypeException exception otherwise
        /// </summary>
        /// <param name="term">A PlTerm is a Prolog integer or float that can be converted without loss to a int.</param>
        /// <returns>A C# int</returns>
        /// <exception cref="PlTypeException">Throws a PlTypeException exception if <see cref="PlType"/> 
        /// is not a <see langword="PlType.PlInteger"/> or a <see langword="PlType.PlFloat"/>.</exception>
        /// <exception cref="SbsSW.DesignByContract.PreconditionException">Is thrown if the operator is used on an uninitialized PlTerm</exception>
        public static explicit operator int(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            int v = 0;
            if (0 != libpl.PL_get_integer(term.TermRef, ref v))
                return v;
            throw new PlTypeException("int", term);
        }

        /// <summary>
        /// Yields a long if the PlTerm is a Prolog integer or float that can be converted 
        /// without loss to a long. Throws a PlTypeException exception otherwise
        /// </summary>
        /// <param name="term">A PlTerm is a Prolog integer or float that can be converted without loss to a long.</param>
        /// <returns>A C# long</returns>
        /// <exception cref="PlTypeException">Throws a PlTypeException exception if <see cref="PlType"/> 
        /// is not a <see langword="PlType.PlInteger"/> or a <see langword="PlType.PlFloat"/>.</exception>
        /// <exception cref="SbsSW.DesignByContract.PreconditionException">Is thrown if the operator is used on an uninitialized PlTerm</exception>
        public static explicit operator long(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            long v = 0;
            if (0 != libpl.PL_get_long(term.TermRef, ref v))
                return v;
            string s = (string) term;
            long longValue;
            if (long.TryParse(s, out longValue))
            {
                return longValue;
            }
            throw new PlTypeException("long", term);
        }

        public static explicit operator IntPtr(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            IntPtr v = IntPtr.Zero;
            if (0 != libpl.PL_get_intptr(term.TermRef, ref v))
                return v;
            throw new PlTypeException("intptr", term);
        }

        public static explicit operator ulong(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            string s = (string) term;
            ulong ulongValue;
            if (ulong.TryParse(s, out ulongValue))
            {
                return ulongValue;
            }
            throw new PlTypeException("ulong", term);
        }

#if USE_IKVM
        public static explicit operator java.math.BigInteger(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            string s = (string)term;
            return new java.math.BigInteger(s);
        }


        public static explicit operator java.math.BigDecimal(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            string s = (string)term;
            return new java.math.BigDecimal(s);
        }
#endif

        /// <summary>
        /// Yields the value as a C# double if PlTerm represents a Prolog integer or float. 
        /// Throws a PlTypeException exception otherwise. 
        /// </summary>
        /// <param name="term">A PlTerm represents a Prolog integer or float</param>
        /// <returns>A C# double</returns>
        /// <exception cref="PlTypeException">Throws a PlTypeException exception if <see cref="PlType"/> 
        /// is not a <see langword="PlType.PlInteger"/> or a <see langword="PlType.PlFloat"/>.</exception>
        /// <exception cref="SbsSW.DesignByContract.PreconditionException">Is thrown if the operator is used on an uninitialized PlTerm</exception>
        public static explicit operator double(PlTerm term)
        {
            Check.Require(term.TermRefIntern != 0);
            double v = 0;
            if (0 != libpl.PL_get_float(term.TermRef, ref v))
                return v;
            throw new PlTypeException("float", term);
        }

        #endregion cast oprators


        #region compare operators
        // Comparison standard order terms
        /// <inheritdoc />
        public override int GetHashCode()
        {
            return this.TermRef.GetHashCode();
        }
        /// <inheritdoc />
        public override bool Equals(Object obj)
        {
            if (obj is PlTerm)
                return this == ((PlTerm)obj);
            else if (obj is int)
                return this == ((int)obj);
            else
                return false;
        }
        /// <overload>Compare the instance term1 with term2 and return the result according to the Prolog defined standard order of terms.</overload>
        /// <summary>
        /// Yields TRUE if the PlTerm is an atom or string representing the same text as the argument, 
        /// FALSE if the conversion was successful, but the strings are not equal and an 
        /// type_error exception if the conversion failed.
        /// </summary>
        /// <param name="term1">a PlTerm</param>
        /// <param name="term2">a PlTerm</param>
        /// <returns>true or false</returns>
        public static bool operator ==(PlTerm term1, PlTerm term2)
        {
            Check.Require(term1.TermRefIntern != 0);
            Check.Require(term2.TermRefIntern != 0);
            return libpl.PL_compare(term1.TermRef, term2.TermRef) == 0;
        }
        /// <inheritdoc cref="op_Equality(PlTerm, PlTerm)" />
        public static bool operator !=(PlTerm term1, PlTerm term2)
        {
            return libpl.PL_compare(term1.TermRef, term2.TermRef) != 0;
        }
        /// <inheritdoc cref="op_Equality(PlTerm, PlTerm)" />
        public static bool operator <(PlTerm term1, PlTerm term2)
        {
            return libpl.PL_compare(term1.TermRef, term2.TermRef) < 0;
        }
        /// <inheritdoc cref="op_Equality(PlTerm, PlTerm)" />
        public static bool operator >(PlTerm term1, PlTerm term2)
        {
            return libpl.PL_compare(term1.TermRef, term2.TermRef) > 0;
        }
        /// <inheritdoc cref="op_Equality(PlTerm, PlTerm)" />
        public static bool operator <=(PlTerm term1, PlTerm term2)
        {
            return libpl.PL_compare(term1.TermRef, term2.TermRef) <= 0;
        }
        /// <inheritdoc cref="op_Equality(PlTerm, PlTerm)" />
        public static bool operator >=(PlTerm term1, PlTerm term2)
        {
            return libpl.PL_compare(term1.TermRef, term2.TermRef) >= 0;
        }


        /*
    int operator <=(const PlTerm &t2)
    {
        return PL_compare(_termRef, t2.TermRef) <= 0;
    }
    int operator >=(const PlTerm &t2)
    {
        return PL_compare(_termRef, t2._termRef) >= 0;
    }
    */
        // comparison (long)
#endregion

        #region Equality Method
        /// <overload>test overload</overload>
        /// <inheritdoc />
        /// <param name="term">a PlTerm</param>
        /// <param name="lng">a int</param>
        /// <returns>A bool</returns>
        public static bool operator ==(PlTerm term, int lng)
        {
            int v0 = 0;
            if (0 != libpl.PL_get_integer(term.TermRef, ref v0))
                return v0 == lng;
            else
                return false; // throw new PlTypeException("integer", term);
        }
        /// <inheritdoc />
        public static bool operator ==(int lng, PlTerm term)
        {
            return term == lng;
        }
        // comparison (string)
        /// <inheritdoc />
        public static bool operator ==(PlTerm term, string value)
        {
            string s0 = "";
            if (0 != libpl.PL_get_chars(term.TermRef, ref s0, libpl.CVT_ALL | libpl.CVT_WRITE | libpl.BUF_RING))
                return s0.Equals(value);
            else
                return false; // throw new PlTypeException("text", term);
        }
        /// <inheritdoc />
        public static bool operator ==(string value, PlTerm term)
        {
            return term == value;
        }

        #endregion


        #region Inequality Method
        /// <overloads>
        /// <summary>
        /// <para>Inequality Method overload</para>
        /// <see cref="op_Equality(PlTerm, PlTerm)"/>
        /// a
        /// <see cref="M:SbsSW.SwiPlCs.PlTerm.op_Equality(SbsSW.SwiPlCs.PlTerm,System.Int32)"/>
        /// </summary>
        /// </overloads>
        /// 
        /// <summary>
        /// summary
        /// </summary>
        /// <param name="term"></param>
        /// <param name="lng"></param>
        /// <returns></returns>
        public static bool operator !=(PlTerm term, int lng)
        {
            int v0 = 0;
            if (0 != libpl.PL_get_integer(term.TermRef, ref v0))
                return v0 != lng;
            else
                return true; // throw new PlTypeException("integer", term);
        }
        /// <inheritdoc cref="op_Inequality(PlTerm, int)" />
        public static bool operator !=(int lng, PlTerm term)
        {
            return term != lng;
        }
        /// <summary>
        /// test
        /// </summary>
        /// <param name="term"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        public static bool operator !=(PlTerm term, string value)
        {
            return !(term == value);
        }
        /// <inheritdoc cref="op_Inequality(PlTerm, string)" />
        public static bool operator !=(string value, PlTerm term)
        {
            return term != value;
        }

        #endregion compare operators



        #region IndexableWithLength<PlTerm> Members

        PlTerm IndexableWithLength<PlTerm>.this[int index]
        {
            get
            {
                return this[index];
            }
        }

        int IndexableWithLength<PlTerm>.Length
        {
            get { return Arity; }
        }

        internal PlTerm[] Args
        {
            get
            {
                if (!IsCompound)
                {
                    if(IsAtom) return new PlTerm[0];
                    if (IsNil) return null;
                     return null;   
                }
                    
                throw new NotImplementedException("PLTerm.Args");
            }
           // set { throw new NotImplementedException(); }
        }

        #endregion

        public bool FromObject(object o)
        {
            if (!IsVar)
            {
                Embedded.Warn("Not a free object! {0}", this);
            }
            var v = PrologCLR.UnifyToProlog(o, this);
            if (IsVar || v == 0)
            {
                if (PrologCLR.MakeArrayImmediate && PrologCLR.MakeNoRefs && PrologCLR.MadeARef && IsVar)
                {
                    return true;
                } 
                Embedded.Warn("Unify failed! {0}", this);
                return false;
            }
            return v != 0;
        }

        /// <summary>
        /// Zero Based 
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        internal PlTerm Arg(int p0)
        {
            return this[p0 + 1];
        }
        /// <summary>
        /// Zero Based 
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        public PlTerm ArgItem(int p0)
        {
            if (IsList)
            {
                PlTerm li = Copy();
                int itnum = p0;
                while (itnum-- > 0)
                {
                    li = li.Arg(1);
                }
                return li.Arg(0);
            }
            return this[p0 + 1];
        }

        public PlTerm Shift(int i)
        {
            return new PlTerm((uint) (_termRef + i));
        }

    } // class PlTerm
    #endregion


    /********************************
	*	      PlTermV				*
	********************************/
    #region public class PlTermV
    /// <summary>
    /// <preliminary>The struct PlTermv represents an array of term-references.</preliminary>
    /// <para>This type is used to pass the arguments to a foreign defined predicate (see <see cref="Callback.DelegateParameterVarArgs"/>), 
    /// construct compound terms (see <see cref="PlTerm.PlCompound(string, PlTermV)"/> 
    /// and to create queries (see <see cref="PlQuery"/>).
    /// </para>
    /// <para>The only useful member function is the overloading of [], providing (0-based) access to the elements. <see cref="this[Int32]"/> 
    /// Range checking is performed and raises a ArgumentOutOfRangeException exception.</para> 
    /// </summary>
    public struct PlTermV : IEquatable<PlTermV>, IEnumerable<PlTerm>, IndexableWithLength<PlTerm>
    {

        private uint _a0; // term_t
        private int _size;


        #region IndexableWithLength<PlTerm> Members

        PlTerm IndexableWithLength<PlTerm>.this[int index]
        {
            get
            {
                return this[index];
            }
        }

        int IndexableWithLength<PlTerm>.Length
        {
            get { return Size; }
        }

        #endregion
        public override string ToString()
        {
            try
            {
                string ts = "<r=" + _a0 + ",a=" + _size + ": ";
                uint tr = _a0;
                uint until = _a0 + (uint) _size;
                bool needComma = false;
                for (int i = 0; i < _size; i++)
                {
                    string s = PlTerm.CastToString(_a0 + (uint) i);
                    if(needComma)
                    {
                        ts += ",";
                    }
                    ts += s;
                    needComma = true;
                }
                return ts + ">";
            }
            catch (Exception)
            {
                return base.ToString();
            }
        }

        #region constructors

        /// <overloads>Create a PlTermV vector from the given PlTerm parameters
        /// <summary>
        /// <para>Create a new vector with PlTerm as elements</para>
        /// <para>It can be created with <paramref name="size"/> elements</para>
        /// <para>or</para>
        /// <para>automatically for 1, 2 or 3 plTerms</para>
        /// </summary>
        /// </overloads>
        /// 
        /// <summary>
        /// Create a vector of PlTerms with <paramref name="size"/> elements
        /// </summary>
        /// <param name="size">The amount of PlTerms in the vector</param>
        public PlTermV(int size)
        {
            _a0 = libpl.PL_new_term_refs(size);
            _size = size;
        }
        /*
        /// <summary>Create a PlTermV from the given <see cref="PlTerm"/>s.</summary>
        /// <param name="term0">The first <see cref="PlTerm"/> in the vector.</param>
        public PlTermV(PlTerm term0)
        {
            Check.Require(term0.TermRefIntern != 0);
            _size = 1;
            _a0 = term0.TermRef;
        }*/

// warning CS1573: Parameter 'term0' has no matching param tag in the XML comment for 'SbsSW.SwiPlCs.PlTermV.PlTermV(SbsSW.SwiPlCs.PlTerm, SbsSW.SwiPlCs.PlTerm)' (but other parameters do)
#pragma warning disable 1573
        /// <inheritdoc cref="PlTermV(PlTerm)" />
        /// <param name="term1">The second <see cref="PlTerm"/> in the vector.</param>
        /*public PlTermV(PlTerm term0, PlTerm term1)
        {
            Check.Require(term0.TermRefIntern != 0);
            Check.Require(term1.TermRefIntern != 0);
            _size = 2;
            _a0 = libpl.PL_new_term_refs(2);
            libpl.PL_put_term(_a0 + 0, term0.TermRef);
            libpl.PL_put_term(_a0 + 1, term1.TermRef);
        }
          */     
        public PlTermV(params PlTerm[] terms)
        {
            _size = terms.Length;
            _a0 = libpl.PL_new_term_refs(_size);
            for (int i = 0; i < _size; i++)
            {
                //if (terms[i].TermRefIntern != 0) 
                    libpl.PL_put_term((uint)(_a0 + i), terms[i].TermRef);
            }
        }

        public PlTermV(PlTerm terms, int arity)
        {
            _size = arity;
            Check.Require(terms.TermRefIntern != 0);
            _a0 = terms.TermRefIntern;
        }

        public void Resize(int newSize)
        {
            int addTo = newSize - _size;
            _size = newSize;
        }
        /*

        /// <inheritdoc cref="PlTermV(PlTerm, PlTerm)" />
        /// <param name="term2">The third <see cref="PlTerm"/> in the vector.</param>
        public PlTermV(PlTerm term0, PlTerm term1, PlTerm term2)
        {
            Check.Require(term0.TermRefIntern != 0);
            Check.Require(term1.TermRefIntern != 0);
            Check.Require(term2.TermRefIntern != 0);
            _size = 3;
            _a0 = libpl.PL_new_term_refs(3);
            libpl.PL_put_term(_a0 + 0, term0.TermRef);
            libpl.PL_put_term(_a0 + 1, term1.TermRef);
            libpl.PL_put_term(_a0 + 2, term2.TermRef);
        }*/
#pragma warning restore 1573
        #endregion



//        internal PlTermV(PlTermV toCopy)
//            : this(toCopy._size)
//        {
//            for (uint i = 0; i < toCopy._size; i++)
//            {
////                libpl.PL_put_term(_a0 + i, new PlTerm(toCopy[(int)i].TermRef).TermRef);
//                this[i].TermRef = libpl.PL_copy_term_ref(toCopy[i].TermRef);
//            }
//        }

        // Properties

        /// <summary>
        /// the first term_t reference of the array
        /// </summary>
        internal uint A0
        {
            get { return _a0; }
        }

        /// <summary>Get the size of a PlTermV</summary>
        public int Size
        {
            get { return _size; }
        }


        /// <summary>
        /// A zero based list
        /// </summary>
        /// <param name="index"></param>
        /// <returns>The PlTerm for the given index</returns>
        /// <exception cref="ArgumentOutOfRangeException">Is thrown if (index &lt;  0 || index >= Size)</exception>
        /// <exception cref="SbsSW.DesignByContract.PreconditionException">Is thrown if the operator is used on an uninitialized PlTerm</exception>
        public PlTerm this[int index]
        {
            get
            {
                if (index < 0 || index >= Size)
                    throw new ArgumentOutOfRangeException("index");
                else
                    return new PlTerm(A0 + (uint)index);  // If this line is deleted -> update comment in PlTern(term_ref)
            }
            set
            {
                if (index < 0 || index >= Size)
                    throw new ArgumentOutOfRangeException("index");
                else
                {
                    Check.Require(value.TermRefIntern != 0, "use of an uninitialized plTerm. If you need a variable use PlTerm.PlVar() instead");
                    libpl.PL_put_term(_a0 + (uint)index, ((PlTerm)value).TermRef);
                }
            }
        }


        #region IEquatable<PlTermV> Members
        // see http://msdn.microsoft.com/de-de/ms182276.aspx


        ///<inheritdoc />
        public override int GetHashCode()
        {
            return (int)A0;
        }

        ///<inheritdoc />
        public override bool Equals(object obj)
        {
            if (!(obj is PlTermV))
                return false;
            return Equals((PlTermV)obj);
        }

        ///<inheritdoc />
        ///<remarks>// TODO compare each PlTerm in PlTermV not only the refereces in A0</remarks>
        public bool Equals(PlTermV other)
        {
            if (this._size != other._size)
                return false;

            if (this.A0 != other.A0)
                return false;

            return true;
        }


        /// <summary>
        /// 
        /// </summary>
        /// <param name="tv1"></param>
        /// <param name="tv2"></param>
        /// <returns></returns>
        public static bool operator ==(PlTermV tv1, PlTermV tv2)
        {
            return tv1.Equals(tv2);
        }


        /// <summary>
        /// 
        /// </summary>
        /// <param name="tv1"></param>
        /// <param name="tv2"></param>
        /// <returns></returns>
        public static bool operator !=(PlTermV tv1, PlTermV tv2)
        {
            return !tv1.Equals(tv2);
        }    


        #endregion



        #region IEnumerable<PlTerm> Members

        IEnumerator<PlTerm> IEnumerable<PlTerm>.GetEnumerator()
        {
            return new PlTermVEnumerator(this, 0);
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return new PlTermVEnumerator(this, 0);
        }

        #endregion
    } // class PlTermV
    #endregion




    /****************************************************
	*	  PlFrame		                                *
    *	  PlQuery is in the file SWI-cs-PlQuery.cs      *
	****************************************************/
    #region public class PlFrame

    /// <summary>
    /// <para>The class PlFrame provides an interface to discard unused term-references as well as rewinding unifications (data-backtracking). 
    /// Reclaiming unused term-references is automatically performed after a call to a C#-defined predicate has finished and 
    /// returns control to Prolog. In this scenario PlFrame is rarely of any use.</para>
    /// <para>This class comes into play if the top level program is defined in C# and calls Prolog multiple times. 
    /// Setting up arguments to a query requires term-references and using PlFrame is the only way to reclaim them.</para>
    /// </summary>
    /// <remarks>see <see href="http://www.swi-prolog.org/packages/pl2cpp.html#sec:8.1"/></remarks>
    /// <example>
    /// A typical use for PlFrame is the definition of C# methods that call Prolog and may be called repeatedly from C#.
    /// Consider the definition of assertWord(), adding a fact to word/1:
    ///     <code source="..\..\TestSwiPl\PlFrame.cs" region="AssertWord2_doc" />
    /// alternatively you can use
    ///     <code source="..\..\TestSwiPl\PlFrame.cs" region="AssertWord_doc" />
    /// <b><note type="caution"> NOTE: in any case you have to destroy any query object used inside a PlFrame</note></b>
    /// </example>
    public class PlFrame : IDisposable
    {
        #region IDisposable
        // see : "Implementing a Dispose Method  [C#]" in  ms-help://MS.VSCC/MS.MSDNVS/cpguide/html/cpconimplementingdisposemethod.htm
        // and IDisposable in class PlQuery


        /// <summary>Implement IDisposable.</summary>
        /// <remarks>
        /// <para>Do not make this method virtual.</para>
        /// <para>A derived class should not be able to override this method.</para>
        /// </remarks>
        public void Dispose()
        {
            Dispose(true);
            // Take yourself off of the Finalization queue 
            // to prevent finalization code for this object
            // from executing a second time.
            GC.SuppressFinalize(this);
        }

        private void Dispose(bool disposing)
        {
            if (disposing)
            {
                // Free other state (managed objects).
            }
            // Free your own state (unmanaged objects).
            // Set large fields to null.
            Free();
        }


        #endregion

        private uint m_fid; // fid_t

        public uint FID
        {
            get { return m_fid; }
        }

        /// <summary>
        /// Creating an instance of this class marks all term-references created afterwards to be valid only in the scope of this instance.
        /// </summary>
        public PlFrame()
        {
            m_fid = libpl.PL_open_foreign_frame();
        }

        /// <summary>
        /// Reclaims all term-references created after constructing the instance.
        /// </summary>
        ~PlFrame()
        {
            Dispose(false);
        }

        /// <summary>
        /// Discards all term-references and global-stack data created as well as undoing all unifications after the instance was created.
        /// </summary>
        public void Rewind()
        {
            libpl.PL_rewind_foreign_frame(m_fid);
        }


        /// <summary>called by Dispose</summary>
        private void Free()
        {
            if (m_fid > 0 && PlEngine.IsInitialized)
            {
                libpl.PL_close_foreign_frame(m_fid);
            }
            m_fid = 0;
        }


    } // PlFrame
    #endregion




    /********************************
	*	      ENGINE				*
	********************************/




    #region public class PlEngine


    /// <summary>
    /// This static class represents the prolog engine.
    /// </summary>
    /// <example>
    /// A sample
    /// <code>
    ///    if (!PlEngine.IsInitialized)
    ///    {
    ///        String[] empty_param = { "" };
    ///        PlEngine.Initialize(empty_param);
    ///        // do some funny things ...
    ///        PlEngine.PlCleanup();
    ///    } 
    ///    // program ends here
    /// </code>
    /// The following sample show how a file is consult via comand-line options.
    /// <code source="..\..\TestSwiPl\PlEngine.cs" region="demo_consult_pl_file_by_param" />
    /// </example>
    public static class PlEngine
    {

        #region RegisterForeign

        /// <summary>
        /// Dictionary to pin foriegn method delegates so that are not GC'd
        /// </summary>
        public static readonly Dictionary<string, Delegate> SavedRegisterForeign = new Dictionary<string, Delegate>();
        public static readonly List<Delegate> MoreSavedDelegates = new List<Delegate>();

        public static bool SaveRegisterForeign(string module, string name, int arity, Delegate method)
        {
            Delegate prev;
            string key = (module ?? "user") + ":" + (name ?? "_NONAME_") + "/" + arity;
            PrologCLR.RegisterInfo(name, arity, method.Method);
            lock (SavedRegisterForeign)
            {
                if (!SavedRegisterForeign.TryGetValue(key, out prev))
                {
                    SavedRegisterForeign[key] = method;
                    return true;
                }
            }
            if (prev == method)
            {
                return true;
            }
            if (true)
            {
                MoreSavedDelegates.Add(method);
                if (false)
                    Embedded.Debug("PinDelegate: " + key + " <- " + method.Method + " from " + prev.Method +
                                    " as " + method.GetType().Name);
                return true;
            }
            Embedded.ConsoleWriteLine("PinDelegate: " + key + " <- " + method.Method + " from " + prev.Method +
                                    " as " + method.GetType().Name);
            return false;
        }

        /// <overloads>
        /// <summary>
        /// <para>Register a C-function to implement a Prolog predicate.</para>
        /// <para>After this call returns successfully a predicate with name <paramref name="name"/>(a string) and arity arity (a C# int) is created in module module.</para>
        /// <para>If module is NULL, the predicate is created in the module of the calling context or if no context is present in the module user.</para>
        /// </summary>
        /// <remarks>
        /// <para>Add a additional namespace by <code>using SbsSW.SwiPlCs.Callback;</code></para>
        /// </remarks>
        /// <example>For an example see <see cref="Callback.DelegateParameter2"/> and <see cref="Callback.DelegateParameter1"/>.</example>
        /// <seealso cref="Callback"/>
        /// </overloads>
        /// 
        /// <summary>
        /// <para>Register a C# callback method</para>
        /// </summary>
        /// <example>For an example see <see cref="Callback.DelegateParameter2"/> and <see cref="Callback.DelegateParameter1"/>.</example>
        /// <param name="method">a delegate to a c# method <see cref="Callback"/></param>
        /// <returns>true if registration succeed otherwise false</returns>
        public static bool RegisterForeign(Delegate method)
        {
            return RegisterForeign(null, method);
        }


// warning CS1573: Parameter 'term0' has no matching param tag in the XML comment for 'SbsSW.SwiPlCs.PlTermV.PlTermV(SbsSW.SwiPlCs.PlTerm, SbsSW.SwiPlCs.PlTerm)' (but other parameters do)
#pragma warning disable 1573

        ///<inheritdoc cref="RegisterForeign(Delegate)" />
        /// <param name="module">the name of a prolog module <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/modules.html">Using Modules</see></param>
        public static bool RegisterForeign(string module, Delegate method)
        {
            Callback.PlForeignSwitches flags = Callback.PlForeignSwitches.None;

            if (method is Callback.DelegateParameterBacktrack2 || method is Callback.DelegateParameterBacktrack1)
                flags |= Callback.PlForeignSwitches.Nondeterministic;

            if (method is Callback.DelegateParameterVarArgs || method is Callback.DelegateParameterBacktrackVarArgs)
                flags |= Callback.PlForeignSwitches.VarArgs;
            return RegisterForeign(module, method, flags);
        }

        /// <inheritdoc cref="RegisterForeign(string, Delegate)" />
        /// <param name="arity">The amount of parameters</param>
        public static bool RegisterForeign(string name, int arity, Delegate method)
        {
            return RegisterForeign(null, name, arity, method);
        }

        /// <inheritdoc cref="RegisterForeign(string, int, Delegate)" />
        /// <param name="name">The name of the method </param>
        public static bool RegisterForeign(string module, string name, int arity, Delegate method)
        {
            return RegisterForeign(module, name, arity, method, Callback.PlForeignSwitches.VarArgs);
        }

#pragma warning restore 1573

        // make public to activate the foreign predicated in named modules
        #region  privates
        internal static bool RegisterForeign(string module, Delegate method, Callback.PlForeignSwitches plForeign)
        {
            string name = method.Method.Name;
            int arity = method.Method.GetParameters().Length;
            return RegisterForeign(module, name, arity, method, plForeign);
        }


        /// <summary>
        /// <see href="http://gollem.science.uva.nl/SWI-Prolog/Manual/foreigninclude.html#PL_register_foreign_in_module()"/>
        /// </summary>
        /// <returns></returns>
        public static bool RegisterForeign(string module, string name, int arity, Delegate method, Callback.PlForeignSwitches plForeign)
        {
            if (!SaveRegisterForeign(module, name, arity, method)) return false;
            return Convert.ToBoolean(libpl.PL_register_foreign_in_module(module, name, arity, method, (int)plForeign));
        }

        #endregion  privates


        #endregion RegisterForeign


        /// <summary>To test if the prolog engine is up.</summary>
        public static bool IsInitialized
        {
            get
            {
                int i = libpl.PL_is_initialised(IntPtr.Zero, IntPtr.Zero);
                if (0 == i)
                    return false;
                else
                    return true;
            }
        }

        /// <summary>
        /// <para>Initialise SWI-Prolog</para>
        /// <para>The write method of the output stream is redirected to <see cref="SbsSW.SwiPlCs.Streams"/> 
        /// before Initialize. The read method of the input stream just after Initialize.</para>
        /// </summary>
        /// <remarks>
        /// <para>A known bug: Initialize work *not* as expected if there are e.g. German umlauts in the parameters
        /// See marshalling in the sorce NativeMethods.cs</para>
        /// </remarks>
        /// <param name="argv">
        /// <para>For a complete parameter description see the <a href="http://gollem.science.uva.nl/SWI-Prolog/Manual/cmdline.html" target="_new">SWI-Prolog reference manual section 2.4 Command-line options</a>.</para>
        /// <para>sample parameter: <code>String[] param = { "-q", "-f", @"some\filename" };</code>
        /// At the first position a parameter "" is added in this method. <see href="http://www.swi-prolog.org/pldoc/doc_for?object=section(3%2C%20%279.6.20%27%2C%20swi(%27%2Fdoc%2FManual%2Fforeigninclude.html%27))">PL_initialise</see>
        /// </para>
        /// </param>
        /// <example>For an example see <see cref="SbsSW.SwiPlCs.PlEngine"/> </example>
        public static void Initialize(String[] argv)
        {
            if (argv == null)
                throw new ArgumentNullException("argv", "Minimum is one empty string");
            if (IsInitialized)
            {
                throw new PlLibException("PlEngine is already initialised");
            }
            else
            {
                libpl.LoadLibPl();
                // redirect input and output stream to receive messages from prolog
                DelegateStreamWriteFunction wf = new DelegateStreamWriteFunction(Swrite_function);
                if (!IsStreamFunctionWriteModified)
                {
                    if (Embedded.RedirectStreams) PlEngine.SetStreamFunctionWrite(PlStreamType.Output, wf);
                    IsStreamFunctionWriteModified = false;
                }
                String[] local_argv = new String[argv.Length+1];
                int idx = 0;
                local_argv[idx++] = "";
                foreach (string s in argv)
                    local_argv[idx++] = s;
                if (Embedded.IsEmbeddedFromProlog && 0 == libpl.PL_initialise(local_argv.Length, local_argv))
                    throw new PlLibException("failed to initialize");
                else
                {
                    Embedded.ConsoleWriteLine("PL_initialised");
                    SetStreamReader(Sread_function);
                }
            }
        }


        public static void SetStreamReader(DelegateStreamReadFunction rf)
        {
            if (Embedded.RedirectStreams)
            {
               // if (!IsStreamFunctionReadModified)
                {
                    PlEngine.SetStreamFunctionRead(PlStreamType.Input, new DelegateStreamReadFunction(rf));
                    IsStreamFunctionReadModified = true;
                }
            }
        }


        /// <summary>
        /// Try a clean up but it is buggy
        /// search the web for "possible regression from pl-5.4.7 to pl-5.6.27" to see reasons
        /// </summary>
        /// <remarks>Use this method only at the last call before run program ends</remarks>
        static public void PlCleanup()
        {
            unsafe
            {
                try
                {
                    libpl.PL_cleanup(0);
                }
                catch (Exception e)
                {
                    PrologCLR.ConsoleTrace(e);
                    if (e is Exception) throw (Exception)e;
                    throw new Exception("SPECIAL: " + e);
                }
            }
        }

        /// <summary>Stops the PlEngine and <b>the program</b></summary>
        /// <remarks>SWI-Prolog calls internally pl_cleanup and than exit(0)</remarks>
        static public void PlHalt()
        {
            libpl.PL_halt(0);
        }

        // *****************************
        // STATICs for STREAMS
        // *****************************
        #region stream IO


        #region default_io_doc
        static internal long Swrite_function(IntPtr handle, string buf, long bufsize)
        {
            long ssize = bufsize;
            if (bufsize > buf.Length)
            {
                ssize = buf.Length;
            }
            string s = buf.Substring(0, (int)ssize);
            Console.Write(s);
            System.Diagnostics.Trace.WriteLine(s);
            return bufsize;
        }

        static internal long Sread_function(IntPtr handle, System.IntPtr buf, long bufsize)
        {
            throw new PlLibException("SwiPlCs: Prolog try to read from stdin");
        }
        #endregion default_io_doc



        public static bool IsStreamFunctionWriteModified;  // default = false;
        public static bool IsStreamFunctionReadModified;   // default = false;

        /// <summary>To Avoid callbackOnCollectedDelegate MDA</summary>
        internal static Streams.DelegateStreamWriteFunction _function_write;
        /// <summary>To Avoid callbackOnCollectedDelegate MDA</summary>
        internal static Streams.DelegateStreamReadFunction _function_read;

        /// <summary>
        /// This is a primitive approach to enter the output from a stream.
        /// </summary>
        /// <example>
        /// <code source="..\..\TestSwiPl\StreamIO.cs" region="StreamWrite_doc" />
        /// </example>
        /// <param name="streamType">Determine which stream to use <see cref="Streams.PlStreamType"/></param>
        /// <param name="function">A <see cref="Streams.DelegateStreamWriteFunction"/></param>
        static public void SetStreamFunctionWrite(Streams.PlStreamType streamType, Streams.DelegateStreamWriteFunction function)
        {
            libpl.LoadLibPl();
            libpl.SetStreamFunction(streamType, libpl.StreamsFunction.Write, function);
            IsStreamFunctionWriteModified = true;
            _function_write = function;
        }

        /// <summary>
        /// TODO
        /// </summary>
        /// <example>
        /// <code source="..\..\TestSwiPl\StreamIO.cs" region="StreamRead_doc" />
        /// </example>
        /// <param name="streamType">Determine which stream to use <see cref="Streams.PlStreamType"/></param>
        /// <param name="function">A <see cref="Streams.DelegateStreamReadFunction"/></param>
        static public void SetStreamFunctionRead(Streams.PlStreamType streamType, Streams.DelegateStreamReadFunction function)
        {
            libpl.LoadLibPl();
            libpl.SetStreamFunction(streamType, libpl.StreamsFunction.Read, function);
            IsStreamFunctionReadModified = true;
            _function_read = function;
        }


        #endregion stream IO




        // *****************************
        // STATICs for MULTI THreading
        // *****************************
        #region STATICs for MULTI THreading

        /// <summary>
        /// <para>return : reference count of the engine</para>
        ///	<para>		If an error occurs, -1 is returned.</para>
        ///	<para>		If this Prolog is not compiled for multi-threading, -2 is returned.</para>
        /// </summary>
        /// <returns>A reference count of the engine</returns>
        public static int PlThreadAttachEngine()
        {
            return libpl.PL_thread_attach_engine(IntPtr.Zero);
        }


        /// <summary>
        /// Returns the integer Prolog identifier of the engine or 
        /// -1 if the calling thread has no Prolog engine. 
        /// This method is also provided in the single-threaded version of SWI-Prolog, where it returns -2. 
        /// </summary>
        /// <returns></returns>
        public static int PlThreadSelf()
        {
            return libpl.PL_thread_self();
        }


        /// <summary>
        /// Returns TRUE on success and FALSE if the calling thread has no 
        /// engine or this Prolog does not support threads. 
        /// </summary>
        /// <returns></returns>
        public static bool PlThreadDestroyEngine()
        {
            return 0 != libpl.PL_thread_destroy_engine();
        }

        #endregion

    } // class PlEngine
    #endregion


    /****************************************************************************
	*	      PrologServer  ( PL_initialise, pl_halt, pl_cleanup )				*
	****************************************************************************/

    #region public class PrologServer

    /// <summary>
    /// Experimental
    /// </summary>
    public class PrologServer
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="argc"></param>
        /// <param name="argv"></param>
        /// <returns></returns>
        static public bool IsInitialized(int argc, String[] argv)
        {
            int i = libpl.PL_is_initialised(ref argc, ref argv);
            if (0 == i)
                return false;
            else
                return true;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="argc"></param>
        /// <param name="argv"></param>
        public PrologServer(int argc, String[] argv)
        {
            if (0 != libpl.PL_is_initialised(ref argc, ref argv))
            {
                throw new PlLibException("PlEngine is already initialized");
            }
            else
            {
                if (0 == libpl.PL_initialise(argc, argv))
                    throw new PlLibException("failed to initialize");
            }
        }

        //static public void PlCleanup()
        //{
        //    libpl.PL_cleanup(0);
        //}

        /// <summary>Stops the PlEngine and <b>the program</b></summary>
        /// <remarks>SWI-Prolog calls internally exit(0)</remarks>
        static public void PLHalt()
        {
            //libpl.PL_cleanup(0);
            libpl.PL_halt(0);
        }


    } // PrologServer

    #endregion public class PrologServer


    #region public class PlMtEngine

    /// <summary>
    /// This class is experimental
    /// </summary>
    public class PlMtEngine : IDisposable
    {
        private IntPtr _iEngineNumber = IntPtr.Zero;
        private IntPtr _iEngineNumberStore = IntPtr.Zero;

        public IntPtr EngineNumber
        {
            get { return _iEngineNumber; }
        }

        #region IDisposable
        // see : "Implementing a Dispose Method  [C#]" in  ms-help://MS.VSCC/MS.MSDNVS/cpguide/html/cpconimplementingdisposemethod.htm
        // and IDisposable in class PlQuery

        // Implement IDisposable.
        // Do not make this method virtual.
        // A derived class should not be able to override this method.
        /// <summary>
        /// 
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            // Take yourself off of the Finalization queue 
            // to prevent finalization code for this object
            // from executing a second time.
            GC.SuppressFinalize(this);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="disposing"></param>
        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                // Free other state (managed objects).
            }
            // Free your own state (unmanaged objects).
            // Set large fields to null.
            Free();
        }


        #endregion

        /// <summary>
        /// 
        /// </summary>
        public void Free()
        {
            if (IntPtr.Zero != _iEngineNumber && PlEngine.IsInitialized)
            {
                if (0 == libpl.PL_destroy_engine(_iEngineNumber))
                    throw (new PlException("failed to destroy engine"));
                _iEngineNumber = IntPtr.Zero;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        ~PlMtEngine()
        {
            Dispose(false);
        }

        /// <summary>
        /// 
        /// </summary>
        public PlMtEngine()
        {
            try
            {
                if (0 != libpl.PL_is_initialised(IntPtr.Zero, IntPtr.Zero))
                {
                    try
                    {
                        _iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                    }
                    catch (Exception ex)
                    {
                        throw (new PlException("PL_create_engine : " + ex.Message));
                    }
                }
                else
                {
                    throw new PlLibException("There is no PlEngine initialized");
                }
            } catch(Exception e)
            {
                throw new PlLibException("new PlEngine: " + e.Message + " " + e.StackTrace);
            }
        }

        public PlMtEngine(IntPtr engineNumber)
        {
            _iEngineNumber = engineNumber;
        }

        // override object.Equals
        public override bool Equals(object obj)
        {
            //       
            // See the full list of guidelines at
            //   http://go.microsoft.com/fwlink/?LinkID=85237  
            // and also the guidance for operator== at
            //   http://go.microsoft.com/fwlink/?LinkId=85238
            //

            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }
            PlMtEngine other = (PlMtEngine)obj;
            return other._iEngineNumber == _iEngineNumber;
            
        }

// override object.GetHashCode
        public override int GetHashCode()
        {
            return _iEngineNumber.GetHashCode();
        }

        /// <summary>
        /// 
        /// </summary>
        public void PlSetEngine()
        {
            IntPtr pNullPointer = IntPtr.Zero;
            int iRet = libpl.PL_set_engine(_iEngineNumber, ref pNullPointer);
            switch (iRet)
            {
                case libpl.PL_ENGINE_SET: break; // all is fine
                case libpl.PL_ENGINE_INVAL: throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                case libpl.PL_ENGINE_INUSE: throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                default: throw (new PlLibException("Unknown return from PlSetEngine"));
            }
        }

        /// <summary>
        /// 
        /// </summary>
        public void PlDetachEngine()
        {
            int iRet = libpl.PL_set_engine(_iEngineNumber, ref _iEngineNumberStore);
            switch (iRet)
            {
                case libpl.PL_ENGINE_SET: break; // all is fine
                case libpl.PL_ENGINE_INVAL: throw (new PlLibException("PlSetEngine(detach) returns Invalid")); //break;
                case libpl.PL_ENGINE_INUSE: throw (new PlLibException("PlSetEngine(detach) returns it is used by an other thread")); //break;
                default: throw (new PlLibException("Unknown return from PlSetEngine(detach)"));
            }
        }


        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        override public string ToString()
        {
            return _iEngineNumber.ToString();
        }

    } // class PlMtEngine
    #endregion


} // namespace SbsSW.SwiPlCs
