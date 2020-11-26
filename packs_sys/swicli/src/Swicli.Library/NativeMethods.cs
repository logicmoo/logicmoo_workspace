#if PROLOG_SWI
#else
#if PROLOG_SWI_6
#else
#if PROLOG_YAP
#else // Yap
#define PROLOG_YAP
#endif
#endif
#endif

#define USESAFELIB
/*  $Id$
*  
*  Project: Swicli.Library - Two Way Interface for .NET and MONO to SWI-Prolog
*  Author:        Douglas R. Miles
*                 Uwe Lesta (SbsSW.SwiPlCs classes)
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.org
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


/*
http://www.codeproject.com/csharp/legacyplugins.asp
 * http://www.cnblogs.com/Dah/archive/2007/01/07/614040.html
 * ggf. 
 * http://www.pcreview.co.uk/forums/thread-2241486.php
 * http://www.msnewsgroups.net/group/microsoft.public.dotnet.languages.csharp/topic12656.aspx
 * 
 * tool to generate the pinvoke lines from Visual Studio 2005
 * http://www.pinvoke.net/
 * */


using System;
using System.IO;
using System.Runtime.ConstrainedExecution;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using Microsoft.Win32.SafeHandles;
using SbsSW.SwiPlCs.Callback;
using Swicli.Library;
//using System.Runtime.CompilerServices;


namespace SbsSW.SwiPlCs
{
    #if USESAFELIB

	#region Safe Handles and Native imports
	// See http://msdn.microsoft.com/msdnmag/issues/05/10/Reliability/ for more about safe handles.
	[SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode = true)]
	public sealed class SafeLibraryHandle : SafeHandleZeroOrMinusOneIsInvalid
	{
		private SafeLibraryHandle() : base(true) { }

        protected override bool ReleaseHandle()
        {
            if (Embedded.IsLinux)
            {
                return NativeMethodsLinux.FreeLibrary(handle);
            }
            else
            {
                return NativeMethodsWindows.FreeLibrary(handle);
            }
        }

	    public bool UnLoad()
		{
			return ReleaseHandle();
		}

	}

    static class NativeMethods
    {
        public static SafeLibraryHandle LoadUnmanagedLibrary(string fileName)
        {
            return LoadUnmanagedLibrary(fileName, true);
        }

        public static SafeLibraryHandle LoadUnmanagedLibrary(string fileName, bool throwOnInvalid)
        {
            SafeLibraryHandle localHLibrary;
            if (Embedded.IsLinux)
            {
				try {
					localHLibrary = NativeMethodsLinux.LoadLibrary(fileName);
					if (!localHLibrary.IsInvalid)
					{
						return localHLibrary;
					}
					PrologCLR.ConsoleTrace("IsInvalid NativeMethodsLinux "  + fileName);
				} catch ( Exception e) {
					PrologCLR.ConsoleTrace("NativeMethodsLinux "  + fileName + " e=" + e );
				}
				
            }
			{
				try {
					localHLibrary = NativeMethodsWindows.LoadLibrary(fileName);
					if (!localHLibrary.IsInvalid)
					{
						return localHLibrary;
					}
					PrologCLR.ConsoleTrace("IsInvalid NativeMethodsWindows " + fileName);
				} catch ( Exception e) {
					PrologCLR.ConsoleTrace("NativeMethodsWindows "  + fileName + " e=" + e );
				}
            }

			localHLibrary = NativeMethodsWindows.LoadLibrary(fileName);
			if (localHLibrary.IsInvalid)
			{
				int hr = Marshal.GetHRForLastWin32Error();
				if (throwOnInvalid) Marshal.ThrowExceptionForHR(hr);
			}
            return localHLibrary;
        }

        static public void UnLoadUnmanagedLibrary(SafeHandleZeroOrMinusOneIsInvalid _hLibrary)
        {
#if USESAFELIB && false
            if (_hLibrary != null && !_hLibrary.IsClosed)
            {
                _hLibrary.Close();
                _hLibrary.UnLoad();
                _hLibrary.Dispose();
                _hLibrary = null;
            }
#endif
        }
    }
	static class NativeMethodsWindows
	{
		const string SKernel = "kernel32.dll";
		[DllImport(SKernel, CharSet = CharSet.Auto, BestFitMapping = false, SetLastError = true)]
		public static extern SafeLibraryHandle LoadLibrary(string fileName);

		[ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)]
		[DllImport(SKernel, SetLastError = true)]
		[return: MarshalAs(UnmanagedType.Bool)]
		public static extern bool FreeLibrary(IntPtr hModule);

        // see: http://blogs.msdn.com/jmstall/archive/2007/01/06/Typesafe-GetProcAddress.aspx
        [DllImport(SKernel, CharSet = CharSet.Ansi, BestFitMapping = false, SetLastError = true)]
        internal static extern IntPtr GetProcAddress(SafeLibraryHandle hModule, String procname);
	}

    static class NativeMethodsLinux
    {
        
		static public SafeLibraryHandle LoadLibrary(string fileName) {
            return dlopen(fileName, RTLD_NOW);
        }

        static public bool FreeLibrary(IntPtr handle) {
            dlclose(handle);
			return true;
        }

        static public IntPtr GetProcAddress(SafeLibraryHandle dllHandle, string name) {
            // clear previous errors if any
            dlerror();
            var res = dlsym(dllHandle, name);
            var errPtr = dlerror();
            if (errPtr != IntPtr.Zero) {
                throw new Exception("dlsym: " + Marshal.PtrToStringAnsi(errPtr));
            }
            return res;
        }

        const int RTLD_NOW = 2;

        [DllImport("libdl.so")]
        private static extern SafeLibraryHandle dlopen(String fileName, int flags);
        
        [DllImport("libdl.so")]
        private static extern IntPtr dlsym(SafeLibraryHandle handle, String symbol);

        [DllImport("libdl.so")]
        private static extern int dlclose(IntPtr handle);

        [DllImport("libdl.so")]
        private static extern IntPtr dlerror();


    }
	#endregion // Safe Handles and Native imports
#endif



	// for details see http://msdn2.microsoft.com/en-us/library/06686c8c-6ad3-42f7-a355-cbaefa347cfc(vs.80).aspx
	// and http://blogs.msdn.com/fxcop/archive/2007/01/14/faq-how-do-i-fix-a-violation-of-movepinvokestonativemethodsclass.aspx

	//NativeMethods - This class does not suppress stack walks for unmanaged code permission. 
	//    (System.Security.SuppressUnmanagedCodeSecurityAttribute must not be applied to this class.) 
	//    This class is for methods that can be used anywhere because a stack walk will be performed.

	//SafeNativeMethods - This class suppresses stack walks for unmanaged code permission. 
	//    (System.Security.SuppressUnmanagedCodeSecurityAttribute is applied to this class.) 
	//    This class is for methods that are safe for anyone to call. Callers of these methods are not 
	//    required to do a full security review to ensure that the usage is secure because the methods are harmless for any caller.

	//UnsafeNativeMethods - This class suppresses stack walks for unmanaged code permission. 
	//    (System.Security.SuppressUnmanagedCodeSecurityAttribute is applied to this class.) 
	//    This class is for methods that are potentially dangerous. Any caller of these methods must do a 
	//    full security review to ensure that the usage is secure because no stack walk will be performed.


	[ System.Security.SuppressUnmanagedCodeSecurity ]
	public static class SafeNativeMethods
	{

		
// .DLL is translated to .so by MONO
#if PROLOG_SWI
        public const string CONST_LIBSWIPL_DllFileName = @"libswipl.dll";
#else
#if PROLOG_SWI_6
        private const string DllFileName = @"LibPl.dll";
#else // Yap
        public const string CONST_LIBSWIPL_DllFileName = @"/usr/local/lib/libYap.so";
#endif
#endif

        static SafeNativeMethods()
        {
            if (Embedded.IsLinux) return;
            if (!File.Exists(CONST_LIBSWIPL_DllFileName))
            {
                if (!File.Exists(SwiplConstLibswiplDllFileName))
                {
                    PrologCLR.ConsoleTrace("No such file: " + SwiplConstLibswiplDllFileName);
                } else
                {
                    return;
                }
                PrologCLR.ConsoleTrace("No such file: " + CONST_LIBSWIPL_DllFileName);
            }

        }

        public static string PlLib
        {
            get { return CONST_LIBSWIPL_DllFileName; }
        }

	    
		public static string SwiplConstLibswiplDllFileName
		{
			get
			{
			    var fileName = CONST_LIBSWIPL_DllFileName;
                if (File.Exists(fileName)) return fileName;
			    fileName = Path.Combine(Path.Combine(PrologCLR.SwiHomeDir ?? PrologCLR.AltSwiHomeDir, "bin"), PlLib);
                if (File.Exists(fileName)) return fileName;
                return CONST_LIBSWIPL_DllFileName;
			}
		}
	    /////////////////////////////
		/// libpl
		///
        // das funktioniert NICHT wenn umlaute e.g. ü im pfad sind.
#if (!PROLOG_YAP)
        [DllImport(CONST_LIBSWIPL_DllFileName,CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
        internal static extern int PL_initialise(int argc, String[] argv);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
			// PL_EXPORT(int)		PL_is_initialised(int *argc, char ***argv);
		internal static extern int PL_is_initialised([In, Out] ref int argc, [In, Out] ref String[] argv);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_initialised(IntPtr argc, IntPtr argv);
#endif
        [DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_halt(int i);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern void PL_cleanup(int status);



            // PL_EXPORT(int)		PL_register_foreign_in_module(const char *module, const char *name, int arity, pl_function_t func, int flags);
            // typedef unsigned long	foreign_t
        // int PL_register_foreign_in_module(const char *module, const char *name, int arity, foreign_t (*function)(), int flags)
        [DllImport(CONST_LIBSWIPL_DllFileName, CallingConvention=CallingConvention.Cdecl, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
        internal static extern int PL_register_foreign_in_module(string module, string name, int arity, Delegate function, int flags);

		//	 ENGINES (MT-ONLY)
		// TYPES :  PL_engine_t			-> void *
		//			PL_thread_attr_t	-> struct
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        // PL_EXPORT(PL_engine_t)	PL_create_engine(PL_thread_attr_t *attributes);
        internal static extern IntPtr PL_create_engine(IntPtr attr);
        [DllImport(CONST_LIBSWIPL_DllFileName)]	// PL_EXPORT(int)		PlSetEngine(PL_engine_t engine, PL_engine_t *old);
		internal static extern int PL_set_engine(IntPtr engine, [In, Out] ref IntPtr old);
        [DllImport(CONST_LIBSWIPL_DllFileName)]	// PL_EXPORT(int)		PL_destroy_engine(PL_engine_t engine);
        internal static extern int PL_destroy_engine(IntPtr engine);

	    [DllImport(CONST_LIBSWIPL_DllFileName)]   // PL_EXPORT(int)	int PL_thread_at_exit(void (*function)(void *), void *closure, int global)
	    internal static extern int PL_thread_at_exit(Delegate function, IntPtr closure, int globlal);

        // typedef int  (*PL_agc_hook_t)(atom_t a);
        // PL_EXPORT(PL_agc_hook_t)      	PL_agc_hook(PL_agc_hook_t);
	    [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern PL_agc_hook_t PL_agc_hook(PL_agc_hook_t newhook);

	    [DllImport(CONST_LIBSWIPL_DllFileName)]
	    internal static extern unsafe void PL_on_halt(SwiOnHalt atom, void* closure);

        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_abort_hook(PL_abort_hook_t ah);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_abort_unhook(PL_abort_hook_t atom);

        /*******************************
        *	     COMPARE		*
        *******************************/

        //PL_EXPORT(int)		PL_compare(term_t t1, term_t t2);
        //PL_EXPORT(int)		PL_same_compound(term_t t1, term_t t2);

        /*******************************
        *	     MESSAGES		*
        *******************************/

        //PL_EXPORT(int)		PL_warning(const char *fmt, ...);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
        internal static extern unsafe int PL_warning(string text, params void*[] varargs);
        //PL_EXPORT(void)		PL_fatal_error(const char *fmt, ...);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
        internal static extern void PL_fatal_error(string text, params IntPtr[] varargs);

        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
        internal static extern uint PL_new_atom(string text);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)] // return const char *
        //TODO ausprobieren		[return: MarshalAs(UnmanagedType.LPStr)]
        //internal static extern String PL_atom_chars(uint t_atom);
        internal static extern IntPtr PL_atom_chars(uint t_atom);


        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Auto, BestFitMapping = true, ThrowOnUnmappableChar = true)]
        internal static extern uint PL_new_atom_wchars(int len, string text);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Auto, BestFitMapping = true, ThrowOnUnmappableChar = true)] // return const char *
        internal static extern IntPtr PL_atom_wchars(uint t_atom, ref int len);



        // Pl_Query
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern uint PL_query(uint pl_query_switch);
        
        // PlFrame
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern uint PL_open_foreign_frame();
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_close_foreign_frame(uint fid_t);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern void PL_rewind_foreign_frame(uint fid_t);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_discard_foreign_frame(uint fid_t);
        // record recorded erase
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern uint PL_record(uint term_t);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_recorded(uint record_t, uint term_t);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_erase(uint record_t);
        // PlQuery
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_next_solution(uint qid_t);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
		internal static extern IntPtr PL_predicate(string name, int arity, string module);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
			//qid_t PL_open_query(module_t m, int flags, predicate_t pred, term_t t0);
		internal static extern uint PL_open_query(IntPtr module, int flags, IntPtr pred, uint term);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_cut_query(uint qid);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_close_query(uint qid);
	

		// PlTerm
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)] // return term_t
		internal static extern void PL_put_atom_chars(uint term, string chars);
		//__pl_export term_t	PL_new_term_ref(void);
		[DllImport(CONST_LIBSWIPL_DllFileName)] // return term_t
		internal static extern uint PL_new_term_ref();
		//__pl_export void	PL_put_integer(term_t term, long i);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern void PL_put_integer(uint term, long i);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern void PL_put_float(uint term, double i);
		// __pl_export void	PL_put_atom(term_t term, atom_t atom);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern void PL_put_atom(uint term, uint atom_handle);
		// __pl_export int		PL_get_chars(term_t term, char **s, unsigned int flags);
        //[DllImport(DllFileName)]
        //internal static extern int PL_get_chars(uint term, ref string s, uint flags);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
        internal static extern int PL_get_chars(uint term, [In, Out]ref IntPtr s, uint flags);

        // __pl_export int		PL_get_integer(term_t term, int *i);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_get_integer(uint term, [In, Out] ref int i);
        // __pl_export int		PL_get_long(term_t term, long *i);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_get_long(uint term, [In, Out] ref long i);

        // __pl_export int		PL_get_intptr(term_t term, intptr_t *f);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_get_intptr(uint term, [In, Out] ref IntPtr i);

        // __pl_export int		PL_get_pointer(term_t term, void **f);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_get_pointer(uint term, [In, Out] ref IntPtr i);

        // __pl_export int		PL_get_int64(term_t term, int64_t *f);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_get_int64(uint term, [In, Out] ref long i);

        
        // __pl_export int		PL_get_float(term_t term, double *f);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_get_float(uint term, [In, Out] ref double i);
        // __pl_export int		PL_get_atom(term_t term, atom_t *atom);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_get_atom(uint term, [In, Out] ref uint atom_t);
		//__pl_export int		PL_term_type(term_t term);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_term_type(uint t);

		// COMPARE
		//__pl_export int		PL_compare(term_t t1, term_t t2);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_compare(uint term1, uint term2);

 

		// PlTermV
		[DllImport(CONST_LIBSWIPL_DllFileName)] // return term_t
		internal static extern uint PL_new_term_refs(int n);
		//__pl_export void	PL_put_term(term_t t1, term_t t2);
		[DllImport(CONST_LIBSWIPL_DllFileName)] 
		internal static extern void PL_put_term(uint t1, uint t2);

		// PlCompound
		// __pl_export int PL_chars_to_term(const char *chars, term_t term);
		//__pl_export void	PL_cons_functor_v(term_t h, functor_t fd, term_t A0);
		//__pl_export functor_t	PL_new_functor(atom_t f, int atom);
		#if !(missing_PL_chars_to_term)
        // ExactSpelling = true
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
        //[DllImport(DllFileName)]
        internal static extern int PL_chars_to_term(string chars, uint term);
        //internal static extern int PL_chars_to_term([In, MarshalAs(UnmanagedType.LPStr)]String chars, uint term);
#endif
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int atom_to_term(uint atom, uint term, uint binding);
        //[DllImport(DllFileName)]
        //
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void PL_cons_functor_v(uint term, uint functor_t, uint term_a0);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern uint PL_new_functor(uint atom_a, int a);

		//__pl_export void	PL_put_string_chars(term_t term, const char *chars);
		//__pl_export void	PL_put_string_nchars(term_t term, unsigned int len, const char *chars);
		//__pl_export void	PL_put_list_codes(term_t term, const char *chars);
		//__pl_export void	PL_put_list_chars(term_t term, const char *chars);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
		internal static extern void PL_put_string_chars(uint term_t, string chars);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
		internal static extern void PL_put_string_nchars(uint term_t, int len, string chars);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
		internal static extern void PL_put_list_codes(uint term_t, string chars);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = false, ThrowOnUnmappableChar = true)]
		internal static extern void PL_put_list_chars(uint term_t, string chars);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern void PL_put_list(uint term_t);

		// Testing the type of a term
		//__pl_export int		PL_is_variable(term_t term);
		//__pl_export int		PL_is_list(term_t term);
		// ...
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_variable(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_ground(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_atom(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_string(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_integer(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_float(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_compound(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_list(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_atomic(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_number(uint term_t);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_is_attvar(uint t);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_get_attr(uint v, uint a);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_is_blob(uint t, [In, Out] ref UIntPtr type);
//PL_EXPORT(int)		PL_unify_blob(term_t t, void *blob, size_t len,PL_blob_t *type);
		// LISTS (PlTail)
		//__pl_export term_t	PL_copy_term_ref(term_t from);
		//__pl_export int		PL_unify_list(term_t l, term_t h, term_t term);
		//__pl_export int		PL_unify_nil(term_t l);
		//__pl_export int		PL_get_list(term_t l, term_t h, term_t term);
		//__pl_export int		PL_get_nil(term_t l);
		// __pl_export int		PL_unify(term_t t1, term_t t2);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_unify_intptr(uint term, IntPtr intptr);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_unify_pointer(uint term, IntPtr intptr);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern uint PL_copy_term_ref(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_unify_list(uint term_t_l, uint term_t_h, uint term_t_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_unify_nil(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_get_list(uint term_t_l, uint term_t_h, uint term_t_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_get_nil(uint term_t);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_unify(uint t1,  uint t2);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_unify_integer(uint t1, Int32 n);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_unify_integer(uint t1, Int64 n);
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_unify_float(uint t1, double n);

        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = true, ThrowOnUnmappableChar = false)]
        internal static extern int PL_unify_atom_chars(uint t1, string atom);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = true, ThrowOnUnmappableChar = false)]
        internal static extern int PL_unify_string_chars(uint t1, string atom);
        [DllImport(CONST_LIBSWIPL_DllFileName, CharSet = CharSet.Ansi, BestFitMapping = true, ThrowOnUnmappableChar = false)]
        internal static extern int PL_unify_list_chars(uint t1, string atom);

        

		// Exceptions
		// Handling exceptions
		//__pl_export term_t	PL_exception(qid_t _qid);
		//__pl_export int		PL_raise_exception(term_t exception);
		//__pl_export int		PL_throw(term_t exception);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern uint PL_exception(uint qid);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_raise_exception(uint exception_term);
		//__pl_export int		PL_get_arg(int index, term_t term, term_t atom);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_get_arg(int index, uint t, uint a );
		//__pl_export int		PL_get_name_arity(term_t term, atom_t *Name, int *Arity);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_get_name_arity(uint t, ref uint name, ref int arity);

		// ******************************
		// *	  PROLOG THREADS		*
		// ******************************

		// from file pl-itf.h
		/*
		typedef struct
				{
					unsigned long	    local_size;		// Stack sizes
					unsigned long	    global_size;
					unsigned long	    trail_size;
					unsigned long	    argument_size;
					char *	    alias;					// alias Name
				} PL_thread_attr_t;
		*/
		//PL_EXPORT(int)	PL_thread_self(void);	/* Prolog thread id (-1 if none) */
		//PL_EXPORT(int)	PL_thread_attach_engine(PL_thread_attr_t *attr);
		//PL_EXPORT(int)	PL_thread_destroy_engine(void);
		//PL_EXPORT(int)	PL_thread_at_exit(void (*function)(void *), void *closure, int global);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_thread_self();
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_thread_attach_engine(IntPtr attr);
		//internal static extern int PL_thread_attach_engine(ref PL_thread_attr_t attr);
		[DllImport(CONST_LIBSWIPL_DllFileName)]
		internal static extern int PL_thread_destroy_engine();



        // ******************************
        // *	  PROLOG STREAM's		*
        // ******************************


        #region structurs

        // int Slinesize

        // IOFUNCTIONS  Sfilefunctions






        /*
         * long ssize_t
         * 
        typedef ssize_t (*Sread_function)(void *handle, char *buf, size_t bufsize);
        typedef ssize_t (*Swrite_function)(void *handle, char*buf, size_t bufsize);
        typedef long  (*Sseek_function)(void *handle, long pos, int whence);
        typedef int64_t (*Sseek64_function)(void *handle, int64_t pos, int whence);
        typedef int   (*Sclose_function)(void *handle);
        typedef int   (*Scontrol_function)(void *handle, int action, void *arg);


        typedef struct io_functions
        { Sread_function	read;		//* fill the buffer
          Swrite_function	write;		//* empty the buffer 
          Sseek_function	seek;		//* seek to position 
          Sclose_function	close;		//* close stream 
          Scontrol_function	control;	//* Info/control 
          Sseek64_function	seek64;		//* seek to position (intptr_t files) 
        } IOFUNCTIONS;
        */

        
        // IOSTREAM    S__iob[3]
        [StructLayout(LayoutKind.Sequential, Pack = 8)]
        public struct MIOSTREAM
        {
            /*
            char		    *bufp;		    // `here'
            char		    *limitp;		    // read/write limit 
            char		    *buffer;		    // the buffer 
            char		    *unbuffer;	    // Sungetc buffer 
            int			    lastc;		    // last character written 
            int			    magic;		    // magic number SIO_MAGIC 
            int  			bufsize;	    // size of the buffer 
            int			    flags;		    // Status flags 
            IOPOS			posbuf;		    // location in file 
            IOPOS *		    position;	    // pointer to above 
            IntPtr	        *handle;		    // function's handle 
            MIOFUNCTIONS	*functions;	    // open/close/read/write/seek 
            int		        locks;		    // lock/unlock count 
            */
            //IOLOCK *		    mutex;		    // stream mutex 
            IntPtr mutex;

            long[] place_holder_1;
					            // SWI-Prolog 4.0.7 
              //void			    (*close_hook)(void* closure);
              //void *		    closure;
              //                  // SWI-Prolog 5.1.3 
              //int			    timeout;	    // timeout (milliseconds) 
              //                  // SWI-Prolog 5.4.4 
              //char *		    message;	    // error/warning message 
              //IOENC			    encoding;	    // character encoding used 
              //struct io_stream *	tee;		// copy data to this stream 
              //mbstate_t *		mbstate;	    // ENC_ANSI decoding 
              //struct io_stream *	upstream;	// stream providing our input 
              //struct io_stream *	downstream;	// stream providing our output 
              //unsigned		    newline : 2;	// Newline mode 
              //void *		    exception;	    // pending exception (record_t) 
              //intptr_t		    reserved[2];	// reserved for extension 
        };

        /*

         * 
typedef struct io_position
{ int64_t		byteno;		// byte-position in file 
  int64_t		charno;		// character position in file 
  int			lineno;		// lineno in file 
  int			linepos;	// position in line 
  intptr_t		reserved[2];	// future extensions 
} IOPOS;

         * 
typedef struct io_stream{ 
  char		       *bufp;		    // `here'
  char		       *limitp;		    // read/write limit 
  char		       *buffer;		    // the buffer 
  char		       *unbuffer;	    // Sungetc buffer 
  int			    lastc;		    // last character written 
  int			    magic;		    // magic number SIO_MAGIC 
  int  			    bufsize;	    // size of the buffer 
  int			    flags;		    // Status flags 
  IOPOS			    posbuf;		    // location in file 
  IOPOS *		    position;	    // pointer to above 
  void		       *handle;		    // function's handle 
  IOFUNCTIONS	   *functions;	    // open/close/read/write/seek 
  int		        locks;		    // lock/unlock count 
  IOLOCK *		    mutex;		    // stream mutex 
					// SWI-Prolog 4.0.7 
  void			    (*close_hook)(void* closure);
  void *		    closure;
					// SWI-Prolog 5.1.3 
  int			    timeout;	    // timeout (milliseconds) 
					// SWI-Prolog 5.4.4 
  char *		    message;	    // error/warning message 
  IOENC			    encoding;	    // character encoding used 
  struct io_stream *	tee;		// copy data to this stream 
  mbstate_t *		mbstate;	    // ENC_ANSI decoding 
  struct io_stream *	upstream;	// stream providing our input 
  struct io_stream *	downstream;	// stream providing our output 
  unsigned		    newline : 2;	// Newline mode 
  void *		    exception;	    // pending exception (record_t) 
  intptr_t		    reserved[2];	// reserved for extension 
} IOSTREAM;

         */

        #endregion structurs


        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int Slinesize();


        // from pl-stream.h
        // PL_EXPORT(IOSTREAM *)	S__getiob(void);	/* get DLL's __iob[] address */
        /// <summary>
        /// 0 -> Sinput
        /// 1 -> Soutput
        /// 2 -> Serror
        /// </summary>
        /// <returns>a array of IOSTREAM * pointers</returns>
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern IntPtr S__getiob();


        // from pl-stream.h
        // PL_EXPORT(IOSTREAM *)	Snew(void *handle, int flags, IOFUNCTIONS *functions);
        /// <summary>
        /// 
        /// </summary>
        /// <param name="handle"></param>
        /// <param name="flags">defined in pl-stream.h all with prefix SIO_</param>
        /// <param name="functions">A set of function pointers see IOFUNCTIONS in pl-stream.h</param>
        /// <returns> a SWI-PROLOG IOSTREAM defined in pl-stream.h</returns>
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern IntPtr Snew(IntPtr handle, int flags, IntPtr functions);

        // from pl-itf.h
        // PL_EXPORT(int)  	PL_unify_stream(term_t t, IOSTREAM *s);
        /// <summary>
        /// 
        /// </summary>
        /// <param name="t"></param>
        /// <param name="iostream">the return value from Snew</param>
        /// <returns></returns>
        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_unify_stream(uint t, IntPtr iostream);



        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern FRG PL_foreign_control(IntPtr ptr);

	    [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern int PL_foreign_context(IntPtr control);

	    [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void _PL_retry(int control);

	    [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal static extern void _PL_retry_address(IntPtr control);

	    [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal extern static IntPtr PL_foreign_context_address(IntPtr control);

        [DllImport(CONST_LIBSWIPL_DllFileName)]
        internal extern static int PL_toplevel();

	    [DllImport(CONST_LIBSWIPL_DllFileName)]
	    internal static extern int PL_write_term(IntPtr iostream, uint term, int precedence, int flags);

	} // class SafeNativeMethods

} // namespace SbsSW.SwiPlCs
