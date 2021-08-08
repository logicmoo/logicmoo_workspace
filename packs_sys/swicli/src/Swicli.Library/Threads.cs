/*  $Id$
*  
*  Project: Swicli.Library - Two Way Interface for .NET and MONO to SWI-Prolog
*  Author:        Douglas R. Miles
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.com
*  Copyright (C):  2010-2012 LogicMOO Developement
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
#if USE_IKVM
using IKVM.Internal;
using ikvm.runtime;
using java.net;
//using jpl;
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
#endif
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using PlTerm = SbsSW.SwiPlCs.PlTerm;

namespace Swicli.Library
{
    public partial class PrologCLR
    {
		static public bool ClientReady = false;
        static private object _threadRegLock = new object();
        static public object ThreadRegLock
        {
            get
            {
                return LockInfo.Watch(_threadRegLock);
            }
        }
        public static bool SaneThreadWorld = true;
        public static Dictionary<Thread, int> ThreadRegisterations = new Dictionary<Thread, int>();
        public static Dictionary<Thread, int> ForiegnFrameCounts = new Dictionary<Thread, int>();

        internal static int IncrementUseCount(Thread thread)
        {
            return IncrementUseCount(thread, ThreadRegisterations);
        }
        internal static int DecrementUseCount(Thread thread)
        {
            return DecrementUseCount(thread, ThreadRegisterations);
        }
        internal static int IncrementUseCount(Thread thread, Dictionary<Thread, int> registration)
        {
            lock (ThreadRegLock) lock (registration)
            {
                int regs = 0;
                if (!registration.TryGetValue(thread, out regs))
                {
                    registration[thread] = 1;
                }
                else
                {
                    registration[thread] = regs + 1;
                }
                return regs + 1;
            }
        }
        internal static int DecrementUseCount(Thread thread, Dictionary<Thread, int> registration)
        {
            lock (ThreadRegLock) lock (registration)
            {
                int regs = 0;
                if (!registration.TryGetValue(thread, out regs))
                {
                    registration[thread] = 0;
                }
                else
                {
                    registration[thread] = regs - 1;
                }
                return regs - 1;
            }
        }



        public static Dictionary<int, IntPtr> SafeThreads = new Dictionary<int, IntPtr>();
        public static Dictionary<int, Thread> engineToThread = new Dictionary<int, Thread>();
        public static Dictionary<int, int> threadToEngine = new Dictionary<int, int>();


        public static Dictionary<int, PlMtEngine> ThreadEngines = new Dictionary<int, PlMtEngine>();
        public static List<IntPtr> FreeEngines = new List<IntPtr>();
        public static bool UseEnginePool = true;

        public static void RegisterMainThread()
        {
            PingThreadFactories();
            lock (ThreadRegLock)
            {
                var fa = Type.GetType("System.Windows.Forms.Application");
                if (fa != null)
                {
                    var evinfo = fa.GetEvent("ThreadExit");
                    if (evinfo != null)
                    {
                        evinfo.AddEventHandler(null, new EventHandler(OnThreadExit));
                    }
                } else
                {
                    Embedded.Debug("Not installing ThreadExit hook to System.Windows.Forms.Application");
                }
                var t = Thread.CurrentThread.ManagedThreadId;
                //libpl.PL_thread_at_exit((DelegateParameter0)PrologThreadAtExitGlobal, IntPtr.Zero, 1);
              //  SafeThreads.Add(t, new IntPtr(libpl.PL_ENGINE_MAIN));
               // int self = libpl.PL_thread_self();
              //  engineToThread.Add(self, t);
            }
        }

        public static bool NoTestThreadFActory = true;
        public static void PingThreadFactories()
        {
            try
            {

                if (NoTestThreadFActory) return;
                Assembly assem = AssemblyLoad("MushDLR223");
                if (assem != null)
                {
                    Type type = assem.GetType("MushDLR223.Utilities.SafeThread");
                    if (type != null)
                    {
                        NoTestThreadFActory = true;
                        type.GetEvent("ThreadAdded").GetAddMethod().Invoke(null,
                                                                           new[] { new Action<Thread>(RegisterThread) });
                        type.GetEvent("ThreadRemoved").GetAddMethod().Invoke(null,
                                                                             new[] { new Action<Thread>(DeregisterThread) });
                    }
                }
            }
            catch (Exception)
            {
            }
        }

        private static void OnThreadExit(object sender, EventArgs e)
        {

        }
        public static void RegisterCurrentThread()
        {
            RegisterThread(Thread.CurrentThread);
        }


        public static bool OneToOneEnginesPeThread = true;


        public static void RegisterThread(Thread thread)
        {
            //if (thread == CreatorThread) return;
            if (OneToOneEnginesPeThread)
            {
                // leaks!
                RegisterThread121(thread);
            }
            else
            {
                RegisterThread12Many(thread);
            }
        }

        static readonly IntPtr PL_ENGINE_CURRENT_PTR = new IntPtr(libpl.PL_ENGINE_CURRENT); // ((PL_engine_t)0x2)
        public static void RegisterThread121(Thread thread)
        {
            try
            {
                IntPtr ce = GetCurrentEngine();
                if (ce == IntPtr.Zero)
                {
                    RegisterThread121A(thread);
                    //ce = GetCurrentEngine();
                } else
                {
                    
                }
                TestEngineViable(ce);
            } catch(Exception e)
            {
                RegisterThread121A(thread);                
            }

        }

        private static void TestEngineViable(IntPtr ce)
        {
            //PlQuery.PlCall(
        }
        public static void RegisterThread121A(Thread thread)
        {
            if (thread == CreatorThread)
            {
                return;
            }
            lock (ThreadRegLock)
            {
                IncrementUseCount(thread);
                int count;
                if (ForiegnFrameCounts.TryGetValue(thread, out count))
                {
                   // if (count > 1) return;
                }
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                IntPtr _iEngineNumber;
                IntPtr _iEngineNumberReally = IntPtr.Zero;
                lock (SafeThreads) if (SafeThreads.TryGetValue(thread.ManagedThreadId, out _iEngineNumber)) return;
                if (0 != libpl.PL_is_initialised(IntPtr.Zero, IntPtr.Zero))
                {
                    try
                    {
                        //_iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                        var self = libpl.PL_thread_attach_engine(_iEngineNumber);
                        var ce = GetCurrentEngine();
                        lock (SafeThreads) SafeThreads.Add(thread.ManagedThreadId, ce);
                        threadToEngine.Add(thread.ManagedThreadId, self);
                        libpl.PL_thread_at_exit((DelegateParameter0)PrologThreadAtExit, IntPtr.Zero, 0);
                        return;
                        int iRet = libpl.PL_set_engine(_iEngineNumber, ref _iEngineNumberReally);
                        EnsureEngine(_iEngineNumber);
                    }
                    catch (Exception ex)
                    {
                        throw (new PlException("PL_create_engine : " + ex.Message));
                    }
                } else
                {
                    string[] local_argv = new string[] { "-q" };
                    if (0 == libpl.PL_initialise(local_argv.Length, local_argv))
                        throw new PlLibException("failed to initialize");
                    RegisterThread121A(thread);              
                }

            }
        }

        private static bool PrologThreadAtExit()
        {
            //throw new NotImplementedException();
            return true;
        }
        private static bool PrologThreadAtExitGlobal()
        {
            //throw new NotImplementedException();
            return true;
        }

        static IntPtr GetCurrentEngine()
        {
            IntPtr _iEngineNumberReallyCurrent = IntPtr.Zero;
            int iRet2 = libpl.PL_set_engine(PL_ENGINE_CURRENT_PTR, ref _iEngineNumberReallyCurrent);
            if (iRet2 == libpl.PL_ENGINE_INVAL)
            {
                return IntPtr.Zero;
            }
            CheckIRet(iRet2);
            return _iEngineNumberReallyCurrent;
        }
        private static void EnsureEngine(IntPtr _iEngineNumber)
        {
            IntPtr _iEngineNumberReallyCurrent = IntPtr.Zero;
            int iRet2 = libpl.PL_set_engine(PL_ENGINE_CURRENT_PTR, ref _iEngineNumberReallyCurrent);
            CheckIRet(iRet2);
            if (_iEngineNumber == _iEngineNumberReallyCurrent)
            {
                return;
            }

            int iRet = libpl.PL_set_engine(_iEngineNumber, ref _iEngineNumberReallyCurrent);           
            if (libpl.PL_ENGINE_SET == iRet)
            {
                EnsureEngine(_iEngineNumber);
                return;
            }
            CheckIRet(iRet);
        }

        private static void CheckIRet(int iRet)
        {
            switch (iRet)
            {
                case libpl.PL_ENGINE_SET:
                    {
                        break; // all is fine!
                    }
                case libpl.PL_ENGINE_INVAL:
                    throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                case libpl.PL_ENGINE_INUSE:
                    throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                default:
                    throw (new PlLibException("Unknown return from PlSetEngine = " + iRet));
            }
        }

        public static void RegisterThread121T(Thread thread)
        {
            if (thread == CreatorThread) return;
            lock (ThreadRegLock)
            {
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                int oldSelf;
                bool threadHasSelf = threadToEngine.TryGetValue(thread.ManagedThreadId, out oldSelf);
                if (!threadHasSelf)
                {
                    //if (thread == CreatorThread) return;
                    IncrementUseCount(thread);
                    threadToEngine[thread.ManagedThreadId] = libpl.PL_thread_attach_engine(IntPtr.Zero);
                }
                IntPtr _iEngineNumber = IntPtr.Zero;
                int iRet = libpl.PL_set_engine(PL_ENGINE_CURRENT_PTR, ref _iEngineNumber);
                if (libpl.PL_ENGINE_SET == iRet) return;
                CheckIRet(iRet);
            }
        }

        public static void RegisterThread121Leak(Thread thread)
        {
            lock (ThreadRegLock)
            {
                IncrementUseCount(thread);
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                int self = libpl.PL_thread_self();
                int oldSelf;
                Thread otherThread;
                bool plthreadHasThread = engineToThread.TryGetValue(self, out otherThread);
                bool threadHasSelf = threadToEngine.TryGetValue(thread.ManagedThreadId, out oldSelf);
                bool plThreadHasDifferntThread = false;
                GCHandle.Alloc(thread, GCHandleType.Normal);
                if (plthreadHasThread)
                {
                    plThreadHasDifferntThread = otherThread != thread;
                }
                if (threadHasSelf)
                {
                    if (self < 1)
                    {
                        Embedded.Debug("self < 1: {0}", thread);
                        return; //maybe mnot fine                       
                    }
                    if (plThreadHasDifferntThread)
                    {
                        Embedded.Debug("plThreadHasDifferntThread {0}", thread);
                        return; //maybe mnot fine       
                    }
                    if (thread == CreatorThread) return;
                    int ret0 = libpl.PL_thread_attach_engine(IntPtr.Zero);
                    //int iRet = CheckEngine();
                    return; // all was fine;
                }
                // thread never had engine
                int ret = libpl.PL_thread_attach_engine(IntPtr.Zero);
                int self0 = libpl.PL_thread_self();
                engineToThread[self0] = thread;
                threadToEngine[thread.ManagedThreadId] = self0;
                RegisterThread121Leak(thread);
                return;
            }
        }

        /// <summary>
        /// FIX ME!!
        /// </summary>
        /// <param name="thread"></param>
        public static void RegisterThread12Many(Thread thread)
        {
            lock (ThreadRegLock)
            {
                IncrementUseCount(thread);
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                PlMtEngine oldSelf;
                if (ThreadEngines.TryGetValue(thread.ManagedThreadId, out oldSelf))
                {
                    oldSelf.PlSetEngine();
                    return;
                }
                try
                {
                    //var _iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                    oldSelf = new PlMtEngine();
                    oldSelf.PlSetEngine();
                    ThreadEngines.Add(thread.ManagedThreadId, oldSelf);
                }
                catch (Exception)
                {                    
                    throw;
                }
            }
        }

        public static void RegisterThreadOrig(Thread thread)
        {
            lock (ThreadRegLock)
            {
                int regs;
                if (!ThreadRegisterations.TryGetValue(thread, out regs))
                {
                    ThreadRegisterations[thread] = 1;
                }
                else
                {
                    ThreadRegisterations[thread] = regs + 1;
                }
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                int self = libpl.PL_thread_self();
                IntPtr _iEngineNumber;
                IntPtr _oiEngineNumber;
                Thread otherThread;
                bool threadOnceHadEngine = SafeThreads.TryGetValue(thread.ManagedThreadId, out _iEngineNumber);
                bool plthreadHasThread = engineToThread.TryGetValue(self, out otherThread);
                bool plThreadHasDifferntThread = false;
                GCHandle.Alloc(thread, GCHandleType.Normal);
                if (plthreadHasThread)
                {
                    plThreadHasDifferntThread = otherThread != thread;
                }
                if (self < 0 || threadOnceHadEngine)
                {
                    if (self < 1)
                    {
                        Embedded.Debug("self < 1: {0}", thread);
                        return; //maybe mnot fine                       
                    }
                    if (plThreadHasDifferntThread)
                    {
                        Embedded.Debug("plThreadHasDifferntThread {0}", thread);
                        return; //maybe mnot fine       
                    }
                    //return; // all was fine;
                    //  if (thread == CreatorThread || true) return;

                    int iRet = CheckEngine();

                    return; // all was fine;
                }
                else
                {
                    // thread never had engine
                    int ret = libpl.PL_thread_attach_engine(IntPtr.Zero);
                    int self0 = libpl.PL_thread_self();
                    if (ret == self0)
                    {
                        lock (SafeThreads) SafeThreads.Add(thread.ManagedThreadId, IntPtr.Zero);
                        engineToThread[self0] = thread;
                        //RegisterThread(thread);
                        return;
                    }
                    _iEngineNumber = GetFreeEngine();
                    lock (SafeThreads) SafeThreads.Add(thread.ManagedThreadId, _iEngineNumber);
                    int self2 = libpl.PL_thread_self();
                    if (self2 == -1)
                    {
                        if (libpl.PL_is_initialised(IntPtr.Zero, IntPtr.Zero) != 0)
                        {
                            try
                            {
                                ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                                int self3 = libpl.PL_thread_self();
                                engineToThread.Add(self3, thread);
                                return;
                            }
                            catch (Exception ex)
                            {
                                throw (new PlException("PL_create_engine : " + ex.Message));
                            }
                        }
                        else
                        {
                            //int ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                            IntPtr pNullPointer = IntPtr.Zero;
                            int iRet = libpl.PL_set_engine(_iEngineNumber, ref pNullPointer);
                            switch (iRet)
                            {
                                case libpl.PL_ENGINE_SET:
                                    {
                                        int self4 = libpl.PL_thread_self();
                                        engineToThread.Add(self4, thread);
                                        return; // all is fine!
                                    }
                                case libpl.PL_ENGINE_INVAL: throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                                case libpl.PL_ENGINE_INUSE: throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                                default: throw (new PlLibException("Unknown return from PlSetEngine"));
                            }
                            int self3 = libpl.PL_thread_self();
                            engineToThread.Add(self3, thread);
                        }
                        return;
                    }

                    engineToThread.Add(self2, thread);
                }
                /*
                //
                if (self != ret)
                {
                    engineToThread[ret] = thread;
                }

                if (engineToThread.TryGetValue(self, out otherThread))
                {
                    // All good!
                    if (otherThread == thread)
                        return;
                    bool othreadOnceHadEngine = SafeThreads.TryGetValue(otherThread, out _oiEngineNumber);
                    int ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                    if (self != ret)
                    {
                        engineToThread[ret] = thread;
                        //what does this mean?
                        SafeThreads.TryGetValue(thread, out _iEngineNumber);
                    }
                }
                libpl.PL_set_engine(libpl.PL_ENGINE_CURRENT, ref oldEngine);
                if (!OneToOneEnginesPeThread)
                {
                }
                SafeThreads.Add(thread, _iEngineNumber);
                  */
            }
        }

        private static IntPtr GetFreeEngine()
        {
            lock (FreeEngines)
            {
                if (FreeEngines.Count > 0)
                {
                    var fe = FreeEngines[0];
                    FreeEngines.RemoveAt(0);
                    return fe;
                }
            }
            return libpl.PL_create_engine(IntPtr.Zero);
        }

        public static int CheckEngine()
        {
            IntPtr _iEngineNumber;
            IntPtr pNullPointer = IntPtr.Zero;
            int iRet = libpl.PL_set_engine(PL_ENGINE_CURRENT_PTR, ref pNullPointer);
            if (libpl.PL_ENGINE_SET == iRet) return iRet;
            switch (iRet)
            {
                case libpl.PL_ENGINE_SET:
                    {
                        break; // all is fine!
                    }
                case libpl.PL_ENGINE_INVAL:
                    throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                case libpl.PL_ENGINE_INUSE:
                    throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                default:
                    throw (new PlLibException("Unknown return from PlSetEngine"));
            }

            return iRet;
        }

        static readonly List<Thread> unregisteredThreads = new List<Thread>();
        public static void DeregisterThread(Thread thread)
        {
            lock (ThreadRegLock)
            {
                int regs = DecrementUseCount(thread);
                if (regs == 0)
                {
                    if (OneToOneEnginesPeThread)
                    {
                      //  libpl.PL_thread_destroy_engine();
                    }
                    else
                    {

                    }
                    //ExitThread(thread);
                }
            }
        }

        public static void ExitThread(Thread thread)
        {
            lock (ThreadRegLock)
            {
                int self = libpl.PL_thread_self();
                IntPtr _iEngineNumber;
                lock (SafeThreads) if (!SafeThreads.TryGetValue(thread.ManagedThreadId, out _iEngineNumber))
                {
                    return;
                }
                //  if (_iEngineNumber == IntPtr.Zero) return;
                lock (SafeThreads) SafeThreads.Remove(thread.ManagedThreadId);
                var rnull = IntPtr.Zero;
                if (libpl.PL_set_engine(IntPtr.Zero, ref rnull) != 0)
                {
                    lock (FreeEngines)
                    {
                        if (_iEngineNumber != IntPtr.Zero) FreeEngines.Add(_iEngineNumber);
                    }
                    return;
                }
                if (libpl.PL_destroy_engine(_iEngineNumber) != 0)
                {
                    try
                    {
                        _iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                        lock (FreeEngines)
                        {
                            FreeEngines.Add(_iEngineNumber);
                        }
                    }
                    catch (Exception ex)
                    {
                        throw (new PlException("PL_create_engine : " + ex.Message));
                    }
                }
            }
        }

        public static void KillPrologThreads()
        {
            lock (ThreadRegLock)
            {
                lock (SafeThreads) foreach (KeyValuePair<int, IntPtr> engine in SafeThreads)
                {
                    IntPtr ptr = engine.Value;
                    if (ptr.ToInt64() > PL_ENGINE_CURRENT_PTR.ToInt64())
                    {
                        libpl.PL_destroy_engine(ptr);
                    }
                }
            }
            throw new NotImplementedException("KillPrologThreads");
        }
    }
}