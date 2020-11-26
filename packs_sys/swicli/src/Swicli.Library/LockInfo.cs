using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;


namespace Swicli.Library
{
    public class LockInfo
    {

        private static object oneSmarty = new object();
        public static bool DisabledWatcher = false;
        public static TimeSpan WatcherMax = TimeSpan.FromSeconds(40);

/*
        public class WaiterThread
        {
            public DateTime StartTime;
            public DateTime EndTime;
            public ManualResetEvent mre = new ManualResetEvent(false);
            public Thread Thread;
            public int depth = 0;

            public WaiterThread(string name, Thread ct)
            {
                Thread = ct;
            }
        }

        public class Watcher //: LockInfo
        {
            private string Named;
            public Queue<WaiterThread> Waiters = new Queue<WaiterThread>();
            public Dictionary<Thread,WaiterThread> WaiterThreads = new Dictionary<Thread, WaiterThread>();

            public Watcher(string type)
            //  : base(type)
            {
                Named = type;
            }

            public void ExitLock(WaiterThread ct)
            {
                if (ct.depth > 0)
                {
                    ct.depth--;
                    return;
                }
                ct.EndTime = DateTime.Now;
                TimeSpan time = ct.EndTime - ct.StartTime;
                if ((time) > WatcherMax)
                {
                    throw new InvalidOperationException("Over time on watcher=" + Named + " for thread=" + ct);   
                }
                //if (Waiters.Count==0) return;
                var prev = Waiters.Peek();
                if (prev.Thread == ct.Thread)
                {
                    WaiterThreads.Remove(ct.Thread);
                    RemoveFirst();
                }
            }

            private void RemoveFirst()
            {
                Waiters.Dequeue();
                if (Waiters.Count == 0) return;
                Waiters.Peek().mre.Set();
            }

            public void EnterLock(WaiterThread ct)
            {
                ct.StartTime = DateTime.Now;
            }


            public WaiterThread AquireWaiter(string name, Thread ct)
            {
                if (Waiters.Count == 0)
                {
                    var wt = new WaiterThread(name, ct);
                    Waiters.Enqueue(wt);
                    wt.mre.Set();
                    return wt;
                }
                WaiterThread wt2;
                if (!WaiterThreads.TryGetValue(ct, out wt2))
                {
                    WaiterThreads[ct] = wt2 = new WaiterThread(name, ct);
                    Waiters.Enqueue(wt2);
                }
                wt2.depth++;
                return wt2;
            }
        }
        private static readonly Dictionary<object, Watcher> Watchers = new Dictionary<object, Watcher>();
        public static Watcher CreateWatcher(string lockType, object codeLock)
        {
            if (codeLock is Watcher) return (Watcher) codeLock;
            lock (Watchers)
            {
                Watcher lockinfo;
                if (!Watchers.TryGetValue(codeLock, out lockinfo))
                {
                    return Watchers[codeLock] = new Watcher(lockType);
                }
                return lockinfo;
            }
        }
        public static object Watch(object o, params string[] named)
        {
            if (DisabledWatcher) return o;
            string name = (named != null && named.Length > 0)
                              ? named[0]
                              : "SmartWatcher:" + o;
            WaiterThread waiter;
            object locker;
            lock (oneSmarty)
            {
                locker = SmartWatcher0(o, name, out waiter);
            }
            if (!waiter.mre.WaitOne(WatcherMax))
            {
                throw new InvalidOperationException("Over time on " + name);
            }
            return locker;
        }

        public static object SmartWatcher0(object o, string name, out WaiterThread waiter)
        {
            string sts = GetStackTraceString();

            Watcher info = CreateWatcher(name, o);
            var ct = Thread.CurrentThread;
            waiter = info.AquireWaiter(name, ct);
            var watchLock = info;// new object();
            WaiterThread waiter1 = waiter;
            new Thread(() =>
                           {
                               string osts = sts;
                               lock (oneSmarty) info.EnterLock(waiter1);
                               Thread.Sleep(5000);
                               if (!Monitor.TryEnter(watchLock, WatcherMax))
                               {
                                   throw new InvalidOperationException("Over time on " + name);
                               }
                               else
                               {
                                   Monitor.Exit(watchLock);
                               }
                               lock (oneSmarty) info.ExitLock(waiter1);

                           }).Start();
            return watchLock;
        }
        */

        public class LWatcher
        {
            public string StackTraceString;
            private readonly string Name;

            public LWatcher(string name)
            {
                this.Name = name;
            }

            public override string ToString()
            {
                return Name + ": " + StackTraceString;
            }
        }

        private static readonly Dictionary<object, LWatcher> Watchers = new Dictionary<object, LWatcher>();
        public static LWatcher CreateWatcher(string lockType, object codeLock)
        {
            if (codeLock is LWatcher) return (LWatcher)codeLock;
            lock (Watchers)
            {
                LWatcher lockinfo;
                if (!Watchers.TryGetValue(codeLock, out lockinfo))
                {
                    return Watchers[codeLock] = new LWatcher(lockType);
                }
                return lockinfo;
            }
        }
        public static object Watch(object o, params string[] named)
        {
            return o;
            if (DisabledWatcher) return o;
            string name = (named != null && named.Length > 0)
                              ? named[0]
                              : "SmartWatcher:" + o;
            LWatcher locker;
            //lock (oneSmarty)
            {
                locker = CreateWatcher(name, o);
                if (Monitor.TryEnter(locker))
                {
                    Monitor.Exit(locker);
                    locker.StackTraceString = GetStackTraceString();
                }
            }
            return locker;
        }

        public static IList<T> CopyOf<T>(List<T> list)
        {
            if (list == null) return new List<T>();
            lock (list)
            {
                return list.ToArray();
            }
        }
        public static IEnumerable<object> CopyOf<T>(System.Collections.ICollection list)
        {
            var copy = new List<object>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var o in copy)
                {
                    copy.Add(o);
                }
            }
            return copy;
        }

        public static IList<T> CopyOf<T>(IEnumerable<T> list)
        {
            var copy = new List<T>();
            if (list == null) return copy;
            lock (list)
            {
                copy.AddRange(list);
            }
            return copy;
        }

        public static IDictionary<K, V> CopyOf<K, V>(IDictionary<K, V> list)
        {
            var copy = new Dictionary<K, V>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var kv in list)
                {
                    copy.Add(kv.Key, kv.Value);
                }
            }
            return copy;
        }

        public static bool DontRealyLock = true;
        public static R WeaklyLock<R>(object lockObject, TimeSpan maxWaitTryEnter, Func<R> action, Func<string> operationType)
        {
            if (DontRealyLock)
            {
                return action();
            }
            Action needsExit = MonitorTryEnter(operationType(), lockObject, maxWaitTryEnter);
            try
            {
                return action();
            }
            finally
            {
                needsExit();
            }
        }
        public static void WeaklyLock(object lockObject, TimeSpan maxWaitTryEnter, Action action, Func<string> operationType)
        {
            if (DontRealyLock)
            {
                action(); ;
                return;
            }
            Action needsExit = MonitorTryEnter(operationType(), lockObject, maxWaitTryEnter);
            try
            {
                action();
            }
            finally
            {
                needsExit();
            }
        }

        public static Action MonitorTryEnter(string lockType, object codeLock, TimeSpan maxWaitTryEnter)
        {
            //lock (LockInfos)
            {
                Thread currentThread = Thread.CurrentThread;
                bool needsExit = Monitor.TryEnter(codeLock, maxWaitTryEnter);

                if (!needsExit)
                {
                    lock (LockInfos)
                    {
                        LockInfo made = CantEnterUserThread(lockType, currentThread, codeLock);
                        return () => { };
                    }
                }
                else
                {
                    lock (LockInfos)
                    {
                        LockInfo made = LockInfo.EnterUserThread(lockType, currentThread, codeLock);
                        return () =>
                                   {
                                       lock (LockInfos)
                                       {
                                           try
                                           {
                                               LockInfo.ExitUserThread(lockType, made, codeLock);
                                           }
                                           finally
                                           {
                                               if (codeLock != null)
                                               {
                                                   Monitor.Exit(codeLock);
                                               }
                                           }
                                       }
                                   };
                    }
                }
            }
        }

        private static LockInfo CantEnterUserThread(string lockType, Thread currentThread, object codeLock)
        {
            LockInfo info = LockInfo.FindLockInfo(codeLock);
            string infostring = "Cannot get lock " + lockType;
            string newVariable = infostring + "in " + (info.StartTime - DateTime.Now) + " on " + info;
            writeDebugLine(newVariable);
            info.MoreInfoWST(infostring);
            return info;
        }

        public static LockInfo FindLockInfo(object codeLock)
        {
            LockInfo info = null;
            lock (LockInfos)
            {
                if (LockInfos.TryGetValue(codeLock, out info))
                {
                }
            }
            return info;
        }
        
        public static LockInfo EnterUserThread(string lockType, Thread currentThread, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(codeLock);
                if (info != null)
                {
                    if (info.FirstThread == Thread.CurrentThread)
                    {
                        info.MoreInfo("entering " + lockType);
                        info.needsUnlock++;
                        return info;
                    }
                    {
                         info.MoreInfoWST("side-entering " + lockType);
                        /*
                        string here = LockInfo.GetStackTraceString();
                        string there = info.StartStack.ToString();

                        string newVariable = "FoundLock ??! " + lockType + " " + info;
                        writeDebugLine(newVariable);
                        writeDebugLine("here: " + here);
                        writeDebugLine("there: " + there);
                        writeDebugLine(newVariable);                                  
                        info.MoreInfo("Weird Entry " + lockType);
                         */
                        info.needsUnlock++;
                        return info;
                    }
                }
                info = CreateLockInfo(lockType, codeLock);
                info.needsUnlock++;
                return info;
            }
        }

        internal static void writeDebugLine(string s)
        {
            System.Console.Error.WriteLine(s);
        }

        public static LockInfo ExitUserThread(string lockType, LockInfo lockInfo, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(codeLock);

                if (info != null)
                {
                    if (info.IsLockerCurrentThread)
                    {
                        if (codeLock != null)
                        {
                            info.needsUnlock--;
                            //if (info.needsUnlock==0)
                            {
                                info.wasUnlocked++;
                            } 
                            
                            if (info.needsUnlock == 0)
                            {
                                info.MoreInfo("Exiting " + lockType);
                                LockInfos.Remove(info);
                            } else
                            {
                                info.MoreInfo("departing " + lockType);
                            }
                        }
                    }
                }
                else
                {
                    writeDebugLine("Cannot exit lock " + lockType + " " + lockInfo);
                }
                return info;
            }
        }
        private static readonly Dictionary<object, LockInfo> LockInfos = new Dictionary<object, LockInfo>();
        public static LockInfo CreateLockInfo(string lockType, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo lockinfo;
                if (!LockInfos.TryGetValue(codeLock, out lockinfo))
                {
                    return LockInfos[codeLock] = new LockInfo(lockType);
                }
                return lockinfo;
            }
        }
        public LockInfo(string named)
        {
            Name = named;
            StartTime = DateTime.Now;
            FirstThread = Thread.CurrentThread;
            StartStack = new StackTrace(true);
            MoreInfoWST("Created" + this);
        }
        public bool IsLockerCurrentThread
        {
            get
            {
                return Thread.CurrentThread == FirstThread;
            }
        }
        public override string ToString()
        {
            string s = "LockInfo " + Name + "\n" + (DateTime.Now - StartTime);
            //s += GetExtraInfo();
            return s;
        }

        public static bool TestLock(string named, object busyTrackingLock, TimeSpan timeSpan)
        {
            if (DontRealyLock) return true;
            // return;
            if (Monitor.TryEnter(busyTrackingLock, timeSpan))
            {
                Monitor.Exit(busyTrackingLock);
                return true;
            }
            writeDebugLine("ERROR: Cant get into " + named + " " + busyTrackingLock);
            return false;
        }

        public string GetExtraInfo()
        {
            StringBuilder sb = new StringBuilder();
            lock(Waiters)
            {
                foreach (string infostring in Waiters)
                {
                    sb.AppendLine(infostring);
                }
            }
            return sb.ToString();
        }

        public readonly Thread FirstThread;
        public readonly DateTime StartTime;
        public StackTrace StartStack;
        public int wasUnlocked = 0;
        public int needsUnlock = 0;
        public readonly string Name;
        public readonly List<string > Waiters = new List<string>();

        public void MoreInfoWST(string s)
        {
            string toString1 = GetStackTraceString();
            MoreInfo(s + "\n" + toString1);
        }

        public static string GetStackTraceString()
        {
            return (new StackTrace(true)).ToString();
        }

        public void MoreInfo(string p)
        {
            lock (Waiters)
                Waiters.Add(p);
        }
    }

}