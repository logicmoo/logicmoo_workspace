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

using System.Diagnostics;
using org.jpl7.fli;
#if USE_IKVM
using Type = System.Type;
#else
using JClass = System.Type;
using Type = System.Type;
#endif
using System.Collections;
using SbsSW.SwiPlCs.Callback;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using SbsSW.SwiPlCs;

namespace Swicli.Library
{
#if NET20
    public abstract class HashSet<T, TDictionary> : ICollection<T>  where TDictionary : IDictionary<T, byte>
    {
        protected TDictionary _internalDictionary;

        protected HashSet(TDictionary dictionary)
        {
            _internalDictionary = dictionary;
        }

        // implement the ICollection<T> interface
        // using your internal dictionary's Keys property

        // for example:
        public bool Add(T value)
        {
            if (!_internalDictionary.ContainsKey(value))
            {
                _internalDictionary.Add(value, 0);
                return true;
            }
            return false;
        }

        // etc.


        #region ICollection<T> Members

        void ICollection<T>.Add(T item)
        {
            throw new NotImplementedException();
        }

        void ICollection<T>.Clear()
        {
            throw new NotImplementedException();
        }

        bool ICollection<T>.Contains(T item)
        {
            throw new NotImplementedException();
        }

        void ICollection<T>.CopyTo(T[] array, int arrayIndex)
        {
            throw new NotImplementedException();
        }

        int ICollection<T>.Count
        {
            get { throw new NotImplementedException(); }
        }

        bool ICollection<T>.IsReadOnly
        {
            get { throw new NotImplementedException(); }
        }

        bool ICollection<T>.Remove(T item)
        {
            throw new NotImplementedException();
        }

        #endregion

        #region IEnumerable<T> Members

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            throw new NotImplementedException();
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            throw new NotImplementedException();
        }

        #endregion
    }

    public class HashSet<T> : HashSet<T, Dictionary<T, byte>>
    {

        public HashSet() : base(new Dictionary<T, byte>()) { }


        internal bool Contains(string s)
        {
            throw new NotImplementedException();
        }
    }
#endif

    public partial class PrologCLR
    {
        [ThreadStatic]
        public static bool PreserveObjectType;

        [ThreadStatic]
        static ThreadEngineObjectTracker _locallyTrackedObjects;
        static ThreadEngineObjectTracker LocallyTrackedObjects
        {
            get
            {
                if (_locallyTrackedObjects == null)
                {
                    _locallyTrackedObjects = new ThreadEngineObjectTracker();
                }
                return _locallyTrackedObjects;
            }
        }

        public static bool DebugRefs = true;
        public static bool StrictRefs = true;
        readonly static public Dictionary<object, TrackedObject> ObjToTag = new Dictionary<object, TrackedObject>();
        readonly static public Dictionary<string, TrackedObject> TagToObj = new Dictionary<string, TrackedObject>();
        readonly static public Hashtable TagToST = new Hashtable();
        readonly static public HashSet<string> NeedSweep = new HashSet<string>();
        public static object tag_to_object(string s)
        {
            return tag_to_object(s, false);
        }

        [PrologVisible]
        public static bool cliAddTag(PlTerm taggedObj, PlTerm tagString)
        {
            object o = GetInstance(taggedObj);
            string tagname = (string)tagString;
            lock (ObjToTag)
            {

                TrackedObject s;
                long adr;
                GCHandle iptr = GetIptr(o, out adr, true);
                var hc = tagname.GetHashCode();

                s = new TrackedObject(o)
                        {
                            TagName = tagname,
                            Pinned = iptr,
                            addr = adr,
                            HashCode = hc,
                            Heaped = true
                        };
                ObjToTag[o] = s;
                TagToObj[tagname] = s;

            }
            return true;
        }
        [PrologVisible]
        public static bool cliRemoveTag(PlTerm tagString)
        {
            string tagname = (string)tagString;
            TrackedObject to;

            lock (ObjToTag)
            {
                if (TagToObj.TryGetValue(tagname, out to))
                {
                    TagToObj.Remove(tagname);
                    to.StrongHold = null;
                    TagToST[tagname] = "cliRemoveTag";
                    ObjToTag.Remove(to.Value);
                    //TODO?? to.RemoveRef();
                }
            }
            return true;
        }

        public static object tag_to_object(string s, bool allowConstants)
        {
            if (s == "true") return true;
            if (s == "false") return false;
            if (s == "null") return null;
            if (String.IsNullOrEmpty(s) || s == "void" /*|| !s.StartsWith("C#")*/)
            {
                Embedded.Warn("tag_to_object: {0} ", s);
                return null;
            }
            lock (ObjToTag)
            {
                TrackedObject o;
                if (TagToObj.TryGetValue(s, out o))
                {
                    if (DebugRefs)
                    {
                        if (NeedSweep.Contains(s))
                        {
                          //  Warn("tag_to_object: {0} was missing due to {1}", s, TagToST[s]);
                        }
                    }
                    if (UsePerThreadObjectTracker) LocallyTrackedObjects.AddTracking(o);
                    return o.Value;
                }
                if (DebugRefs)
                {
                    Embedded.Warn("tag_to_object: {0} was missing due to {1}", s, TagToST[s]);
                }
#if USE_IKVM
                return Prolog.tag_to_object(s);
#else
                return null;
#endif
            }
        }

        readonly public static GCHandle NULL_GCHANDLE = default(GCHandle);
        public static long nextTrackingNum = 666;
        private static GCHandle GetIptr(object o, out long adr, bool pinObj)
        {
            GCHandle iptr;
            if (pinObj)
            {
                iptr = PinObject(o);
                adr = ((IntPtr) iptr).ToInt64();
            }
            else
            {
                iptr = NULL_GCHANDLE;
                adr = nextTrackingNum++;
                if (nextTrackingNum == Int64.MaxValue)
                {
                    nextTrackingNum = Int64.MinValue;
                }
            }
            return iptr;
        }

        private static bool ShouldNotTrack(Type t)
        {
            if (t.IsPrimitive) return true;
            if (IsStructRecomposable(t)) return true;
            if (t.IsArray)
            {
                return true;
            }
            if (typeof(List<>).IsAssignableFrom(t))
            {
                return true;
            }
            return false;
        }

        private static int Tracker_FreeAtom(uint arg)
        {
            FreeTag(libpl.PL_atom_chars(arg), false);
            return 1;
        }

        public static bool MakeArrayImmediate = false;           
        [ThreadStatic]
        public static bool MakeNoRefs;
        [ThreadStatic]
        public static bool MadeARef;

        public static string object_to_tag(object o)
        {
            if (o == null)
            {
                Embedded.Warn("object_to_tag: NULL");
                return null;
            }

            Type t = o.GetType();
            if (ShouldNotTrack(t))
            {
                if (DebugRefs) Embedded.Debug("object_to_tag:{0} from {1}", t, o);
            }

            lock (ObjToTag)
            {
                TrackedObject s;
                if (ObjToTag.TryGetValue(o, out s))
                {
                    if (UsePerThreadObjectTracker) LocallyTrackedObjects.AddTracking(s);
                    return s.TagName;
                }

                long adr;
                GCHandle iptr = GetIptr(o, out adr, false);

                string tagname = "C#" + adr;
                var hc = tagname.GetHashCode();
                if (!InstalledAtomCGHook) InstallAtomGCHook();
                //libpl.PL_new_atom(tagname);
                s = new TrackedObject(o)
                        {
                            TagName = tagname,
                            Pinned = iptr,
                            addr = adr,
                            Heaped = false,
                            HashCode = hc
                        };
                ObjToTag[o] = s;
                TagToObj[tagname] = s;

                if (UsePerThreadObjectTracker) LocallyTrackedObjects.AddTracking(s);
                if (DebugRefs && ObjToTag.Count % 10000 == 0)
                {
                    ConsoleTrace("ObjToTag=" + ObjToTag);
                }

                return s.TagName;
            }
            //return jpl.fli.Prolog.object_to_tag(o);
        }

        private static bool IsTaggedObject(PlTerm info)
        {
            return info.IsCompound && info.Name == "@";
        }

        [PrologVisible]
        static public bool cliToRef(PlTerm obj, PlTerm str)
        {
            if (!str.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliToRef(obj, plvar) && SpecialUnify(str, plvar);
            }
            //if (obj.IsString) return str.Unify(obj);
            if (obj.IsVar) return str.Unify(obj);
            object o = GetInstance(obj);
            return UnifyTagged(o, str);
        }

        [PrologVisible]
        static public bool cliToImmediate(PlTerm valueIn, PlTerm valueOut)
        {
            if (valueIn.IsVar)
            {
                if (StrictRefs) return Embedded.Error("Cant find instance {0}", valueIn);
                return valueIn.Unify(valueOut);
            }
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliToImmediate(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(valueIn);
            return PlSucceedOrFailOrError(UnifyToPrologImmediate(getInstance, valueOut));
        }

        public static bool UnifyTagged(object c, PlTerm term2)
        {
            string tag = object_to_tag(c);
            var t1 = term2;
            if (t1.IsCompound)
            {
                t1 = t1[1];
            }
            else if (t1.IsVar)
            {
                int retcode = AddTagged(t1.TermRef, tag);
                if (retcode == libpl.PL_succeed)
                {
                    return true;
                }
                return BP();
            }
            //var t2 = new PlTerm(t1.TermRef + 1);

            //libpl.PL_put_atom_chars(t1.TermRef + 1, tag);
            bool ret = t1.UnifyAtom(tag); // = t1;
            return ret;
        }

        private static int AddTagged(uint TermRef, string tag)
        {
            /*
            PlTerm term2 = new PlTerm(TermRef);
            var t1 = term2;
            if (t1.IsCompound)
            {
                t1 = t1[1];
            }
            else if (t1.IsVar)
            {
            }
            //var t2 = new PlTerm(t1.TermRef + 1);

            //libpl.PL_put_atom_chars(t1.TermRef + 1, tag);
            bool ret = t1.Unify(tag); // = t1;*/
            uint fid = 0;// libpl.PL_open_foreign_frame();
            uint nt = libpl.PL_new_term_ref();
            libpl.PL_cons_functor_v(nt,
                                    OBJ_1,
                                    new PlTermV(PlTerm.PlAtom(tag)).A0);
            PlTerm termValue = new PlTerm(nt);
            PlTerm termVar = new PlTerm(TermRef);
            int retcode = libpl.PL_unify(TermRef, nt);
            if (fid > 0) libpl.PL_close_foreign_frame(fid);
            if (retcode != libpl.PL_succeed)
            {
                
                if (retcode == libpl.PL_fail)
                {
                    //libpl.PL_put_term(nt, TermRef);
                    return retcode;
                }
                //libpl.PL_put_term(nt, TermRef);
                return retcode;
            }
            return retcode;
        }

        protected static uint OBJ_1
        {
            get
            {
                if (_obj1 == default(uint))
                {
                    _obj1 = libpl.PL_new_functor(libpl.PL_new_atom("@"), 1);
                }
                return _obj1;
            }
        }

        [PrologVisible]
        static public bool cliTrackerBegin(PlTerm trackerOut)
        {
            ThisThreadTracked++;
            var newTracking = LocallyTrackedObjects.CreateFrame();
            return UnifyTagged(newTracking, trackerOut);
        }

        [PrologVisible]
        static public bool cliTrackerFree(PlTerm trackerIn)
        {
            ThisThreadTracked--;
            TrackedFrame tc0 = (TrackedFrame)GetInstance(trackerIn);
            if (tc0 != null)
            {
                LocallyTrackedObjects.RemoveFrame(tc0);
                return true;
            }
            return false;
        }

        [PrologVisible]
        static public bool cliFree(PlTerm taggedObject)
        {
            if (taggedObject.IsVar)
            {
                return false;
            }
            string tag;
            if (taggedObject.IsCompound)
            {
                tag = taggedObject[1].Name;
            }
            else if (taggedObject.IsAtomOrString)
            {
                tag = taggedObject.Name;
            }       
            else
            {
                return true;
            }
            return FreeTag(tag, true);
        }

        private static bool FreeTag(string tag, bool forced)
        {
            lock (TagToObj)
            {
                TrackedObject oref;
                if (TagToObj.TryGetValue(tag, out oref))
                {
                    if (oref.Heaped && !forced)
                    {
                        return true;
                    }
                    oref.Heaped = false;
                    oref.RemoveRef();
                    if (oref.Refs > 0 && forced)
                    {
                        return RemoveTaggedObject(tag);
                    }
                    return true;
                }
                return forced;
            }
        }

        /// <summary>
        /// This is no longer needed since we use atom GC (maybe)
        /// </summary>
        public static bool AlwaysUsePerThreadObjectTracker = true;

        [ThreadStatic] public static int ThisThreadTracked;// = 0;

        public static bool UsePerThreadObjectTracker
        {
            get { return AlwaysUsePerThreadObjectTracker || ThisThreadTracked > 0; }
        }

        private static bool InstalledAtomCGHook = false;
        private static readonly object InstalledAtomCGHookLock = new object();
        [PrologVisible]
        public static bool InstallAtomGCHook()
        {
            lock (InstalledAtomCGHookLock)
            {
                if (InstalledAtomCGHook) return true;
                InstalledAtomCGHook = true;
            }
            return ForceInstallAtomGCHook();
        }
        public static bool ForceInstallAtomGCHook()
        {
            PL_agc_hook_t old = libpl.PL_agc_hook(Tracker_FreeAtom);
            if (old == null) return true;
            return true;
        }

        [PrologVisible]
        static public bool cliHeap(PlTerm taggedObject)
        {
            if (taggedObject.IsVar)
            {
                return false;
            }
            string tag;
            if (taggedObject.IsCompound)
            {
                tag = taggedObject[1].Name;
            }
            else if (taggedObject.IsAtomOrString)
            {
                tag = taggedObject.Name;
            }
            else
            {
                return true;
            }
            lock (TagToObj)
            {
                TrackedObject oref;
                if (TagToObj.TryGetValue(tag, out oref))
                {
                    oref.Heaped = true;
                    return true;
                }
                return false;
            }
        }

        public static bool CantPin(object pinme)
        {
            return pinme.GetType().Name.Contains("e");
        }

        public readonly static List<object> PinnedObjects = new List<object>();
        public static GCHandle PinObject(object pinme)
        {
            lock (PinnedObjects) PinnedObjects.Add(pinme);
            return GCHandle.Alloc(pinme);
#if false
            if (true) return pinme;
            try
            {
                if (CantPin(pinme))
                {
                    GCHandle.Alloc(pinme, GCHandleType.Normal);
                    return pinme;
                }
                if (!Monitor.TryEnter(pinme))
                {
                    return pinme;
                }
                Monitor.Exit(pinme);
                GCHandle gch = GCHandle.Alloc(pinme, GCHandleType.Pinned);
                GCHandle gch2 = GCHandle.Alloc(pinme, GCHandleType.Pinned);
                if (gch != gch2)
                {

                }
            }
            catch (Exception)
            {
                GCHandle gch = GCHandle.Alloc(pinme, GCHandleType.Normal);
            }
            return pinme;
#endif
        }
        public static object UnPinObject(object pinme)
        {
            try
            {
                GCHandle gch = GCHandle.Alloc(pinme, GCHandleType.Pinned);
                gch.Free();
            }
            catch (Exception)
            {
                GCHandle gch = GCHandle.Alloc(pinme, GCHandleType.Normal);
                gch.Free();
            }
            return pinme;
        }

        public static bool SweepAtomGC = true;
        public static bool RemoveTaggedObject(string tag)
        {
            lock (TagToObj)
            {
                TrackedObject obj;
                if (TagToObj.TryGetValue(tag, out obj))
                {
                    //UnPinObject(obj);
                    TagToST[tag] = (new StackTrace(true)).ToString();
                    if (SweepAtomGC)
                    {
                        lock (NeedSweep) NeedSweep.Add(tag);
                        obj.StrongHold = null;
                        if (obj.Weak.IsAlive)
                        {
                            return true;
                        } else
                        {
                            if (DebugRefs)
                            {
                                Embedded.Debug("Still alive: " + obj);
                            }
                        }
                    }
                    TagToObj.Remove(tag);
                    if (false && obj is IDisposable)
                    {
                        try
                        {
                            ((IDisposable)obj).Dispose();
                        }
                        catch (Exception e)
                        {
                            if (DebugRefs) Embedded.Warn("Dispose of {0} had problem {1}", obj, e);
                        }
                    }
                    if (obj.Pinned != NULL_GCHANDLE)
                    {
                        obj.Pinned.Free();
                    }
                    return ObjToTag.Remove(obj);
                }
                return false;
            }
        }
        private static int PlObject(uint TermRef, object o)
        {
            var tag = object_to_tag(o);
            AddTagged(TermRef, tag);
            return libpl.PL_succeed;
#if plvar_pins
                PlRef oref;
                if (!objectToPlRef.TryGetValue(o, out oref))
                {
                    objectToPlRef[o] = oref = new PlRef();
                    oref.Value = o;
                    oref.CSType = o.GetType();
                    oref.Tag = tag;
                    lock (atomToPlRef)
                    {
                        PlRef oldValue;
                        if (atomToPlRef.TryGetValue(tag, out oldValue))
                        {
                            Warn("already a value for tag=" + oldValue);
                        }
                        atomToPlRef[tag] = oref;
                    }
#if PLVARBIRTH
                    Term jplTerm = JPL.newJRef(o);
                    oref.JPLRef = jplTerm;

                    Int64 ohandle = TopOHandle++;
                    oref.OHandle = ohandle;
                    // how do we track the birthtime?
                    var plvar = oref.Variable = PlTerm.PlVar();
                    lock (termToObjectPins)
                    {
                        PlRef oldValue;
                        if (termToObjectPins.TryGetValue(ohandle, out oldValue))
                        {
                            Warn("already a value for ohandle=" + oldValue);
                        }
                        termToObjectPins[ohandle] = oref;
                    }
                    //PL_put_integer
                    oref.Term = comp("$cli_object", new PlTerm((long) ohandle), plvar);
#else
                    oref.Term = comp("@", PlTerm.PlAtom(tag));
#endif
                    return -1; // oref.Term;
                }
                else
                {
                    oref.Term = comp("@", PlTerm.PlAtom(tag));
                    return -1; // oref.Term;
                }
#endif
        }
    }

    public class TrackedObject : IComparable<TrackedObject>
    {
        public string TagName;
        public int Refs = 0;
        public object Value
        {
            get
            {
                if (StrongHold != null) return StrongHold;
                if (IsValue)
                {
                    throw new KeyNotFoundException("Tracked ValueType has been collected " + this);
                }
                if (!Weak.IsAlive) throw new KeyNotFoundException("Tracked object has been collected " + this);
                return Weak.Target;
            }
        }
        public GCHandle Pinned;
        public long addr;
        public bool Heaped = false;
        public int HashCode;
        public Thread LastThread;
        public object StrongHold;
        public WeakReference Weak;
        public readonly bool IsValue;

        public TrackedObject(object value)
        {
            StrongHold = value;
            IsValue = value is ValueType;
            Weak = new WeakReference(StrongHold);
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }
            TrackedObject other = (TrackedObject)obj;
            return Pinned == other.Pinned && addr == other.addr;
        }

        public override int GetHashCode()
        {
            return HashCode;
        }

        public long ToInt64()
        {
            return addr;
        }

        #region IComparable<ObjectWRefCounts> Members

        public int CompareTo(TrackedObject other)
        {
            return ToInt64().CompareTo(other.ToInt64());
        }

        #endregion

        public void RemoveRef()
        {
            Refs--;
            if (Refs == 0 && !Heaped)
            {
                PrologCLR.RemoveTaggedObject(TagName);
            }
        }

        public override string ToString()
        {
            string vs;
            try
            {
                vs = Value.ToString();
            }
            catch (Exception e)
            {
                vs = "" + e;
            }
            return TagName + " for " + vs;
        }

        public void AddRef()
        {
            Refs++;
        }
    }
    public class TrackedFrame
    {
        HashSet<TrackedObject> TrackedObjects;
        public void AddTracking(TrackedObject info)
        {
            if (TrackedObjects == null)
            {
                TrackedObjects = new HashSet<TrackedObject>();
            }
            if (TrackedObjects.Add(info))
            {
                info.AddRef();
            }
        }

        public void RemoveRefs()
        {
            if (TrackedObjects == null) return;
            foreach (var oref in TrackedObjects)
            {
                oref.RemoveRef();
            }
        }
        public TrackedFrame Prev;
    }
    internal class ThreadEngineObjectTracker
    {
        private TrackedFrame CurrentTrackedFrame = null;
        public ThreadEngineObjectTracker()
        {
            CurrentTrackedFrame = new TrackedFrame();
        }

        public TrackedFrame CreateFrame()
        {
            if (CurrentTrackedFrame == null)
            {
                CurrentTrackedFrame = new TrackedFrame();
                return CurrentTrackedFrame;
            }
            TrackedFrame newTrackedFrame = new TrackedFrame { Prev = CurrentTrackedFrame };
            CurrentTrackedFrame = newTrackedFrame;
            return newTrackedFrame;
        }
        public TrackedFrame PopFrame()
        {
            if (CurrentTrackedFrame == null)
            {
                return null;
            }
            TrackedFrame old = CurrentTrackedFrame;
            CurrentTrackedFrame = old.Prev;
            old.RemoveRefs();
            return old;
        }

        public void AddTracking(TrackedObject info)
        {
            if (CurrentTrackedFrame == null)
            {
                return;
            }
            else
            {
                if (CurrentTrackedFrame.Prev == null)
                {
                    Thread lt = info.LastThread;
                    Thread ct = Thread.CurrentThread;
                    if (ct == lt)
                    {
                        return;
                    }
                    info.LastThread = ct;
                }
                CurrentTrackedFrame.AddTracking(info);
            }
        }

        public bool RemoveFrame(TrackedFrame frame)
        {
            if (CurrentTrackedFrame == frame)
            {
                PopFrame();
                return true;
            }
            else
            {
                if (PrologCLR.DebugRefs)
                {
                    Embedded.Debug("Removing wierd frame{0}", frame);
                }
                frame.RemoveRefs();
                return false;
            }
        }
#if plvar_pins
        public static Dictionary<Int64, PlRef> termToObjectPins = new Dictionary<Int64, PlRef>();
        public static Dictionary<object, PlRef> objectToPlRef = new Dictionary<object, PlRef>();
        public static Dictionary<string, PlRef> atomToPlRef = new Dictionary<string, PlRef>();
#endif
    }
#if plvar_pins
    public class PlRef
    {
        public object Value;
        public PlTerm Term;
        public Int64 OHandle;
        public PlTerm Variable;
        public Type CSType;
        public Term JPLRef;
        public string Tag;
    }
#endif
}
