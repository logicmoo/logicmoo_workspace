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
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using SbsSW.SwiPlCs;

namespace Swicli.Library
{
    public partial class PrologCLR
    {
        /// <summary>
        /// Create a Event Handler with a ResetEvent
        /// </summary>
        /// <param name="clazzOrInstance"></param>
        /// <param name="memberSpec"></param>
        /// <param name="blockOn"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliNewEventWaiter(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm blockOn)
        {
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            Type[] paramz = null;
            if (!CheckBound(memberSpec)) return false;
            EventInfo fi = findEventInfo(memberSpec, c, ref paramz, BindingFlagsALL);
            if (fi == null)
            {
                return Embedded.Error("Cant find event {0} on {1}", memberSpec, (object) c ?? clazzOrInstance);
            }
            WaitUntilDelegateList list = new WaitUntilDelegateList();
            list.WaitOns.Add(new WaitUntilDelegate(list, fi, getInstance));
            return blockOn.FromObject(list);
        }

        /// <summary>
        /// Add another event to be waited on
        /// </summary>
        /// <param name="clazzOrInstance"></param>
        /// <param name="memberSpec"></param>
        /// <param name="blockOn"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliAddEventWaiter(PlTerm blockOn, PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm newBlockOn)
        {
            WaitUntilDelegateList list = null;
            object getInstance1 = GetInstance(blockOn);
            var wud = getInstance1 as WaitUntilDelegate;
            if (wud == null)
            {
                if (!(getInstance1 is WaitUntilDelegateList)) return Embedded.Error("Not an instance of WaitUntilDelegate: " + blockOn);
                list = getInstance1 as WaitUntilDelegateList;
            }
            else
            {
                list = wud.parent;
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            Type[] paramz = null;
            if (!CheckBound(memberSpec, blockOn)) return false;
            EventInfo fi = findEventInfo(memberSpec, c, ref paramz, BindingFlagsALL);
            if (fi == null)
            {
                return Embedded.Error("Cant find event {0} on {1}", memberSpec, (object)c ?? clazzOrInstance);
            }
            var wud2 = new WaitUntilDelegate(list, fi, getInstance);
            list.WaitOns.Add(wud2);
            return newBlockOn.FromObject(list);
        }

        /// <summary>
        /// Block until ResetEvent occures and run the prolog code.. if it fails.. wait again for the reset event
        ///  if over maxTime return time_limit_exceeded
        /// </summary>
        /// <param name="blockOn"></param>
        /// <param name="maxTime"></param>
        /// <param name="testVarsCode"></param>
        /// <param name="exitCode"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliBlockUntilEvent(PlTerm blockOn, PlTerm maxTime, PlTerm testVarsCode, PlTerm exitCode)
        {
            WaitUntilDelegateList list = null;
            object getInstance1 = GetInstance(blockOn);
            var wud = getInstance1 as WaitUntilDelegate;
            if (wud == null)
            {
                if (!(getInstance1 is WaitUntilDelegateList)) return Embedded.Error("Not an instance of WaitUntilDelegate: " + blockOn);
                list = getInstance1 as WaitUntilDelegateList;
            }
            else
            {
                list = wud.parent;
            }

            var timeSpan = TimeSpan.FromDays(3650);
            if (maxTime.IsInteger)
            {
                timeSpan = TimeSpan.FromMilliseconds(maxTime.intValue());
            }
            else if (!maxTime.IsVar)
            {
                timeSpan = (TimeSpan) CastTerm(maxTime, typeof (TimeSpan));
            }

            DateTime expireyTime = DateTime.Now.Add(timeSpan);
            while (DateTime.Now < expireyTime)
            {
                var results = list.WaitOne(timeSpan, out wud);
                if (results == null)
                {
                    return exitCode.UnifyAtom("time_limit_exceeded");
                }
                PlTerm copyTo = PlTerm.PlVar();
                PlTermV newPlTermV = new PlTermV(testVarsCode, copyTo);
                PlCall("system", "copy_term", newPlTermV);
                PlTerm ctestVars = copyTo.Arg(0);
                PlTerm ctestCode = copyTo.Arg(1);
                PlTerm[] terms = ToTermArray(ctestVars);
                int idx = terms.Length - 1;
                int resdex = results.Length - 1;                
                while (idx >= 0 && resdex >= 0)
                {
                    terms[idx--].FromObject(results[resdex--]);
                }
                try
                {
                    if (PlCall("user", "call", new PlTermV(ctestCode)))
                        return UnifyToProlog(PlCall(null, "=", newPlTermV), exitCode) != 0;
                }
                finally
                {
                    list.Reset();
                }
            }
            return exitCode.UnifyAtom("time_limit_exceeded");
        }
    }
    public class WaitUntilDelegateList: IDisposable
    {
        public List<WaitUntilDelegate> WaitOns = new List<WaitUntilDelegate>();
        public ManualResetEvent mre = new ManualResetEvent(false);
        public object[] WaitOne(TimeSpan ts, out WaitUntilDelegate wud0)
        {
            wud0 = null;
            if (!mre.WaitOne(ts)) return null;
            foreach (WaitUntilDelegate wud in WaitOns)
            {
                var tr = wud.Result;
                if (tr != null)
                {
                    wud0 = wud;
                    return tr;
                }
            }
            return null;
        }

        internal void Set()
        {
            mre.Set();
        }
        internal void Reset()
        {
            mre.Reset();
        }
        #region IDisposable Members

        public void Dispose()
        {
            foreach (WaitUntilDelegate wud in WaitOns)
            {
                wud.Dispose();
            }
            mre.Close();
        }

        #endregion
    }
    public class WaitUntilDelegate : PrologGenericDelegate, IDisposable
    {
        public WaitUntilDelegateList parent;
        public object[] Result;
        public EventInfo Event;
        public object Instance;

        public WaitUntilDelegate(WaitUntilDelegateList re, EventInfo info, object instance)
        {
            parent = re;
            Event = info;
            Instance = instance;
            SetInstanceOfDelegateType(info.EventHandlerType);
            Event.AddEventHandler(instance, Delegate);
        }

        public override object CallProlog(params object[] paramz)
        {
            return CallPrologFast(paramz);
        }
        public override void CallPrologV(params object[] paramz)
        {
            CallPrologFast(paramz);
        }

        public override object CallPrologFast(object[] paramz)
        {
            Result = paramz;
            parent.Set();
            return null;
        }


        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            Event.RemoveEventHandler(Instance, Delegate);
        }

        #endregion
    }
}
