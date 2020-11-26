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
using SbsSW.SwiPlCs;

namespace Swicli.Library
{
    public struct EventHandlerInPrologKey : PrologKey
    {
        public String Module { get; set; }
        public String Name { get; set; }
        public int Arity { get; set; }
        public EventInfo Event;
        public Object Origin;
        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + Event;
        }
    }

    public partial class PrologCLR
    {
        public static Dictionary<EventHandlerInPrologKey, EventHandlerInProlog> PrologEventHandlers =
            new Dictionary<EventHandlerInPrologKey, EventHandlerInProlog>();

#if USE_MUSHDLR
        public static TaskQueueHandler PrologEventQueue = new TaskQueueHandler("PrologEventHandler");
#endif

        [PrologVisible]
        static public bool cliAddEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm prologPred)
        {
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false; 
            Type[] paramz = null;
            if (!CheckBound(memberSpec, prologPred)) return false;
            EventInfo fi = findEventInfo(memberSpec, c, ref paramz, BindingFlagsALL);
            if (fi == null)
            {
                return Embedded.Error("Cant find event {0} on {1}", memberSpec, c);
            }
            var Key = new EventHandlerInPrologKey
            {
                Name = PredicateName(prologPred),
                Module = PredicateModule(prologPred),
                Arity = PredicateArity(prologPred),
                Origin = getInstance,
                Event = fi
            };

            lock (PrologEventHandlers)
            {
                EventHandlerInProlog handlerInProlog;
                if (PrologEventHandlers.TryGetValue(Key, out handlerInProlog))
                {
                    fi.RemoveEventHandler(getInstance, handlerInProlog.Delegate);
                    PrologEventHandlers.Remove(Key);
                }
                handlerInProlog = new EventHandlerInProlog(Key);
                PrologEventHandlers.Add(Key, handlerInProlog);
                fi.AddEventHandler(getInstance, handlerInProlog.Delegate);
            }
            return true;
        }
        [PrologVisible]
        static public bool cliRemoveEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm prologPred)
        {
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            Type[] paramz = null;
            if (!CheckBound(memberSpec, prologPred)) return false;
            EventInfo fi = findEventInfo(memberSpec, c, ref paramz, BindingFlagsALL);
            if (fi == null)
            {
                return Embedded.Error("Cant find event {0} on {1}", memberSpec, c);
            }
            var Key = new EventHandlerInPrologKey
            {
                Name = PredicateName(prologPred),
                Module = PredicateModule(prologPred),
                Arity = PredicateArity(prologPred),
                Origin = getInstance,
                Event = fi
            };
            EventHandlerInProlog handlerInProlog;
            lock (PrologEventHandlers) if (PrologEventHandlers.TryGetValue(Key, out handlerInProlog))
                {
                    UnPinObject(handlerInProlog.Delegate);
                    fi.RemoveEventHandler(getInstance, handlerInProlog.Delegate);
                    PrologEventHandlers.Remove(Key);
                    return true;
                }
            return Embedded.Error("Cant find registered handler {0} for {1} on {2}", prologPred, memberSpec, c);
        }

    }

    public class EventHandlerInProlog : PrologGenericDelegate
    {
        public EventHandlerInPrologKey Key;

        public EventHandlerInProlog(EventHandlerInPrologKey key)
        {
            Key = key;
            var keyEvent = key.Event;
            var eht = keyEvent.EventHandlerType;
            SetInstanceOfDelegateType(eht);
            if (PrologArity != key.Arity)
            {
                throw new ArgumentException("Arity of needed info " + PrologArity + " does not match " + key.Arity + " for " + this);
            }
            SyncLock = Delegate;
        }

        public override string ToString()
        {
            return "EventHandlerInProlog: " + Key;
        }

        private bool knownDefined = false;
        //#pragma unsafe
        public override object CallPrologFast(object[] paramz)
        {
            //lock (oneEvtHandlerAtATime)
            {
                try
                {
                    string module = Key.Module ?? "user";
                    PrologEvents++;
                    if (!knownDefined && !PrologCLR.IsDefined(module, Key.Name, PrologArity))
                    {
                        //PrologCLR.Warn("Undefined Event Handler {0}:{1}/{2}", module, Key.Name, PrologArity);
                        return null;
                    }
                    knownDefined = true;
                    return PrologCLR.CallProlog(this, module, Key.Name, PrologArity, Key.Origin, paramz,
                                                ReturnType, true);
                }
                catch (AccessViolationException e)
                {
                    Embedded.Warn("CallProlog: {0} ex: {1}", this, e);
                    return null;
                }
                catch (Exception e)
                {
                    Embedded.Warn("CallProlog: {0} ex: {1}", this, e);

                    return null;
                }
            }
        }

        //static readonly Object oneEvtHandlerAtATime = new object();

        public static ulong PrologEvents;
    }
}
