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
using SbsSW.SwiPlCs;

namespace Swicli.Library
{

    public struct DelegateObjectInPrologKey : PrologKey
    {
        public String Module { get; set; }
        public String Name { get; set; }
        public int Arity { get; set; }
        public Type DelegateType;
        //public PlTerm Origin;
        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + DelegateType;
        }
    }

    public partial class PrologCLR
    {

        public static Dictionary<DelegateObjectInPrologKey, DelegateObjectInProlog> PrologDelegateHandlers =
            new Dictionary<DelegateObjectInPrologKey, DelegateObjectInProlog>();

        /// <summary>
        /// 
        /// </summary>
        /// <param name="delegateClass"></param>
        /// <param name="prologPred"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible]
        static public bool cliNewDelegate(PlTerm delegateClass, PlTerm prologPred, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliNewDelegate(delegateClass, prologPred, plvar) && SpecialUnify(valueOut, plvar);
            }
            object retval = cliNewDelegateTerm(GetTypeThrowIfMissing(delegateClass), prologPred, true);
            return valueOut.FromObject(retval);
        }
        [PrologVisible]
        static public Delegate cliNewDelegateTerm(Type fi, PlTerm prologPred, bool saveKey)
        {
            if (prologPred.IsCompound)
            {
                if (prologPred.Name == "delegate")
                {
                    if (prologPred.Arity == 1)
                    {
                        return cliNewDelegateTerm(fi, prologPred.Arg(0), saveKey);
                    }
                    Type dt = GetTypeThrowIfMissing(prologPred.Arg(0));
                    var obj = cliNewDelegateTerm(dt, prologPred.Arg(1), saveKey);
                    return (Delegate)RecastObject(fi, obj, dt);
                }
                if (prologPred.Name == "@")
                {
                    return (Delegate)RecastObject(fi, tag_to_object((string)prologPred.Arg(0)), null);
                }
            }
            string pn = prologPred.Name;
            if (pn == "." || pn == "{}")
            {
                // Warn("Delegate term = " + pn);
            }
            var Key = new DelegateObjectInPrologKey
            {
                Name = PredicateName(prologPred),
                Arity = PredicateArity(prologPred),
                Module = PredicateModule(prologPred),
                DelegateType = fi
            };
            //uint fid = libpl.PL_open_foreign_frame();
            //Key.Origin = prologPred.Copy();

            DelegateObjectInProlog handlerInProlog;
            lock (PrologDelegateHandlers)
            {
                if (PrologDelegateHandlers.TryGetValue(Key, out handlerInProlog))
                {
                    //   fi.RemoveEventHandler(getInstance, handlerInProlog.Delegate);
                    PrologDelegateHandlers.Remove(Key);
                }
                handlerInProlog = new DelegateObjectInProlog(Key);
                if (saveKey) PrologDelegateHandlers.Add(Key, handlerInProlog);
                // fi.AddEventHandler(getInstance, handlerInProlog.Delegate);
            }
            return handlerInProlog.Delegate;

        }
    }
    public class DelegateObjectInProlog : PrologGenericDelegate
    {
        public static bool UseCallN = false;

        DelegateObjectInPrologKey Key;

        public override string ToString()
        {
            return "DelegateObjectInProlog: " + Key;
        }

        //public PlTerm Origin;

        public DelegateObjectInProlog(DelegateObjectInPrologKey key)
        {
            Key = key;
            Type eht = key.DelegateType;
            SetInstanceOfDelegateType(eht);
            SyncLock = Delegate;
        }

        private bool knownDefined = false;
        //#pragma unsafe
        public override object CallPrologFast(object[] paramz)
        {
            //lock (oneEvtHandlerAtATime)
            {
                try
                {
                    object arg1 =
                        //Key.Origin; //makes sense for UseCallN
                        this;
                    PrologEvents++;
                    if (UseCallN)
                    {
                        return PrologCLR.CallProlog(this, Key.Module, "call", PrologArity, arg1, paramz, ReturnType,
                                                       false);
                    }
                    string module = Key.Module ?? "user";
                    PrologEvents++;
                    if (!knownDefined && !PrologCLR.IsDefined(module, Key.Name, PrologArity))
                    {
                        Embedded.Warn("Undefined Delegate Handler {0}:{1}/{2}", module, Key.Name, PrologArity);
                        return null;
                    }

                    knownDefined = true;
                    return PrologCLR.CallProlog(this, module, Key.Name, PrologArity, arg1, paramz,
                                                ReturnType, false);
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
