using System;
using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// Static checker for Prolog code.  Runs over KBs looking for inconsistencies.
    /// </summary>
    internal class PrologChecker
    {
        public static void Check()
        {
            new PrologChecker().ReallyCheck();
        }

        void ReallyCheck()
        {
            WalkKB(KnowledgeBase.Global);
            foreach (var component in UnityEngine.Object.FindObjectsOfType<KB>())
            {
                WalkKB(component.KnowledgeBase);
            }
            foreach (var pair in checkerInfoTable)
            {
                var checkerInfo = pair.Value;
                if (!checkerInfo.Referenced)
                {
                    KnowledgeBaseRule rule = checkerInfo.DefiningRule;
                    PredicateInfo global = KnowledgeBase.Global.CheckForPredicateInfo(new PredicateIndicator(rule.HeadFunctor, rule.HeadArity));
                    if (global == null || !global.External)
                        rule.PrintWarning("{0}/{1} is never used.", rule.HeadFunctor, rule.HeadArity);
                }
            }
        }

        /// <summary>
        /// Scans all rules for references to undefined predicates.
        /// </summary>
        public void WalkKB(KnowledgeBase kb)
        {
            foreach (var predicate in kb.Predicates)
                if (predicate != null && predicate.Entries != null)
                {
                    if (predicate.Public || predicate.Shadow)
                        MarkReferenced(predicate);
                    bool firstOne = true;
                    foreach (var kbEntry in predicate.Entries)
                    {
                        var rule = kbEntry as KnowledgeBaseRule;
                        if (rule != null)
                        {
                            if (firstOne)
                            {
                                MarkDefined(predicate, rule);
                                firstOne = false;
                            }
                            WalkRule(kb, rule);
                            if (predicate.HigherOrderArguments != null)
                                foreach (var arg in predicate.HigherOrderArguments)
                                    WalkGoal(kb, rule, rule.HeadArgs[arg]);
                        }
                    }
                }
        }

        private void WalkRule(KnowledgeBase kb, KnowledgeBaseRule rule)
        {
            foreach (Structure goal in rule.BodyGoals)
                WalkGoal(kb, rule, goal);
        }

        private void WalkGoal(KnowledgeBase kb, KnowledgeBaseRule rule, object goal)
        {
            goal = Term.Deref(goal);
            var atom = goal as Symbol;
            if (atom != null)
            {
                var p = new PredicateIndicator(atom, 0);
                if (PrologPrimitives.IsDefined(p))
                    return;
                var predicate = kb.CheckForPredicateInfo(p);
                if (predicate == null)
                    rule.PrintWarning("undefined predicate {0}", p);
                else
                    MarkReferenced(predicate);
            }
            else
            {
                var s = goal as Structure;
                if (s != null)
                    WalkGoal(kb, rule, s);
                else if (!(goal is LogicVariable) && !(goal is bool))
                    rule.PrintWarning("malformed goal: {0}", goal);
            }
        }

        private void WalkGoal(KnowledgeBase kb, KnowledgeBaseRule rule, Structure goal)
        {
            var predicateIndicator = goal.PredicateIndicator;
            Symbol functor = goal.Functor;
            int arity = goal.Arity;
            switch (functor.Name)
            {
                case "begin":
                    foreach (var arg in goal.Arguments)
                        WalkGoal(kb, rule, arg);
                    break;

                case "once":
                case "check":
                case "randomize":
                case "not":
                case "\\+":
                    if (arity == 1)
                    {
                        WalkGoal(kb, rule, goal.Argument(0));
                    }
                    else
                        WarnUndefined(rule, functor, arity);
                    break;

                case ",":
                case ";":
                case "->":
                    if (arity == 2)
                    {
                        WalkGoal(kb, rule, goal.Argument(0));
                        WalkGoal(kb, rule, goal.Argument(1));
                    }
                    else
                        WarnUndefined(rule, functor, arity);
                    break;

                case "call":
                case "maplist":
                    if (arity < 1)
                        WarnUndefined(rule, functor, arity);
                    else
                    {
                        object goalToCall = goal.Argument(0);
                        var goalToCallAsStructure = goalToCall as Structure;
                        if (goalToCallAsStructure != null)
                        {
                            var newArgs = new object[arity - 1 + goalToCallAsStructure.Arity];
                            goalToCallAsStructure.Arguments.CopyTo(newArgs, 0);
                            WalkGoal(kb, rule, new Structure(goalToCallAsStructure.Functor, newArgs));
                        }
                        else
                        {
                            var call = goalToCall as Symbol;
                            if (call != null)
                            {
                                this.WalkGoal(kb, rule, new Structure(call, new object[arity - 1]));
                            }
                        }
                    }
                    break;

                case "arg_min":
                case "arg_max":
                                        if (arity == 3)
                    {
                        WalkGoal(kb, rule, goal.Argument(2));
                    }
                    else
                        WarnUndefined(rule, functor, arity);
                    break;

                case "find_all":
                                        if (arity == 3)
                    {
                        WalkGoal(kb, rule, goal.Argument(1));
                    }
                    else
                        WarnUndefined(rule, functor, arity);
                    break;

                default:
                    if (PrologPrimitives.IsDefined(predicateIndicator))
                    {
                        var arglist = PrologPrimitives.Arglist(predicateIndicator.Functor);
                        for (int i = 0; i < Math.Min(predicateIndicator.Arity,arglist.Count); i++)
                        {
                            var argSym = arglist[i] as Symbol;
                            if (argSym != null)
                            {
                                var arg = argSym.Name;
                                if (arg[0] == ':')
                                    WalkGoal(kb, rule, goal.Argument(i));
                                else if (arg == "..." && arglist[i - 1] is string && ((string)arglist[i - 1])[0] == ':')
                                {
                                    // Predicate accepts a rest arg of goals
                                    for (int j = i; j < predicateIndicator.Arity; j++)
                                        WalkGoal(kb, rule, goal.Argument(j));
                                }
                            }
                        }
                    }
                    else
                    {
                        var predicate = kb.CheckForPredicateInfo(predicateIndicator);
                        if (predicate == null)
                            WarnUndefined(rule, functor, arity);
                        else
                        {
                            MarkReferenced(predicate);
                            if (predicate.HigherOrderArguments != null)
                                foreach (int argIndex in predicate.HigherOrderArguments)
                                    WalkGoal(kb, rule, goal.Argument(argIndex));
                        }
                    }
                    break;
            }
        }

        private void WarnUndefined(KnowledgeBaseRule rule,Symbol functor,int arity)
        {
            rule.PrintWarning("{0}/{1} undefined", functor, arity);
        }

        private readonly Dictionary<PredicateInfo, CheckerInfo> checkerInfoTable =
            new Dictionary<PredicateInfo, CheckerInfo>();

        private class CheckerInfo
        {
            public KnowledgeBaseRule DefiningRule;
            public bool Referenced;
        }

        private CheckerInfo PredicateCheckerInfo(PredicateInfo predicate)
        {
            CheckerInfo result;
            if (checkerInfoTable.TryGetValue(predicate, out result))
                return result;
            return checkerInfoTable[predicate] = new CheckerInfo();
        }

        private void MarkDefined(PredicateInfo predicate, KnowledgeBaseRule rule)
        {
            CheckerInfo predicateCheckerInfo = PredicateCheckerInfo(predicate);
            if (predicateCheckerInfo.DefiningRule == null)
                predicateCheckerInfo.DefiningRule = rule;
        }

        private void MarkReferenced(PredicateInfo predicate)
        {
            PredicateCheckerInfo(predicate).Referenced = true;
        }
    }
}
