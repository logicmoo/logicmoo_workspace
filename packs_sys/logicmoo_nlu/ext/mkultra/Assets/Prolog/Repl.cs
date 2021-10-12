// Copyright 2007, 2008, 2009, 2010, 2011 Ian Horswill
// This file is part of Twig.
//
// Twig is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as 
// published by the Free Software Foundation, either version 3 of
//  the License, or (at your option) any later version.
//
// Twig is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Twig.  If not, see <http://www.gnu.org/licenses/>.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using UnityEngine;

namespace Prolog
{
    /// <summary>
    /// Read/eval/print loop for MinimaLisp
    /// </summary>
    public class Repl
    {
        #region Global flags
        /// <summary>
        /// If true, Repls start in Prolog mode
        /// </summary>
        public static bool DefaultToPrologMode
        {
            get;
            set;
        }

        /// <summary>
        /// If true, Repls start in timing mode
        /// </summary>
        public static bool DefaultToTimingMode
        {
            get;
            set;
        }
        #endregion

        #region Instance variables
        /// <summary>
        /// Raw input stream for reading input
        /// </summary>
        public Stream InputStream { get; set; }
        /// <summary>
        /// TextWriter for writing output
        /// </summary>
        public TextWriter Output { get; set; }
        /// <summary>
        /// Callback for when KB is changed from one gameobject to another
        /// </summary>
        public Action<KnowledgeBase> OnChangeKB;

        /// <summary>
        /// Outputs a newline to the current repl's output stream.
        /// </summary>
        public void NewLine()
        {
            Output.WriteLine();
        }


        /// <summary>
        /// Prevents REPL from automatically printing results of commands
        /// </summary>
        public bool SuppressReturnValue { get; set; }

        public GameObject CurrentGameObject { get; set; }

        public KnowledgeBase CurrentKnowledgeBase
        {
            get
            {
                if (CurrentGameObject == null)
                    return KnowledgeBase.Global;
                return CurrentGameObject.KnowledgeBase();
            }
        }

        /// <summary>
        /// Last exception object thrown by user code
        /// </summary>
        public Exception LastException { get; private set; }
        #endregion

        public void ProcessCommandLine(string command)
        {
            command = command.Trim();
            try
            {
                string[] parsed = command.Split();
                switch ((parsed.Length > 0) ? parsed[0].Trim(new [] {'.'}) : "")
                {
                    case "quit":
                    case "exit":
                    case "halt":
                        Application.Quit();
                        break;

                    case "within":
                        SwitchComponents(command);
                        break;

                    case "global":
                        this.SwitchComponents("global");
                        break;

                    case "make":
                        ReloadModifiedSourceFilesForAllComponents();
                        break;

                    default:
                        PrologModeCommandLineHandler(command);
                        break;
                }
            }
            catch (EndOfStreamException)
            {
                //CaptureStack();
                StartErrorReport();
                Output.WriteLine(
                    "Syntax error: incomplete expression.  Command line ended before the end of an expression or the file being loaded is incomplete.");
            }
            catch (Exception e)
            {
                LastException = e;
                //CaptureStack();
                while (e is TargetInvocationException)
                    e = e.InnerException;
                var s = e as SyntaxErrorException;

                StartErrorReport();
                if (currentException == InnermostException(e))
                    Output.Write("\n{0}", currentExceptionSourceLocation);
                if (s != null)
                {
                    string message = s.Message;
                    Output.WriteLine(
                        "{0}:\n{1}{2}In expression:",
                        s.GetType().Name,
                        message,
                        message.EndsWith("\n") ? "" : "\n");
                    NewLine();
                }
                else
                {
                    Output.WriteLine("{0}: {1}", e.GetType().Name, e.Message);
                }
            }
            if (!SuppressReturnValue)
            {
                NewLine();
                Prompt();
            }
        }

        private void Prompt()
        {
            //throw new NotImplementedException();
        }

        private void ReloadModifiedSourceFilesForAllComponents()
        {
            throw new NotImplementedException();
        }

        private void SwitchComponents(string command)
        {
            var gameObjectName = command.Replace("within", "").Trim();
            if (gameObjectName.EndsWith("."))
                gameObjectName = gameObjectName.Substring(0, gameObjectName.Length - 1);
            if (gameObjectName == "global")
                gameObjectName = "GlobalKB";
            var newGameObject = GameObject.Find(gameObjectName);
            if (newGameObject == null)
                newGameObject = GameObject.Find(char.ToUpper(gameObjectName[0]) + gameObjectName.Substring(1));
            if (newGameObject == null)
            {
                Output.WriteLine("No GameObject named {0}", gameObjectName);
                return;
            }
            var kb = newGameObject.GetComponent<KB>();
            if (kb == null)
            {
                Output.WriteLine("Cannot change GameObjects: {0} has no KB.", gameObjectName);
                return;
            }
            CurrentGameObject = newGameObject;
            PrologContext.KnowledgeBase = kb.KnowledgeBase;
            PrologContext.Reset(CurrentGameObject);
            if (OnChangeKB != null)
                OnChangeKB(kb.KnowledgeBase);
            Output.WriteLine("Now using the KB of {0}", gameObjectName);
        }

        #region Prolog mode command processing
        IEnumerator<bool> prologModeAnswerStream;
        readonly List<LogicVariable> freeVariablesInCurrentQuery = new List<LogicVariable>();
        bool foundOneSolution;
        PrologContext prologContext;
        PrologContext PrologContext
        {
            get { return prologContext ?? (prologContext = PrologContext.Allocate(CurrentKnowledgeBase, CurrentGameObject)); }
        }

        void PrologModeCommandLineHandler(string command)
        {
            string trimmed = command.Trim(' ', '.');
            switch (trimmed)
            {
                case "time":
                    timeCommands = true;
                    break;

                case "notime":
                    timeCommands = false;
                    break;

                case "trace":
                    CurrentKnowledgeBase.Trace = true;
                    break;

                case "notrace":
                    CurrentKnowledgeBase.Trace = false;
                    break;

                case ";":
                case "next":
                    if (prologModeAnswerStream == null)
                        Output.WriteLine("No pending goal.");
                    else
                        PrintNextQuerySolution();
                    break;

                case "stack":
                    DumpPrologStack(PrologContext);
                    break;

                case "errorstack":
                    if (PrologContext.LastExceptionContext == null)
                        Output.WriteLine("No asynchronous exceptions have been caught by the prolog interpreter.  Did you mean the 'stack' command?");
                    DumpPrologStack(PrologContext.LastExceptionContext);
                    break;

                default:
                    StartNewQuery(command);
                    break;
            }
        }

        private void DumpPrologStack(PrologContext context)
        {
            if (context.GoalStackDepth > 0)
                for (ushort i = 0; i <= context.CurrentFrame; i++)
                {
                    Structure g = context.GoalStackGoal(i);
                    if (g != null)
                    {
                        ushort frame = i;
                        while (frame != 0)
                        {
                            //Output.Write("{0}/", frame);
                            Output.Write("  ");
                            frame = context.GoalStackParent(frame);
                        }
                        //Output.Write(' ');
                        //Output.Write("{0}<{1}: ", i, PrologContext.GoalStackParent(i));
                        Output.WriteLine(Term.ToStringInPrologFormat(g));
                    }
                }
            else
                Output.WriteLine("Goal stack is empty.");
        }

        private void StartNewQuery(string command)
        {
            try
            {
                foundOneSolution = false;
                PrologContext.Reset(CurrentGameObject);
                PrologContext.Output = Output;
                PrologContext.PushGoalStack(Symbol.Intern("parse"), new object[] { command }, 0);
                object query = ISOPrologReader.ReadAndGetFreeVariables(command, freeVariablesInCurrentQuery);
                if (query == null) throw new ArgumentNullException("command", "Prolog query may not be null.");
                var goal = Term.Structurify(query, "Not a valid Prolog goal.");
                if (goal.IsFunctor(Symbol.PrologListConstructor, 2) && goal.Argument(1) == null)
                    goal = new Structure("reconsult", goal.Argument(0));
                prologModeAnswerStream = PrologContext.ResetStackAndProve(goal).GetEnumerator();
                PrintNextQuerySolution();
            }
            catch (Exception)
            {
                StartErrorReport();
                prologModeAnswerStream = null;
                if (PrologContext.GoalStackDepth>0)
                    Output.WriteLine("In goal: {0}", PrologContext.GoalStackTop);
                throw;
            }
        }

        private readonly Stopwatch timer = new Stopwatch();

        private bool timeCommands;
        void PrintNextQuerySolution()
        {
            try
            {
                PrologContext.ResetStepLimit();
                timer.Reset();
                timer.Start();
                bool gotOne = prologModeAnswerStream.MoveNext();
                timer.Stop();
                if (gotOne)
                {
                    if (freeVariablesInCurrentQuery.Count > 0)
                    {
                        foundOneSolution = true;
                        foreach (LogicVariable v in freeVariablesInCurrentQuery)
                            Output.WriteLine("{0} = {1}", v.Name, Term.ToStringInPrologFormat(Term.Deref(v)));
                    }
                    else
                    {
                        Output.WriteLine("yes");
                        prologModeAnswerStream = null;
                    }
                }
                else
                {
                    Output.WriteLine(foundOneSolution?"no more solutions found":"no");
                    prologModeAnswerStream = null;
                }
                if (timeCommands)
                {
                    double ms = timer.Elapsed.TotalMilliseconds;
                    Output.WriteLine("{0:###}ms, {1} inference steps, {2:0.##} KLIPS.\n", ms, prologContext.StepsUsed, prologContext.StepsUsed/ms);
                }
            }
            catch (Exception)
            {
                prologModeAnswerStream = null;
                throw;
            }
        }
        #endregion

        #region Exception handling
        //private static bool suppressStyleChecks;
        void StartErrorReport()
        {
            Output.Write("\nError:\n");
            //suppressStyleChecks = true;
        }

        private static Exception currentException;
        private static string currentExceptionSourceLocation;

        /// <summary>
        /// Prints an error message from loading a file at startup time.
        /// Needed because there may not be a Repl to do the printing.
        /// </summary>
        public static void PrintExceptionToConsole(Exception e)
        {
            e = InnermostException(e);
            Console.Write("Error:");
            if (currentException == e)
                Console.Write("\n{0}", currentExceptionSourceLocation);
            Console.WriteLine(e.Message);
            //suppressStyleChecks = true;
        }

        /// <summary>
        /// Unwindws InnerExceptions until it finds the original exception that was thrown.
        /// </summary>
        static Exception InnermostException(Exception e)
        {
            if (e.InnerException == null)
                return e;
            return InnermostException(e.InnerException);
        }

        /// <summary>
        /// Declare that the exception was generated at the specified line of the CurrentSourceFile.
        /// </summary>
        public static void RecordExceptionSourceLocation(Exception e, int lineNumber)
        {
            e = InnermostException(e);
            if (currentException != e)  // Only accept the first attempt to define the location of an exception.
            {
                currentException = e;
                currentExceptionSourceLocation = string.Format("{0}:{1} ", Prolog.CurrentSourceFile, lineNumber);
            }
            //suppressStyleChecks = true;
        }
        #endregion
    }
}
