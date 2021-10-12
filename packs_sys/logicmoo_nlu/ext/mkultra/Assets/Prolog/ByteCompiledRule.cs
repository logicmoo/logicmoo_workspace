using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Prolog

{
    /// <summary>
    /// Implements rules in a bytecode language vaguely similar to the WAM.
    /// </summary>
    public class ByteCompiledRule : KnowledgeBaseRule
    {
        /// <summary>
        /// For testing purposes
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public static ByteCompiledRule FromCode(string code)
        {
            var term = (Structure)(new ISOPrologReader(code).ReadTerm());
            Structure head;
            Structure[] body;
            if (term.IsFunctor(Symbol.Intern(":-"), 2))
            {
                head = Term.Structurify(term.Argument(0), "Bad rule head.");
                var bodyList = new List<Structure>();
                UnwindCommaExpression(term.Argument(1), bodyList);
                
                body = bodyList.ToArray();
            }
            else
            {
                head = Term.Structurify(term, "Bad rule head");
                body = new Structure[0];
            }
            return new ByteCompiledRule(KnowledgeBase.Global.EntryForStoring(head.PredicateIndicator), head, body, "none", 1);
        }

        /// <summary>
        /// Testing jig for byte compiler
        /// </summary>
        public void Test(string call)
        {
            var term = (Structure)(new ISOPrologReader(call).ReadTerm());
            if (term.Arity != predicate.Arity)
                throw new Exception("Wrong number of arguments");
            using (var c = PrologContext.Allocate(KnowledgeBase.Global, null))
            {
                c.PushArguments(term.Arguments);
                int solutions = 0;
                foreach (var x in StackCall(c))
                {
                    foreach (var arg in term.Arguments)
                        if (arg is LogicVariable)
                        {
                            var l = arg as LogicVariable;
                            Console.WriteLine("{0}={1}", l.Name, ISOPrologWriter.WriteToString(l.Value));
                        }
                    Console.WriteLine(x);
                    if (solutions++ > 10)
                    {
                        Console.WriteLine("Max solutions found; terminating search");
                        c.PopFrame(0);
                        goto abort;
                    }
                }
                Console.WriteLine("fail");
                abort:
                int tos = c.MakeFrame(0);
                if (tos != 0)
                    Console.WriteLine("Error: tos is " + tos);
            }
        }

        internal ByteCompiledRule(PredicateInfo predicate, Structure head, Structure[] ruleBody, string source, int line)
            : base(head, ruleBody, false, source, line)
        {
            this.predicate = predicate;
            var c = new Compiler(head, ruleBody, KnowledgeBase);
            code = c.Code;
            frameSize = c.FrameSize;
            Disassemble();
            Console.WriteLine();
        }

        #region Fields and properties
        /// <summary>
        /// The largest allowable arity for an executable predicate.
        /// </summary>
        public const int MaxArity = 50;
        private readonly PredicateInfo predicate;
        private readonly byte[] code;
        private readonly int frameSize;

        /// <summary>
        /// The KB this rule is defined in.
        /// </summary>
        KnowledgeBase KnowledgeBase
        {
            get { return predicate.KnowledgeBase; }
        }
        #endregion

        #region Literal management
        class LiteralTable<T>
        {
            private readonly List<T> table = new List<T>();
            public T this[int index]
            {
                get { return table[index]; }
            }

            public ushort IndexOf(T item)
            {
                    int position = table.IndexOf(item);
                    if (position >= 0)
                        return (ushort)position;
                    table.Add(item);
                    return (ushort)(table.Count - 1);
            }
        }

        private readonly static LiteralTable<object> GlobalLiteralTable = new LiteralTable<object>();
        private readonly static LiteralTable<PredicateInfo> PredicateTable = new LiteralTable<PredicateInfo>();
        private readonly static LiteralTable<Symbol> SymbolTable = new LiteralTable<Symbol>();
        private readonly static LiteralTable<PrologPrimitives.PrimitiveImplementation> PrimitiveTable = new LiteralTable<PrologPrimitives.PrimitiveImplementation>();
        #endregion

        #region Opcode definitions
        // In the following, OP means an addressing mode of the following form:
        // REG               // Operand is register number REG
        // REG ARG           // Register number REG holds, a structure, operand is its ARG'th argument
        // REG ARG STORE     // Same as above, but store said argument into register STORE
        //
        // Length is determined by MSB of byte.  If set, there's another byte.

        enum Opcode
        {

            MatchStructure,     // functor, arity, endOfMatch, OP
            MatchVarFirst,      // Symbol/16, OP
            MatchVar,           // varReg, OP
            MatchLiteral,       // literal/16, OP
            BuildStructure,     // functor/16, arity, OP
            BuildVar,           // symbol/16, OP
            BuildLiteral,       // literal/16, OP
            BuildReg,           // reg, OP
            CallWokenGoals,     // reg, failAddress/16, successAddress/16
            Call,               // reg, failAddress/16, successAddress/16, predicateInfo, registerOrLiteral ...
            CallPrimitive,      // reg, failAddress/16, successAddress/16, implementation, arity, registerOrLiteral ...
        }

        // Magic continuation addresses for use in call instructions
        // ReSharper disable InconsistentNaming
        private const ushort SuccessContinuationPC = 0x8000;  // This is the final call of the rule; return success
        private const ushort FailContinuationPC = 0x8001;     // This was the first call of the rule; return failure
        private const ushort CutContinuationPC = 0x8002;      // This call was preceded by a cut; return CutState.ForceFail.
        // ReSharper restore InconsistentNaming
        
        // Other opcode-related constants
        private const int CallTargetOffset = 6;               // Offset between Call/CallPrimitive opcode and its target
        private const byte NoRegister = 0xff;                 // Target register meaning not to write to register

        // So here's append([], X, X). :
        //    MatchLiteral([], 0)
        //    MatchVarFirst(1)               % This is actually a no-op
        //    MatchVar(1, 2)
        //    Return
        //
        // And here's append([H|T], X, [H|T1]) :- append(T, X, T1).
        //    MatchStructure(dot, 2, L1, 0)
        //    MatchVarFirst(0>0>3)      % H is at 3
        //    MatchVarFirst(0>1>4)      % T is at 4
        // L1 MatchVarFirst(1)               % Actually a no-op
        //    MatchStructure(dot, 2, L2, 2)
        //    MatchVar(3, 2>0)
        //    MatchVarFirst(5, 2>1)        % T1 is at 5
        // L2 Call(6, Fail, Succeed, append/3, register(4), register(2), register(5))
        #endregion

        #region Interpreter
        internal IEnumerable<CutState> StackCall(PrologContext context)
        {
            int traceMark = context.MarkTrace();
            // On entry, the stack pointer points at the base of our arguments.
            int framePointer = context.MakeFrame(frameSize);
            ushort pc = 0;
            ushort writeModeEndAddress = 0; // We're in write mode if pc < writeModeEndAddress.
// ReSharper disable TooWideLocalVariableScope
            // Resharper is confused - can't move this inside the while loop, or its lifetime changes.
            ushort endOfBuilds;         // PC of last call instruction whose arguments have been built.
// ReSharper restore TooWideLocalVariableScope

            while (true)
            {
                switch ((Opcode) code[pc++])
                {
                    #region Head opcodes
                    case Opcode.MatchLiteral:
                        {
                            object literal = GetLiteral(ref pc);
                            if (pc < writeModeEndAddress)
                            {
                                // Write mode
                                SetOperand(context, framePointer, ref pc, literal);
                            }
                            else
                            {
                                // Read mode
                                if (!Term.Unify(DecodeOperand(context, framePointer, ref pc), literal, context))
                                    goto fail;
                            }
                        }
                        break;

                    case Opcode.MatchVarFirst:
                        {
                            if (pc < writeModeEndAddress)
                            {
                                // Write mode
                                Symbol name = GetSymbol(ref pc);
                                int register = code[pc++];
                                var l = new LogicVariable(name);
                                SetOperand(context, framePointer, ref pc, l);
                                if (register != NoRegister)
                                    context.SetStack(framePointer, register, l);
                            }
                            else
                            {
                                pc += 2; // Skip over variable name.
                                int register = code[pc++];
                                // Read mode
                                object operand = DecodeOperand(context, framePointer, ref pc);
                                if (register != NoRegister)
                                    context.SetStack(framePointer, register, operand);
                            }
                        }
                        break;

                    case Opcode.MatchVar:
                        {
                            object register = Register(context, framePointer, pc++);
                            if (pc < writeModeEndAddress)
                            {
                                // Write mode
                                SetOperand(context, framePointer, ref pc, register);
                            }
                            else
                            {
                                // Read mode
                                if (!Term.Unify(DecodeOperand(context, framePointer, ref pc), register, context))
                                    goto fail;
                            }
                        }
                        break;

                    case Opcode.MatchStructure:
                        {
                            Symbol functor = GetSymbol(ref pc);
                            int arity = code[pc++];
                            ushort endAddress = GetUShort(ref pc);

                            if (pc < writeModeEndAddress)
                            {
                                // Write mode
                                SetOperand(context, framePointer, ref pc, new Structure(functor, new object[arity]));
                            }
                            else
                            {
                                // Read mode
                                object arg = DecodeOperand(context, framePointer, ref pc);
                                var s = arg as Structure;
                                if (s == null || !s.IsFunctor(functor, arity))
                                {
                                    var l = arg as LogicVariable;
                                    if (l == null)
                                        goto fail;
                                    l.UnifyWithStructure(new Structure(functor, new object[arity]));
                                    writeModeEndAddress = endAddress;
                                }
                            }
                        }
                        break;
                    #endregion

                    #region Body opcodes: structure building
                    case Opcode.BuildStructure:
                        {
                            Symbol functor = GetSymbol(ref pc);
                            int arity = code[pc++];
                            SetOperand(context, framePointer, ref pc, 
                                        new Structure(functor, new object[arity]));
                        }
                        break;

                    case Opcode.BuildLiteral:
                        SetOperand(context, framePointer, ref pc, GetLiteral(ref pc));
                        break;

                    case Opcode.BuildVar:
                        {
                            Symbol name = GetSymbol(ref pc);
                            SetOperand(context, framePointer, ref pc, new LogicVariable(name));
                        }
                        break;

                    case Opcode.BuildReg:
                        object registerValue = Register(context, framePointer, pc++);
                        SetOperand(context, framePointer, ref pc, registerValue);
                        break;
                    #endregion

                    #region Body: control flow
                    case Opcode.CallWokenGoals:
                    case Opcode.Call:
                    case Opcode.CallPrimitive:
                        endOfBuilds = pc;
                        // ReSharper disable once InconsistentNaming
                        ushort succeedPC;
                        IEnumerator<CutState> iterator;
                        var failPC = StartCallInstruction(context, framePointer, ref pc, out succeedPC, out iterator);
                        while (true)
                        {
                            if (iterator == null || iterator.MoveNext())
                            {
                                // Call succeeds
                                if (succeedPC == SuccessContinuationPC)
                                {
                                    yield return CutState.Continue;
                                    // If we get here, then our caller is requesting a redo
                                    // Fall through and continue this iterator.
                                    if (iterator == null)
                                        goto fail;                 // Kluge: special case to handle CallWokenGoals instruction where there are no woken goals.
                                }
                                else
                                {
                                    if (succeedPC > endOfBuilds)
                                        // Haven't run the build instructions for next goal, so break out and run them.
                                        break;
                                    pc = (ushort)(succeedPC+1); // StartCallInstruction assumes we've skipped over the opcode.
                                    // succeedPC is address of call/callprimitive instruction; next byte is iterator register
                                    failPC = this.StartCallInstruction(context, framePointer, ref pc, out succeedPC, out iterator);
                                    // Continue while loop
                                }
                            }
                            else
                            {
                                // Call fails
                                switch (failPC)
                                {
                                    case FailContinuationPC:
                                        goto fail;

                                    case CutContinuationPC:
                                        goto cut;

                                    default:
                                        // failPC is address of call/callprimitive instruction; next byte is iterator register
                                        pc = (ushort) (failPC + 1); // +1 because we're skipping over the opcode
                                        iterator =
                                            (IEnumerator<CutState>) context.GetStack(framePointer, code[pc++]);
                                        if (iterator == null)  // Kluge: special case to handle CallWokenGoals instruction where there are no woken goals.
                                            goto fail;

                                        failPC = GetUShort(ref pc);
                                        succeedPC = GetUShort(ref pc);
                                        break;
                                }
                            }
                        }
                        break;
                    #endregion

                    default:
                        Debug.Assert(false, "Bad opcode in byte compiled Prolog rule, pc="+(pc-1)+", opcode="+((Opcode)code[pc-1]));
                        break;
                }
            }

            // The while loop above never terminates normally; it only yeilds or branches to one of the following labels
            cut:
            context.PopFrame(framePointer);
            context.RestoreVariables(traceMark);
            yield return CutState.ForceFail;

            fail:
            context.PopFrame(framePointer);
            context.RestoreVariables(traceMark);
        }

        private ushort StartCallInstruction(PrologContext context, int framePointer, ref ushort pc, out ushort succeedPC,
                                            out IEnumerator<CutState> iterator)
        {
            int iteratorRegister = code[pc++];
            ushort failPC = GetUShort(ref pc);
            succeedPC = GetUShort(ref pc);
            iterator = null;
            // Make the iterator.  How we do this depends on the opcode, so re-fetch it.
            switch ((Opcode) code[pc - CallTargetOffset])
            {
                case Opcode.CallWokenGoals:
                    {
                        if (context.GoalsHaveWoken)
                            iterator = context.ProveAllWokenGoals().GetEnumerator();
                    }
                    break;

                case Opcode.Call:
                    {
                        // It's a user-defined predicate call
                        PredicateInfo calledPredicate = GetPredicate(ref pc);
                        PushCallArgs(context, framePointer, predicate.Arity, ref pc);
                        iterator = calledPredicate.StackCall(context).GetEnumerator();
                    }
                    break;

                case Opcode.CallPrimitive:
                    {
                        // It's a primitive call
                        PrologPrimitives.PrimitiveImplementation implementation = GetPrimitive(ref pc);
                        int arity = code[pc++];
                        PushCallArgs(context, framePointer, arity, ref pc);
                        iterator =
                            PrologPrimitives.StackCall(implementation, arity, context).GetEnumerator();
                    }
                    break;

                default:
                    Debug.Assert(false, "Bad call opcode");
                    break;
            }
            context.SetStack(framePointer, iteratorRegister, iterator);
            return failPC;
        }

        private void PushCallArgs(PrologContext context, int framePointer, int arity, ref ushort pc)
        {
            for (int i = 0; i < arity; i++)
            {
                byte maybeReg = code[pc++];
                object argumentValue = (maybeReg < 0x80) ? 
                                          context.GetStack(framePointer, maybeReg & 0x7f)
                                          : GlobalLiteralTable[((maybeReg & 0x7f) << 8) + code[pc++]];
                context.SetCallArg(i, argumentValue);
            }
        }

        private object Register(PrologContext context, int framePointer, ushort pc)
        {
            return Term.Deref(context.GetStack(framePointer, code[pc]));
        }

        private object DecodeOperand(PrologContext context, int framePointer, ref ushort pc)
        {
            int registerNumber = code[pc++];
            object value = Term.Deref(context.GetStack(framePointer, registerNumber & 0x7f));
            if (registerNumber < 0x80)
                return value;
            int argNumber = code[pc++];
            value = ((Structure)value).Argument(argNumber&0x7f);
            if (argNumber>=0x80)
                context.SetStack(framePointer, code[pc++], value);
            return value;
        }

        private void SetOperand(PrologContext context, int framePointer, ref ushort pc, object newValue)
        {
            int registerNumber = code[pc++];
            if (registerNumber < 0x80)
                context.SetStack(framePointer, registerNumber, newValue);
            else
            {
                object s = Term.Deref(context.GetStack(framePointer, registerNumber & 0x7f));
                int argNumber = code[pc++];
                object value = ((Structure) s).Arguments[argNumber & 0x7f] = newValue;
                if (argNumber >= 0x80)
                    context.SetStack(framePointer, code[pc++], value);
            }
        }

        private object GetLiteral(ref ushort pc)
        {
            object reg = GlobalLiteralTable[GetUShort(ref pc)];
            return reg;
        }

        private Symbol GetSymbol(ref ushort pc)
        {
            Symbol sym = SymbolTable[GetUShort(ref pc)];
            return sym;
        }

        private PredicateInfo GetPredicate(ref ushort pc)
        {
            PredicateInfo pred = PredicateTable[GetUShort(ref pc)];
            return pred;
        }

        PrologPrimitives.PrimitiveImplementation GetPrimitive(ref ushort pc)
        {
            return PrimitiveTable[GetUShort(ref pc)];
        }

        private ushort GetUShort(ref ushort pc)
        {
            int firstByte = code[pc++]<<8;
            int secondByte = code[pc++];
            int value = firstByte+secondByte;
            return (ushort)value;
        }

        #endregion

        #region Compiler

        private class Compiler
        {
            public Compiler(Structure head, Structure[] body, KnowledgeBase kb)
            {
                this.head = head;
                bodyGoals = body;
                this.knowledgeBase = kb;
                env = new CompileTimeEnvironment(head.Arity);
                Compile();
            }

            #region Fields
            private readonly Structure head;
            private readonly Structure[] bodyGoals;
            private readonly List<byte> code = new List<byte>();
            private readonly CompileTimeEnvironment env;
            private readonly KnowledgeBase knowledgeBase;
            #endregion

            /// <summary>
            /// PC of the next instruction to be emitted.
            /// </summary>
            private ushort CurrentPC
            {
                get { return (ushort)code.Count;  }
            }
            
            public byte[] Code
            {
                get { return code.ToArray(); }
            }

            public int FrameSize
            {
                get { return env.MaxRegister; }
            }

            private void Compile()
            {
                CountVariableReferences();
                //env.PreallocateRegisters();
                CompileHead();
                CompileBody();
            }

            private void CompileHead()
            {
                object[] args = head.Arguments;
                for (byte i = 0; i < args.Length; i++)
                {
                    CompileMatcher(NoRegister, i, args[i]);
                }
            }

            private void CompileMatcher(byte register, byte argument, object obj)
            {
                obj = Term.Deref(obj);
                var lv = obj as LogicVariable;
                if (lv != null)
                {
                    var info = env[lv];
                    if (register != NoRegister || info.register != argument)   // If top level and first occurance, don't bother.
                    {
                        env.InsureAllocated(info);
                        if (info.AlreadyStored)
                        {
                            Emit(Opcode.MatchVar, info.register);
                        }
                        else
                        {
                            Emit(Opcode.MatchVarFirst, SymbolTable.IndexOf(lv.Name), info.register);    
                        }
                        EmitOperand(register, argument);
                    }
                    env.OneUseCompiled(info);
                }
                else
                {
                    var structure = obj as Structure;
                    if (structure != null)
                    {
                        Structure s = structure;
                        byte destinationRegister = (register==NoRegister)?argument:this.env.GetRegister();
                        this.Emit(Opcode.MatchStructure, SymbolTable.IndexOf(s.Functor), (byte)s.Arity);
                        ushort backPatchAddress = this.CurrentPC;
                        this.EmitUShort(0);
                        this.EmitOperand(register, argument, destinationRegister);
                        for (byte i = 0; i < s.Arity; i++)
                            this.CompileMatcher(destinationRegister, i, s.Argument(i));
                        this.BackPatch(backPatchAddress, this.CurrentPC);
                        if (register != NoRegister)
                            this.env.FreeRegister(destinationRegister);
                    }
                    else
                    {
                        this.Emit(Opcode.MatchLiteral, GlobalLiteralTable.IndexOf(obj));
                        this.EmitOperand(register, argument);
                    }
                }
            }

            private void CompileBody()
            {
                // Generate "neck" (link from head code to body)
                // This is just one instruction: CallWokenGoals
                ushort failAddress = CurrentPC;
                Emit(Opcode.CallWokenGoals, env.GetRegister());
                EmitUShort(FailContinuationPC);
                ushort backPatchAddress = CurrentPC;
                EmitUShort(SuccessContinuationPC);
                foreach (var goal in bodyGoals)
                {
                    switch (goal.Functor.Name)
                    {
                        case "true":
                            // Don't bother to generate code
                            break;

                        case "!":
                            failAddress = CutContinuationPC;
                            break;

                        default:
                            CompileGoal(goal, ref failAddress, ref backPatchAddress);
                            break;
                    }
                }
                BackPatch(backPatchAddress, SuccessContinuationPC);
            }

            private void CompileGoal(Structure goal, ref ushort failAddress, ref ushort backPatchAddress)
            {
                byte continuationRegister = env.GetRegister();
                // Allocate registers to goal arguments.
                var argRegisters = new byte[goal.Arity];
                for (int i=0; i<goal.Arity; i++)
                {
                    object arg = goal.Argument(i);
                    if (arg is Structure)
                        argRegisters[i] = env.GetRegister();
                    else
                    {
                        var @var = arg as LogicVariable;
                        if (@var != null)
                            argRegisters[i] = this.env.InsureRegisterAndLock(@var);
                        else
                            // It's a literal.
                            argRegisters[i] = NoRegister;
                    }
                }

                // Build goal arguments into registers.
                for (int i = 0; i < goal.Arity; i++)
                    if (argRegisters[i] != NoRegister)
                        CompileBuild(goal.Argument(i), NoRegister, argRegisters[i]);

                // Emit call instruction
                ushort startOfCallInstruction = CurrentPC; 
                BackPatch(backPatchAddress, startOfCallInstruction);
                PrologPrimitives.PrimitiveImplementation primitiveImplementation;
                bool isPrimitive = PrologPrimitives.Implementations.TryGetValue(goal.Functor,
                                                                                out primitiveImplementation);
                // Call header
                Emit(isPrimitive?Opcode.CallPrimitive : Opcode.Call, continuationRegister);
                EmitUShort(failAddress);
                backPatchAddress = CurrentPC;
                EmitUShort(0);      // This will get backpatched
                // Call target
                if (isPrimitive)
                {
                    EmitUShort(PrimitiveTable.IndexOf(primitiveImplementation));
                    EmitByte((byte)goal.Arity);
                }
                else
                {
                    EmitUShort(PredicateTable.IndexOf(this.knowledgeBase.EntryForStoring(goal.PredicateIndicator)));
                }
                // Call arguments
                for (int i = 0; i < goal.Arity; i++)
                {
                    byte reg = argRegisters[i];
                    if (reg == NoRegister)
                        EmitUShort((ushort)(0x8000+GlobalLiteralTable.IndexOf(goal.Argument(i))));
                    else
                        EmitByte(reg);
                }
                failAddress = startOfCallInstruction;
            }

            private void CompileBuild(object term, byte argRegister, byte argument)
            {
                if (term is Structure)
                    CompileBuildStructure(term, argRegister, argument);
                else if (term is LogicVariable)
                    CompileBuildVariable(term, argRegister, argument);
                else
                    CompileBuildLiteral(term, argRegister, argument);
            }

            private void CompileBuildLiteral(object term, byte argRegister, byte argument)
            {
                Emit(Opcode.BuildLiteral, GlobalLiteralTable.IndexOf(term));
                EmitOperand(argRegister, argument);
            }

            private void CompileBuildVariable(object term, byte argRegister, byte argument)
            {
                var logicVariable = (LogicVariable) term;
                VariableInfo variableInfo = env[logicVariable];
                if (variableInfo.AlreadyStored)
                {
                    // Just need to copy it (not not even that if copying to its current location)
                    if (argRegister != NoRegister || argument != variableInfo.register)
                    {
                        Emit(Opcode.BuildReg, variableInfo.register);
                        EmitOperand(argRegister, argument);
                    }
                }
                else
                {
                    // Not already stored
                    if (variableInfo.References > 1)
                    {
                        // Multiple use; need to stash it in a register
                        env.InsureAllocated(variableInfo);
                        Emit(Opcode.BuildVar, SymbolTable.IndexOf(logicVariable.Name));
                        EmitOperand(argRegister, argument, variableInfo.register);
                    }
                    else
                    {
                        // Single use.  Don't bother with another register.
                        Emit(Opcode.BuildVar, SymbolTable.IndexOf(logicVariable.Name));
                        EmitOperand(argRegister, argument);
                    }
                }
                env.OneUseCompiled(variableInfo);
            }

            private void CompileBuildStructure(object term, byte argRegister, byte argument)
            {
                var s = (Structure) term;
                byte destinationRegister = (argRegister == NoRegister) ? argument : env.GetRegister();
                Emit(Opcode.BuildStructure, SymbolTable.IndexOf(s.Functor), (byte) s.Arity);
                EmitOperand(argRegister, argument);
                for (byte i = 0; i < s.Arity; i++)
                    CompileBuild(s.Argument(i), destinationRegister, i);
                if (argRegister != NoRegister)
                    env.FreeRegister(destinationRegister);
            }

            #region Analysis
            /// <summary>
            /// Count references to variables in the head and body
            /// </summary>
            private void CountVariableReferences()
            {
                for (byte i = 0; i < head.Arity; i++ )
                    CountVariableReferences(head.Argument(i), true, i);
                foreach (var goal in bodyGoals)
                    foreach (var goalArg in goal.Arguments)
                        CountVariableReferences(goalArg, false, NoRegister);
            }
            /// <summary>
            /// Count references to variables in ARG
            /// </summary>
            private void CountVariableReferences(object arg, bool isHead, byte argRegister)
            {
                arg = Term.Deref(arg);
                if (arg is LogicVariable)
                {
                    var l = arg as LogicVariable;
                    if (isHead)
                    {
                        VariableInfo variableInfo = env[l];
                        if (argRegister != NoRegister && variableInfo.register == NoRegister)
                        {
                            variableInfo.register = argRegister;
                            variableInfo.LockInRegister = true;
                        }
                        env.MarkHeadReference(l);
                    }
                    else
                        env.MarkBodyReference(l);
                }
                else
                {
                    var structure = arg as Structure;
                    if (structure != null)
                        foreach (var structureArg in structure.Arguments)
                            this.CountVariableReferences(structureArg, isHead, NoRegister);
                }
            }
            #endregion

            #region Emit procedures
            void Emit(Opcode op, byte arg1)
            {
                code.Add((byte)op);
                code.Add(arg1);
            }

            void Emit(Opcode op, ushort arg1)
            {
                code.Add((byte)op);
                EmitUShort(arg1);
            }

            void Emit(Opcode op, ushort arg1, byte arg2)
            {
                code.Add((byte)op);
                EmitUShort(arg1);
                code.Add(arg2);
            }
            
            private void EmitUShort(ushort arg1)
            {
                code.Add((byte)(arg1>>8));
                code.Add((byte)(arg1 & 0xff));
            }

            private void EmitByte(byte arg)
            {
                code.Add(arg);
            }

            private void EmitOperand(byte register, byte argument)
            {
                if (register == NoRegister)
                    code.Add(argument);
                else
                {
                    code.Add((byte)(register+0x80));
                    code.Add(argument);
                }
            }

            private void EmitOperand(byte register, byte argument, byte destination)
            {
                if (register == NoRegister)
                {
                    Debug.Assert(argument == destination, "Three address EmitOperand called with register=NoRegister but improper destination");
                    code.Add(argument);
                }
                else
                {
                    code.Add((byte)(register + 0x80));
                    code.Add((byte)(argument + 0x80));
                    code.Add(destination);
                }
            }
            
            private void BackPatch(ushort pc, ushort address)
            {
                code[pc] = (byte)(address >> 8);
                code[pc+1] = (byte)(address & 0xff);
            }
            #endregion

            #region Compile time environment
            class VariableInfo
            {
                public int References;
                public int ReferencesCompiled;
                // ReSharper disable once InconsistentNaming
                public byte register = NoRegister;
                public bool AlreadyStored
                {
                    get { return this.ReferencesCompiled > 0; }
                }
                // Never free this register.
                public bool LockInRegister;
            }

            class CompileTimeEnvironment
            {
                readonly Dictionary<LogicVariable, VariableInfo> database = new Dictionary<LogicVariable, VariableInfo>();
                private byte nextRegister;
                readonly Stack<byte> freeRegisters = new Stack<byte>();

                public int MaxRegister
                {
                    get { return nextRegister; }
                }

                public CompileTimeEnvironment(int headArity)
                {
                    nextRegister = (byte)headArity;
                }

                public VariableInfo this[LogicVariable variable]
                {
                    get
                    {
                        VariableInfo result;
                        if (!database.TryGetValue(variable, out result))
                        {
                            database[variable] = result = new VariableInfo();
                        }
                        return result;
                    }
                }

                public void MarkHeadReference(LogicVariable variable)
                {
                    this[variable].References++;
                }

                public void MarkBodyReference(LogicVariable variable)
                {
                    this[variable].References++;
                }

                /// <summary>
                /// Allocates a register to the specified variable.
                /// </summary>
                private void AllocateRegister(VariableInfo info)
                {
                    info.register = GetRegister();
                }

                public byte InsureRegisterAndLock(LogicVariable var)
                {
                    var info = this[var];
                    if (info.register == NoRegister)
                        AllocateRegister(info);
                    info.LockInRegister = true;
                    return info.register;
                }

                //public void PreallocateRegisters()
                //{
                //    foreach (var pair in database)
                //        PreallocateHeadRegisterIfNeeded(pair.Value);
                //}

                ///// <summary>
                ///// Allocates a register to the specified variable, if it isn't a singleton.
                ///// </summary>
                //private void PreallocateHeadRegisterIfNeeded(VariableInfo info)
                //{
                //    if (info.register == NoRegister
                //           && (info.headReferences > 1 || info.bodyReferences > 0))
                //        info.register = nextRegister++;
                //}

                internal byte GetRegister()
                {
                    if (freeRegisters.Count > 0)
                        return freeRegisters.Pop();
                    return this.nextRegister++;
                }

                internal void FreeRegister(byte reg)
                {
                    freeRegisters.Push(reg);
                }

                internal void InsureAllocated(VariableInfo info)
                {
                    if (info.register == NoRegister)
                        info.register = GetRegister();
                }

                internal void OneUseCompiled(VariableInfo info)
                {
                    info.ReferencesCompiled++;
                    if (!info.LockInRegister && info.ReferencesCompiled == info.References && info.register != NoRegister)
                        FreeRegister(info.register);
                }
            }
            #endregion
        }
        #endregion

        #region Disassembler
        /// <summary>
        /// Prints the byte code for the rule to System.Console.
        /// </summary>
        public void Disassemble()
        {
            ushort pc = 0;
            while (pc < code.Length)
            {
                var opcode = (Opcode) code[pc++];
                Console.Write("L{0}: {1} ", pc - 1, opcode.ToString());
                switch (opcode)
                {
                    case Opcode.MatchStructure:
                        {
                            Symbol functor = GetSymbol(ref pc);
                            int arity = code[pc++];
                            int endofMatch = GetUShort(ref pc);
                            Console.Write("{0}/{1}, L{2}, ", functor.Name, arity, endofMatch);
                            DisassembleOperand(ref pc);
                        }
                        break;

                    case Opcode.MatchVar:
                    case Opcode.MatchVarFirst:
                        {
                            if (opcode == Opcode.MatchVarFirst)
                                Console.Write("{0}, ", GetSymbol(ref pc));
                            byte register = code[pc++];
                            Console.Write("{0}, ", FormatRegister(register));
                            DisassembleOperand(ref pc);
                        }
                        break;

                    case Opcode.MatchLiteral:
                        {
                            Console.Write(GetLiteral(ref pc));
                            Console.Write(", ");
                            DisassembleOperand(ref pc);
                        }
                        break;

                    case Opcode.BuildStructure:
                        {
                            Console.Write(GetSymbol(ref pc));
                            Console.Write("/{0}, ", code[pc++]);
                            DisassembleOperand(ref pc);
                        }
                        break;

                    case Opcode.BuildVar:
                        {
                            Console.Write(GetSymbol(ref pc));
                            Console.Write(", ");
                            DisassembleOperand(ref pc);
                        }
                        break;

                    case Opcode.BuildReg:
                        {
                            byte reg = code[pc++];
                            Console.Write("{0}, ", FormatRegister(reg));
                            DisassembleOperand(ref pc);
                        }
                        break;

                    case Opcode.BuildLiteral:
                        {
                            Console.Write(GetLiteral(ref pc));
                            DisassembleOperand(ref pc);
                        }
                        break;

                    case Opcode.Call:
                    case Opcode.CallPrimitive:
                    case Opcode.CallWokenGoals:
                        {
                            Console.Write("{0}", FormatRegister(code[pc++]));
                            Console.Write(", {0}", FormatLabel(GetUShort(ref pc)));
                            Console.Write(", {0}", FormatLabel(GetUShort(ref pc)));
                            if (opcode != Opcode.CallWokenGoals)
                            {
                                int arity;
                                if (opcode == Opcode.Call)
                                {
                                    PredicateInfo p = GetPredicate(ref pc);
                                    Console.Write(", {0}/{1}", p.Name, p.Arity);
                                    arity = p.Arity;
                                }
                                else
                                {
                                    PrologPrimitives.PrimitiveImplementation impl = GetPrimitive(ref pc);
                                    arity = code[pc++];
                                    Console.Write(", {0}/{1}", PrologPrimitives.PrimitiveName(impl), arity);
                                }
                                while (arity-- > 0)
                                {
                                    Console.Write(", ");
                                    int arg = code[pc++];
                                    if (arg < 0x80)
                                        Console.Write("{0}", FormatRegister(arg)); // It's a register
                                    else
                                    {
                                        // It's a literal
                                        arg = ((arg & 0x7f)<<8) + code[pc++];
                                        Console.Write(GlobalLiteralTable[arg]);
                                    }
                                }
                            }
                        }
                        break;

                    default:
                        throw new Exception("Unknown opcode " + opcode);
                }
                Console.WriteLine();
            }
        }

        private string FormatLabel(ushort label)
        {
            switch (label)
            {
                case SuccessContinuationPC:
                    return "Succeed";

                case FailContinuationPC:
                    return "Fail";

                case CutContinuationPC:
                    return "Cut";

                default:
                    return "L" + label;
            }
        }

        private string FormatRegister(int r)
        {
            if (r == NoRegister)
                return "NoRegister";
            if (r < this.predicate.Arity)
                return "a" + r;
            return "t" + (r-this.predicate.Arity);
        }

        private void DisassembleOperand(ref ushort pc)
        {
            int registerNumber = code[pc++];
            Console.Write("{0}", FormatRegister(registerNumber&0x7f));
            if (registerNumber < 0x80)
                return;
            int argNumber = code[pc++];
            Console.Write("({0})", argNumber&0x7f);
            if (argNumber >= 0x80)
                Console.Write("=>{0}", FormatRegister(code[pc++]));
        }

        #endregion

        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            throw new NotImplementedException();
        }
    }
}
