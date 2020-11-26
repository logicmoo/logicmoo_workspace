package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_parse_file46pl_11_17.java
 * @procedure $dummy_parse_file.pl_11/17 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_parse_file46pl_11_17 extends Predicate {
    static Predicate $dummy_parse_file46pl_11_17_1 = new PRED_$dummy_parse_file46pl_11_17_1();
    static Predicate $dummy_parse_file46pl_11_17_2 = new PRED_$dummy_parse_file46pl_11_17_2();
    static Predicate $dummy_parse_file46pl_11_17_sub_1 = new PRED_$dummy_parse_file46pl_11_17_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17;

    public PRED_$dummy_parse_file46pl_11_17(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Term a7, Term a8, Term a9, Term a10, Term a11, Term a12, Term a13, Term a14, Term a15, Term a16, Term a17, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        arg6 = a6; 
        arg7 = a7; 
        arg8 = a8; 
        arg9 = a9; 
        arg10 = a10; 
        arg11 = a11; 
        arg12 = a12; 
        arg13 = a13; 
        arg14 = a14; 
        arg15 = a15; 
        arg16 = a16; 
        arg17 = a17; 
        this.cont = cont;
    }

    public PRED_$dummy_parse_file46pl_11_17(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        arg6 = args[5]; 
        arg7 = args[6]; 
        arg8 = args[7]; 
        arg9 = args[8]; 
        arg10 = args[9]; 
        arg11 = args[10]; 
        arg12 = args[11]; 
        arg13 = args[12]; 
        arg14 = args[13]; 
        arg15 = args[14]; 
        arg16 = args[15]; 
        arg17 = args[16]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.aregs[6] = arg6;
        engine.aregs[7] = arg7;
        engine.aregs[8] = arg8;
        engine.aregs[9] = arg9;
        engine.aregs[10] = arg10;
        engine.aregs[11] = arg11;
        engine.aregs[12] = arg12;
        engine.aregs[13] = arg13;
        engine.aregs[14] = arg14;
        engine.aregs[15] = arg15;
        engine.aregs[16] = arg16;
        engine.aregs[17] = arg17;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_parse_file46pl_11_17_1, $dummy_parse_file46pl_11_17_sub_1);
    }

    public int arity() { return 17; }

    public String toString() {
        return "$dummy_parse_file.pl_11(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ", " + arg7 + ", " + arg8 + ", " + arg9 + ", " + arg10 + ", " + arg11 + ", " + arg12 + ", " + arg13 + ", " + arg14 + ", " + arg15 + ", " + arg16 + ", " + arg17 + ")";
    }
}

class PRED_$dummy_parse_file46pl_11_17_sub_1 extends PRED_$dummy_parse_file46pl_11_17 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_parse_file46pl_11_17_2);
    }
}

class PRED_$dummy_parse_file46pl_11_17_1 extends PRED_$dummy_parse_file46pl_11_17 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("");
    static IntegerTerm s2 = new IntegerTerm(1);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("chart", 3);
    static SymbolTerm f4 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("sentence_n");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("edges");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("next_edge_number");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        a7 = engine.aregs[7].dereference();
        a8 = engine.aregs[8].dereference();
        a9 = engine.aregs[9].dereference();
        a10 = engine.aregs[10].dereference();
        a11 = engine.aregs[11].dereference();
        a12 = engine.aregs[12].dereference();
        a13 = engine.aregs[13].dereference();
        a14 = engine.aregs[14].dereference();
        a15 = engine.aregs[15].dereference();
        a16 = engine.aregs[16].dereference();
        a17 = engine.aregs[17].dereference();
        Predicate cont = engine.cont;

        Term[] h6 = {s5, a3};
        a19 = new StructureTerm(f4, h6);
        Term[] h8 = {s7, a14};
        a20 = new StructureTerm(f4, h8);
        Term[] h10 = {s9, a17};
        a21 = new StructureTerm(f4, h10);
        Term[] h11 = {a19, a20, a21};
        a18 = new StructureTerm(f3, h11);
        p1 = new PRED_assert_1(a18, cont);
        p2 = new PRED_$plus_3(a16, s2, a17, p1);
        p3 = new PRED_gensymmark_2(s1, a16, p2);
        p4 = new PRED_$dummy_parse_file46pl_13_13(a14, a9, a7, a4, a11, a6, a2, a8, a5, a10, a15, a12, a1, p3);
        return new PRED_chart_file_1(a13, p4);
    }
}

class PRED_$dummy_parse_file46pl_11_17_2 extends PRED_$dummy_parse_file46pl_11_17 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

