package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_plcafe_supple_io46pl_2_13.java
 * @procedure $dummy_plcafe_supple_io.pl_2/13 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_plcafe_supple_io46pl_2_13 extends Predicate {
    static Predicate $dummy_plcafe_supple_io46pl_2_13_1 = new PRED_$dummy_plcafe_supple_io46pl_2_13_1();
    static Predicate $dummy_plcafe_supple_io46pl_2_13_2 = new PRED_$dummy_plcafe_supple_io46pl_2_13_2();
    static Predicate $dummy_plcafe_supple_io46pl_2_13_sub_1 = new PRED_$dummy_plcafe_supple_io46pl_2_13_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13;

    public PRED_$dummy_plcafe_supple_io46pl_2_13(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Term a7, Term a8, Term a9, Term a10, Term a11, Term a12, Term a13, Predicate cont) {
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
        this.cont = cont;
    }

    public PRED_$dummy_plcafe_supple_io46pl_2_13(){}
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
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_plcafe_supple_io46pl_2_13_1, $dummy_plcafe_supple_io46pl_2_13_sub_1);
    }

    public int arity() { return 13; }

    public String toString() {
        return "$dummy_plcafe_supple_io.pl_2(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ", " + arg7 + ", " + arg8 + ", " + arg9 + ", " + arg10 + ", " + arg11 + ", " + arg12 + ", " + arg13 + ")";
    }
}

class PRED_$dummy_plcafe_supple_io46pl_2_13_sub_1 extends PRED_$dummy_plcafe_supple_io46pl_2_13 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_plcafe_supple_io46pl_2_13_2);
    }
}

class PRED_$dummy_plcafe_supple_io46pl_2_13_1 extends PRED_$dummy_plcafe_supple_io46pl_2_13 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
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
        Predicate cont = engine.cont;

        return new PRED_edge_10(a3, a4, a5, s1, a6, s1, a7, a8, a9, a1, cont);
    }
}

class PRED_$dummy_plcafe_supple_io46pl_2_13_2 extends PRED_$dummy_plcafe_supple_io46pl_2_13 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
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
        Predicate cont = engine.cont;

        Term[] h3 = {a10, a11, a5, s2, a12, s2, a13, a8, a9, a1};
        a14 = new StructureTerm(f1, h3);
        return new PRED_memberchk_2(a14, a2, cont);
    }
}

