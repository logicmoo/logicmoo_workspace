package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_plcafe_supple_io46pl_1_6.java
 * @procedure $dummy_plcafe_supple_io.pl_1/6 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_plcafe_supple_io46pl_1_6 extends Predicate {
    static Predicate $dummy_plcafe_supple_io46pl_1_6_1 = new PRED_$dummy_plcafe_supple_io46pl_1_6_1();
    static Predicate $dummy_plcafe_supple_io46pl_1_6_2 = new PRED_$dummy_plcafe_supple_io46pl_1_6_2();
    static Predicate $dummy_plcafe_supple_io46pl_1_6_sub_1 = new PRED_$dummy_plcafe_supple_io46pl_1_6_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6;

    public PRED_$dummy_plcafe_supple_io46pl_1_6(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        arg6 = a6; 
        this.cont = cont;
    }

    public PRED_$dummy_plcafe_supple_io46pl_1_6(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        arg6 = args[5]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.aregs[6] = arg6;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_plcafe_supple_io46pl_1_6_1, $dummy_plcafe_supple_io46pl_1_6_sub_1);
    }

    public int arity() { return 6; }

    public String toString() {
        return "$dummy_plcafe_supple_io.pl_1(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ")";
    }
}

class PRED_$dummy_plcafe_supple_io46pl_1_6_sub_1 extends PRED_$dummy_plcafe_supple_io46pl_1_6 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_plcafe_supple_io46pl_1_6_2);
    }
}

class PRED_$dummy_plcafe_supple_io46pl_1_6_1 extends PRED_$dummy_plcafe_supple_io46pl_1_6 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("chart", 3);
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("sentence_n");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("edges");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("next_edge_number");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        a7 = new VariableTerm(engine);
        Term[] h4 = {s3, a2};
        a9 = new StructureTerm(f2, h4);
        Term[] h6 = {s5, a4};
        a10 = new StructureTerm(f2, h6);
        Term[] h8 = {s7, a1};
        a11 = new StructureTerm(f2, h8);
        Term[] h9 = {a9, a10, a11};
        a8 = new StructureTerm(f1, h9);
        a12 = new ListTerm(a6, a5);
        p1 = new PRED_$cut_1(a7, cont);
        p2 = new PRED_read_charts_2(a12, a3, p1);
        p3 = new PRED_$unify_2(a6, a8, p2);
        return new PRED_$get_level_1(a7, p3);
    }
}

class PRED_$dummy_plcafe_supple_io46pl_1_6_2 extends PRED_$dummy_plcafe_supple_io46pl_1_6 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("Malformed chart in file, involving ");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        a7 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a7, cont);
        p2 = new PRED_$unify_2(a3, s2, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_vwrite_1(a6, p3);
        p5 = new PRED_vwrite_1(s1, p4);
        p6 = new PRED_nl_0(p5);
        return new PRED_$get_level_1(a7, p6);
    }
}

