package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_parse_file46pl_10_11.java
 * @procedure $dummy_parse_file.pl_10/11 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_parse_file46pl_10_11 extends Predicate {
    static Predicate $dummy_parse_file46pl_10_11_1 = new PRED_$dummy_parse_file46pl_10_11_1();
    static Predicate $dummy_parse_file46pl_10_11_2 = new PRED_$dummy_parse_file46pl_10_11_2();
    static Predicate $dummy_parse_file46pl_10_11_sub_1 = new PRED_$dummy_parse_file46pl_10_11_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11;

    public PRED_$dummy_parse_file46pl_10_11(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Term a7, Term a8, Term a9, Term a10, Term a11, Predicate cont) {
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
        this.cont = cont;
    }

    public PRED_$dummy_parse_file46pl_10_11(){}
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
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_parse_file46pl_10_11_1, $dummy_parse_file46pl_10_11_sub_1);
    }

    public int arity() { return 11; }

    public String toString() {
        return "$dummy_parse_file.pl_10(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ", " + arg7 + ", " + arg8 + ", " + arg9 + ", " + arg10 + ", " + arg11 + ")";
    }
}

class PRED_$dummy_parse_file46pl_10_11_sub_1 extends PRED_$dummy_parse_file46pl_10_11 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_parse_file46pl_10_11_2);
    }
}

class PRED_$dummy_parse_file46pl_10_11_1 extends PRED_$dummy_parse_file46pl_10_11 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("all");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");
    static IntegerTerm s4 = new IntegerTerm(0);
    static IntegerTerm s5 = new IntegerTerm(1);
    static SymbolTerm f7 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s8 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");
    static IntegerTerm s9 = new IntegerTerm(2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
        Predicate p1;
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
        Predicate cont = engine.cont;

        Term[] h6 = {a3, a4, a5, s3, s4, a6, s5, a7, a8, a9};
        a12 = new StructureTerm(f2, h6);
        Term[] h10 = {a3, a4, a5, s3, a10, a6, s9, a7, a8, a9};
        a14 = new StructureTerm(f2, h10);
        Term[] h11 = {s8, a14};
        a13 = new StructureTerm(f7, h11);
        p1 = new PRED_findall_3(a12, a13, a11, cont);
        return new PRED_best_parse_cats_2(a1, s1, p1);
    }
}

class PRED_$dummy_parse_file46pl_10_11_2 extends PRED_$dummy_parse_file46pl_10_11 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
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
        Predicate cont = engine.cont;

        return new PRED_best_parse_3(a1, a11, a2, cont);
    }
}

