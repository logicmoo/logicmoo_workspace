package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_add_edge_11.java
 * @procedure add_edge/11 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_add_edge_11 extends Predicate {
    static Predicate add_edge_11_1 = new PRED_add_edge_11_1();
    static Predicate add_edge_11_2 = new PRED_add_edge_11_2();
    static Predicate add_edge_11_3 = new PRED_add_edge_11_3();
    static Predicate add_edge_11_sub_1 = new PRED_add_edge_11_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11;

    public PRED_add_edge_11(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Term a7, Term a8, Term a9, Term a10, Term a11, Predicate cont) {
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

    public PRED_add_edge_11(){}
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
        return engine.jtry(add_edge_11_1, add_edge_11_sub_1);
    }

    public int arity() { return 11; }

    public String toString() {
        return "add_edge(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ", " + arg7 + ", " + arg8 + ", " + arg9 + ", " + arg10 + ", " + arg11 + ")";
    }
}

class PRED_add_edge_11_sub_1 extends PRED_add_edge_11 {
    static Predicate add_edge_11_sub_2 = new PRED_add_edge_11_sub_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(add_edge_11_2, add_edge_11_sub_2);
    }
}

class PRED_add_edge_11_sub_2 extends PRED_add_edge_11 {

    public Predicate exec(Prolog engine) {
        return engine.trust(add_edge_11_3);
    }
}

class PRED_add_edge_11_1 extends PRED_add_edge_11 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("edge");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("sem");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("head");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("s_form");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37;
        Predicate p1, p2, p3, p4, p5;
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

        if ( !s1.unify(a4, engine.trail) ) return engine.fail();
        a12 = new VariableTerm(engine);
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        a16 = new VariableTerm(engine);
        a19 = new VariableTerm(engine);
        Term[] h4 = {s3, a19};
        a18 = new StructureTerm(f2, h4);
        a22 = new VariableTerm(engine);
        Term[] h6 = {s5, a22};
        a21 = new StructureTerm(f2, h6);
        a25 = new VariableTerm(engine);
        Term[] h8 = {s7, a25};
        a24 = new StructureTerm(f2, h8);
        Term[] h10 = {s9, new VariableTerm(engine)};
        a27 = new StructureTerm(f2, h10);
        a26 = new ListTerm(a27, new VariableTerm(engine));
        a23 = new ListTerm(a24, a26);
        a20 = new ListTerm(a21, a23);
        a17 = new ListTerm(a18, a20);
        a15 = new ListTerm(a16, a17);
        Term[] h11 = {s3, a19};
        a30 = new StructureTerm(f2, h11);
        a33 = new VariableTerm(engine);
        Term[] h12 = {s5, a33};
        a32 = new StructureTerm(f2, h12);
        Term[] h13 = {s7, a25};
        a35 = new StructureTerm(f2, h13);
        Term[] h14 = {s9, new VariableTerm(engine)};
        a37 = new StructureTerm(f2, h14);
        a36 = new ListTerm(a37, new VariableTerm(engine));
        a34 = new ListTerm(a35, a36);
        a31 = new ListTerm(a32, a34);
        a29 = new ListTerm(a30, a31);
        a28 = new ListTerm(a16, a29);
        p1 = new PRED_$cut_1(a12, cont);
        p2 = new PRED_$dummy_supple46pl_0_4(a5, a14, a22, a33, p1);
        p3 = new PRED_$614646_2(a13, a28, p2);
        p4 = new PRED_$614646_2(a3, a15, p3);
        p5 = new PRED_edge_10(a1, a2, a13, s1, a14, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), p4);
        return new PRED_$get_level_1(a12, p5);
    }
}

class PRED_add_edge_11_2 extends PRED_add_edge_11 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
        Predicate p1, p2;
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

        a12 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a12, cont);
        p2 = new PRED_edge_10(a1, a2, a3, a4, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), p1);
        return new PRED_$get_level_1(a12, p2);
    }
}

class PRED_add_edge_11_3 extends PRED_add_edge_11 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static IntegerTerm s2 = new IntegerTerm(2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
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

        if ( !s1.unify(a4, engine.trail) ) return engine.fail();
        p1 = new PRED_combine_edge_8(a1, a2, a3, s2, a8, a9, a10, a11, cont);
        return new PRED_assert_edge_10(a1, a2, a3, s1, a5, a6, a7, a8, a9, a10, p1);
    }
}

