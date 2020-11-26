package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_generate_name_match_2.java
 * @procedure generate_name_match/2 in update_name_match.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_generate_name_match_2 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");
    static SymbolTerm f4 = SymbolTerm.makeSymbol("^", 2);
    static SymbolTerm f5 = SymbolTerm.makeSymbol("member", 2);
    static SymbolTerm f6 = SymbolTerm.makeSymbol("match", 2);
    static SymbolTerm f7 = SymbolTerm.makeSymbol("list", 1);
    static SymbolTerm f20 = SymbolTerm.makeSymbol("name_match", 2);
    static SymbolTerm f22 = SymbolTerm.makeSymbol(",", 2);
    static SymbolTerm f24 = SymbolTerm.makeSymbol("delete", 3);

    public Term arg1, arg2;

    public PRED_generate_name_match_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_generate_name_match_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42;
        Predicate p1, p2;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        a4 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a7 = new ListTerm(a8, s1);
        a5 = new ListTerm(a6, a7);
        a3 = new ListTerm(a4, a5);
        a11 = new VariableTerm(engine);
        Term[] h8 = {a11};
        a17 = new StructureTerm(f7, h8);
        Term[] h9 = {a8, a17};
        a16 = new StructureTerm(f6, h9);
        a15 = new ListTerm(a16, s1);
        a14 = new ListTerm(a6, a15);
        a13 = new ListTerm(a4, a14);
        Term[] h10 = {a13, a1};
        a12 = new StructureTerm(f5, h10);
        Term[] h11 = {a11, a12};
        a10 = new StructureTerm(f4, h11);
        Term[] h12 = {s3, a10};
        a9 = new StructureTerm(f2, h12);
        a18 = new VariableTerm(engine);
        Term[] h13 = {a11};
        a28 = new StructureTerm(f7, h13);
        Term[] h14 = {a8, a28};
        a27 = new StructureTerm(f6, h14);
        a26 = new ListTerm(a27, s1);
        a25 = new ListTerm(a6, a26);
        a24 = new ListTerm(a4, a25);
        Term[] h15 = {a24, a1};
        a23 = new StructureTerm(f5, h15);
        Term[] h16 = {a11, a23};
        a22 = new StructureTerm(f4, h16);
        Term[] h17 = {a6, a22};
        a21 = new StructureTerm(f4, h17);
        Term[] h18 = {a4, a21};
        a20 = new StructureTerm(f4, h18);
        Term[] h19 = {s3, a20};
        a19 = new StructureTerm(f2, h19);
        a29 = new VariableTerm(engine);
        a34 = new VariableTerm(engine);
        Term[] h21 = {a8, a34};
        a33 = new StructureTerm(f20, h21);
        a32 = new ListTerm(a33, s1);
        a31 = new ListTerm(a6, a32);
        a30 = new ListTerm(a4, a31);
        a40 = new ListTerm(a8, s1);
        a39 = new ListTerm(a6, a40);
        a38 = new ListTerm(a4, a39);
        Term[] h23 = {a38, a18};
        a37 = new StructureTerm(f5, h23);
        a42 = new ListTerm(a8, s1);
        Term[] h25 = {a42, a29, a34};
        a41 = new StructureTerm(f24, h25);
        Term[] h26 = {a37, a41};
        a36 = new StructureTerm(f22, h26);
        Term[] h27 = {s3, a36};
        a35 = new StructureTerm(f2, h27);
        p1 = new PRED_findall_3(a30, a35, a2, cont);
        p2 = new PRED_findall_3(a8, a19, a29, p1);
        return new PRED_findall_3(a3, a9, a18, p2);
    }

    public int arity() { return 2; }

    public String toString() {
        return "generate_name_match(" + arg1 + ", " + arg2 + ")";
    }
}

