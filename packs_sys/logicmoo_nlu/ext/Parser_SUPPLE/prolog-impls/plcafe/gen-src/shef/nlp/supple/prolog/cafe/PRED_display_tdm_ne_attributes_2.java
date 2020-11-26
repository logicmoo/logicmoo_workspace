package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_tdm_ne_attributes_2.java
 * @procedure display_tdm_ne_attributes/2 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_tdm_ne_attributes_2 extends Predicate {
    static Predicate display_tdm_ne_attributes_2_1 = new PRED_display_tdm_ne_attributes_2_1();
    static Predicate display_tdm_ne_attributes_2_2 = new PRED_display_tdm_ne_attributes_2_2();
    static Predicate display_tdm_ne_attributes_2_sub_1 = new PRED_display_tdm_ne_attributes_2_sub_1();

    public Term arg1, arg2;

    public PRED_display_tdm_ne_attributes_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_display_tdm_ne_attributes_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(display_tdm_ne_attributes_2_1, display_tdm_ne_attributes_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "display_tdm_ne_attributes(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_display_tdm_ne_attributes_2_sub_1 extends PRED_display_tdm_ne_attributes_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tdm_ne_attributes_2_2);
    }
}

class PRED_display_tdm_ne_attributes_2_1 extends PRED_display_tdm_ne_attributes_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("member", 2);
    static SymbolTerm f2 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f6 = SymbolTerm.makeSymbol(",", 2);
    static SymbolTerm f7 = SymbolTerm.makeSymbol("=..", 2);
    static SymbolTerm f9 = SymbolTerm.makeSymbol("semantics", 4);
    static SymbolTerm s10 = SymbolTerm.makeSymbol("inactive");
    static SymbolTerm f12 = SymbolTerm.makeSymbol("foreach", 2);
    static SymbolTerm f13 = SymbolTerm.makeSymbol("ne_tag", 2);
    static SymbolTerm f14 = SymbolTerm.makeSymbol("offsets", 2);
    static SymbolTerm f18 = SymbolTerm.makeSymbol("ne_tag", 3);
    static SymbolTerm f20 = SymbolTerm.makeSymbol("write", 1);
    static SymbolTerm s21 = SymbolTerm.makeSymbol("name ");
    static Term[] h42 = {s21};
    static StructureTerm s22 = new StructureTerm(f20, h42);
    static SymbolTerm s24 = SymbolTerm.makeSymbol(" ");
    static Term[] h43 = {s24};
    static StructureTerm s25 = new StructureTerm(f20, h43);
    static SymbolTerm s29 = SymbolTerm.makeSymbol("nl");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        Term[] h4 = {new VariableTerm(engine), new VariableTerm(engine), a6, s3, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), a7};
        a5 = new StructureTerm(f2, h4);
        Term[] h5 = {a5, a2};
        a4 = new StructureTerm(f1, h5);
        a11 = new VariableTerm(engine);
        a10 = new ListTerm(a11, new VariableTerm(engine));
        Term[] h8 = {a6, a10};
        a9 = new StructureTerm(f7, h8);
        a14 = new VariableTerm(engine);
        Term[] h11 = {a11, s10, a7, a14};
        a13 = new StructureTerm(f9, h11);
        a18 = new VariableTerm(engine);
        a20 = new VariableTerm(engine);
        a21 = new VariableTerm(engine);
        Term[] h15 = {a20, a21};
        a19 = new StructureTerm(f14, h15);
        Term[] h16 = {a18, a19};
        a17 = new StructureTerm(f13, h16);
        Term[] h17 = {a17, a14};
        a16 = new StructureTerm(f1, h17);
        a24 = new VariableTerm(engine);
        Term[] h19 = {a18, a14, a24};
        a23 = new StructureTerm(f18, h19);
        Term[] h23 = {a20};
        a27 = new StructureTerm(f20, h23);
        Term[] h26 = {a21};
        a30 = new StructureTerm(f20, h26);
        Term[] h27 = {a24};
        a33 = new StructureTerm(f20, h27);
        Term[] h28 = {a18};
        a36 = new StructureTerm(f20, h28);
        Term[] h30 = {a36, s29};
        a35 = new StructureTerm(f6, h30);
        Term[] h31 = {s25, a35};
        a34 = new StructureTerm(f6, h31);
        Term[] h32 = {a33, a34};
        a32 = new StructureTerm(f6, h32);
        Term[] h33 = {s25, a32};
        a31 = new StructureTerm(f6, h33);
        Term[] h34 = {a30, a31};
        a29 = new StructureTerm(f6, h34);
        Term[] h35 = {s25, a29};
        a28 = new StructureTerm(f6, h35);
        Term[] h36 = {a27, a28};
        a26 = new StructureTerm(f6, h36);
        Term[] h37 = {s22, a26};
        a25 = new StructureTerm(f6, h37);
        Term[] h38 = {a23, a25};
        a22 = new StructureTerm(f6, h38);
        Term[] h39 = {a16, a22};
        a15 = new StructureTerm(f12, h39);
        Term[] h40 = {a13, a15};
        a12 = new StructureTerm(f6, h40);
        Term[] h41 = {a9, a12};
        a8 = new StructureTerm(f6, h41);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_foreach_2(a4, a8, p1);
        p3 = new PRED_nonvar_1(a2, p2);
        return new PRED_$get_level_1(a3, p3);
    }
}

class PRED_display_tdm_ne_attributes_2_2 extends PRED_display_tdm_ne_attributes_2 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

