package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_tdm_attributes_3.java
 * @procedure display_tdm_attributes/3 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_tdm_attributes_3 extends Predicate {
    static Predicate display_tdm_attributes_3_1 = new PRED_display_tdm_attributes_3_1();
    static Predicate display_tdm_attributes_3_2 = new PRED_display_tdm_attributes_3_2();
    static Predicate display_tdm_attributes_3_sub_1 = new PRED_display_tdm_attributes_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_display_tdm_attributes_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_display_tdm_attributes_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(display_tdm_attributes_3_1, display_tdm_attributes_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "display_tdm_attributes(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_display_tdm_attributes_3_sub_1 extends PRED_display_tdm_attributes_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tdm_attributes_3_2);
    }
}

class PRED_display_tdm_attributes_3_1 extends PRED_display_tdm_attributes_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("member", 2);
    static SymbolTerm f2 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f6 = SymbolTerm.makeSymbol(",", 2);
    static SymbolTerm f7 = SymbolTerm.makeSymbol("=..", 2);
    static SymbolTerm f9 = SymbolTerm.makeSymbol("reverse", 2);
    static SymbolTerm f11 = SymbolTerm.makeSymbol("display_tdm_attributes", 5);
    static SymbolTerm f13 = SymbolTerm.makeSymbol("semantics", 4);
    static SymbolTerm s14 = SymbolTerm.makeSymbol("inactive");
    static SymbolTerm f16 = SymbolTerm.makeSymbol("\\=", 2);
    static SymbolTerm s18 = SymbolTerm.makeSymbol("nl");
    static SymbolTerm f19 = SymbolTerm.makeSymbol("foreach", 2);
    static SymbolTerm f20 = SymbolTerm.makeSymbol("ne_tag", 2);
    static SymbolTerm f21 = SymbolTerm.makeSymbol("offsets", 2);
    static SymbolTerm f25 = SymbolTerm.makeSymbol("ne_tag", 3);
    static SymbolTerm f27 = SymbolTerm.makeSymbol("write", 1);
    static SymbolTerm s28 = SymbolTerm.makeSymbol("name ");
    static Term[] h59 = {s28};
    static StructureTerm s29 = new StructureTerm(f27, h59);
    static SymbolTerm s31 = SymbolTerm.makeSymbol(" ");
    static Term[] h60 = {s31};
    static StructureTerm s32 = new StructureTerm(f27, h60);
    static SymbolTerm s46 = SymbolTerm.makeSymbol("!");
    static SymbolTerm f47 = SymbolTerm.makeSymbol("write_semantics", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        a4 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a11 = new VariableTerm(engine);
        Term[] h4 = {new VariableTerm(engine), new VariableTerm(engine), a7, s3, new VariableTerm(engine), a8, new VariableTerm(engine), a9, a10, a11};
        a6 = new StructureTerm(f2, h4);
        Term[] h5 = {a6, a2};
        a5 = new StructureTerm(f1, h5);
        a15 = new VariableTerm(engine);
        a14 = new ListTerm(a15, new VariableTerm(engine));
        Term[] h8 = {a7, a14};
        a13 = new StructureTerm(f7, h8);
        a18 = new VariableTerm(engine);
        Term[] h10 = {a8, a18};
        a17 = new StructureTerm(f9, h10);
        Term[] h12 = {a15, a18, a9, a10, a3};
        a20 = new StructureTerm(f11, h12);
        a23 = new VariableTerm(engine);
        Term[] h15 = {a15, s14, a11, a23};
        a22 = new StructureTerm(f13, h15);
        Term[] h17 = {a23, s3};
        a25 = new StructureTerm(f16, h17);
        a31 = new VariableTerm(engine);
        a33 = new VariableTerm(engine);
        a34 = new VariableTerm(engine);
        Term[] h22 = {a33, a34};
        a32 = new StructureTerm(f21, h22);
        Term[] h23 = {a31, a32};
        a30 = new StructureTerm(f20, h23);
        Term[] h24 = {a30, a23};
        a29 = new StructureTerm(f1, h24);
        a37 = new VariableTerm(engine);
        Term[] h26 = {a31, a23, a37};
        a36 = new StructureTerm(f25, h26);
        Term[] h30 = {a33};
        a40 = new StructureTerm(f27, h30);
        Term[] h33 = {a34};
        a43 = new StructureTerm(f27, h33);
        Term[] h34 = {a37};
        a46 = new StructureTerm(f27, h34);
        Term[] h35 = {a31};
        a49 = new StructureTerm(f27, h35);
        Term[] h36 = {a49, s18};
        a48 = new StructureTerm(f6, h36);
        Term[] h37 = {s32, a48};
        a47 = new StructureTerm(f6, h37);
        Term[] h38 = {a46, a47};
        a45 = new StructureTerm(f6, h38);
        Term[] h39 = {s32, a45};
        a44 = new StructureTerm(f6, h39);
        Term[] h40 = {a43, a44};
        a42 = new StructureTerm(f6, h40);
        Term[] h41 = {s32, a42};
        a41 = new StructureTerm(f6, h41);
        Term[] h42 = {a40, a41};
        a39 = new StructureTerm(f6, h42);
        Term[] h43 = {s29, a39};
        a38 = new StructureTerm(f6, h43);
        Term[] h44 = {a36, a38};
        a35 = new StructureTerm(f6, h44);
        Term[] h45 = {a29, a35};
        a28 = new StructureTerm(f19, h45);
        a56 = new ListTerm(a23, s3);
        a55 = new ListTerm(a10, a56);
        a54 = new ListTerm(a9, a55);
        Term[] h48 = {a54};
        a53 = new StructureTerm(f47, h48);
        Term[] h49 = {a53, s18};
        a52 = new StructureTerm(f6, h49);
        Term[] h50 = {s46, a52};
        a51 = new StructureTerm(f6, h50);
        Term[] h51 = {s18, a51};
        a50 = new StructureTerm(f6, h51);
        Term[] h52 = {a28, a50};
        a27 = new StructureTerm(f6, h52);
        Term[] h53 = {s18, a27};
        a26 = new StructureTerm(f6, h53);
        Term[] h54 = {a25, a26};
        a24 = new StructureTerm(f6, h54);
        Term[] h55 = {a22, a24};
        a21 = new StructureTerm(f6, h55);
        Term[] h56 = {a20, a21};
        a19 = new StructureTerm(f6, h56);
        Term[] h57 = {a17, a19};
        a16 = new StructureTerm(f6, h57);
        Term[] h58 = {a13, a16};
        a12 = new StructureTerm(f6, h58);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_foreach_2(a5, a12, p1);
        p3 = new PRED_nonvar_1(a2, p2);
        return new PRED_$get_level_1(a4, p3);
    }
}

class PRED_display_tdm_attributes_3_2 extends PRED_display_tdm_attributes_3 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

