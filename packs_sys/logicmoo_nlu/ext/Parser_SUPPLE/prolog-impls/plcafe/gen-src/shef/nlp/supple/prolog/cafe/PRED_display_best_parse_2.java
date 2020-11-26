package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_best_parse_2.java
 * @procedure display_best_parse/2 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_best_parse_2 extends Predicate {
    static Predicate display_best_parse_2_1 = new PRED_display_best_parse_2_1();
    static Predicate display_best_parse_2_2 = new PRED_display_best_parse_2_2();
    static Predicate display_best_parse_2_sub_1 = new PRED_display_best_parse_2_sub_1();

    public Term arg1, arg2;

    public PRED_display_best_parse_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_display_best_parse_2(){}
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
        return engine.jtry(display_best_parse_2_1, display_best_parse_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "display_best_parse(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_display_best_parse_2_sub_1 extends PRED_display_best_parse_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_best_parse_2_2);
    }
}

class PRED_display_best_parse_2_1 extends PRED_display_best_parse_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("member", 2);
    static SymbolTerm f2 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f6 = SymbolTerm.makeSymbol(",", 2);
    static SymbolTerm f7 = SymbolTerm.makeSymbol("=..", 2);
    static SymbolTerm f9 = SymbolTerm.makeSymbol("reverse", 2);
    static SymbolTerm f11 = SymbolTerm.makeSymbol(";", 2);
    static SymbolTerm s12 = SymbolTerm.makeSymbol("bracketed_parses");
    static SymbolTerm f13 = SymbolTerm.makeSymbol("display_bracketed_tree", 3);
    static SymbolTerm f16 = SymbolTerm.makeSymbol("display_tree", 3);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21;
        Predicate p1, p2, p3, p4, p5, p6, p7;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        Term[] h4 = {new VariableTerm(engine), new VariableTerm(engine), a8, s3, new VariableTerm(engine), a9, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a7 = new StructureTerm(f2, h4);
        Term[] h5 = {a7, a2};
        a6 = new StructureTerm(f1, h5);
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        a12 = new ListTerm(a13, a14);
        Term[] h8 = {a8, a12};
        a11 = new StructureTerm(f7, h8);
        a17 = new VariableTerm(engine);
        Term[] h10 = {a9, a17};
        a16 = new StructureTerm(f9, h10);
        Term[] h14 = {a13, a14, a17};
        a20 = new StructureTerm(f13, h14);
        Term[] h15 = {s12, a20};
        a19 = new StructureTerm(f6, h15);
        Term[] h17 = {a13, a14, a17};
        a21 = new StructureTerm(f16, h17);
        Term[] h18 = {a19, a21};
        a18 = new StructureTerm(f11, h18);
        Term[] h19 = {a16, a18};
        a15 = new StructureTerm(f6, h19);
        Term[] h20 = {a11, a15};
        a10 = new StructureTerm(f6, h20);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_tell_1(a5, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_foreach_2(a6, a10, p3);
        p5 = new PRED_tell_1(a4, p4);
        p6 = new PRED_telling_1(a5, p5);
        p7 = new PRED_best_parse_file_1(a4, p6);
        return new PRED_$get_level_1(a3, p7);
    }
}

class PRED_display_best_parse_2_2 extends PRED_display_best_parse_2 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

