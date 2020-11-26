package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_best_parse_stats_3.java
 * @procedure display_best_parse_stats/3 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_best_parse_stats_3 extends Predicate {
    static Predicate display_best_parse_stats_3_1 = new PRED_display_best_parse_stats_3_1();
    static Predicate display_best_parse_stats_3_2 = new PRED_display_best_parse_stats_3_2();
    static Predicate display_best_parse_stats_3_sub_1 = new PRED_display_best_parse_stats_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_display_best_parse_stats_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_display_best_parse_stats_3(){}
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
        return engine.jtry(display_best_parse_stats_3_1, display_best_parse_stats_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "display_best_parse_stats(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_display_best_parse_stats_3_sub_1 extends PRED_display_best_parse_stats_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_best_parse_stats_3_2);
    }
}

class PRED_display_best_parse_stats_3_1 extends PRED_display_best_parse_stats_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("Sentence No.: ");
    static IntegerTerm s2 = new IntegerTerm(1);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("Coverage is ");
    static SymbolTerm s4 = SymbolTerm.makeSymbol(" tokens out of ");
    static SymbolTerm s5 = SymbolTerm.makeSymbol(" (");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("%)");
    static SymbolTerm f7 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s8 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");
    static SymbolTerm f9 = SymbolTerm.makeSymbol("edge", 10);
    static IntegerTerm s10 = new IntegerTerm(3);
    static SymbolTerm s13 = SymbolTerm.makeSymbol(" inactive edges in final chart, ");
    static SymbolTerm s14 = SymbolTerm.makeSymbol(" edges in best parse");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        Term[] h11 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), s10, new VariableTerm(engine), new VariableTerm(engine), a10};
        a12 = new StructureTerm(f9, h11);
        Term[] h12 = {s8, a12};
        a11 = new StructureTerm(f7, h12);
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        a15 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_tell_1(a6, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_vwrite_1(s14, p3);
        p5 = new PRED_vwrite_1(a15, p4);
        p6 = new PRED_length_2(a2, a15, p5);
        p7 = new PRED_vwrite_1(s13, p6);
        p8 = new PRED_vwrite_1(a14, p7);
        p9 = new PRED_length_2(a13, a14, p8);
        p10 = new PRED_findall_3(a10, a11, a13, p9);
        p11 = new PRED_nl_0(p10);
        p12 = new PRED_vwrite_1(s6, p11);
        p13 = new PRED_vwrite_1(a9, p12);
        p14 = new PRED_vwrite_1(s5, p13);
        p15 = new PRED_vwrite_1(a3, p14);
        p16 = new PRED_vwrite_1(s4, p15);
        p17 = new PRED_vwrite_1(a8, p16);
        p18 = new PRED_vwrite_1(s3, p17);
        p19 = new PRED_$dummy_plcafe_supple_io46pl_6_3(a3, a8, a9, p18);
        p20 = new PRED_$minus_3(a3, a7, a8, p19);
        p21 = new PRED_calculate_skip_4(a2, s2, a3, a7, p20);
        p22 = new PRED_nl_0(p21);
        p23 = new PRED_vwrite_1(a1, p22);
        p24 = new PRED_vwrite_1(s1, p23);
        p25 = new PRED_$dummy_plcafe_supple_io46pl_5_2(a5, new VariableTerm(engine), p24);
        p26 = new PRED_telling_1(a6, p25);
        p27 = new PRED_$dummy_plcafe_supple_io46pl_4_1(a5, p26);
        return new PRED_$get_level_1(a4, p27);
    }
}

class PRED_display_best_parse_stats_3_2 extends PRED_display_best_parse_stats_3 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

