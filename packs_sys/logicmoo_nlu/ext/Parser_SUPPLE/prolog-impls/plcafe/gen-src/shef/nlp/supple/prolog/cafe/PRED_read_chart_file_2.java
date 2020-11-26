package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_read_chart_file_2.java
 * @procedure read_chart_file/2 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_read_chart_file_2 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Term arg1, arg2;

    public PRED_read_chart_file_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_read_chart_file_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3, p4, p5, p6, p7;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_seen_0(p1);
        p3 = new PRED_reverse_2(a4, a2, p2);
        p4 = new PRED_read_charts_2(s1, a4, p3);
        p5 = new PRED_see_1(a1, p4);
        p6 = new PRED_seen_0(p5);
        p7 = new PRED_see_1(a1, p6);
        return new PRED_$get_level_1(a3, p7);
    }

    public int arity() { return 2; }

    public String toString() {
        return "read_chart_file(" + arg1 + ", " + arg2 + ")";
    }
}

