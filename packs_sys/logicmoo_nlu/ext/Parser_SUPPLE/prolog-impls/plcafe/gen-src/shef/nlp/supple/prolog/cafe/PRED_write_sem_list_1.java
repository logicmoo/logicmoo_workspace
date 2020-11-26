package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_write_sem_list_1.java
 * @procedure write_sem_list/1 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_write_sem_list_1 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("]");

    public Term arg1;

    public PRED_write_sem_list_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_write_sem_list_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1;
        Predicate p1, p2;
        a1 = arg1.dereference();

        p1 = new PRED_write_1(s2, cont);
        p2 = new PRED_write_sem_list3_1(a1, p1);
        return new PRED_write_1(s1, p2);
    }

    public int arity() { return 1; }

    public String toString() {
        return "write_sem_list(" + arg1 + ")";
    }
}

