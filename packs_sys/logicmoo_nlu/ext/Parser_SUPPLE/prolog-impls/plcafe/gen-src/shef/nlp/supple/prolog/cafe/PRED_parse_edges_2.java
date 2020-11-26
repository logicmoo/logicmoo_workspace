package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_parse_edges_2.java
 * @procedure parse_edges/2 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_parse_edges_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate parse_edges_2_1 = new PRED_parse_edges_2_1();
    static Predicate parse_edges_2_2 = new PRED_parse_edges_2_2();
    static Predicate parse_edges_2_var = new PRED_parse_edges_2_var();

    public Term arg1, arg2;

    public PRED_parse_edges_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_parse_edges_2(){}
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
        return engine.switch_on_term(
                                   parse_edges_2_var,
                                   fail_0,
                                   parse_edges_2_1,
                                   fail_0,
                                   parse_edges_2_2
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "parse_edges(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_parse_edges_2_var extends PRED_parse_edges_2 {
    static Predicate parse_edges_2_var_1 = new PRED_parse_edges_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(parse_edges_2_1, parse_edges_2_var_1);
    }
}

class PRED_parse_edges_2_var_1 extends PRED_parse_edges_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(parse_edges_2_2);
    }
}

class PRED_parse_edges_2_1 extends PRED_parse_edges_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_parse_edges_2_2 extends PRED_parse_edges_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).car();
            a4 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a3, a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a3.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            a5 = args[0];
            a6 = args[1];
            a7 = args[2];
            a8 = args[3];
            a9 = args[4];
            a10 = args[5];
            a11 = args[6];
            a12 = args[7];
            a13 = args[8];
            a14 = args[9];
        } else if (a3.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            Term[] args = {a5, a6, a7, a8, a9, a10, a11, a12, a13, a14};
            if ( !a3.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a15 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a15, cont);
        p2 = new PRED_parse_edges_2(a4, a2, p1);
        p3 = new PRED_add_edge_11(a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a2, p2);
        return new PRED_$get_level_1(a15, p3);
    }
}

