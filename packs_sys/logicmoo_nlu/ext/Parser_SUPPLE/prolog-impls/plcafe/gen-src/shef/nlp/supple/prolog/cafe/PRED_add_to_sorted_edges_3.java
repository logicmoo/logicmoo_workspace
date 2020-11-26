package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_add_to_sorted_edges_3.java
 * @procedure add_to_sorted_edges/3 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_add_to_sorted_edges_3 extends Predicate {
    static Predicate add_to_sorted_edges_3_1 = new PRED_add_to_sorted_edges_3_1();
    static Predicate add_to_sorted_edges_3_2 = new PRED_add_to_sorted_edges_3_2();
    static Predicate add_to_sorted_edges_3_sub_1 = new PRED_add_to_sorted_edges_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_add_to_sorted_edges_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_add_to_sorted_edges_3(){}
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
        return engine.jtry(add_to_sorted_edges_3_1, add_to_sorted_edges_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "add_to_sorted_edges(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_add_to_sorted_edges_3_sub_1 extends PRED_add_to_sorted_edges_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(add_to_sorted_edges_3_2);
    }
}

class PRED_add_to_sorted_edges_3_1 extends PRED_add_to_sorted_edges_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
        Predicate p1, p2, p3, p4, p5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            a4 = ((ListTerm)a2).car();
            a5 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a3.isList() ){
            if ( !a4.unify(((ListTerm)a3).car(), engine.trail) )
                return engine.fail();
            a6 = ((ListTerm)a3).cdr();
        } else if ( a3.isVariable() ){
            a6 = new VariableTerm(engine);
            if ( !a3.unify(new ListTerm(a4, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a7 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        Term[] h2 = {a9, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a8 = new StructureTerm(f1, h2);
        a11 = new VariableTerm(engine);
        Term[] h3 = {a11, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a10 = new StructureTerm(f1, h3);
        p1 = new PRED_add_to_sorted_edges_3(a1, a5, a6, cont);
        p2 = new PRED_$cut_1(a7, p1);
        p3 = new PRED_$less_or_equal_2(a11, a9, p2);
        p4 = new PRED_$unify_2(a4, a10, p3);
        p5 = new PRED_$unify_2(a1, a8, p4);
        return new PRED_$get_level_1(a7, p5);
    }
}

class PRED_add_to_sorted_edges_3_2 extends PRED_add_to_sorted_edges_3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a3.isList() ){
            if ( !a1.unify(((ListTerm)a3).car(), engine.trail) )
                return engine.fail();
            if ( !a2.unify(((ListTerm)a3).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a3.isVariable() ){
            if ( !a3.unify(new ListTerm(a1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return cont;
    }
}

