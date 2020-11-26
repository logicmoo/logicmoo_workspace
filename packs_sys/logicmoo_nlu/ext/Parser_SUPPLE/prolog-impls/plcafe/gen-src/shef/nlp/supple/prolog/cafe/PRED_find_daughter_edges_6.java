package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_find_daughter_edges_6.java
 * @procedure find_daughter_edges/6 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_find_daughter_edges_6 extends Predicate {
    static Predicate find_daughter_edges_6_1 = new PRED_find_daughter_edges_6_1();
    static Predicate find_daughter_edges_6_2 = new PRED_find_daughter_edges_6_2();
    static Predicate find_daughter_edges_6_3 = new PRED_find_daughter_edges_6_3();
    static Predicate find_daughter_edges_6_sub_1 = new PRED_find_daughter_edges_6_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6;

    public PRED_find_daughter_edges_6(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        arg6 = a6; 
        this.cont = cont;
    }

    public PRED_find_daughter_edges_6(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        arg6 = args[5]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.aregs[6] = arg6;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(find_daughter_edges_6_1, find_daughter_edges_6_sub_1);
    }

    public int arity() { return 6; }

    public String toString() {
        return "find_daughter_edges(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ")";
    }
}

class PRED_find_daughter_edges_6_sub_1 extends PRED_find_daughter_edges_6 {
    static Predicate find_daughter_edges_6_sub_2 = new PRED_find_daughter_edges_6_sub_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(find_daughter_edges_6_2, find_daughter_edges_6_sub_2);
    }
}

class PRED_find_daughter_edges_6_sub_2 extends PRED_find_daughter_edges_6 {

    public Predicate exec(Prolog engine) {
        return engine.trust(find_daughter_edges_6_3);
    }
}

class PRED_find_daughter_edges_6_1 extends PRED_find_daughter_edges_6 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("top");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("top", 4);
    static SymbolTerm f3 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("s_form");
    static Term[] h14 = {s4, s1};
    static StructureTerm s5 = new StructureTerm(f3, h14);
    static SymbolTerm s6 = SymbolTerm.makeSymbol("m_root");
    static Term[] h15 = {s6, s1};
    static StructureTerm s7 = new StructureTerm(f3, h15);
    static SymbolTerm s8 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s13 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        if ( a5.isList() ){
            a7 = ((ListTerm)a5).car();
            a8 = ((ListTerm)a5).cdr();
        } else if ( a5.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a5.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a9 = new ListTerm(s1, new VariableTerm(engine));
        Term[] h9 = {s8, new VariableTerm(engine)};
        a11 = new StructureTerm(f3, h9);
        Term[] h11 = {s10, new VariableTerm(engine)};
        a12 = new StructureTerm(f3, h11);
        Term[] h12 = {s5, s7, a11, a12};
        a10 = new StructureTerm(f2, h12);
        a13 = new VariableTerm(engine);
        p1 = new PRED_find_daughter_edges_6(a1, a2, a13, a4, a8, a6, cont);
        p2 = new PRED_edge_10(new VariableTerm(engine), a1, a10, s13, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), a13, new VariableTerm(engine), new VariableTerm(engine), p1);
        return new PRED_$614646_2(a7, a9, p2);
    }
}

class PRED_find_daughter_edges_6_2 extends PRED_find_daughter_edges_6 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        if ( a5.isList() ){
            a7 = ((ListTerm)a5).car();
            a8 = ((ListTerm)a5).cdr();
        } else if ( a5.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a5.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a9 = ((ListTerm)a6).car();
            a10 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a11 = new VariableTerm(engine);
        a12 = new VariableTerm(engine);
        p1 = new PRED_find_daughter_edges_6(a11, a2, a12, a4, a8, a10, cont);
        p2 = new PRED_edge_10(a11, a1, a7, s1, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), a12, new VariableTerm(engine), a9, p1);
        return new PRED_$dummy_supple46pl_3_2(a7, new VariableTerm(engine), p2);
    }
}

class PRED_find_daughter_edges_6_3 extends PRED_find_daughter_edges_6 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        if ( !a1.unify(a2, engine.trail) ) return engine.fail();
        if ( !a3.unify(a4, engine.trail) ) return engine.fail();
        if ( !s1.unify(a5, engine.trail) ) return engine.fail();
        if ( !s1.unify(a6, engine.trail) ) return engine.fail();
        return cont;
    }
}

