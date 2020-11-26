package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_extract_match_3.java
 * @procedure extract_match/3 in update_name_match.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_extract_match_3 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate extract_match_3_1 = new PRED_extract_match_3_1();
    static Predicate extract_match_3_2 = new PRED_extract_match_3_2();
    static Predicate extract_match_3_var = new PRED_extract_match_3_var();

    public Term arg1, arg2, arg3;

    public PRED_extract_match_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_extract_match_3(){}
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
        return engine.switch_on_term(
                                   extract_match_3_var,
                                   fail_0,
                                   extract_match_3_2,
                                   fail_0,
                                   extract_match_3_1
                                   );
    }

    public int arity() { return 3; }

    public String toString() {
        return "extract_match(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_extract_match_3_var extends PRED_extract_match_3 {
    static Predicate extract_match_3_var_1 = new PRED_extract_match_3_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(extract_match_3_1, extract_match_3_var_1);
    }
}

class PRED_extract_match_3_var_1 extends PRED_extract_match_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(extract_match_3_2);
    }
}

class PRED_extract_match_3_1 extends PRED_extract_match_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("match", 2);
    static SymbolTerm f2 = SymbolTerm.makeSymbol("list", 1);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f6 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s7 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");
    static SymbolTerm f8 = SymbolTerm.makeSymbol("member", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a4 = ((ListTerm)a1).car();
            a5 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isList() ){
            a6 = ((ListTerm)a5).car();
            a7 = ((ListTerm)a5).cdr();
        } else if ( a5.isVariable() ){
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            if ( !a5.unify(new ListTerm(a6, a7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isList() ){
            a8 = ((ListTerm)a7).car();
            a9 = ((ListTerm)a7).cdr();
        } else if ( a7.isVariable() ){
            a8 = new VariableTerm(engine);
            a9 = new VariableTerm(engine);
            if ( !a7.unify(new ListTerm(a8, a9), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a14 = new VariableTerm(engine);
        a17 = new VariableTerm(engine);
        a18 = new VariableTerm(engine);
        a16 = new ListTerm(a17, a18);
        Term[] h3 = {a16};
        a15 = new StructureTerm(f2, h3);
        Term[] h4 = {a14, a15};
        a13 = new StructureTerm(f1, h4);
        a12 = new ListTerm(a13, s5);
        a11 = new ListTerm(a6, a12);
        a10 = new ListTerm(a4, a11);
        a23 = new ListTerm(a17, a18);
        Term[] h9 = {a23};
        a22 = new StructureTerm(f2, h9);
        Term[] h10 = {a14, a22};
        a21 = new StructureTerm(f1, h10);
        Term[] h11 = {a21, a8};
        a20 = new StructureTerm(f8, h11);
        Term[] h12 = {s7, a20};
        a19 = new StructureTerm(f6, h12);
        a24 = new VariableTerm(engine);
        a25 = new VariableTerm(engine);
        a26 = new VariableTerm(engine);
        a27 = new VariableTerm(engine);
        p1 = new PRED_append_3(a25, a27, a3, cont);
        p2 = new PRED_append_3(a24, a26, a2, p1);
        p3 = new PRED_extract_match_3(a9, a26, a27, p2);
        p4 = new PRED_generate_uninstantiated_2(a24, a25, p3);
        return new PRED_findall_3(a10, a19, a24, p4);
    }
}

class PRED_extract_match_3_2 extends PRED_extract_match_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        return cont;
    }
}

