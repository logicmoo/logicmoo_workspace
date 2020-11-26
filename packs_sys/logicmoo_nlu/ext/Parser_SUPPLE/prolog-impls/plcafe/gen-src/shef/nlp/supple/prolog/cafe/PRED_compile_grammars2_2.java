package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_grammars2_2.java
 * @procedure compile_grammars2/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_grammars2_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate compile_grammars2_2_1 = new PRED_compile_grammars2_2_1();
    static Predicate compile_grammars2_2_2 = new PRED_compile_grammars2_2_2();
    static Predicate compile_grammars2_2_3 = new PRED_compile_grammars2_2_3();
    static Predicate compile_grammars2_2_lis = new PRED_compile_grammars2_2_lis();
    static Predicate compile_grammars2_2_var = new PRED_compile_grammars2_2_var();

    public Term arg1, arg2;

    public PRED_compile_grammars2_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_compile_grammars2_2(){}
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
                                   compile_grammars2_2_var,
                                   fail_0,
                                   compile_grammars2_2_1,
                                   fail_0,
                                   compile_grammars2_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "compile_grammars2(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_compile_grammars2_2_var extends PRED_compile_grammars2_2 {
    static Predicate compile_grammars2_2_var_1 = new PRED_compile_grammars2_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(compile_grammars2_2_1, compile_grammars2_2_var_1);
    }
}

class PRED_compile_grammars2_2_var_1 extends PRED_compile_grammars2_2 {
    static Predicate compile_grammars2_2_var_2 = new PRED_compile_grammars2_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(compile_grammars2_2_2, compile_grammars2_2_var_2);
    }
}

class PRED_compile_grammars2_2_var_2 extends PRED_compile_grammars2_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(compile_grammars2_2_3);
    }
}

class PRED_compile_grammars2_2_lis extends PRED_compile_grammars2_2 {
    static Predicate compile_grammars2_2_lis_1 = new PRED_compile_grammars2_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(compile_grammars2_2_2, compile_grammars2_2_lis_1);
    }
}

class PRED_compile_grammars2_2_lis_1 extends PRED_compile_grammars2_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(compile_grammars2_2_3);
    }
}

class PRED_compile_grammars2_2_1 extends PRED_compile_grammars2_2 {
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

class PRED_compile_grammars2_2_2 extends PRED_compile_grammars2_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("rule", 2);
    static SymbolTerm f4 = SymbolTerm.makeSymbol("word", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4, p5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).car();
            if ( !s1.unify(((ListTerm)a1).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a3, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h3 = {new VariableTerm(engine), new VariableTerm(engine)};
        a4 = new StructureTerm(f2, h3);
        Term[] h5 = {new VariableTerm(engine), new VariableTerm(engine)};
        a5 = new StructureTerm(f4, h5);
        Term[] h6 = {new VariableTerm(engine), new VariableTerm(engine)};
        a6 = new StructureTerm(f2, h6);
        Term[] h7 = {new VariableTerm(engine), new VariableTerm(engine)};
        a7 = new StructureTerm(f4, h7);
        p1 = new PRED_retractall_1(a7, cont);
        p2 = new PRED_retractall_1(a6, p1);
        p3 = new PRED_$dummy_compile_grammar46pl_0_2(a2, new VariableTerm(engine), p2);
        p4 = new PRED_consult_1(a3, p3);
        p5 = new PRED_retractall_1(a5, p4);
        return new PRED_retractall_1(a4, p5);
    }
}

class PRED_compile_grammars2_2_3 extends PRED_compile_grammars2_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("rule", 2);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("word", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4;
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
        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine)};
        a5 = new StructureTerm(f1, h2);
        Term[] h4 = {new VariableTerm(engine), new VariableTerm(engine)};
        a6 = new StructureTerm(f3, h4);
        a7 = new VariableTerm(engine);
        p1 = new PRED_compile_grammars2_2(a4, a7, cont);
        p2 = new PRED_compile_grammar_2(a2, a7, p1);
        p3 = new PRED_consult_1(a3, p2);
        p4 = new PRED_retractall_1(a6, p3);
        return new PRED_retractall_1(a5, p4);
    }
}

