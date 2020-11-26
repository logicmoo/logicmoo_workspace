package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_default_table_2.java
 * @procedure default_table/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_default_table_2 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_root");

    public Term arg1, arg2;

    public PRED_default_table_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_default_table_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        if ( a2.isList() ){
            a3 = ((ListTerm)a2).car();
            a4 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a3, a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a3.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s2.unify(args[0],  engine.trail) )
                return engine.fail();
            if ( !a1.unify(args[1],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s2, a1};
            if ( !a3.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            if ( !s3.unify(((ListTerm)a4).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, s3), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
            if ( !a1.unify(args[1],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, a1};
            if ( !a5.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return cont;
    }

    public int arity() { return 2; }

    public String toString() {
        return "default_table(" + arg1 + ", " + arg2 + ")";
    }
}

