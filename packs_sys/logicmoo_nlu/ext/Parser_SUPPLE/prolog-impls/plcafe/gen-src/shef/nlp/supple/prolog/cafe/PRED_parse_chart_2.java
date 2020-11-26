package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_parse_chart_2.java
 * @procedure parse_chart/2 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_parse_chart_2 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("chart", 3);
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static IntegerTerm s3 = new IntegerTerm(1);
    static SymbolTerm f4 = SymbolTerm.makeSymbol("gensymmark", 2);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("");

    public Term arg1, arg2;

    public PRED_parse_chart_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_parse_chart_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
        Predicate p1, p2, p3;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        if ( a1.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a1).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a1).args();
            a3 = args[0];
            a4 = args[1];
            a5 = args[2];
        } else if (a1.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            Term[] args = {a3, a4, a5};
            if ( !a1.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a3.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            a6 = args[1];
        } else if (a3.isVariable() ){
            a6 = new VariableTerm(engine);
            Term[] args = {new VariableTerm(engine), a6};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a4).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a4).args();
            a7 = args[1];
        } else if (a4.isVariable() ){
            a7 = new VariableTerm(engine);
            Term[] args = {new VariableTerm(engine), a7};
            if ( !a4.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            a8 = args[1];
        } else if (a5.isVariable() ){
            a8 = new VariableTerm(engine);
            Term[] args = {new VariableTerm(engine), a8};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a9 = new VariableTerm(engine);
        Term[] h6 = {s5, new VariableTerm(engine)};
        a10 = new StructureTerm(f4, h6);
        Term[] h7 = {s5, a9};
        a11 = new StructureTerm(f4, h7);
        p1 = new PRED_parse_chart2_3(a7, a2, a6, cont);
        p2 = new PRED_assert_1(a11, p1);
        p3 = new PRED_retractall_1(a10, p2);
        return new PRED_$minus_3(a8, s3, a9, p3);
    }

    public int arity() { return 2; }

    public String toString() {
        return "parse_chart(" + arg1 + ", " + arg2 + ")";
    }
}

