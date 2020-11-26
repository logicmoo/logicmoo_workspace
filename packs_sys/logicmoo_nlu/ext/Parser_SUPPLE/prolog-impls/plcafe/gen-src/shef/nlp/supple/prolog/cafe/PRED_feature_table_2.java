package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_feature_table_2.java
 * @procedure feature_table/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_feature_table_2 extends Predicate {
    static Predicate feature_table_2_1 = new PRED_feature_table_2_1();
    static Predicate feature_table_2_2 = new PRED_feature_table_2_2();
    static Predicate feature_table_2_3 = new PRED_feature_table_2_3();
    static Predicate feature_table_2_4 = new PRED_feature_table_2_4();
    static Predicate feature_table_2_5 = new PRED_feature_table_2_5();
    static Predicate feature_table_2_6 = new PRED_feature_table_2_6();
    static Predicate feature_table_2_7 = new PRED_feature_table_2_7();
    static Predicate feature_table_2_8 = new PRED_feature_table_2_8();
    static Predicate feature_table_2_9 = new PRED_feature_table_2_9();
    static Predicate feature_table_2_10 = new PRED_feature_table_2_10();
    static Predicate feature_table_2_11 = new PRED_feature_table_2_11();
    static Predicate feature_table_2_12 = new PRED_feature_table_2_12();
    static Predicate feature_table_2_int = new PRED_feature_table_2_int();
    static Predicate feature_table_2_lis = new PRED_feature_table_2_lis();
    static Predicate feature_table_2_str = new PRED_feature_table_2_str();
    static Predicate feature_table_2_var = new PRED_feature_table_2_var();

    public Term arg1, arg2;

    public PRED_feature_table_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_feature_table_2(){}
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
                                   feature_table_2_var,
                                   feature_table_2_int,
                                   feature_table_2_var,
                                   feature_table_2_str,
                                   feature_table_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "feature_table(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_feature_table_2_var extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_1 = new PRED_feature_table_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(feature_table_2_1, feature_table_2_var_1);
    }
}

class PRED_feature_table_2_var_1 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_2 = new PRED_feature_table_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_2, feature_table_2_var_2);
    }
}

class PRED_feature_table_2_var_2 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_3 = new PRED_feature_table_2_var_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_3, feature_table_2_var_3);
    }
}

class PRED_feature_table_2_var_3 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_4 = new PRED_feature_table_2_var_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_4, feature_table_2_var_4);
    }
}

class PRED_feature_table_2_var_4 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_5 = new PRED_feature_table_2_var_5();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_5, feature_table_2_var_5);
    }
}

class PRED_feature_table_2_var_5 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_6 = new PRED_feature_table_2_var_6();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_6, feature_table_2_var_6);
    }
}

class PRED_feature_table_2_var_6 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_7 = new PRED_feature_table_2_var_7();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_7, feature_table_2_var_7);
    }
}

class PRED_feature_table_2_var_7 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_8 = new PRED_feature_table_2_var_8();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_8, feature_table_2_var_8);
    }
}

class PRED_feature_table_2_var_8 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_9 = new PRED_feature_table_2_var_9();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_9, feature_table_2_var_9);
    }
}

class PRED_feature_table_2_var_9 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_10 = new PRED_feature_table_2_var_10();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_10, feature_table_2_var_10);
    }
}

class PRED_feature_table_2_var_10 extends PRED_feature_table_2 {
    static Predicate feature_table_2_var_11 = new PRED_feature_table_2_var_11();

    public Predicate exec(Prolog engine) {
        return engine.retry(feature_table_2_11, feature_table_2_var_11);
    }
}

class PRED_feature_table_2_var_11 extends PRED_feature_table_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(feature_table_2_12);
    }
}

class PRED_feature_table_2_int extends PRED_feature_table_2 {
    static Predicate feature_table_2_int_1 = new PRED_feature_table_2_int_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(feature_table_2_10, feature_table_2_int_1);
    }
}

class PRED_feature_table_2_int_1 extends PRED_feature_table_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(feature_table_2_12);
    }
}

class PRED_feature_table_2_str extends PRED_feature_table_2 {
    static Predicate feature_table_2_str_1 = new PRED_feature_table_2_str_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(feature_table_2_10, feature_table_2_str_1);
    }
}

class PRED_feature_table_2_str_1 extends PRED_feature_table_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(feature_table_2_12);
    }
}

class PRED_feature_table_2_lis extends PRED_feature_table_2 {
    static Predicate feature_table_2_lis_1 = new PRED_feature_table_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(feature_table_2_10, feature_table_2_lis_1);
    }
}

class PRED_feature_table_2_lis_1 extends PRED_feature_table_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(feature_table_2_12);
    }
}

class PRED_feature_table_2_1 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("list_np");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("ne_tag");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("ne_type");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("gender");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            a12 = ((ListTerm)a10).cdr();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s7.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s7, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            a14 = ((ListTerm)a12).cdr();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a13).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a13).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a13.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a13.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a14.isList() ){
            a15 = ((ListTerm)a14).car();
            if ( !s9.unify(((ListTerm)a14).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a14.isVariable() ){
            a15 = new VariableTerm(engine);
            if ( !a14.unify(new ListTerm(a15, s9), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a15.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a15).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a15).args();
            if ( !s10.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a15.isVariable() ){
            Term[] args = {s10, new VariableTerm(engine)};
            if ( !a15.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_2 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("n");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("person");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("number");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            a12 = ((ListTerm)a10).cdr();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s7.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s7, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            if ( !s8.unify(((ListTerm)a12).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, s8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a13).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a13).args();
            if ( !s9.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a13.isVariable() ){
            Term[] args = {s9, new VariableTerm(engine)};
            if ( !a13.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_3 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("pn");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("person");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("number");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            a12 = ((ListTerm)a10).cdr();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s7.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s7, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            if ( !s8.unify(((ListTerm)a12).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, s8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a13).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a13).args();
            if ( !s9.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a13.isVariable() ){
            Term[] args = {s9, new VariableTerm(engine)};
            if ( !a13.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_4 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("jj");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("degree");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            if ( !s7.unify(((ListTerm)a10).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, s7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_5 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("rb");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("degree");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            if ( !s7.unify(((ListTerm)a10).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, s7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_6 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("v");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("person");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("number");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("tense");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s11 = SymbolTerm.makeSymbol("vform");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            a12 = ((ListTerm)a10).cdr();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s7.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s7, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            a14 = ((ListTerm)a12).cdr();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a13).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a13).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a13.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a13.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a14.isList() ){
            a15 = ((ListTerm)a14).car();
            a16 = ((ListTerm)a14).cdr();
        } else if ( a14.isVariable() ){
            a15 = new VariableTerm(engine);
            a16 = new VariableTerm(engine);
            if ( !a14.unify(new ListTerm(a15, a16), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a15.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a15).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a15).args();
            if ( !s9.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a15.isVariable() ){
            Term[] args = {s9, new VariableTerm(engine)};
            if ( !a15.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a16.isList() ){
            a17 = ((ListTerm)a16).car();
            if ( !s10.unify(((ListTerm)a16).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a16.isVariable() ){
            a17 = new VariableTerm(engine);
            if ( !a16.unify(new ListTerm(a17, s10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a17.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a17).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a17).args();
            if ( !s11.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a17.isVariable() ){
            Term[] args = {s11, new VariableTerm(engine)};
            if ( !a17.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_7 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("sem_cat");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("type");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("kind");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("name");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            if ( !s7.unify(((ListTerm)a10).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, s7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_8 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("sem_cat_1");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("type");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("unit");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("count");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("kind");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("name");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            a12 = ((ListTerm)a10).cdr();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s7.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s7, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            a14 = ((ListTerm)a12).cdr();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a13).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a13).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a13.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a13.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a14.isList() ){
            a15 = ((ListTerm)a14).car();
            if ( !s9.unify(((ListTerm)a14).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a14.isVariable() ){
            a15 = new VariableTerm(engine);
            if ( !a14.unify(new ListTerm(a15, s9), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a15.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a15).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a15).args();
            if ( !s10.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a15.isVariable() ){
            Term[] args = {s10, new VariableTerm(engine)};
            if ( !a15.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_9 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("ne_date");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("text");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            if ( !s4.unify(((ListTerm)a4).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, s4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_10 extends PRED_feature_table_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("sym");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("period");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("comma");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("pps");
    static SymbolTerm s11 = SymbolTerm.makeSymbol("wps");
    static SymbolTerm s12 = SymbolTerm.makeSymbol("sgml");
    static SymbolTerm s13 = SymbolTerm.makeSymbol("date");
    static SymbolTerm s14 = SymbolTerm.makeSymbol("cdg");
    static SymbolTerm s15 = SymbolTerm.makeSymbol("cc");
    static SymbolTerm s16 = SymbolTerm.makeSymbol("cd");
    static SymbolTerm s17 = SymbolTerm.makeSymbol("dt");
    static SymbolTerm s18 = SymbolTerm.makeSymbol("ex");
    static SymbolTerm s19 = SymbolTerm.makeSymbol("fw");
    static SymbolTerm s20 = SymbolTerm.makeSymbol("in");
    static SymbolTerm s21 = SymbolTerm.makeSymbol("ls");
    static SymbolTerm s22 = SymbolTerm.makeSymbol("md");
    static SymbolTerm s23 = SymbolTerm.makeSymbol("pdt");
    static SymbolTerm s24 = SymbolTerm.makeSymbol("pos");
    static SymbolTerm s25 = SymbolTerm.makeSymbol("prp");
    static SymbolTerm s26 = SymbolTerm.makeSymbol("rp");
    static SymbolTerm s27 = SymbolTerm.makeSymbol("to");
    static SymbolTerm s28 = SymbolTerm.makeSymbol("uh");
    static SymbolTerm s29 = SymbolTerm.makeSymbol("wdt");
    static SymbolTerm s30 = SymbolTerm.makeSymbol("wp");
    static SymbolTerm s31 = SymbolTerm.makeSymbol("wrb");
    static SymbolTerm s32 = SymbolTerm.makeSymbol("top");
    static SymbolTerm s33 = SymbolTerm.makeSymbol("bottom");
    static SymbolTerm s34 = SymbolTerm.makeSymbol("ordinal");
    static SymbolTerm s35 = SymbolTerm.makeSymbol("char");
    static ListTerm s36 = new ListTerm(s35, s5);
    static ListTerm s37 = new ListTerm(s34, s36);
    static ListTerm s38 = new ListTerm(s33, s37);
    static ListTerm s39 = new ListTerm(s32, s38);
    static ListTerm s40 = new ListTerm(s31, s39);
    static ListTerm s41 = new ListTerm(s30, s40);
    static ListTerm s42 = new ListTerm(s29, s41);
    static ListTerm s43 = new ListTerm(s28, s42);
    static ListTerm s44 = new ListTerm(s27, s43);
    static ListTerm s45 = new ListTerm(s26, s44);
    static ListTerm s46 = new ListTerm(s25, s45);
    static ListTerm s47 = new ListTerm(s24, s46);
    static ListTerm s48 = new ListTerm(s23, s47);
    static ListTerm s49 = new ListTerm(s22, s48);
    static ListTerm s50 = new ListTerm(s21, s49);
    static ListTerm s51 = new ListTerm(s20, s50);
    static ListTerm s52 = new ListTerm(s19, s51);
    static ListTerm s53 = new ListTerm(s18, s52);
    static ListTerm s54 = new ListTerm(s17, s53);
    static ListTerm s55 = new ListTerm(s16, s54);
    static ListTerm s56 = new ListTerm(s15, s55);
    static ListTerm s57 = new ListTerm(s14, s56);
    static ListTerm s58 = new ListTerm(s13, s57);
    static ListTerm s59 = new ListTerm(s12, s58);
    static ListTerm s60 = new ListTerm(s11, s59);
    static ListTerm s61 = new ListTerm(s10, s60);
    static ListTerm s62 = new ListTerm(s9, s61);
    static ListTerm s63 = new ListTerm(s8, s62);
    static ListTerm s64 = new ListTerm(s7, s63);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

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
        } else if (a3.isVariable() ){
            Term[] args = {s2, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            if ( !s5.unify(((ListTerm)a8).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, s5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a10 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a10, cont);
        p2 = new PRED_memberchk_2(a1, s64, p1);
        return new PRED_$get_level_1(a10, p2);
    }
}

class PRED_feature_table_2_11 extends PRED_feature_table_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("tagged_location_np");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("edge");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("sem");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("head");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("source");
    static SymbolTerm s11 = SymbolTerm.makeSymbol("ne_tag");
    static SymbolTerm s12 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s13 = SymbolTerm.makeSymbol("ne_type");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
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
            if (! f2.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a3.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            a12 = ((ListTerm)a10).cdr();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s7.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s7, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            a14 = ((ListTerm)a12).cdr();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a13).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a13).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a13.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a13.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a14.isList() ){
            a15 = ((ListTerm)a14).car();
            a16 = ((ListTerm)a14).cdr();
        } else if ( a14.isVariable() ){
            a15 = new VariableTerm(engine);
            a16 = new VariableTerm(engine);
            if ( !a14.unify(new ListTerm(a15, a16), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a15.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a15).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a15).args();
            if ( !s9.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a15.isVariable() ){
            Term[] args = {s9, new VariableTerm(engine)};
            if ( !a15.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a16.isList() ){
            a17 = ((ListTerm)a16).car();
            a18 = ((ListTerm)a16).cdr();
        } else if ( a16.isVariable() ){
            a17 = new VariableTerm(engine);
            a18 = new VariableTerm(engine);
            if ( !a16.unify(new ListTerm(a17, a18), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a17.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a17).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a17).args();
            if ( !s10.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a17.isVariable() ){
            Term[] args = {s10, new VariableTerm(engine)};
            if ( !a17.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a18.isList() ){
            a19 = ((ListTerm)a18).car();
            a20 = ((ListTerm)a18).cdr();
        } else if ( a18.isVariable() ){
            a19 = new VariableTerm(engine);
            a20 = new VariableTerm(engine);
            if ( !a18.unify(new ListTerm(a19, a20), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a19.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a19).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a19).args();
            if ( !s11.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a19.isVariable() ){
            Term[] args = {s11, new VariableTerm(engine)};
            if ( !a19.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a20.isList() ){
            a21 = ((ListTerm)a20).car();
            if ( !s12.unify(((ListTerm)a20).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a20.isVariable() ){
            a21 = new VariableTerm(engine);
            if ( !a20.unify(new ListTerm(a21, s12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a21.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a21).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a21).args();
            if ( !s13.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a21.isVariable() ){
            Term[] args = {s13, new VariableTerm(engine)};
            if ( !a21.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_feature_table_2_12 extends PRED_feature_table_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("edge");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("sem");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("head");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("s_form");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("m_root");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("m_affix");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("text");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("source");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("person");
    static SymbolTerm s11 = SymbolTerm.makeSymbol("number");
    static SymbolTerm s12 = SymbolTerm.makeSymbol("gender");
    static SymbolTerm s13 = SymbolTerm.makeSymbol("tense");
    static SymbolTerm s14 = SymbolTerm.makeSymbol("aspect");
    static SymbolTerm s15 = SymbolTerm.makeSymbol("voice");
    static SymbolTerm s16 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s17 = SymbolTerm.makeSymbol("vform");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

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
        } else if (a3.isVariable() ){
            Term[] args = {s2, new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a5 = ((ListTerm)a4).car();
            a6 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            if ( !s3.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a5.isVariable() ){
            Term[] args = {s3, new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a7).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a7).args();
            if ( !s4.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a7.isVariable() ){
            Term[] args = {s4, new VariableTerm(engine)};
            if ( !a7.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            a10 = ((ListTerm)a8).cdr();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, a10), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            if ( !s5.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a9.isVariable() ){
            Term[] args = {s5, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isList() ){
            a11 = ((ListTerm)a10).car();
            a12 = ((ListTerm)a10).cdr();
        } else if ( a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a10.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            if ( !s6.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a11.isVariable() ){
            Term[] args = {s6, new VariableTerm(engine)};
            if ( !a11.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            a14 = ((ListTerm)a12).cdr();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a13).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a13).args();
            if ( !s7.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a13.isVariable() ){
            Term[] args = {s7, new VariableTerm(engine)};
            if ( !a13.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a14.isList() ){
            a15 = ((ListTerm)a14).car();
            a16 = ((ListTerm)a14).cdr();
        } else if ( a14.isVariable() ){
            a15 = new VariableTerm(engine);
            a16 = new VariableTerm(engine);
            if ( !a14.unify(new ListTerm(a15, a16), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a15.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a15).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a15).args();
            if ( !s8.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a15.isVariable() ){
            Term[] args = {s8, new VariableTerm(engine)};
            if ( !a15.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a16.isList() ){
            a17 = ((ListTerm)a16).car();
            a18 = ((ListTerm)a16).cdr();
        } else if ( a16.isVariable() ){
            a17 = new VariableTerm(engine);
            a18 = new VariableTerm(engine);
            if ( !a16.unify(new ListTerm(a17, a18), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a17.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a17).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a17).args();
            if ( !s9.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a17.isVariable() ){
            Term[] args = {s9, new VariableTerm(engine)};
            if ( !a17.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a18.isList() ){
            a19 = ((ListTerm)a18).car();
            a20 = ((ListTerm)a18).cdr();
        } else if ( a18.isVariable() ){
            a19 = new VariableTerm(engine);
            a20 = new VariableTerm(engine);
            if ( !a18.unify(new ListTerm(a19, a20), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a19.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a19).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a19).args();
            if ( !s10.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a19.isVariable() ){
            Term[] args = {s10, new VariableTerm(engine)};
            if ( !a19.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a20.isList() ){
            a21 = ((ListTerm)a20).car();
            a22 = ((ListTerm)a20).cdr();
        } else if ( a20.isVariable() ){
            a21 = new VariableTerm(engine);
            a22 = new VariableTerm(engine);
            if ( !a20.unify(new ListTerm(a21, a22), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a21.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a21).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a21).args();
            if ( !s11.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a21.isVariable() ){
            Term[] args = {s11, new VariableTerm(engine)};
            if ( !a21.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a22.isList() ){
            a23 = ((ListTerm)a22).car();
            a24 = ((ListTerm)a22).cdr();
        } else if ( a22.isVariable() ){
            a23 = new VariableTerm(engine);
            a24 = new VariableTerm(engine);
            if ( !a22.unify(new ListTerm(a23, a24), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a23.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a23).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a23).args();
            if ( !s12.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a23.isVariable() ){
            Term[] args = {s12, new VariableTerm(engine)};
            if ( !a23.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a24.isList() ){
            a25 = ((ListTerm)a24).car();
            a26 = ((ListTerm)a24).cdr();
        } else if ( a24.isVariable() ){
            a25 = new VariableTerm(engine);
            a26 = new VariableTerm(engine);
            if ( !a24.unify(new ListTerm(a25, a26), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a25.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a25).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a25).args();
            if ( !s13.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a25.isVariable() ){
            Term[] args = {s13, new VariableTerm(engine)};
            if ( !a25.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a26.isList() ){
            a27 = ((ListTerm)a26).car();
            a28 = ((ListTerm)a26).cdr();
        } else if ( a26.isVariable() ){
            a27 = new VariableTerm(engine);
            a28 = new VariableTerm(engine);
            if ( !a26.unify(new ListTerm(a27, a28), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a27.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a27).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a27).args();
            if ( !s14.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a27.isVariable() ){
            Term[] args = {s14, new VariableTerm(engine)};
            if ( !a27.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a28.isList() ){
            a29 = ((ListTerm)a28).car();
            a30 = ((ListTerm)a28).cdr();
        } else if ( a28.isVariable() ){
            a29 = new VariableTerm(engine);
            a30 = new VariableTerm(engine);
            if ( !a28.unify(new ListTerm(a29, a30), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a29.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a29).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a29).args();
            if ( !s15.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a29.isVariable() ){
            Term[] args = {s15, new VariableTerm(engine)};
            if ( !a29.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a30.isList() ){
            a31 = ((ListTerm)a30).car();
            if ( !s16.unify(((ListTerm)a30).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a30.isVariable() ){
            a31 = new VariableTerm(engine);
            if ( !a30.unify(new ListTerm(a31, s16), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a31.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a31).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a31).args();
            if ( !s17.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a31.isVariable() ){
            Term[] args = {s17, new VariableTerm(engine)};
            if ( !a31.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return cont;
    }
}

