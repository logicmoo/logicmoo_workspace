package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_clean_semantics_2.java
 * @procedure clean_semantics/2 in semantics.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_clean_semantics_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate clean_semantics_2_1 = new PRED_clean_semantics_2_1();
    static Predicate clean_semantics_2_2 = new PRED_clean_semantics_2_2();
    static Predicate clean_semantics_2_3 = new PRED_clean_semantics_2_3();
    static Predicate clean_semantics_2_4 = new PRED_clean_semantics_2_4();
    static Predicate clean_semantics_2_5 = new PRED_clean_semantics_2_5();
    static Predicate clean_semantics_2_6 = new PRED_clean_semantics_2_6();
    static Predicate clean_semantics_2_7 = new PRED_clean_semantics_2_7();
    static Predicate clean_semantics_2_8 = new PRED_clean_semantics_2_8();
    static Predicate clean_semantics_2_lis = new PRED_clean_semantics_2_lis();
    static Predicate clean_semantics_2_var = new PRED_clean_semantics_2_var();

    public Term arg1, arg2;

    public PRED_clean_semantics_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_clean_semantics_2(){}
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
                                   clean_semantics_2_var,
                                   fail_0,
                                   clean_semantics_2_1,
                                   clean_semantics_2_2,
                                   clean_semantics_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "clean_semantics(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_clean_semantics_2_var extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_var_1 = new PRED_clean_semantics_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(clean_semantics_2_1, clean_semantics_2_var_1);
    }
}

class PRED_clean_semantics_2_var_1 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_var_2 = new PRED_clean_semantics_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_2, clean_semantics_2_var_2);
    }
}

class PRED_clean_semantics_2_var_2 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_var_3 = new PRED_clean_semantics_2_var_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_3, clean_semantics_2_var_3);
    }
}

class PRED_clean_semantics_2_var_3 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_var_4 = new PRED_clean_semantics_2_var_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_4, clean_semantics_2_var_4);
    }
}

class PRED_clean_semantics_2_var_4 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_var_5 = new PRED_clean_semantics_2_var_5();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_5, clean_semantics_2_var_5);
    }
}

class PRED_clean_semantics_2_var_5 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_var_6 = new PRED_clean_semantics_2_var_6();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_6, clean_semantics_2_var_6);
    }
}

class PRED_clean_semantics_2_var_6 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_var_7 = new PRED_clean_semantics_2_var_7();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_7, clean_semantics_2_var_7);
    }
}

class PRED_clean_semantics_2_var_7 extends PRED_clean_semantics_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(clean_semantics_2_8);
    }
}

class PRED_clean_semantics_2_lis extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_lis_1 = new PRED_clean_semantics_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(clean_semantics_2_3, clean_semantics_2_lis_1);
    }
}

class PRED_clean_semantics_2_lis_1 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_lis_2 = new PRED_clean_semantics_2_lis_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_4, clean_semantics_2_lis_2);
    }
}

class PRED_clean_semantics_2_lis_2 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_lis_3 = new PRED_clean_semantics_2_lis_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_5, clean_semantics_2_lis_3);
    }
}

class PRED_clean_semantics_2_lis_3 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_lis_4 = new PRED_clean_semantics_2_lis_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_6, clean_semantics_2_lis_4);
    }
}

class PRED_clean_semantics_2_lis_4 extends PRED_clean_semantics_2 {
    static Predicate clean_semantics_2_lis_5 = new PRED_clean_semantics_2_lis_5();

    public Predicate exec(Prolog engine) {
        return engine.retry(clean_semantics_2_7, clean_semantics_2_lis_5);
    }
}

class PRED_clean_semantics_2_lis_5 extends PRED_clean_semantics_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(clean_semantics_2_8);
    }
}

class PRED_clean_semantics_2_1 extends PRED_clean_semantics_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_clean_semantics_2_2 extends PRED_clean_semantics_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("^", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("e");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a1).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a1).args();
            a3 = args[0];
            a4 = args[1];
        } else if (a1.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            Term[] args = {a3, a4};
            if ( !a1.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a5 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_clean_semantics_2(a4, a2, p1);
        p3 = new PRED_buchart_gensym_2(s2, a3, p2);
        return new PRED_$get_level_1(a5, p3);
    }
}

class PRED_clean_semantics_2_3 extends PRED_clean_semantics_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("^", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("e");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Predicate p1, p2, p3, p4, p5, p6, p7;
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
        a5 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        Term[] h2 = {a7, a8};
        a6 = new StructureTerm(f1, h2);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_append_3(a9, a10, a2, p1);
        p3 = new PRED_clean_semantics_2(a4, a10, p2);
        p4 = new PRED_clean_semantics_2(a8, a9, p3);
        p5 = new PRED_buchart_gensym_2(s3, a7, p4);
        p6 = new PRED_$unify_2(a3, a6, p5);
        p7 = new PRED_nonvar_1(a3, p6);
        return new PRED_$get_level_1(a5, p7);
    }
}

class PRED_clean_semantics_2_4 extends PRED_clean_semantics_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("name");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("head");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("head1");
    static SymbolTerm s5 = SymbolTerm.makeSymbol("head2");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("det");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("adj");
    static SymbolTerm s8 = SymbolTerm.makeSymbol("pronoun");
    static SymbolTerm s9 = SymbolTerm.makeSymbol("count");
    static SymbolTerm s10 = SymbolTerm.makeSymbol("more");
    static SymbolTerm s11 = SymbolTerm.makeSymbol("less");
    static SymbolTerm s12 = SymbolTerm.makeSymbol("title");
    static ListTerm s13 = new ListTerm(s12, s1);
    static ListTerm s14 = new ListTerm(s11, s13);
    static ListTerm s15 = new ListTerm(s10, s14);
    static ListTerm s16 = new ListTerm(s9, s15);
    static ListTerm s17 = new ListTerm(s8, s16);
    static ListTerm s18 = new ListTerm(s7, s17);
    static ListTerm s19 = new ListTerm(s6, s18);
    static ListTerm s20 = new ListTerm(s5, s19);
    static ListTerm s21 = new ListTerm(s4, s20);
    static ListTerm s22 = new ListTerm(s3, s21);
    static ListTerm s23 = new ListTerm(s2, s22);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
        Predicate p1, p2, p3, p4, p5, p6;
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
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            if ( !s1.unify(((ListTerm)a6).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a2.isList() ){
            a8 = ((ListTerm)a2).car();
            if ( !s1.unify(((ListTerm)a2).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a2.isVariable() ){
            a8 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a8, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a11 = new VariableTerm(engine);
        a14 = new ListTerm(a11, s1);
        a13 = new ListTerm(a5, a14);
        a12 = new ListTerm(a3, a13);
        p1 = new PRED_$cut_1(a9, cont);
        p2 = new PRED_$614646_2(a8, a12, p1);
        p3 = new PRED_name_conc_2(a10, a11, p2);
        p4 = new PRED_semflatten_2(a7, a10, p3);
        p5 = new PRED_member_2(a3, s23, p4);
        p6 = new PRED_nonvar_1(a3, p5);
        return new PRED_$get_level_1(a9, p6);
    }
}

class PRED_clean_semantics_2_5 extends PRED_clean_semantics_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2;
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
        if ( a2.isList() ){
            a5 = ((ListTerm)a2).car();
            if ( !s1.unify(((ListTerm)a2).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a2.isVariable() ){
            a5 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a5, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a6 = new VariableTerm(engine);
        a7 = new ListTerm(a6, a4);
        p1 = new PRED_$614646_2(a5, a7, cont);
        p2 = new PRED_lower_2(a3, a6, p1);
        return new PRED_$dummy_semantics46pl_1_3(a4, new VariableTerm(engine), new VariableTerm(engine), p2);
    }
}

class PRED_clean_semantics_2_6 extends PRED_clean_semantics_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("compound", 1);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
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
        if ( a3.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            a5 = args[0];
        } else if (a3.isVariable() ){
            a5 = new VariableTerm(engine);
            Term[] args = {a5};
            if ( !a3.unify(new StructureTerm(f1, args), engine.trail) )
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
        if ( a2.isList() ){
            a8 = ((ListTerm)a2).car();
            if ( !s2.unify(((ListTerm)a2).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a2.isVariable() ){
            a8 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a8, s2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a9 = new VariableTerm(engine);
        a10 = new ListTerm(a9, a7);
        a11 = new VariableTerm(engine);
        a12 = new VariableTerm(engine);
        a13 = new ListTerm(a12, a4);
        p1 = new PRED_$614646_2(a8, a13, cont);
        p2 = new PRED_clean_predicate_2(a11, a12, p1);
        p3 = new PRED_name_conc_2(a10, a11, p2);
        p4 = new PRED_$dummy_semantics46pl_2_4(a6, new VariableTerm(engine), new VariableTerm(engine), a9, p3);
        return new PRED_nonvar_1(a6, p4);
    }
}

class PRED_clean_semantics_2_7 extends PRED_clean_semantics_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("MISSING");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2;
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
        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        p1 = new PRED_member_2(s2, a4, cont);
        p2 = new PRED_$dummy_semantics46pl_4_3(a4, new VariableTerm(engine), new VariableTerm(engine), p1);
        return new PRED_$dummy_semantics46pl_3_3(a3, new VariableTerm(engine), new VariableTerm(engine), p2);
    }
}

class PRED_clean_semantics_2_8 extends PRED_clean_semantics_2 {

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
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_append_3(a6, a7, a2, p1);
        p3 = new PRED_clean_semantics_2(a4, a7, p2);
        p4 = new PRED_clean_semantics_2(a3, a6, p3);
        return new PRED_$get_level_1(a5, p4);
    }
}

