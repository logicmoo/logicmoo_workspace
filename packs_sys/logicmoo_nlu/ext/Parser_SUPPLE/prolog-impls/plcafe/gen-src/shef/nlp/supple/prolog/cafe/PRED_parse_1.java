package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_parse_1.java
 * @procedure parse/1 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_parse_1 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate parse_1_1 = new PRED_parse_1_1();
    static Predicate parse_1_2 = new PRED_parse_1_2();
    static Predicate parse_1_3 = new PRED_parse_1_3();
    static Predicate parse_1_4 = new PRED_parse_1_4();
    static Predicate parse_1_5 = new PRED_parse_1_5();
    static Predicate parse_1_6 = new PRED_parse_1_6();
    static Predicate parse_1_7 = new PRED_parse_1_7();
    static Predicate parse_1_8 = new PRED_parse_1_8();
    static Predicate parse_1_9 = new PRED_parse_1_9();
    static Predicate parse_1_10 = new PRED_parse_1_10();
    static Predicate parse_1_11 = new PRED_parse_1_11();
    static Predicate parse_1_12 = new PRED_parse_1_12();
    static Predicate parse_1_13 = new PRED_parse_1_13();
    static Predicate parse_1_14 = new PRED_parse_1_14();
    static Predicate parse_1_lis = new PRED_parse_1_lis();
    static Predicate parse_1_var = new PRED_parse_1_var();

    public Term arg1;

    public PRED_parse_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_parse_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   parse_1_var,
                                   fail_0,
                                   parse_1_1,
                                   fail_0,
                                   parse_1_lis
                                   );
    }

    public int arity() { return 1; }

    public String toString() {
        return "parse(" + arg1 + ")";
    }
}

class PRED_parse_1_var extends PRED_parse_1 {
    static Predicate parse_1_var_1 = new PRED_parse_1_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(parse_1_1, parse_1_var_1);
    }
}

class PRED_parse_1_var_1 extends PRED_parse_1 {
    static Predicate parse_1_var_2 = new PRED_parse_1_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_2, parse_1_var_2);
    }
}

class PRED_parse_1_var_2 extends PRED_parse_1 {
    static Predicate parse_1_var_3 = new PRED_parse_1_var_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_3, parse_1_var_3);
    }
}

class PRED_parse_1_var_3 extends PRED_parse_1 {
    static Predicate parse_1_var_4 = new PRED_parse_1_var_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_4, parse_1_var_4);
    }
}

class PRED_parse_1_var_4 extends PRED_parse_1 {
    static Predicate parse_1_var_5 = new PRED_parse_1_var_5();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_5, parse_1_var_5);
    }
}

class PRED_parse_1_var_5 extends PRED_parse_1 {
    static Predicate parse_1_var_6 = new PRED_parse_1_var_6();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_6, parse_1_var_6);
    }
}

class PRED_parse_1_var_6 extends PRED_parse_1 {
    static Predicate parse_1_var_7 = new PRED_parse_1_var_7();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_7, parse_1_var_7);
    }
}

class PRED_parse_1_var_7 extends PRED_parse_1 {
    static Predicate parse_1_var_8 = new PRED_parse_1_var_8();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_8, parse_1_var_8);
    }
}

class PRED_parse_1_var_8 extends PRED_parse_1 {
    static Predicate parse_1_var_9 = new PRED_parse_1_var_9();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_9, parse_1_var_9);
    }
}

class PRED_parse_1_var_9 extends PRED_parse_1 {
    static Predicate parse_1_var_10 = new PRED_parse_1_var_10();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_10, parse_1_var_10);
    }
}

class PRED_parse_1_var_10 extends PRED_parse_1 {
    static Predicate parse_1_var_11 = new PRED_parse_1_var_11();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_11, parse_1_var_11);
    }
}

class PRED_parse_1_var_11 extends PRED_parse_1 {
    static Predicate parse_1_var_12 = new PRED_parse_1_var_12();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_12, parse_1_var_12);
    }
}

class PRED_parse_1_var_12 extends PRED_parse_1 {
    static Predicate parse_1_var_13 = new PRED_parse_1_var_13();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_13, parse_1_var_13);
    }
}

class PRED_parse_1_var_13 extends PRED_parse_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(parse_1_14);
    }
}

class PRED_parse_1_lis extends PRED_parse_1 {
    static Predicate parse_1_lis_1 = new PRED_parse_1_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(parse_1_2, parse_1_lis_1);
    }
}

class PRED_parse_1_lis_1 extends PRED_parse_1 {
    static Predicate parse_1_lis_2 = new PRED_parse_1_lis_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_3, parse_1_lis_2);
    }
}

class PRED_parse_1_lis_2 extends PRED_parse_1 {
    static Predicate parse_1_lis_3 = new PRED_parse_1_lis_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_4, parse_1_lis_3);
    }
}

class PRED_parse_1_lis_3 extends PRED_parse_1 {
    static Predicate parse_1_lis_4 = new PRED_parse_1_lis_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_5, parse_1_lis_4);
    }
}

class PRED_parse_1_lis_4 extends PRED_parse_1 {
    static Predicate parse_1_lis_5 = new PRED_parse_1_lis_5();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_6, parse_1_lis_5);
    }
}

class PRED_parse_1_lis_5 extends PRED_parse_1 {
    static Predicate parse_1_lis_6 = new PRED_parse_1_lis_6();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_7, parse_1_lis_6);
    }
}

class PRED_parse_1_lis_6 extends PRED_parse_1 {
    static Predicate parse_1_lis_7 = new PRED_parse_1_lis_7();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_8, parse_1_lis_7);
    }
}

class PRED_parse_1_lis_7 extends PRED_parse_1 {
    static Predicate parse_1_lis_8 = new PRED_parse_1_lis_8();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_9, parse_1_lis_8);
    }
}

class PRED_parse_1_lis_8 extends PRED_parse_1 {
    static Predicate parse_1_lis_9 = new PRED_parse_1_lis_9();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_10, parse_1_lis_9);
    }
}

class PRED_parse_1_lis_9 extends PRED_parse_1 {
    static Predicate parse_1_lis_10 = new PRED_parse_1_lis_10();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_11, parse_1_lis_10);
    }
}

class PRED_parse_1_lis_10 extends PRED_parse_1 {
    static Predicate parse_1_lis_11 = new PRED_parse_1_lis_11();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_12, parse_1_lis_11);
    }
}

class PRED_parse_1_lis_11 extends PRED_parse_1 {
    static Predicate parse_1_lis_12 = new PRED_parse_1_lis_12();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_1_13, parse_1_lis_12);
    }
}

class PRED_parse_1_lis_12 extends PRED_parse_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(parse_1_14);
    }
}

class PRED_parse_1_1 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_parse_1_2 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("--");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        p1 = new PRED_parse_1(a2, cont);
        return new PRED_$neck_cut_0(p1);
    }
}

class PRED_parse_1_3 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-n");

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
        } else if ( a1.isVariable() ){
            if ( !a1.unify(new ListTerm(s1, new VariableTerm(engine)), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_parse_1_4 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-v");
    static IntegerTerm s2 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a3 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_parse_1(a2, p1);
        p3 = new PRED_$dummy_parse_file46pl_0_2(new VariableTerm(engine), new VariableTerm(engine), p2);
        p4 = new PRED_verbose_1(s2, p3);
        return new PRED_$get_level_1(a3, p4);
    }
}

class PRED_parse_1_5 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-d");
    static IntegerTerm s2 = new IntegerTerm(1);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("-v");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3, p4, p5;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a3 = new VariableTerm(engine);
        a4 = new ListTerm(s3, a2);
        p1 = new PRED_parse_1(a4, cont);
        p2 = new PRED_$cut_1(a3, p1);
        p3 = new PRED_$dummy_parse_file46pl_1_3(a2, new VariableTerm(engine), new VariableTerm(engine), p2);
        p4 = new PRED_$cut_1(a3, p3);
        p5 = new PRED_buchart_debug_1(s2, p4);
        return new PRED_$get_level_1(a3, p5);
    }
}

class PRED_parse_1_6 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-ne");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("grammar1");
    static SymbolTerm f3 = SymbolTerm.makeSymbol("best_parse_cats", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        Predicate p1, p2, p3, p4, p5;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        Term[] h4 = {new VariableTerm(engine), new VariableTerm(engine)};
        a5 = new StructureTerm(f3, h4);
        Term[] h5 = {s2, a4};
        a6 = new StructureTerm(f3, h5);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_parse_1(a2, p1);
        p3 = new PRED_assert_1(a6, p2);
        p4 = new PRED_retractall_1(a5, p3);
        p5 = new PRED_best_parse_cats_2(s2, a4, p4);
        return new PRED_$get_level_1(a3, p5);
    }
}

class PRED_parse_1_7 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-p");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("best_parse_file", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
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
        a5 = new VariableTerm(engine);
        Term[] h3 = {new VariableTerm(engine)};
        a6 = new StructureTerm(f2, h3);
        Term[] h4 = {a3};
        a7 = new StructureTerm(f2, h4);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_parse_1(a4, p1);
        p3 = new PRED_$dummy_parse_file46pl_4_2(a3, new VariableTerm(engine), p2);
        p4 = new PRED_$dummy_parse_file46pl_3_2(a3, a4, p3);
        p5 = new PRED_assert_1(a7, p4);
        p6 = new PRED_retractall_1(a6, p5);
        return new PRED_$get_level_1(a5, p6);
    }
}

class PRED_parse_1_8 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-b");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("best_parse_file", 1);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("bracketed_parses");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4, p5, p6, p7;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
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
        a5 = new VariableTerm(engine);
        Term[] h3 = {new VariableTerm(engine)};
        a6 = new StructureTerm(f2, h3);
        Term[] h4 = {a3};
        a7 = new StructureTerm(f2, h4);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_parse_1(a4, p1);
        p3 = new PRED_$dummy_parse_file46pl_6_2(a3, new VariableTerm(engine), p2);
        p4 = new PRED_$dummy_parse_file46pl_5_2(a3, a4, p3);
        p5 = new PRED_assert_1(s5, p4);
        p6 = new PRED_assert_1(a7, p5);
        p7 = new PRED_retractall_1(a6, p6);
        return new PRED_$get_level_1(a5, p7);
    }
}

class PRED_parse_1_9 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-c");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("chart_file", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
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
        a5 = new VariableTerm(engine);
        Term[] h3 = {new VariableTerm(engine)};
        a6 = new StructureTerm(f2, h3);
        Term[] h4 = {a3};
        a7 = new StructureTerm(f2, h4);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_parse_1(a4, p1);
        p3 = new PRED_assert_1(a7, p2);
        p4 = new PRED_retractall_1(a6, p3);
        return new PRED_$get_level_1(a5, p4);
    }
}

class PRED_parse_1_10 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-g");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("grammar_file", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
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
        a5 = new VariableTerm(engine);
        Term[] h3 = {new VariableTerm(engine)};
        a6 = new StructureTerm(f2, h3);
        Term[] h4 = {a3};
        a7 = new StructureTerm(f2, h4);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_parse_1(a4, p1);
        p3 = new PRED_assert_1(a7, p2);
        p4 = new PRED_retractall_1(a6, p3);
        return new PRED_$get_level_1(a5, p4);
    }
}

class PRED_parse_1_11 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-f");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("filter_chart");
    static SymbolTerm f3 = SymbolTerm.makeSymbol("filter_grammar", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a3 = new VariableTerm(engine);
        Term[] h4 = {new VariableTerm(engine)};
        a4 = new StructureTerm(f3, h4);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_parse_1(a2, p1);
        p3 = new PRED_assert_1(a4, p2);
        p4 = new PRED_retractall_1(s2, p3);
        return new PRED_$get_level_1(a3, p4);
    }
}

class PRED_parse_1_12 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-flag");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("flag_file", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
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
        a5 = new VariableTerm(engine);
        Term[] h3 = {new VariableTerm(engine)};
        a6 = new StructureTerm(f2, h3);
        Term[] h4 = {a3};
        a7 = new StructureTerm(f2, h4);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_parse_1(a4, p1);
        p3 = new PRED_assert_1(a7, p2);
        p4 = new PRED_retractall_1(a6, p3);
        return new PRED_$get_level_1(a5, p4);
    }
}

class PRED_parse_1_13 extends PRED_parse_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("-o");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("output_file", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
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
        a5 = new VariableTerm(engine);
        Term[] h3 = {new VariableTerm(engine)};
        a6 = new StructureTerm(f2, h3);
        Term[] h4 = {a3};
        a7 = new StructureTerm(f2, h4);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_parse_1(a4, p1);
        p3 = new PRED_assert_1(a7, p2);
        p4 = new PRED_retractall_1(a6, p3);
        return new PRED_$get_level_1(a5, p4);
    }
}

class PRED_parse_1_14 extends PRED_parse_1 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("chart", 3);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("gensymmark", 2);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("reading initial chart...");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("done");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("parsing charts...");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a2 = ((ListTerm)a1).car();
            a3 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            a3 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a2, a3), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a4 = new VariableTerm(engine);
        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a5 = new StructureTerm(f1, h2);
        Term[] h4 = {new VariableTerm(engine), new VariableTerm(engine)};
        a6 = new StructureTerm(f3, h4);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_parse_1(a3, p1);
        p3 = new PRED_$dummy_parse_file46pl_9_2(a2, new VariableTerm(engine), p2);
        p4 = new PRED_vnl_0(p3);
        p5 = new PRED_vwrite_1(s6, p4);
        p6 = new PRED_told_0(p5);
        p7 = new PRED_parse_charts_2(a7, a8, p6);
        p8 = new PRED_vnl_0(p7);
        p9 = new PRED_vwrite_1(s7, p8);
        p10 = new PRED_$dummy_parse_file46pl_8_1(a8, p9);
        p11 = new PRED_vnl_0(p10);
        p12 = new PRED_vwrite_1(s6, p11);
        p13 = new PRED_read_chart_file_2(a2, a7, p12);
        p14 = new PRED_vwrite_1(s5, p13);
        p15 = new PRED_retractall_1(a6, p14);
        p16 = new PRED_retractall_1(a5, p15);
        p17 = new PRED_$dummy_parse_file46pl_7_2(new VariableTerm(engine), new VariableTerm(engine), p16);
        return new PRED_$get_level_1(a4, p17);
    }
}

