package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_ne_tag_3.java
 * @procedure ne_tag/3 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_ne_tag_3 extends Predicate {
    static Predicate ne_tag_3_1 = new PRED_ne_tag_3_1();
    static Predicate ne_tag_3_2 = new PRED_ne_tag_3_2();
    static Predicate ne_tag_3_3 = new PRED_ne_tag_3_3();
    static Predicate ne_tag_3_4 = new PRED_ne_tag_3_4();
    static Predicate ne_tag_3_5 = new PRED_ne_tag_3_5();
    static Predicate ne_tag_3_6 = new PRED_ne_tag_3_6();
    static Predicate ne_tag_3_7 = new PRED_ne_tag_3_7();
    static Predicate ne_tag_3_8 = new PRED_ne_tag_3_8();
    static Predicate ne_tag_3_9 = new PRED_ne_tag_3_9();
    static Predicate ne_tag_3_10 = new PRED_ne_tag_3_10();
    static Predicate ne_tag_3_11 = new PRED_ne_tag_3_11();
    static Predicate ne_tag_3_sub_1 = new PRED_ne_tag_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_ne_tag_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_ne_tag_3(){}
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
        return engine.jtry(ne_tag_3_1, ne_tag_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "ne_tag(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_ne_tag_3_sub_1 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_2 = new PRED_ne_tag_3_sub_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_2, ne_tag_3_sub_2);
    }
}

class PRED_ne_tag_3_sub_2 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_3 = new PRED_ne_tag_3_sub_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_3, ne_tag_3_sub_3);
    }
}

class PRED_ne_tag_3_sub_3 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_4 = new PRED_ne_tag_3_sub_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_4, ne_tag_3_sub_4);
    }
}

class PRED_ne_tag_3_sub_4 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_5 = new PRED_ne_tag_3_sub_5();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_5, ne_tag_3_sub_5);
    }
}

class PRED_ne_tag_3_sub_5 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_6 = new PRED_ne_tag_3_sub_6();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_6, ne_tag_3_sub_6);
    }
}

class PRED_ne_tag_3_sub_6 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_7 = new PRED_ne_tag_3_sub_7();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_7, ne_tag_3_sub_7);
    }
}

class PRED_ne_tag_3_sub_7 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_8 = new PRED_ne_tag_3_sub_8();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_8, ne_tag_3_sub_8);
    }
}

class PRED_ne_tag_3_sub_8 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_9 = new PRED_ne_tag_3_sub_9();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_9, ne_tag_3_sub_9);
    }
}

class PRED_ne_tag_3_sub_9 extends PRED_ne_tag_3 {
    static Predicate ne_tag_3_sub_10 = new PRED_ne_tag_3_sub_10();

    public Predicate exec(Prolog engine) {
        return engine.retry(ne_tag_3_10, ne_tag_3_sub_10);
    }
}

class PRED_ne_tag_3_sub_10 extends PRED_ne_tag_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(ne_tag_3_11);
    }
}

class PRED_ne_tag_3_1 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("organization");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("organization", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_2 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("location");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("location", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_3 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("person");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("person", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_4 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("date");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("date", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_5 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("time");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("time", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_6 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("percent");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("percent", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_7 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("money");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("money", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_8 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("aircraft");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("aircraft", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_9 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("boat");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("boat", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_10 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("flight");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("flight", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        Term[] h3 = {a1};
        a4 = new StructureTerm(f2, h3);
        return new PRED_memberchk_2(a4, a2, cont);
    }
}

class PRED_ne_tag_3_11 extends PRED_ne_tag_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("unknown");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        return cont;
    }
}

