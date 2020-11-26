package shef.nlp.supple.prolog;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import shef.nlp.supple.prolog.cafe.PRED_parse_3;
import shef.nlp.supple.prolog.cafe.PRED_best_parse_cats_2;

import gate.creole.ExecutionException;

import jp.ac.kobe_u.cs.prolog.builtin.PRED_consult_1;
import jp.ac.kobe_u.cs.prolog.lang.JavaObjectTerm;
import jp.ac.kobe_u.cs.prolog.lang.ListTerm;
import jp.ac.kobe_u.cs.prolog.lang.Predicate;
import jp.ac.kobe_u.cs.prolog.lang.PrologControl;
import jp.ac.kobe_u.cs.prolog.lang.SymbolTerm;
import jp.ac.kobe_u.cs.prolog.lang.Term;

/**
 * Prolog wrapper for SUPPLE running on PrologCafe.
 */
public class PrologCafe extends Prolog
{
   private Hashtable database = null;

   public boolean init(File f)
   {
      try
      {
         database = new Hashtable();

         ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));

         int numKeys = in.readInt();

         while (database.keySet().size() < numKeys)
         {
            Term key = (Term)in.readObject();

            database.put(key,readTerm(in));
         }

         in.close();

      }
      catch (Exception e)
      {
         database = null;
      }

      return database != null;

   }

   public void parse(File in, File out, boolean debugMode)
                   throws ExecutionException {
      PrologControl p = new PrologControl();

      Term t = SymbolTerm.makeSymbol("[]");
      t = new ListTerm(SymbolTerm.makeSymbol(in.getAbsolutePath()),t);

      Predicate code = new PRED_parse_3();

      //Predicate test = new PRED_best_parse_cats_2();

      Term hash = new JavaObjectTerm(database);

      boolean success = false;

      try {
         PrintWriter pout = new PrintWriter(new FileWriter(out));

         Term writeOut = new JavaObjectTerm(pout);

         success = p.execute(code,new Term[]{writeOut,hash,t});

         pout.flush();
         pout.close();

      }
      catch(Exception e) {
         throw new ExecutionException(e);
      }
      if(!success) {
        throw new ExecutionException("PrologCafe execution failed");
      }
   }

   public static void main(String args[]) throws Exception
   {
      PrologControl p = new PrologControl();

      Predicate consult = new PRED_consult_1();
      Term f = SymbolTerm.makeSymbol(args[0]);

      p.setPredicate(consult,new Term[]{f});

      System.out.println("Consulting completed: " + p.call());

      Hashtable database = p.engine.getDynamicHash();

      Set keys = database.keySet();

      Iterator it = keys.iterator();

      ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(args[1]));

      out.writeInt(keys.size());

      while (it.hasNext())
      {
         Term k = (Term)it.next();

         out.writeObject(k.dereference());

         Term v = (Term)database.get(k);

         writeTerm(out,v);
      }

      out.flush();
      out.close();

      System.exit(0);
   }

   private static void writeTerm(ObjectOutputStream out, Term t) throws Exception
   {
      if (t instanceof ListTerm)
      {
         ListTerm lt = (ListTerm)t;

         out.writeInt(lt.length());

         boolean working = true;

         while (working)
         {
            Term ct = lt.car().dereference();
            writeTerm(out,ct);

            if (lt.cdr().dereference() instanceof ListTerm)
               lt = (ListTerm)lt.cdr().dereference();
            else
               working = false;
         }
      }
      else
      {
         out.writeInt(-1);
         out.writeObject(t.dereference());
      }
   }

   private static Term readTerm(ObjectInputStream in) throws Exception
   {
      int length = in.readInt();

      if (length != -1)
      {
         List terms = new ArrayList();

         for (int i = 0 ; i < length ; ++i)
         {
            terms.add(readTerm(in));
         }

         Term t = SymbolTerm.makeSymbol("[]");

         for (int i = terms.size() -1; i>=0 ; --i)
         {
            t = new ListTerm((Term)terms.get(i),t);
         }

         return t;
      }
      else
      {
         Term t = (Term)in.readObject();

         return t;
      }
   }
}
