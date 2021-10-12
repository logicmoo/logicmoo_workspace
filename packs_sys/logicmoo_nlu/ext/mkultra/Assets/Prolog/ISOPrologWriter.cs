using System;
using System.IO;

// There are a lot of stylistic things that ReSharper complains about either because of C/C#
// stylistic differences or because I had to do surgery on the code to make it work in a
// different environment.  I could let resharper fix them, but then if something went wrong
// then I'd have to debug a big chunk of code that I don't fully understand.  So instead I'm
// just disabling ReSharper's stylistic complaints.

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedParameter.Local
// ReSharper disable RedundantIfElseBlock
// ReSharper disable PossibleNullReferenceException
// ReSharper disable RedundantAssignment
// ReSharper disable SuggestUseVarKeywordEvident
// ReSharper disable TooWideLocalVariableScope
// ReSharper disable JoinDeclarationAndInitializer
// ReSharper disable ExpressionIsAlwaysNull
// ReSharper disable ForCanBeConvertedToForeach

namespace Prolog
{
    internal class ISOPrologWriter
    {
        public ISOPrologWriter(TextWriter writer)
        {
            output = writer;
        }

        public static string WriteToString(object o)
        {
            StringWriter s = new StringWriter();
            new ISOPrologWriter(s).writeterm(o, 0);
            return s.ToString();
        }

        private readonly TextWriter output;
        private int lastitem = SOLO_ITEM;
        private char lastChar;

        private const int QUOTED = 1;
        private const int IGNORE_OPS = 2;
        private const int NUMBERVARS = 4;
        private const int WLTERM = 256;

        private const int OPTIONMASK = 255;

        private const int SOLO_ITEM = 0;
        private const int NUMERIC_ITEM = 1;
        private const int IDENTIFIER_ITEM = 2;
        private const int QUOTED_ATOM_ITEM = 3;
        private const int GRAPHIC_ITEM = 4;

//const int VARNUM 1024

        private static bool USDVAR(string s)
        {
            return (s == "$VAR");
        }

        private static int __ARITY(Structure s)
        {
            return s.Arguments.Length;
        }

        private static object __ARG(Structure s, int n)
        {
            return s.Arguments[n - 1];
        }

        private static object __LASTARG(Structure s, int n)
        {
            return __ARG(s, n);
        }

        private void FUNCTIONAL(Structure tp, string termname, int options)
        {
            if ((options & NUMBERVARS) != 0 && USDVAR(termname) && __ARITY(tp) == 1)
            {
                int varno;
                Object tp2;
                
                tp2 = __LASTARG(tp, 1);
                tp2 = Term.Deref(tp2);
                if (tp2 is int)
                {
                    varno = (int) tp2;
                    if (lastitem == IDENTIFIER_ITEM) PUTCHAR(' ');
                    PUTCHAR((char)(varno%26 + 'A'));
                    if ((varno = varno/26) != 0)
                    {
                        LowLevelWrite(varno);
                    }
                    lastitem = IDENTIFIER_ITEM;
                    return;
                }
            }
            showname(termname, options);
            PUTSOLO('(');
            for (int i = 1; i < __ARITY(tp); i++)
            {
                recwriteterm(__ARG(tp, i), options, 1000, 1201);
                PUTSOLO(',');
            }
            if (__ARITY(tp)>0)
                recwriteterm(__LASTARG(tp, __ARITY(tp)), options, 1000, 1201);
            PUTSOLO(')');
        }

        private static bool needquotes(string s)
/*
 * function that checks whether an atom should be quoted
 * in order to make it readable by read_term/3 again
 */
        {
            if (ISOPrologReader.small_letter_char(s[0]))
            {
                for (int i = 1; i < s.Length; i++)
                    if (!ISOPrologReader.alpha_numeric_char(s[i]))
                        return false;
                return true;
            }
            else if (ISOPrologReader.graphic_token_char(s[0]))
            {
                for (int i = 1; i < s.Length; i++)
                    if (!ISOPrologReader.graphic_token_char(s[i]))
                        return false;
                return true;
            }
            else if (ISOPrologReader.semicolon_char(s[0]) && s.Length == 1)
                return false;
            else if (ISOPrologReader.cut_char(s[0]) && s.Length == 1)
                return false;
            else if (ISOPrologReader.solo_char(s[0]) && s.Length == 1)
                return true;
            else if (s=="[]")
                return false;
            else if (s=="{}")
                return false;
            return true;
        }

        private void showname(string s, int options)
/* 
 * outputs a name, adds quotes and layout text if needed
 */
        {
            if ((options & QUOTED) != 0 && needquotes(s))
            {
                if (lastitem == QUOTED_ATOM_ITEM) PUTCHAR(' ');
                PUTCHAR('\'');
                for (int i = 0; i < s.Length; i++)
                {
                    char c = s[i];
                    if (c >= ' ')
                    {
                        if (ISOPrologReader.backslash_char(c))
                        {
                            /* escape char should be duplicated 
                             in order to denote itself */
                            PUTCHAR(c);
                            PUTCHAR(c);
                        }
                        else
                            PUTCHAR(c);
                    }
                    else
                    {
                        PUTCHAR('\\');
                        switch ((int)c)
                        {
                            case 7:
                                PUTCHAR('a');
                                break;
                            case 8:
                                PUTCHAR('b');
                                break;
                            case 9:
                                PUTCHAR('t');
                                break;
                            case 10:
                                PUTCHAR('n');
                                break;
                            case 11:
                                PUTCHAR('v');
                                break;
                            case 12:
                                PUTCHAR('f');
                                break;
                            case 13:
                                PUTCHAR('r');
                                break;
                        }
                    }
                }
                PUTCHAR('\'');
                lastitem = QUOTED_ATOM_ITEM;
            }
            else
            {
                if ((lastitem == IDENTIFIER_ITEM || lastitem == NUMERIC_ITEM) &&
                    (s.Length>0 && ISOPrologReader.alpha_numeric_char(s[0])) ||
                    lastitem == GRAPHIC_ITEM && (s.Length>0 && ISOPrologReader.graphic_token_char(s[0])))
                    PUTCHAR(' ');
                for (int i = 0; i < s.Length; i++)
                {
                    char c = s[i];
                    PUTCHAR(c);
                }
                this.lastitem = ISOPrologReader.graphic_token_char(this.lastChar) ? GRAPHIC_ITEM : IDENTIFIER_ITEM;
            }
        }

        private static bool needspace(Object tp, int termpri)
/*
 * checks whether the term tp will be displayed between
 * brackets, given the priority level termpri. In that
 * case a space need to be inserted before the left bracket.
 */
        {
            int prepri, inpri, postpri, spec;

            if (tp == null) return false;
            tp = Term.Deref(tp);
            var symbol = tp as Symbol;
            if (symbol != null)
                return ISOPrologReader.Operator(symbol.Name, out prepri, out inpri, out postpri, out spec);
        else
            {
                var structure = tp as Structure;
                if (structure != null)
                {
                    Structure str = structure;
                    if (ISOPrologReader.Operator(str.Functor.Name, out prepri, out inpri, out postpri, out spec))
                    {
                        if (__ARITY(str) == 2)
                            return inpri >= termpri;
                        if (__ARITY(str) == 1)
                        {
                            if (ISOPrologReader.isprefix(spec) && ISOPrologReader.ispostfix(spec))
                            {
                                /* in case of ambiguity:
             select the associative operator over the nonassociative
             select prefix over postfix */
                                if (ISOPrologReader.isfy(spec)) spec = ISOPrologReader.FY;
                                else if (ISOPrologReader.isyf(spec)) spec = ISOPrologReader.YF;
                                else spec = ISOPrologReader.FX;
                            }
                            if (ISOPrologReader.isprefix(spec))
                                return prepri >= termpri;
                            else
                                return postpri >= termpri;
                        }
                        else
                            return false;
                    }
                    else
                        return false;
                }
                else
                    return false;
            }
        }

        private void recwriteterm(Object tp, int options, int termpri, int atompri)
/*
 * recursive writeterm routine
 */
        {
            int prepri, inpri, postpri, spec;

            tp = Term.Deref(tp); 
            if (tp == null)
            {
                LowLevelWrite(tp);
                return;
            }
            if (tp is int)
            {
                int intval = (int) tp;
                if (intval < 0)
                {
                    if (lastitem == GRAPHIC_ITEM)
                        PUTCHAR(' ');
                    PUTCHAR('-');
                    intval = -intval;
                }
                else
                {
                    if (lastitem == NUMERIC_ITEM || lastitem == IDENTIFIER_ITEM)
                        PUTCHAR(' ');
                }
                LowLevelWrite(intval);
                lastitem = NUMERIC_ITEM;
            }
            else
            {
                var symbol = tp as Symbol;
                if (symbol != null)
                {
                    bool brackets = false;
                    string atomname = symbol.Name;
                    if (atompri == 1201)
                        this.showname(atomname, options);
                    else
                    {
                        if (ISOPrologReader.Operator(atomname, out prepri, out inpri, out postpri, out spec))
                        {
                            if (prepri >= atompri) brackets = true;
                            if (inpri >= atompri) brackets = true;
                            if (postpri >= atompri) brackets = true;
                            if (ISOPrologReader.comma_char(atomname[0]) && atomname.Length==1) brackets = true;
                        }
                        if (brackets) this.PUTSOLO('(');
                        this.showname(atomname, options);
                        if (brackets) this.PUTSOLO(')');
                    }
                }
                else if (tp is float)
                {
                    float ff = (float) tp;
                    if (ff < 0.0)
                    {
                        if (this.lastitem == GRAPHIC_ITEM)
                            this.PUTCHAR(' ');
                    }
                    else
                    {
                        if (this.lastitem == NUMERIC_ITEM || this.lastitem == IDENTIFIER_ITEM)
                            this.PUTCHAR(' ');
                    }
                    this.LowLevelWrite(ff);
                    this.lastitem = NUMERIC_ITEM;
                }
                else if (tp is Structure)
                {
                    Structure str = tp as Structure;
                    string termname = str.Functor.Name;
                    int termarity = str.Arguments.Length;

                    if ((options & IGNORE_OPS) != 0 || termarity > 2)
                    {
                        this.FUNCTIONAL(str, termname, options);
                    }
                    else /* operator definitions are now in effect */ if (termarity == 2)
                    {
                        if (str.Functor == Symbol.PrologListConstructor)
                        {
                            /* list constructor */
                            bool first = true;

                            this.PUTSOLO('[');
                            do
                            {
                                if (first) first = false;
                                else this.PUTSOLO(',');
                                this.recwriteterm(__ARG(str, 1), options, 1000, 1201);
                                tp = __LASTARG(str, 2);
                                tp = Term.Deref(tp);
                                str = tp as Structure;
                            } while (str != null && str.Functor == Symbol.PrologListConstructor);
                            if (!(tp is Symbol) && tp != null)
                            {
                                this.PUTSOLO('|');
                                this.recwriteterm(tp, options, 1000, 1201);
                            }
                            this.PUTSOLO(']');
                        }
                        else if (ISOPrologReader.Operator(termname, out prepri, out inpri, out postpri, out spec) &&
                                 ISOPrologReader.isinfix(spec) && (inpri != 0))
                        {
                            if (inpri >= termpri ||
                                (options & WLTERM) != 0 && ISOPrologReader.isxfy(spec) && inpri + 1 == termpri) this.PUTSOLO('(');
                            if (ISOPrologReader.isyfx(spec))
                                this.recwriteterm(__ARG(str, 1), options | WLTERM, inpri + 1, 0);
                            else
                                this.recwriteterm(__ARG(str, 1), options & OPTIONMASK, inpri, 0);
                            if (str.Functor == Symbol.Comma)
                            {
                                this.PUTSOLO(',');
                            }
                            else
                                this.showname(termname, options);
                            this.recwriteterm(__LASTARG(str, 2), options, ISOPrologReader.isxfy(spec) ? inpri + 1 : inpri, 0);
                            if (inpri >= termpri ||
                                (options & WLTERM) != 0 && ISOPrologReader.isxfy(spec) && inpri + 1 == termpri) this.PUTSOLO(')');
                        }
                        else
                        {
                            this.FUNCTIONAL(str, termname, options);
                        }
                    }
                    else if (termarity == 1)
                    {
                        if (ISOPrologReader.Operator(termname, out prepri, out inpri, out postpri, out spec))
                        {
                            if (ISOPrologReader.isprefix(spec) && ISOPrologReader.ispostfix(spec))
                            {
                                /* in case of ambiguity:
               select the associative operator over the nonassociative
               select prefix over postfix */
                                if (ISOPrologReader.isfy(spec)) spec = ISOPrologReader.FY;
                                else if (ISOPrologReader.isyf(spec)) spec = ISOPrologReader.YF;
                                else spec = ISOPrologReader.FX;
                            }
                            if (ISOPrologReader.isprefix(spec) && prepri != 0)
                            {
                                if (prepri >= termpri ||
                                    (options & WLTERM) != 0 && ISOPrologReader.isfy(spec) && prepri + 1 == termpri) this.PUTSOLO('(');
                                this.showname(termname, options);
                                if (needspace(tp, ISOPrologReader.isfy(spec) ? prepri + 1 : prepri)) this.PUTSOLO(' ');
                                this.recwriteterm(__LASTARG(str, 1), options, ISOPrologReader.isfy(spec) ? prepri + 1 : prepri, 0);
                                if (prepri >= termpri ||
                                    (options & WLTERM) != 0 && ISOPrologReader.isfy(spec) && prepri + 1 == termpri) this.PUTSOLO(')');
                            }
                            else if (ISOPrologReader.ispostfix(spec) && postpri != 0)
                            {
                                if (postpri >= termpri) this.PUTSOLO('(');
                                if (ISOPrologReader.isyf(spec))
                                    this.recwriteterm(__LASTARG(str, 1), options | WLTERM, postpri + 1, 0);
                                else
                                    this.recwriteterm(__LASTARG(str, 1), options, postpri, 0);
                                this.showname(termname, options);
                                if (postpri >= termpri) this.PUTSOLO(')');
                            }
                            else
                            {
                                this.FUNCTIONAL(str, termname, options);
                            }
                        }
                        else if (str.Functor == Symbol.CurlyBrackets)
                        {
                            /* curly brackets */
                            this.PUTSOLO('{');
                            this.recwriteterm(__LASTARG(str, 1), options, 1201, 1201);
                            this.PUTSOLO('}');
                        }
                        else
                        {
                            this.FUNCTIONAL(str, termname, options);
                        }
                    }
                    else
                    {
                        this.FUNCTIONAL(str, termname, options);
                    }
                }
                else
                {
                    var variable = tp as LogicVariable;
                    if (variable != null)
                    {
                        if (this.lastitem == IDENTIFIER_ITEM)
                        {
                            this.PUTCHAR(' ');
                        }
                        this.LowLevelWrite(variable);
                        this.lastitem = IDENTIFIER_ITEM; /* same properties as variable */
                    }
                    else
                    {
                        this.LowLevelWrite(tp);
                    }
                }
            }
        }

        private void writeterm(Object tp, int options)
        {
            lastitem = SOLO_ITEM;
            recwriteterm(tp, options, 1201, 1201);
        }

        public void Write(object value)
        {
            writeterm(value, 0);
        }

        public void WriteString(string s)
        {
            output.Write(s);
            if (s.Length > 0)
                lastChar = s[s.Length - 1];
        }

        #region Low-level IO compatability
    void PUTCHAR(char c)
    {
        output.Write(c);
        lastChar = c;
    }
        void PUTSOLO(char c)
        {
            lastitem = SOLO_ITEM;
            lastChar = c;
            output.Write(c);
        }
#endregion

        void LowLevelWrite(int x)
        {
            output.Write(x);
            lastChar = ' ';
        }

        void LowLevelWrite(float x)
        {
            output.Write(x);
            lastChar = ' ';
        }

        void LowLevelWrite(LogicVariable x)
        {
            output.Write(x.Name);
            output.Write(x.UID);
            lastChar = '1';
        }

        void LowLevelWrite(object x)
        {
            var g = x as UnityEngine.GameObject;
            if (g != null)
            {
                output.Write('$');
                if (g.name.IndexOf(' ') >= 0 || !char.IsLower(g.name[0]))
                {
                    output.Write('\'');
                    output.Write(g.name);
                    output.Write('\'');
                }
                else
                    output.Write(g.name);
            }
            else if (x is string)
            {
                output.Write('"');
                output.Write(x);
                output.Write('"');
            }
            else
                output.Write(x ?? "null");
            lastChar = '1';
        }
    }
}
