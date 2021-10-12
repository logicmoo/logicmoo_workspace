using System;
using System.Collections.Generic;
using System.IO;

// Port of Koen De Bosschere's C-language Prolog reader
// Anything good about this code is due to him, anything bad is
// due to Ian Horswill's lame port of it to C#.

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

namespace Prolog
{
    public class ISOPrologReader
    {
        /// <summary>
        /// Reads a term from a string.
        /// </summary>
        /// <param name="s">String to read the term from</param>
        /// <returns>The term represented in the string</returns>
        public static object Read(string s)
        {
            return new ISOPrologReader(s).ReadTerm();
        }

        #region Constructors
        public ISOPrologReader(TextReader input)
        {
            inputReader = input;

        }

        public ISOPrologReader(string input) : this(new StringReader(input)) {}

        static ISOPrologReader()
        {
            InitTokenizer();
            InitOperators();
        }
        #endregion

        private readonly TextReader inputReader;
        private int linenumber = 1;

        public int LineNumber
        {
            get { return linenumber; }
        }

        public void Close()
        {
            var s = inputReader as StreamReader;
            if (s != null) s.Close();
        }

        #region Operator table
        private class OperatorInfo
        {
            public int Prefixpriority;
            public int Infixpriority;
            public int Postfixpriority;
            // ReSharper disable once InconsistentNaming
            public int specifier;
        }

        static readonly Dictionary<string, OperatorInfo> OperatorTable = new Dictionary<string,OperatorInfo>();

        internal static bool Operator(string op, out int prepri, out int inpri, out int postpri, out int spec)
        {
            OperatorInfo info;
            if (OperatorTable.TryGetValue(op, out info))
            {
                prepri = info.Prefixpriority;
                inpri = info.Infixpriority;
                postpri = info.Postfixpriority;
                spec = info.specifier;
                return true;
            }
            prepri = inpri = postpri = spec = 0;
            return false;
        }

        public static bool DeclareOperator(int pri, int spec, string op)
        {
            OperatorInfo i;
            if (!OperatorTable.TryGetValue(op, out i))
            {
                i = new OperatorInfo();
                OperatorTable[op] = i;
            }
            if ((isprefix(spec) && isprefix(i.specifier) && pri != i.Prefixpriority)
                || (isinfix(spec) && ((isinfix(i.specifier) && pri != i.Infixpriority) || ispostfix(i.specifier)))
                || (ispostfix(spec) && ((ispostfix(i.specifier) && pri != i.Postfixpriority) || isinfix(i.specifier))))
                throw new InvalidOperationException("Operator definition conflicts with previous definition.");

            i.specifier |= spec;
            if (isprefix(spec))
                i.Prefixpriority = pri;
            else if (isinfix(spec))
                i.Infixpriority = pri;
            else
                i.Postfixpriority = pri;

            return true;
        }

        public static void DeclareOperator(int pri, Symbol type, Symbol op)
        {
            DeclareOperator(pri, Specifier(type.Name), op.Name);
        }

        static void InitOperators()
        {
            DeclareOperator(1200, XFX, ":-");
            DeclareOperator(1200, XFX, "-->");
            DeclareOperator(1200, FX, ":-");
            DeclareOperator(1200, FX, "?-");
            DeclareOperator(1100, XFY, ";");
            DeclareOperator(1050, XFY, "->");
            DeclareOperator(1000, XFY, ",");
            DeclareOperator(900, FY, "\\+");  /* binprolog */
            DeclareOperator(700, XFX, "=");
            DeclareOperator(700, XFX, "\\=");
            DeclareOperator(700, XFX, "==");
            DeclareOperator(700, XFX, "\\==");
            DeclareOperator(700, XFX, "@<");
            DeclareOperator(700, XFX, "@=<");
            DeclareOperator(700, XFX, "@>");
            DeclareOperator(700, XFX, "@>=");
            DeclareOperator(700, XFX, "=..");
            DeclareOperator(700, XFX, "is");
            DeclareOperator(700, XFX, "=:=");
            DeclareOperator(700, XFX, "=\\=");
            DeclareOperator(700, XFX, "<");
            DeclareOperator(700, XFX, "=<");
            DeclareOperator(700, XFX, ">");
            DeclareOperator(700, XFX, ">=");
            //DeclareOperator(600, XFY, ":");
            DeclareOperator(500, YFX, "+");
            DeclareOperator(500, YFX, "-");
            DeclareOperator(500, YFX, "/\\");
            DeclareOperator(500, YFX, "\\/");
            DeclareOperator(500, YFX, "#");  /* binprolog */
            DeclareOperator(400, YFX, "*");
            DeclareOperator(400, YFX, ":");  // Differs from SWI!
            DeclareOperator(400, YFX, "/");
            DeclareOperator(400, YFX, "//");
            DeclareOperator(400, YFX, "rem");
            DeclareOperator(400, YFX, "mod");
            DeclareOperator(400, YFX, "<<");
            DeclareOperator(400, YFX, ">>");
            DeclareOperator(300, FX, "/");
            DeclareOperator(200, XFX, "**");
            DeclareOperator(200, XFY, "^");
            DeclareOperator(200, FY, "-");
            DeclareOperator(200, FY, "~");
            DeclareOperator(200, FY, "\\");
            DeclareOperator(100, XFY, "@");
            DeclareOperator(40, YFX, ".");
            DeclareOperator(40, XFX, "::");

            DeclareOperator(950, YF, "&");
            DeclareOperator(950, FY, "?");
            /*  addoperator(950, FY, "!"); */

            DeclareOperator(200, FY, "++");
            DeclareOperator(200, YF, "--");

            DeclareOperator(945, XF, "xf");
            DeclareOperator(945, YF, "yf");
            DeclareOperator(945, FX, "fx");
            DeclareOperator(945, FY, "fy");
            DeclareOperator(945, XFX, "xfx");
            DeclareOperator(945, XFY, "xfy");
            DeclareOperator(945, YFX, "yfx");

            DeclareOperator(1150, FX, "dynamic");
            DeclareOperator(1150, FX, "randomizable");
            DeclareOperator(1150, FX, "discontiguous");
            DeclareOperator(1150, FX, "multifile");
            DeclareOperator(1150, FX, "public");
            DeclareOperator(1150, FX, "external");
            DeclareOperator(1150, FX, "shadow");
            DeclareOperator(1150, FX, "higher_order");
            DeclareOperator(1150, FX, "indexical");
        }
        #endregion

        #region Input buffer
        private const int InputBufferSize = 1024;
        private readonly char[] inputBuffer = new char[InputBufferSize];
        private int bufferptr;
        private int bufferend;
        #endregion

        #region Token buffer

        // ReSharper disable InconsistentNaming
        private const int MAXTOKENLENGTH = 1024;
        private const int SLACK = 64;
        // ReSharper restore InconsistentNaming
        private readonly char[] tokenBuffer = new char[MAXTOKENLENGTH];
        private int tokenptr;
        private char tokenchar;

        private string TokenString
        {
            get
            {
                if (tokenptr < 2)
                    return "";
                return new string(this.tokenBuffer, 0, this.tokenptr - 1);
            }
        }

        private float floattoken;
        private int integertoken;

        private int parseerror;

        #endregion

        #region Tokenizer
        private const int SEPARATORBIT = 128;
        
        private const int EOFFILE = 0;
        private const int NAME_TOKEN = 1;
        private const int VARIABLE_TOKEN = 2;
        private const int INTEGER_TOKEN = 3;
        private const int FLOAT_NUMBER_TOKEN = 4;
        private const int CHAR_CODE_LIST_TOKEN = 5;
        private const int OPEN_TOKEN = (6 + SEPARATORBIT);
        private const int OPEN_CT_TOKEN = (7 + SEPARATORBIT);
        private const int CLOSE_TOKEN = (8 + SEPARATORBIT);
        private const int OPEN_LIST_TOKEN = (9 + SEPARATORBIT);
        private const int CLOSE_LIST_TOKEN = (10 + SEPARATORBIT);
        private const int OPEN_CURLY_TOKEN = (11 + SEPARATORBIT);
        private const int CLOSE_CURLY_TOKEN = (12 + SEPARATORBIT);
        private const int HEAD_TAIL_SEPARATOR_TOKEN = (13 + SEPARATORBIT);
        private const int COMMA_TOKEN = (14 + SEPARATORBIT);
        private const int END_TOKEN = 15;

        private const int TERMTYPE = 40; /* meta token */

        private static bool separator_token(int X)
        {
            return ((X) & SEPARATORBIT) > 0;
        }

        private char LOOKAHEAD_CHAR
        {
            get
            {
                return ((bufferptr >= bufferend)
                            ? lookahead_char2()
                            : inputBuffer[bufferptr]);
            }
        }

        private bool open_token(char X)
        {
            return open_char(X);
        }

        private bool close_token(char X)
        {
            return close_char(X);
        }

        private bool open_list_token(char X)
        {
            return open_list_char(X);
        }

        private bool close_list_token(char X)
        {
            return close_list_char(X);
        }

        private bool open_curly_token(char X)
        {
            return open_curly_char(X);
        }

        private bool close_curly_token(char X)
        {
            return close_curly_char(X);
        }

        private bool head_tail_separator_token(char X)
        {
            return head_tail_separator_char(X);
        }

        private bool comma_token(char X)
        {
            return comma_char(X);
        }

        private bool end_token(char X)
        {
            return end_char(X);
        }

        private bool semicolon_token(char X)
        {
            return semicolon_char(X);
        }

        private bool cut_token(char X)
        {
            return cut_char(X);
        }


        /* character definitions */

        /* symbols for use in inittokenizer */

        private const char _PROLOGCHAR = (char) 1;
        private const char _GRAPHIC_CHAR = (char) 2;
        private const char _GRAPHIC_TOKEN_CHAR = (char) 4;
        private const char _ALPHA_NUMERIC_CHAR = (char) 8;
        private const char _HEXADECIMAL_DIGIT_CHAR = (char) 16;
        private const char _SOLO_CHAR = (char) 32;
        private const char _SYMBOLIC_CONTROL_CHAR = (char) 64;
        private const char _LAYOUT_CHAR = (char) 128;


        public static bool PROLOGCHAR(char X)
        {
            return ((GRAPHIC_CHAR(X)) || (ALPHA_NUMERIC_CHAR(X)) ||
                    (SOLO_CHAR(X)) || (LAYOUT_CHAR(X)) || (meta_char(X)));
        }

        public static bool GRAPHIC_CHAR(char X)
        {
            return ((X) == '#' || (X) == '$' || (X) == '&' ||
                    (X) == '*' || (X) == '+' || (X) == '-' ||
                    (X) == '.' || (X) == '/' || (X) == ':' ||
                    (X) == '<' || (X) == '=' || (X) == '>' ||
                    (X) == '?' || (X) == '@' || (X) == '^' ||
                    (X) == '~' || (extra_graphic_char(X)));
        }

        public static bool GRAPHIC_TOKEN_CHAR(char X)
        {
            return (GRAPHIC_CHAR(X) || backslash_char(X));
        }

        public static bool ALPHA_NUMERIC_CHAR(char X)
        {
            return ((alpha_char(X)) || (decimal_digit_char(X)));
        }

        public static bool HEXADECIMAL_DIGIT_CHAR(char X)
        {
            return ((X) >= '0' && (X) <= '9' ||
                    (X) >= 'A' && (X) <= 'F' ||
                    (X) >= 'a' && (X) <= 'f');
        }

        public static bool SOLO_CHAR(char X)
        {
            return ((cut_char(X)) ||
                    (open_char(X)) ||
                    (close_char(X)) ||
                    (comma_char(X)) ||
                    (semicolon_char(X)) ||
                    (open_list_char(X)) ||
                    (close_list_char(X)) ||
                    (open_curly_char(X)) ||
                    (close_curly_char(X)) ||
                    (head_tail_separator_char(X)) ||
                    (end_line_comment_char(X)) ||
                    (extra_solo_char(X)));
        }

        public static bool SYMBOLIC_CONTROL_CHAR(char X)
        {
            return (symbolic_alert_char(X) ||
                    symbolic_backspace_char(X) ||
                    symbolic_form_feed_char(X) ||
                    symbolic_new_line_char(X) ||
                    symbolic_carriage_return_char(X) ||
                    symbolic_horizontal_tab_char(X) ||
                    symbolic_vertical_tab_char(X));
        }

        public static bool LAYOUT_CHAR(char X)
        {
            return ((space_char(X)) ||
                    (new_line_char(X)) ||
                    (extra_layout_char(X)));
                    //|| X == (char)0);         // Added idh to treat end of file as a layout character.
        }


        public static bool prologchar(char X)
        {
            return (chararr[X] & _PROLOGCHAR) > 0;
        }

        public static bool graphic_char(char X)
        {
            return (chararr[X] & _GRAPHIC_CHAR) > 0;
        }

        public static bool graphic_token_char(char X)
        {
            return (chararr[X] & _GRAPHIC_TOKEN_CHAR) > 0;
        }

        public static bool alpha_numeric_char(char X)
        {
            return (chararr[X] & _ALPHA_NUMERIC_CHAR) > 0;
        }

        public static bool alpha_char(char X)
        {
            return ((underscore_char(X)) || (letter_char(X)));
        }

        public static bool letter_char(char X)
        {
            return ((capital_letter_char(X)) || (small_letter_char(X)));
        }

        public static bool small_letter_char(char X)
        {
            return ((X) >= 'a' && (X) <= 'z' ||
                    extra_small_letter_char(X));
        }

        public static bool capital_letter_char(char X)
        {
            return ((X) >= 'A' && (X) <= 'Z' ||
                    extra_capital_letter_char(X));
        }

        public static bool decimal_digit_char(char X)
        {
            return ((X) >= '0' && (X) <= '9');
        }

        public static bool binary_digit_char(char X)
        {
            return ((X) >= '0' && (X) <= '1');
        }

        public static bool octal_digit_char(char X)
        {
            return ((X) >= '0' && (X) <= '7');
        }

        public static bool hexadecimal_digit_char(char X)
        {
            return (chararr[X] & _HEXADECIMAL_DIGIT_CHAR) > 0;
        }

        public static bool underscore_char(char X)
        {
            return ((X) == '_');
        }

        public static bool solo_char(char X)
        {
            return (chararr[X] & _SOLO_CHAR) > 0;
        }

        public static bool cut_char(char X)
        {
            return ((X) == '!');
        }

        public static bool open_char(char X)
        {
            return ((X) == '(');
        }

        public static bool close_char(char X)
        {
            return ((X) == ')');
        }

        public static bool comma_char(char X)
        {
            return ((X) == ',');
        }

        public static bool semicolon_char(char X)
        {
            return ((X) == ';');
        }

        public static bool open_list_char(char X)
        {
            return ((X) == '[');
        }

        public static bool close_list_char(char X)
        {
            return ((X) == ']');
        }

        public static bool open_curly_char(char X)
        {
            return ((X) == '{');
        }

        public static bool close_curly_char(char X)
        {
            return ((X) == '}');
        }

        public static bool head_tail_separator_char(char X)
        {
            return ((X) == '|');
        }

        public static bool end_line_comment_char(char X)
        {
            return ((X) == '%');
        }


        public static bool layout_char(char X)
        {
            return (chararr[X] & _LAYOUT_CHAR) > 0;
        }

        public static bool space_char(char X)
        {
            return ((X) == ' ');
        }

        public static bool new_line_char(char X)
        {
            return ((X) == '\n'); /* implementation dependent */
        }

        public static bool meta_char(char X)
        {
            return ((backslash_char(X)) ||
                    (single_quote_char(X)) ||
                    (double_quote_char(X)) ||
                    (back_quote_char(X)));
        }

        public static bool backslash_char(char X)
        {
            return ((X) == '\\');
        }

        public static bool single_quote_char(char X)
        {
            return ((X) == '\'');
        }

        public static bool double_quote_char(char X)
        {
            return ((X) == '"');
        }

        public static bool back_quote_char(char X)
        {
            return ((X) == '`');
        }

        public static bool comment_1_char(char X)
        {
            return ((X) == '/');
        }

        public static bool comment_2_char(char X)
        {
            return ((X) == '*');
        }

        public static bool end_char(char X)
        {
            return ((X) == '.');
        }

        public static bool symbolic_alert_char(char X)
        {
            return ((X) == 'a');
        }

        public static bool symbolic_backspace_char(char X)
        {
            return ((X) == 'b');
        }

        public static bool symbolic_form_feed_char(char X)
        {
            return ((X) == 'f');
        }

        public static bool symbolic_new_line_char(char X)
        {
            return ((X) == 'n');
        }

        public static bool symbolic_carriage_return_char(char X)
        {
            return ((X) == 'r');
        }

        public static bool symbolic_horizontal_tab_char(char X)
        {
            return ((X) == 't');
        }

        public static bool symbolic_vertical_tab_char(char X)
        {
            return ((X) == 'v');
        }

        public static bool symbolic_hexadecimal_char(char X)
        {
            return ((X) == 'x');
        }

        public static bool symbolic_control_char(char X)
        {
            return (chararr[X] & _SYMBOLIC_CONTROL_CHAR) > 0;
        }

        public static bool positive_sign_char(char X)
        {
            return ((X) == '+');
        }

        public static bool negative_sign_char(char X)
        {
            return ((X) == '-');
        }

        public static bool sign_char(char X)
        {
            return (positive_sign_char(X) || negative_sign_char(X));
        }

        public static bool decimal_point_char(char X)
        {
            return ((X) == '.');
        }

        public static bool exponent_char(char X)
        {
            return ((X) == 'e' || (X) == 'E');
        }

        public static bool variable_indicator_char(char X)
        {
            return underscore_char(X);
        }


        /* error flags */

        private const int ERR_NOERROR = 0;
        //private const int ERR_MISSING_QUOTE = 1;
        //private const int ERR_TOKEN_TOO_LONG = 2;
        //private const int ERR_UNEXPECTED_EOF = 3;
        //private const int ERR_BACK_QUOTED_STRING = 4;
        //private const int ERR_NONDET = 5;
        //private const int ERR_INCOMPLETE_REDUCTION = 6;

#if UnusedParserStuff
        private static bool is_latin1_upper(char X)
        {
            return (((X) >= 192) && ((X) <= 214) ||
                    ((X) >= 216) && ((X) <= 222));
        }

        private static bool is_latin1_lower(char X)
        {
            return (((X) >= 223) && ((X) <= 246) ||
                    ((X) >= 248) && ((X) <= 255));
        }
#endif

        private static bool extra_graphic_char(char X)
        {
            return false;
        }

        private static bool extra_small_letter_char(char X)
        {
            return false; /* is_latin1_lower(X) */
        }

        private static bool extra_capital_letter_char(char X)
        {
            return false; /* is_latin1_upper(X) */
        }

        private static bool extra_solo_char(char X)
        {
            return false;
        }

        private static bool extra_layout_char(char X)
        {
            return ((carriage_return_char(X)) ||
                    (tab_char(X)) ||
                    (formfeed_char(X)) ||
                    (vertab_char(X)));
        }

        private static bool carriage_return_char(char X)
        {
            return ((X) == '\r'); /* ascii */
        }

        private static bool tab_char(char X)
        {
            return ((X) == '\t'); /* ascii */
        }

        private static bool formfeed_char(char X)
        {
            return ((X) == 12); /* ascii */
        }

        private static bool vertab_char(char X)
        {
            return ((X) == 11); /* ascii */
        }

        private static readonly char[] chararr = new char[256];
        private static readonly char[] charconv = new char[256];

        private void CHAR_CONVERSION()
        {
            tokenchar = charconv[tokenchar];
        }

        private void RESETTOKEN()
        {
            tokenptr = 0;
        }

        private void ADDTOKENCHAR(char c)
        {
            if (tokenptr < MAXTOKENLENGTH)
            {
                tokenBuffer[tokenptr++] = (c);
            }
            else
                throw new SyntaxErrorException(this.TokenString, "Token too long", inputReader);
            //seterr(ERR_TOKEN_TOO_LONG); 
        }

        private int TOKENLENGTH
        {
            get { return tokenptr; }
        }

        /* return next char and skip it ; update line count */

        private char SKIPCHAR()
        {
            if (new_line_char(inputBuffer[bufferptr]))
                linenumber++;
            return inputBuffer[bufferptr++];
        }

        private static void InitTokenizer()
        {
            for (char i = (char) 0; i <= (char) 255; i++)
            {
                chararr[i] = (char) 0;
                if (PROLOGCHAR(i)) chararr[i] |= _PROLOGCHAR;
                if (GRAPHIC_CHAR(i)) chararr[i] |= _GRAPHIC_CHAR;
                if (GRAPHIC_TOKEN_CHAR(i)) chararr[i] |= _GRAPHIC_TOKEN_CHAR;
                if (ALPHA_NUMERIC_CHAR(i)) chararr[i] |= _ALPHA_NUMERIC_CHAR;
                if (HEXADECIMAL_DIGIT_CHAR(i)) chararr[i] |= _HEXADECIMAL_DIGIT_CHAR;
                if (SOLO_CHAR(i)) chararr[i] |= _SOLO_CHAR;
                if (SYMBOLIC_CONTROL_CHAR(i)) chararr[i] |= _SYMBOLIC_CONTROL_CHAR;
                if (LAYOUT_CHAR(i)) chararr[i] |= _LAYOUT_CHAR;
                //printf("");
            }
            for (char i = (char) 0; i <= (char) 255; i++)
            {
                charconv[i] = i;
            }
        }

        private char lookahead_char2()
            /* return current char and refill buffer is empty  */
        {
            if (bufferptr >= bufferend)
            {
                int count = inputReader.Read(inputBuffer, SLACK, InputBufferSize - SLACK);
                // fileread(&(inputbuffer[SLACK]), INPUTBUFFERSIZE);
                if (count == 0)
                {
                    /* end-of-file ; return false */
                    count++;
                    inputBuffer[SLACK] = (char) 0;
                }
                bufferend = SLACK + count;
                bufferptr = SLACK;
            }
            return inputBuffer[bufferptr];
        }


        private void returnchar(char c)
            /* return char to buffer */
            /* SLACK is used in case the buffer would have been refilled */
        {
            if (new_line_char(c)) linenumber--;
            inputBuffer[--bufferptr] = c;
        }

        private void single_line_comment()
            /*
*   single line comment (* 6.4.1 *)
*      = end line comment char (* 6.5.3 *),
*        comment txt (* 6.4.1 *),
*        new line char (* 6.5.4 *) ;
*/
        {
            /* end_line_comment_char(LOOKAHEAD_CHAR) is true */
            SKIPCHAR();
            while (prologchar(LOOKAHEAD_CHAR) && !new_line_char(LOOKAHEAD_CHAR))
                SKIPCHAR();
            SKIPCHAR();
        }

        private bool bracketed_comment()
            /*
*  bracketed comment (* 6.4.1 *)
*     = comment open (* 6.4.1 *),
*       comment text (* 6.4.1 *),
*       comment close (* 6.4.1 *) ;

*  comment open (* 6.4.1 *)
*    = comment 1 char (* 6.4.1 *),
*      comment 2 char (* 6.4.1 *) ;

*  comment close (* 6.4.1 *)
*    = comment 2 char (* 6.4.1 *),
*      comment 1 char (* 6.4.1 *) ;

*  comment text (* 6.4.1 *)
*    = { char (* 6.5 *) } ;
*/
        {
            if (comment_1_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (comment_2_char(LOOKAHEAD_CHAR))
                {
                    SKIPCHAR();
                    do
                    {
                        while (prologchar(LOOKAHEAD_CHAR) && !comment_2_char(LOOKAHEAD_CHAR))
                            SKIPCHAR();
                        SKIPCHAR();
                    } while (prologchar(LOOKAHEAD_CHAR) && !comment_1_char(LOOKAHEAD_CHAR));
                    SKIPCHAR();
                    return true;
                }
                else
                {
                    returnchar(c);
                    return false;
                }
            }
            else
                return false;
        }

        private bool char_code_list_token()
            /*
* char code list token (* 6.4.6 *)
*    = double quote char (* 6.5.5 *),
*      { double quoted item } (* 6.5.6 *),
*      double quote char (* 6.5.5 *) ;
*/
        {
            /* double_quote_token(LOOKAHEAD_CHAR) is true */
            SKIPCHAR();
            RESETTOKEN();
            while (get_double_quoted_item())
            {
            }
            if (double_quote_char(LOOKAHEAD_CHAR))
            {
                SKIPCHAR();
                ADDTOKENCHAR((char) 0); /* terminator */
                return true;
            }
            else
                return false;
        }

        private int number_token()
            /*
*  float number token (* 6.4.5 *)
*    = integer constant (* 6.4.4 *),
*      fraction (* 6.4.5 *),
*      [ exponent (* 6.4.5 *) ] ;

*  fraction (* 6.4.5 *)
*    = decimal point char (* 6.4.5 *),
*      decimal digit char (* 6.4.5 *),
*      { decimal digit char (* 6.5.2 *) ;
 
*  exponent (* 6.4.5 *)
*    = exponent char (* 6.4.5 *),
*      sign (* 6.4.5 *),
*      integer constant (* 6.4.4 *) ;

*  sign (* 6.4.5 *)
*    = negative sign char (* 6.4.5 *)
*    | [ positive sign char (* 6.4.5 *) ] ;


*  integer token (* 6.4.4 *)
*    = integer constant (* 6.4.4 *)
*    | character code constant (* 6.4.4 *)
*    | binary constant (* 6.4.4 *)
*    | octal constant (* 6.4.4 *)
*    | hexadecimal constant (* 6.4.4 *) ;
 
*  integer constant (* 6.4.4 *)
*    = decimal digit char (* 6.4.4 *),
*      { decimal digit char (* 6.4.4 *) } ;

*  character code constant (* 6.4.4 *)
*    = "0", single quote char (* 6.5.5 *),
*           single quoted char (* 6.4.2.1 *) ;
*/
        {
            RESETTOKEN();
            ADDTOKENCHAR(SKIPCHAR());
            while (decimal_digit_char(LOOKAHEAD_CHAR))
                ADDTOKENCHAR(SKIPCHAR());
            if (decimal_point_char(LOOKAHEAD_CHAR))
            {
                SKIPCHAR();
                if (decimal_digit_char(LOOKAHEAD_CHAR))
                {
                    ADDTOKENCHAR('.');
                    ADDTOKENCHAR(SKIPCHAR());
                    while (decimal_digit_char(LOOKAHEAD_CHAR))
                        ADDTOKENCHAR(SKIPCHAR());
                    if (exponent_char(LOOKAHEAD_CHAR))
                    {
                        ADDTOKENCHAR(SKIPCHAR());
                        if (sign_char(LOOKAHEAD_CHAR))
                            ADDTOKENCHAR(SKIPCHAR());
                        if (decimal_digit_char(LOOKAHEAD_CHAR))
                        {
                            ADDTOKENCHAR(SKIPCHAR());
                            while (decimal_digit_char(LOOKAHEAD_CHAR))
                                ADDTOKENCHAR(SKIPCHAR());
                            ADDTOKENCHAR((char) 0); /* terminator */
                            floattoken = float.Parse(this.TokenString);
                            return FLOAT_NUMBER_TOKEN;
                        }
                        else
                            return 0;
                    }
                    else
                    {
                        ADDTOKENCHAR((char) 0); /* terminator */
                        floattoken = float.Parse(this.TokenString);
                        return FLOAT_NUMBER_TOKEN;
                    }
                }
                else
                {
                    returnchar('.');
                    ADDTOKENCHAR((char) 0); /* terminator */
                    integertoken = int.Parse(this.TokenString);
                    return INTEGER_TOKEN;
                }
            }
            else
            {
                /* up to here we have read an integer */
                if (tokenBuffer[0] == '0' && TOKENLENGTH == 1)
                {
                    char c = LOOKAHEAD_CHAR;
                    if (c == 'x' &&
                        (integertoken = hexadecimal_constant()) != -1)
                        return INTEGER_TOKEN;
                    if (c == 'o' &&
                        (integertoken = octal_constant()) != -1)
                        return INTEGER_TOKEN;
                    if (c == 'b' &&
                        (integertoken = binary_constant()) != -1)
                        return INTEGER_TOKEN;
                    if (single_quote_char(c))
                    {
                        SKIPCHAR();
                        if (get_single_quoted_char())
                        {
                            integertoken = tokenchar;
                            return INTEGER_TOKEN;
                        }
                        else
                            return 0;
                    }
                    integertoken = 0; /* read number 0 */
                    return INTEGER_TOKEN;
                }
                else
                {
                    ADDTOKENCHAR((char) 0); /* terminator */
                    integertoken = int.Parse(this.TokenString);
                    return INTEGER_TOKEN;
                }
            }
        }

        private int hexadecimal_constant()
            /*
*   hexadecimal constant (* 6.4.4 *)
*     = hexadecimal constant indicator (* 6.4.4 *),
*       hexadecimal digit char (* 6.5.2 *),
*       { hexadecimal digit char (* 6.5.2 *) } ;
*
*   hexadecimal constant indicator (* 6.4.4 *) 
*     = "0x" ;   ---> 0 already read by caller 
*/
        {
            int n = 0;

            /* LOOKAHEAD == x is true */
            SKIPCHAR();
            if (hexadecimal_digit_char(LOOKAHEAD_CHAR))
            {
                char c;
                while (hexadecimal_digit_char(c = LOOKAHEAD_CHAR))
                {
                    if (decimal_digit_char(c))
                        n = n*16 + SKIPCHAR() - '0';
                    else if (capital_letter_char(c))
                        n = n*16 + SKIPCHAR() - 'A' + 10;
                    else
                        n = n*16 + SKIPCHAR() - 'a' + 10;
                }
                return n;
            }
            else
            {
                returnchar('x');
                return -1;
            }
        }

        private int octal_constant()
            /*
*   octal constant (* 6.4.4 *)
*     = octal constant indicator (* 6.4.4 *),
*       octal digit char (* 6.5.2 *),
*       { octal digit char (* 6.5.2 *) } ;
*
*   octal constant indicator (* 6.4.4 *) 
*     = "0o" ;   ---> 0 already read by caller 
*/
        {
            int n = 0;

            /* LOOKAHEAD == o is true */
            SKIPCHAR();
            if (octal_digit_char(LOOKAHEAD_CHAR))
            {
                while (octal_digit_char(LOOKAHEAD_CHAR))
                {
                    n = n*8 + SKIPCHAR() - '0';
                }
                return n;
            }
            else
            {
                returnchar('o');
                return -1;
            }
        }

        private int binary_constant()
            /*
*   binary constant (* 6.4.4 *)
*     = binary constant indicator (* 6.4.4 *),
*       binary digit char (* 6.5.2 *),
*       { binary digit char (* 6.5.2 *) } ;
*
*   binary constant indicator (* 6.4.4 *) 
*     = "0b" ;   ---> 0 already read by caller 
*/
        {
            int n = 0;

            /* LOOKAHEAD == b is true */
            SKIPCHAR();
            if (binary_digit_char(LOOKAHEAD_CHAR))
            {
                while (binary_digit_char(LOOKAHEAD_CHAR))
                {
                    n = n*2 + SKIPCHAR() - '0';
                }
                return n;
            }
            else
            {
                returnchar('b');
                return -1;
            }
        }

        private int variable_token()
            /*
* variable token (* 6.4.3 *)
*    = anonymous variable (* 6.4.3 *)
*    | named variable (* 6.4.3 *)

* anonymous variable (* 6.4.3 *)
*    = variable indicator char (* 6.4.3 *)

* named variable (* 6.4.3 *)
*    = variable indicator char (* 6.4.3 *), 
*      alpha numeric char (* 6.5.2 *), 
*      { alpha numeric char (* 6.5.2 *) }
*    | capital letter char (* 6.5.2 *),
*      { alpha numeric char (* 6.5.2 *) } ;
*/
        {
            /* variable_indicator_char(LOOKAHEAD_CHAR) || 
	   capital_letter_char(LOOKAHEAD_CHAR) is true */
            RESETTOKEN();
            ADDTOKENCHAR(SKIPCHAR());
            while (alpha_numeric_char(LOOKAHEAD_CHAR))
                ADDTOKENCHAR(SKIPCHAR());
            ADDTOKENCHAR((char) 0); /* terminator */
            return VARIABLE_TOKEN;
        }

        private bool get_meta_escape_sequence()
            /*
*  meta escape sequence (* 6.4.2.1 *)
*    = backslash char (* 6.5.5 *),
*      meta char (* 6.5.5 *)
*/
        {
            if (backslash_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (meta_char(LOOKAHEAD_CHAR))
                {
                    tokenchar = SKIPCHAR();
                    return true;
                }
                else
                {
                    returnchar(c);
                    return false;
                }
            }
            else
                return false;
        }

        private bool get_control_escape_sequence()
            /*
* control escape sequence (* 6.4.2.1 *)
*   = backslash char (* 6.5.5 *),
*     symbolic control char (* 6.4.2.1 *)
 
* symbolic control char (* 6.4.2.1 *)
*   = symbolic alert char (* 6.4.2.1 *)               "a"
*   = symbolic vertical tab char (* 6.4.2.1 *)        "v"
*   = symbolic horizontal tab char (* 6.4.2.1 *)      "t"
*   = symbolic backspace char (* 6.4.2.1 *)           "b"
*   = symbolic form feed char (* 6.4.2.1 *)           "f"
*   = symbolic new line char (* 6.4.2.1 *)            "n"
*   = symbolic carriage return char (* 6.4.2.1 *)     "r"
*/
        {
            if (backslash_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (symbolic_control_char(LOOKAHEAD_CHAR))
                {
                    switch (SKIPCHAR())
                    {
                        case 'a':
                            tokenchar = (char) 7;
                            break; /* alert char, bell */
                        case 'b':
                            tokenchar = (char) 8;
                            break; /* backspace */
                        case 'f':
                            tokenchar = (char) 12;
                            break; /* formfeed */
                        case 'n':
                            tokenchar = (char) 10;
                            break; /* newline */
                        case 'r':
                            tokenchar = (char) 13;
                            break; /* carriage return */
                        case 't':
                            tokenchar = (char) 9;
                            break; /* horizontal tab */
                        case 'v':
                            tokenchar = (char) 11;
                            break; /* vertical tab */
                    }
                    return true;
                }
                else
                {
                    returnchar(c);
                    return false;
                }
            }
            else
                return false;
        }

        private bool get_octal_escape_sequence()
            /*
*   octal escape sequence (* 6.4.2.1 *)
*     = backslash char (* 6.5.5 *),
*       octal digit char (* 6.5.2 *),
*       { octal digit char (* 6.5.2 *) },
*       backslash char (* 6.5.5 *) ;
*/
        {
            if (backslash_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (octal_digit_char(LOOKAHEAD_CHAR))
                {
                    int n = SKIPCHAR() - '0';
                    while (octal_digit_char(LOOKAHEAD_CHAR))
                    {
                        n = n*8 + SKIPCHAR() - '0';
                    }
                    if (backslash_char(LOOKAHEAD_CHAR))
                    {
                        SKIPCHAR();
                        tokenchar = (char) n;
                        return true;
                    }
                    else
                    {
                        SyntaxError("Error in octal constant");
                        return false;
                    }
                }
                else
                {
                    returnchar(c);
                    return false;
                }
            }
            else
                return false;
        }

        private bool get_hexadecimal_escape_sequence()
            /*
*   hexadecimal escape sequence (* 6.4.2.1 *)
*      = backslash char (* 6.5.5 *),
*        symbolic hexadecimal char (* 6.4.2.1 *),
*        hexadecimal digit char (* 6.5.2 *),
*        { hexadecimal digit char (* 6.5.2 *) },
*        backslash char (* 6.5.5 *) ;
*/
        {
            if (backslash_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (symbolic_hexadecimal_char(LOOKAHEAD_CHAR))
                {
                    char lac;
                    int n = 0;

                    SKIPCHAR();
                    while (hexadecimal_digit_char(lac = LOOKAHEAD_CHAR))
                    {
                        if (decimal_digit_char(lac))
                            n = n*16 + SKIPCHAR() - '0';
                        else if (capital_letter_char(lac))
                            n = n*16 + SKIPCHAR() - 'A' + 10;
                        else
                            n = n*16 + SKIPCHAR() - 'a' + 10;
                    }
                    if (backslash_char(lac))
                    {
                        tokenchar = (char) n;
                        SKIPCHAR();
                        return true;
                    }
                    else
                    {
                        SyntaxError("Error in hexidecimal constant");
                        return false;
                    }
                }
                else
                {
                    returnchar(c);
                    return false;
                }
            }
            else
                return false;
        }

        private bool get_non_quote_char()
            /*
*   non quote char (* 6.4.2.1 *)
*     = graphic char (* 6.5.1 *)
*     | alpha numeric char (* 6.5.2 *)
*     | solo char (* 6.5.3 *)
*     | space char (* 6.5.4 *)
*     | meta escape sequence (* 6.4.2.1 *)
*     | control escape sequence (* 6.4.2.1 *)
*     | octal escape sequence (* 6.4.2.1 *)
*     | hexadecimal escape sequence (* 6.4.2.1 *)
*/
        {
            if (graphic_char(LOOKAHEAD_CHAR) ||
                alpha_numeric_char(LOOKAHEAD_CHAR) ||
                solo_char(LOOKAHEAD_CHAR) ||
                space_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            else
            {
                if (get_meta_escape_sequence()) return true;
                if (get_control_escape_sequence()) return true;
                if (get_octal_escape_sequence()) return true;
                if (get_hexadecimal_escape_sequence()) return true;
                return false;
            }
        }

        private bool get_single_quoted_char()
            /*
*  single quoted char (* 6.4.2.1 *)
*    = non quote char (* 6.4.2.1 *)
*    | single quote char (* 6.5.5 *),
*      single quote char (* 6.5.5 *)
*    | double quote char (* 6.5.5 *)
*    | back quote char (* 6.5.5 *) ;
*/
        {
            if (single_quote_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (!single_quote_char(LOOKAHEAD_CHAR))
                {
                    returnchar(c);
                    return false;
                }
                else
                {
                    tokenchar = SKIPCHAR();
                    return true;
                }
            }
            if (double_quote_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            if (back_quote_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            if (get_non_quote_char())
            {
                CHAR_CONVERSION();
                return true;
            }
            return false;
        }

        private bool get_double_quoted_char()
            /*
*  double quoted char (* 6.4.2.1 *)
*    = non quote char (* 6.4.2.1 *)
*    | single quote char (* 6.5.5 *)
*    | double quote char (* 6.5.5 *),
*      double quote char (* 6.5.5 *)
*    | back quote char (* 6.5.5 *) ;
*/
        {
            if (double_quote_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (!double_quote_char(LOOKAHEAD_CHAR))
                {
                    returnchar(c);
                    return false;
                }
                else
                {
                    tokenchar = SKIPCHAR();
                    return true;
                }
            }
            if (single_quote_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            if (back_quote_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            if (get_non_quote_char())
            {
                CHAR_CONVERSION();
                return true;
            }
            return false;
        }

        private bool get_back_quoted_char()
            /*
*  back quoted char (* 6.4.2.1 *)
*    = non quote char (* 6.4.2.1 *)
*    | single quote char (* 6.5.5 *)
*    | double quote char (* 6.5.5 *)
*    | back quote char (* 6.5.5 *), 
*      back quote char (* 6.5.5 *) ;
*/
        {
            if (back_quote_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (!back_quote_char(LOOKAHEAD_CHAR))
                {
                    returnchar(c);
                    return false;
                }
                else
                {
                    tokenchar = SKIPCHAR();
                    return true;
                }
            }
            if (single_quote_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            if (single_quote_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            if (single_quote_char(LOOKAHEAD_CHAR))
            {
                tokenchar = SKIPCHAR();
                return true;
            }
            if (get_non_quote_char())
            {
                CHAR_CONVERSION();
                return true;
            }
            return false;
        }

        private bool get_single_quoted_item()
            /*
*   single quoted item (* 6.4.2 *)
*      = single quoted char (* 6.4.2.1 *)
*      | continuation escape sequence (* 6.4.2.1 *) ;

*   continuation escape sequence (* 6.4.2.1 *)
*      = backslash char (* 6.5.5 *),
*        new line char (* 6.5.4 *);
*/
        {
            if (backslash_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (new_line_char(LOOKAHEAD_CHAR))
                {
                    SKIPCHAR();
                    return true;
                }
                else
                    returnchar(c);
            }
            if (get_single_quoted_char())
            {
                ADDTOKENCHAR(tokenchar);
                return true;
            }
            else
                return false;
        }

        private bool get_double_quoted_item()
            /*
*   double quoted item (*6.4.6 *)
*      = double quoted char (* 6.4.2.1 *)
*      | continuation escape sequence (* 6.4.2 *)
*/
        {
            if (backslash_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (new_line_char(LOOKAHEAD_CHAR))
                {
                    SKIPCHAR();
                    return true;
                }
                else
                    returnchar(c);
            }
            if (get_double_quoted_char())
            {
                ADDTOKENCHAR(tokenchar);
                return true;
            }
            else
                return false;
        }

        private bool get_back_quoted_item()
            /*
*   back quoted item (*6.4.6 *)
*      = back quoted char (* 6.4.2.1 *)
*      | continuation escape sequence (* 6.4.2 *)
*/
        {
            if (backslash_char(LOOKAHEAD_CHAR))
            {
                char c = SKIPCHAR();
                if (new_line_char(LOOKAHEAD_CHAR))
                {
                    SKIPCHAR();
                    ADDTOKENCHAR((char) 0);
                    return true;
                }
                else
                    returnchar(c);
            }
            if (get_back_quoted_char())
            {
                return true;
            }
            else
                return false;
        }

        private bool get_back_quoted_string()
            /*
*   back quoted string (* 6.4.2 *)
*     = back quote char (* 6.5.5 *),
*       { back quoted item (* 6.4.2 *) },
*       back quote char (* 6.5.5 *)
*/
        {
            if (back_quote_char(LOOKAHEAD_CHAR))
            {
                SKIPCHAR();
                while (get_back_quoted_item())
                {
                }
                if (back_quote_char(LOOKAHEAD_CHAR))
                {
                    SKIPCHAR();
                    ADDTOKENCHAR(tokenchar);
                    return true;
                }
                else
                {
                    //seterr(ERR_MISSING_QUOTE);
                    throw new SyntaxErrorException(this.TokenString, "String is missing quote", inputReader);
                    //return false;
                }
            }
            else
                return false;
        }

        private bool get_name_token()
            /*
*   name token (* 6.4.2 *)
*     = identifier token (* 6.4.2 *) 
*     | graphic token (* 6.4.2 *)
*     | quoted token (* 6.4.2 *)
*     | semicolon token (* 6.4.2 *)
*     | cut token (* 6.4.2 *)
 
*   identifier token (* 6.4.2 *)
*      = small letter char (* 6.5.2 *),
*        { alpha numeric char (* 6.5.2 *) } ;

*   graphic token (* 6.4.2 *)
*      = graphic token char (* 6.4.2 *)
*        { graphic token char (* 6.4.2 *) } ; 

*   quoted token (* 6.4.2 *)
*     = single quote char (* 6.5.5 *),
*       { single quoted item (* 6.4.2 *) },
*       single quote char (* 6.5.5 *)
*/
        {
            char c = LOOKAHEAD_CHAR;
            RESETTOKEN();
            if (small_letter_char(c))
            {
                ADDTOKENCHAR(SKIPCHAR());
                while (alpha_numeric_char(LOOKAHEAD_CHAR))
                    ADDTOKENCHAR(SKIPCHAR());
                ADDTOKENCHAR((char) 0); /* term */
                return true;
            }
            if (graphic_token_char(c))
            {
                ADDTOKENCHAR(SKIPCHAR());
                while (graphic_token_char(LOOKAHEAD_CHAR))
                    ADDTOKENCHAR(SKIPCHAR());
                ADDTOKENCHAR((char) 0); /* term */
                return true;
            }
            if (cut_token(c))
            {
                ADDTOKENCHAR(SKIPCHAR());
                ADDTOKENCHAR((char) 0); /* term */
                return true;
            }
            if (semicolon_token(c))
            {
                ADDTOKENCHAR(SKIPCHAR());
                ADDTOKENCHAR((char) 0); /* term */
                return true;
            }
            if (single_quote_char(c))
            {
                SKIPCHAR();
                while (get_single_quoted_item())
                {
                }
                if (single_quote_char(LOOKAHEAD_CHAR))
                {
                    SKIPCHAR();
                    ADDTOKENCHAR((char) 0); /* term */
                    return true;
                }
                else
                {
                    //seterr(ERR_MISSING_QUOTE);
                    throw new SyntaxErrorException(this.TokenString, "String is missing quote", inputReader);
                    //return false;
                }
            }
            if (get_back_quoted_string()) throw new SyntaxErrorException(this.TokenString, "Back quoted strings not supported", inputReader);//seterr(ERR_BACK_QUOTED_STRING);
            return false;
        }

        private int token()
            /*
*   token (* 6.4 *)
*     = name token (* 6.4.2 *)
*     | variable token (* 6.4.3 *)
*     | integer token (* 6.4.4 *)
*     | float token (* 6.4.5 *)
*     | char code list token (* 6.4.6 *)
*     | open token (* 6.4.8 *)
*     | close token (* 6.4.8 *)
*     | open list token (* 6.4.8 *)
*     | close list token (* 6.4.8 *)
*     | open curly token (* 6.4.8 *)
*     | close curly token (* 6.4.8 *)
*     | head tail separator token (* 6.4.8 *)
*     | comma token (* 6.4.8 *)
*     | end token (* 6.4.8 *)

*  layout text sequence (* 6.4.1 *)
*     = layout text (* 6.4.1 *),
*       { layout text (* 6.4.1 *) } ;

*  layout text (* 6.4.1 *)
*     = layout char (* 6.5.4 *)
*     | comment (* 6.4.1 *)

*   comment (* 6.4.1 *)
*     = single line commment (* 6.4.1 *)
*     | bracketed comment (* 6.4.1 *)
*/
        {
            bool layout_inserted = SkipLayout();

            char c = LOOKAHEAD_CHAR;
            if (capital_letter_char(c) || variable_indicator_char(c))
                return variable_token();
            if (comma_token(c))
            {
                SKIPCHAR();
                return COMMA_TOKEN;
            }
            if (close_token(c))
            {
                SKIPCHAR();
                return CLOSE_TOKEN;
            }
            if (open_token(c))
            {
                SKIPCHAR();
                return layout_inserted ? OPEN_TOKEN : OPEN_CT_TOKEN;
            }
            if (end_token(c))
            {
                SKIPCHAR();
                if (layout_char(LOOKAHEAD_CHAR) || end_line_comment_char(LOOKAHEAD_CHAR)
                    || LOOKAHEAD_CHAR == (char)0)   // ADDED BY IDH FOR .NET COMPAT
                    return END_TOKEN;
                else
                    returnchar('.');
            }
            if (decimal_digit_char(c)) return number_token();
            if (close_list_token(c))
            {
                SKIPCHAR();
                return CLOSE_LIST_TOKEN;
            }
            if (open_list_token(c))
            {
                SKIPCHAR();
                return OPEN_LIST_TOKEN;
            }
            if (head_tail_separator_token(c))
            {
                SKIPCHAR();
                return HEAD_TAIL_SEPARATOR_TOKEN;
            }
            if (open_curly_token(c))
            {
                SKIPCHAR();
                return OPEN_CURLY_TOKEN;
            }
            if (close_curly_token(c))
            {
                SKIPCHAR();
                return CLOSE_CURLY_TOKEN;
            }
            if (double_quote_char(LOOKAHEAD_CHAR))
            {
                if (char_code_list_token())
                    return CHAR_CODE_LIST_TOKEN;
            }
            if (get_name_token())
                return NAME_TOKEN;
            if (LOOKAHEAD_CHAR == (char) 0)
            {
                SKIPCHAR();
                return EOFFILE;
            }
            return 0;
        }

        /// <summary>
        /// Skip over whitespace and comments.
        /// </summary>
        public bool SkipLayout()
        {
            bool layout_inserted = false;
            bool more_layout = true;

            do
            {
                if (layout_char(LOOKAHEAD_CHAR))
                {
                    SKIPCHAR();
                    layout_inserted = true;
                }
                else if (end_line_comment_char(LOOKAHEAD_CHAR))
                {
                    single_line_comment();
                    layout_inserted = true;
                }
                else if (comment_1_char(LOOKAHEAD_CHAR))
                {
                    if (bracketed_comment())
                        layout_inserted = true;
                    else
                        more_layout = false;
                }
                else
                    more_layout = false;
            } while (more_layout);
            return layout_inserted;
        }

#if UnusedParserStuff
        private bool char_conversion(char c1, char c2)
        {
            charconv[c1] = c2;
            return true;
        }
#endif

        #endregion

        #region Parser
        static int Specifier(string specifierName)
        {
            switch (specifierName)
            {
                case "xfx":
                    return XFX;
                case "xfy":
                    return XFY;
                case "xf":
                    return XF;
                case "yf":
                    return YF;
                case "fx":
                    return FX;
                case "fy":
                    return FY;
                default:
                    throw new ArgumentException("Invalid operator specified: " + specifierName, "specifierName");
            }
        }

        private const ushort XFX = 0x0001;
        private const ushort XFY = 0x0002;
        private const ushort YFX = 0x0004;
        private const ushort XF = 0x0010;
        public const ushort YF = 0x0020;
        public const ushort FX = 0x0040;
        public const ushort FY = 0x0080;
        private const ushort DELIMITER = 0x0100;
        private const ushort TERM = 0x1000;
        private const ushort LTERM = 0x3000;

        public static bool isprefix(int X)
        {
            return ((X) & (FX | FY)) != 0;
        }

        private static int prefixbits(int X)
        {
            return ((X) & (FX | FY));
        }

        public static bool ispostfix(int X)
        {
            return ((X) & (XF | YF)) != 0;
        }

        public static bool isinfix(int X)
        {
            return ((X) & (XFX | XFY | YFX)) != 0;
        }

        public static bool isfx(int X)
        {
            return ((X) & FX) != 0;
        }

        public static bool isfy(int X)
        {
            return ((X) & FY) != 0;
        }

        public static bool isxf(int X)
        {
            return ((X) & XF) != 0;
        }

        public static bool isyf(int X)
        {
            return ((X) & YF) != 0;
        }

        public static bool isxfx(int X)
        {
            return ((X) & XFX) != 0;
        }

        public static bool isxfy(int X)
        {
            return ((X) & XFY) != 0;
        }

        public static bool isyfx(int X)
        {
            return ((X) & YFX) != 0;
        }

        private static bool isop(int X)
        {
            return ((X) & (XFX | XFY | YFX | XF | YF | FX | FY)) != 0;
        }

        private static bool isterm(int X)
        {
            return ((X) & TERM) != 0;
        }

        private static bool islterm(int X)
        {
            return (((X) & LTERM) == LTERM);
        }

#if UnusedParserStuff
        private static bool isdelimiter(int X)
        {
            return ((X) & DELIMITER) != 0;
        }
#endif

        /* parse stack */

        private class StackFrame
        {
            public int tokentype;
            public int priority;
            public int specifier;
            public Object term;
            public StackFrame down;
        }

        private StackFrame pStack;

        private readonly List<LogicVariable> vars = new List<LogicVariable>();
        /* variable management ; all correctly parsed variable names are put
           in a list to be used later to return the variables names to the
           options of read_term/3 */

        private void initvars()
            /* initialize variable list */
        {
            vars.Clear();
        }

        private void reduce(int newpri)
            /*
     *  lterm = term, op, term ;
     *  f(a,b)    a   f    b
     *    n      n-a  n   n-1
     *               xfx
     *
     *  lterm = lterm, op, term ;
     *  f(a,b)    a    f     b
     *     n      n    n    n-1
     *                yfx
     *
     *  term = term, op, term ;
     *  f(a,b)   a    f   b
     *    n     n-1   n   n
     *               xfy
     *
     *  lterm = lterm, op ;
     *  f(a)      a     f
     *   n        n     n
     *                  yf
     *
     *  lterm = term, op ;
     *  f(a)      a    f
     *   n       n-1   n
     *                 xf
     *
     *  term = op, term ;
     *  f(a)    f   a
     *  n       n   n
     *          fy
     *
     *  lterm = op, term ;
     *  f(a)    f    a
     *  n       n   n-1
     *          fx
     */
        {
            StackFrame top1, top2, top3;

            reduce:
            if ((top1 = pStack) != null)
            {
                if ((top2 = top1.down) != null)
                {
                    if ((top3 = top2.down) != null)
                    {
                        if (isxfx(top2.specifier))
                        {
                            if (top2.priority <= newpri &&
                                isterm(top3.specifier) && top3.priority < top2.priority &&
                                isterm(top1.specifier) && top1.priority < top2.priority)
                            {
                                top2.term = new Structure((Symbol) top2.term, top3.term, top1.term);
                                top2.down = top3.down;
                                //BFREE(top3);
                                //BFREE(top1);
                                pStack = top2;
                                pStack.specifier = LTERM;
                                pStack.tokentype = TERMTYPE;
                                goto reduce;
                            }
                        }
                        else if (isyfx(top2.specifier))
                        {
                            if (top2.priority <= newpri &&
                                (isterm(top3.specifier) && top3.priority < top2.priority ||
                                 islterm(top3.specifier) && top3.priority == top2.priority) &&
                                isterm(top1.specifier) && top1.priority < top2.priority)
                            {
                                top2.term = new Structure((Symbol) top2.term, top3.term, top1.term);
                                top2.down = top3.down;
                                //BFREE(top3);
                                //BFREE(top1);
                                pStack = top2;
                                pStack.specifier = LTERM;
                                pStack.tokentype = TERMTYPE;
                                goto reduce;
                            }
                        }
                        else if (isxfy(top2.specifier))
                        {
                            if (top2.priority < newpri &&
                                isterm(top3.specifier) && top3.priority < top2.priority &&
                                isterm(top1.specifier) && top1.priority <= top2.priority)
                            {
                                top2.term = new Structure((Symbol) top2.term, top3.term, top1.term);
                                top2.down = top3.down;
                                //BFREE(top3);
                                //BFREE(top1);
                                pStack = top2;
                                pStack.specifier = TERM;
                                pStack.tokentype = TERMTYPE;
                                goto reduce;
                            }
                        }
                    }
                    if (isyf(top1.specifier))
                    {
                        if (isterm(top2.specifier) && top2.priority < top1.priority ||
                            islterm(top2.specifier) && top2.priority == top1.priority)
                        {
                            top1.term = new Structure((Symbol) top1.term, top2.term);
                            top1.down = top2.down;
                            //BFREE(top2);
                            pStack.specifier = LTERM;
                            pStack.tokentype = TERMTYPE;
                            goto reduce;
                        }
                    }
                    else if (isxf(top1.specifier))
                    {
                        if (isterm(top2.specifier) && top2.priority < top1.priority)
                        {
                            top1.term = new Structure((Symbol) top1.term, top2.term);
                            top1.down = top2.down;
                            //BFREE(top2);
                            pStack.specifier = LTERM;
                            pStack.tokentype = TERMTYPE;
                            goto reduce;
                        }
                    }
                    else if (isfy(top2.specifier))
                    {
                        if (top2.priority < newpri &&
                            isterm(top1.specifier) && top1.priority <= top2.priority)
                        {
                            top2.term = new Structure((Symbol) top2.term, top1.term);
                            //BFREE(top1);
                            pStack = top2;
                            pStack.specifier = TERM;
                            pStack.tokentype = TERMTYPE;
                            goto reduce;
                        }
                    }
                    else if (isfx(top2.specifier))
                    {
                        if (top2.priority <= newpri &&
                            isterm(top1.specifier) && top1.priority < top2.priority)
                        {
                            top2.term = new Structure((Symbol) top2.term, top1.term);
                            //BFREE(top1);
                            pStack = top2;
                            pStack.specifier = LTERM;
                            pStack.tokentype = TERMTYPE;
                            goto reduce;
                        }
                    }
                        // Kluge to handle Twig's $ operator but still allow legacy code to redefine it.
                    else if (top2.term==Symbol.DollarSign && (top1.term is Symbol || top1.term is string))
                    {
                        var name = top1.term as string;
                        top2.term = name!=null?ResolveNamedString(name): ResolveNamedValue((Symbol)top1.term);
                        //BFREE(top1);
                        pStack = top2;
                        pStack.specifier = LTERM;
                        pStack.tokentype = TERMTYPE;
                        goto reduce;
                    }
                }
            }
        }

        static private object ResolveNamedString(string name)
        {
            var result =
                UnityEngine.GameObject.Find(name)
                ?? (object)UnityEngine.GameObject.Find(char.ToUpper(name[0])+name.Substring(1))
                ?? TypeUtils.FindType(name);
            if (result == null)
            {
                throw new SyntaxErrorException(name, "Unknown global constant name: " + name);
            }
            return result;
        }

        static object ResolveNamedValue(Symbol nameSymbol)
        {
            if (nameSymbol == Symbol.Global)
                return KnowledgeBase.Global;
            return Indexical.Find(nameSymbol) ?? ResolveNamedString(nameSymbol.Name);
        }

        private void shift(int tok, Object tterm, int pri, int spec)
        {
            pStack = new StackFrame
                         {
                             tokentype = tok,
                             term = tterm,
                             priority = pri,
                             specifier = spec,
                             down = pStack
                         };
        }

        private void releasestack()
            /* return memory used for the variable list */
        {
            StackFrame vlp = pStack;
            while (vlp != null)
            {
                StackFrame vlq = vlp.down;
                //BFREE(vlp);
                vlp = vlq;
            }
        }


        private void shift_char_code_list(string s)
            /*
     * term = char code list 
     *
     * this is list containing the ascii values of the characters belonging
     * to the string.
     */
        {
            // termtype listterm, secondarg;
            // string p = s+strlen(s);

            // secondarg = Symbol.Intern("[]");
            // while (p > s) {
            //   listterm = maketerm(".", 2);
            //   __ARG(listterm,1) = makeinteger(*--p);
            //   __ARG(listterm,2) = secondarg;
            //   secondarg = listterm;
            // }
            //shift(TERMTYPE,secondarg,0,TERM);
            shift(TERMTYPE, s, 0, TERM);
        }

        private void shifttoken(int tok)
            /*
     *  term = integer ;        (* 6.3.1.1 *)
     *  term = float number ;   (* 6.3.1.1 *)
     *  term = - integer ;      (* 6.3.1.2 *)
     *  term = - float number ; (* 6.3.1.2 *)
     *  term = atom ; (not an operator)  (* 6.3.1.2 *)
     *  term = atom ; (an operator )     (* 6.3.1.2 *)
     *    atom = name ;
     *    atom = empty list ;
     *       empty list = open list, close list ;
     *    atom = curly brackets ;
     *       curly brackets = open curly, close curly ;
     *  term = variable
     */
        {
            switch (tok)
            {
                case NAME_TOKEN:
                    {
                        int prepri, inpri, postpri, spec;
                        int inpostpri, sumpri;

                        if (Operator(this.TokenString, out prepri, out inpri, out postpri, out spec))
                        {
                            /* an operator */
                            if (prepri > 0 && (inpostpri = inpri + postpri) > 0)
                            {
                                /* infix and postfix do never occur together 6.3.4.2 */
                                if (open_char(LOOKAHEAD_CHAR))
                                {
                                    /* prefix can never be followed directly by an open char */
                                    reduce(inpostpri);
                                    shift(NAME_TOKEN, Symbol.Intern(this.TokenString), inpostpri,
                                          spec & (XFX | XFY | YFX | YF | XF));
                                }
                                else
                                {
                                    if (pStack != null)
                                    {
                                        reduce(inpostpri); /* must be done before testing the stack */
                                        if (isterm(pStack.specifier))
                                        {
                                            /* can be either infix of postfix */
                                            shift(NAME_TOKEN, Symbol.Intern(this.TokenString), inpostpri,
                                                  spec & (XFX | XFY | YFX | XF | YF));
                                        }
                                        else
                                        {
                                            /* in the beginning of an expression . must be prefix */
                                            shift(NAME_TOKEN, Symbol.Intern(this.TokenString), prepri, prefixbits(spec));
                                        }
                                    }
                                    else /* empty stack */
                                        shift(NAME_TOKEN, Symbol.Intern(this.TokenString), prepri, spec & (FX | FY));
                                }
                            }
                            else
                            {
                                /* there is only one specifier with priority sumpri */
                                reduce(sumpri = prepri + inpri + postpri);
                                shift(NAME_TOKEN, Symbol.Intern(this.TokenString), sumpri, spec);
                            }
                        }
                        else /* not an operator */
                            switch (this.TokenString)
                            {
                                    // GROSS KLUGE ADDED BY IAN TO DEAL WITH CONSTANTS YOU WANT FOR .NET
                                case "null":
                                    shift(NAME_TOKEN, null, 0, TERM);
                                    break;

                                case "true":
                                    shift(NAME_TOKEN, true, 0, TERM);
                                    break;

                                case "false":
                                    shift(NAME_TOKEN, false, 0, TERM);
                                    break;
                                
                                default:
                                    shift(NAME_TOKEN, Symbol.Intern(this.TokenString), 0, TERM);
                                    break;
                            }
                        break;
                    }
                case VARIABLE_TOKEN:
                    string name = this.TokenString;
                    // Look for existing variable if name doesn't start with underscore
                    var lv = (name[0] == '_')?null:vars.Find(v => v.Name.Name == name);
                    if (lv == null)
                    {
                        // Anonymous or otherwise new variable
                        lv = new LogicVariable(Symbol.Intern(name));
                        vars.Add(lv);
                    }
                    shift(VARIABLE_TOKEN, lv, 0, TERM);
                    break;
                case INTEGER_TOKEN:
                    if (pStack != null && pStack.term != null &&
                        (pStack.term is Symbol) && isprefix(pStack.specifier))
                    {
                        /*
                         *  if a is a numeric constant, f is not -
                         */
                        string s = ((Symbol) (pStack.term)).Name;
                        if (s[0] == '-' && s.Length == 1)
                        {
                            //StackFrame sp = pStack;
                            pStack = pStack.down;
                            //BFREE(sp);
                            shift(INTEGER_TOKEN, -integertoken, 0, TERM);
                            break;
                        }
                    }
                    shift(INTEGER_TOKEN, integertoken, 0, TERM);
                    break;
                case FLOAT_NUMBER_TOKEN:
                    if (pStack != null && pStack.term != null &&
                        (pStack.term is Symbol) && isprefix(pStack.specifier))
                    {
                        /*
                         *  if a is a numeric constant, f is not -
                         */
                        string s = ((Symbol) (pStack.term)).Name;
                        if (s[0] == '-' && s.Length == 1)
                        {
                            //StackFrame sp = pStack;
                            pStack = pStack.down;
                            //BFREE(sp);
                            shift(FLOAT_NUMBER_TOKEN, -floattoken, 0, TERM);
                            break;
                        }
                    }
                    shift(FLOAT_NUMBER_TOKEN, floattoken, 0, TERM);
                    break;
                case CHAR_CODE_LIST_TOKEN:
                    shift_char_code_list(this.TokenString);
                    break;
                case OPEN_TOKEN:
                    shift(OPEN_TOKEN, null, 1300, DELIMITER);
                    break;
                case OPEN_CT_TOKEN:
                    shift(OPEN_CT_TOKEN, null, 1300, DELIMITER);
                    break;
                case CLOSE_TOKEN:
                    if (!reduceterm())
                        if (!reducebrackets())
                            //seterr(ERR_NONDET);
                            SyntaxError("Ambiguous input");
                    break;
                case OPEN_LIST_TOKEN:
                    shift(OPEN_LIST_TOKEN, null, 1300, DELIMITER);
                    break;
                case CLOSE_LIST_TOKEN:
                    if (!reducelist()) SyntaxError("Parse error in list");//puts("Error in reducelist");
                    break;
                case OPEN_CURLY_TOKEN:
                    shift(OPEN_CURLY_TOKEN, null, 1300, DELIMITER);
                    break;
                case CLOSE_CURLY_TOKEN:
                    if (!reducecurly()) SyntaxError("Parse error in { } expression");//puts("Error in reducecurly");
                    break;
                case HEAD_TAIL_SEPARATOR_TOKEN:
                    reduce(1000);
                    shift(HEAD_TAIL_SEPARATOR_TOKEN, null, 1000, DELIMITER);
                    break;
                case COMMA_TOKEN:
                    reduce(1000);
                    shift(COMMA_TOKEN, Symbol.Comma, 1000, XFY);
                    break;
                case EOFFILE:
                    reduce(1400);
                    shift(EOFFILE, Symbol.EndOfFile, 1400, DELIMITER);
                    break;
                case END_TOKEN:
                    break;
            }
        }

        public static List<object> ReadTerms(TextReader inStream)
        {
            return new ISOPrologReader(inStream).ReadTerms();
        }

        public static Object ReadAndGetFreeVariables(string exp, List<LogicVariable> variables)
        {
            ISOPrologReader r = new ISOPrologReader(exp);
            var term = r.ReadTerm();
            variables.Clear();
            variables.AddRange(r.vars);
            return term;
        }

        public List<object> ReadTerms()
        {
            List<object> terms = new List<object>();
            object term;
            while ((term = ReadTerm()) != Symbol.EndOfFile)
                terms.Add(term);
            return terms;
        }

        public Object ReadTerm()
            /*
     * this procedure reads tokens and puts them on the stack
     * until it read either eof or end token. If one term is
     * left on the stack, the clause is correctly parsed.
     */
        {
            int tok;
            pStack = null;
            initvars();
            parseerror = ERR_NOERROR;
            do
            {
                shifttoken(tok = token());
            } while (tok != EOFFILE && tok != END_TOKEN && parseerror == ERR_NOERROR);
            reduce(1400);
            if (tok == EOFFILE)
            {
                if (pStack.down != null)
                {
                    //seterr(ERR_UNEXPECTED_EOF);
                    SyntaxError("Unexpected end of input.");
                    //showerror();
                    //showstack();
                    //releasestack();
                    //return null;
                }
            }
            if (parseerror == ERR_NOERROR && (tok == EOFFILE || tok == END_TOKEN) &&
                pStack != null && pStack.down == null)
            {
                Object tp = pStack.term;
                releasestack();
                return tp;
            }
            else
            {
                while (tok != EOFFILE && tok != END_TOKEN)
                    tok = token();
                if (parseerror == ERR_NOERROR) SyntaxError("Parse error");//seterr(ERR_INCOMPLETE_REDUCTION);
                //showerror();
                //showstack();
                releasestack();
                return null;
            }
        }

        private bool reducelist()
            /*
     * term = open list, items, close list ; (* 6.3.5 *)
     *   items = exp, comma, items ;
     *   items = exp, ht sep, exp ;
     *   items = exp ;
     *
     * exp = atom  (operator but no comma);
     * exp = term  (priority < 1000)
     */
        {
            if (pStack == null) return false; /* empty stack */
            if (pStack.tokentype == OPEN_LIST_TOKEN)
            {
                /* empty list */
                pStack.specifier = TERM;
                pStack.tokentype = NAME_TOKEN; /* behaves like atom 6.3.1.3 */
                pStack.term = null;
                pStack.priority = 0;
                return true;
            }
            else
            {
                StackFrame sp;
                Object tterm;
                int arty = 0;
                bool inlist = true;

                reduce(1000);
                sp = pStack;
                do
                {
                    if (isterm(sp.specifier) ||
                        isop(sp.specifier) && sp.tokentype != COMMA_TOKEN)
                    {
                        arty++;
                        sp = sp.down;
                    }
                    else
                        return false;
                    if (sp == null) return false;
                    if (sp.tokentype == HEAD_TAIL_SEPARATOR_TOKEN)
                    {
                        if (arty == 1)
                        {
                            sp = sp.down;
                            if (sp == null) return false;
                        }
                        else
                            return false;
                    }
                    else if (sp.tokentype == COMMA_TOKEN)
                    {
                        sp = sp.down;
                        if (sp == null) return false;
                    }
                    else if (sp.tokentype == OPEN_LIST_TOKEN)
                        inlist = false;
                    else return false;
                } while (inlist);

                if (pStack.down.tokentype != HEAD_TAIL_SEPARATOR_TOKEN)
                {
                    shift(HEAD_TAIL_SEPARATOR_TOKEN, null, 1000, DELIMITER);
                    shift(NAME_TOKEN, null /*Symbol.Intern("[]")*/, 0, TERM);
                    arty++;
                }
                tterm = pStack.term;
                sp = pStack;
                pStack = pStack.down;
                //BFREE(sp);
                while (--arty != 0)
                {
                    object[] args = new object[2];
                    Object lterm = new Structure(Symbol.PrologListConstructor, args);
                    sp = pStack.down;
                    //BFREE(pStack); 
                    args[0] = sp.term;
                    args[1] = tterm;
                    tterm = lterm;
                    pStack = sp.down;
                    //BFREE(sp); 
                }
                pStack.term = tterm;
                pStack.tokentype = TERMTYPE;
                pStack.specifier = TERM;
                pStack.priority = 0;
                return true;
            }
        }

        private bool reduceterm()
            /*
     * term = atom, open ct, arg list, close ;
     * 
     * arg list = exp ;
     * arg list = exp , comma, arg list ;
     */
        {
            StackFrame sp;
            Object tterm;
            int arty = 0;
            bool interm = true;

            if (pStack == null) return false;
            reduce(1000);
            sp = pStack;
            do
            {
                if (isterm(sp.specifier) ||
                    isop(sp.specifier) && sp.tokentype != COMMA_TOKEN)
                {
                    arty++;
                    sp = sp.down;
                }
                else
                    return false;
                if (sp == null) return false;
                if (sp.tokentype == COMMA_TOKEN)
                {
                    sp = sp.down;
                    if (sp == null) return false;
                }
                else if (sp.tokentype == OPEN_CT_TOKEN)
                {
                    interm = false;
                }
                else
                    return false;
            } while (interm);
            sp = sp.down;
            if (sp == null) return false;
            if (sp.tokentype != NAME_TOKEN) return false; /* term must contain atom */
            /* postfix operator can never occur individually, it immediately reduces.
               if it occurs, it must be a function name */
            if (isinfix(sp.specifier) && sp.down != null)
            {
                /* if (isterm(sp.down.specifier)) return false; term infix_op (exp) */
                if (!(separator_token(sp.down.tokentype) || isop(sp.down.specifier)))
                {
                    return false;
                }
            }
            object[] args = new object[arty];
            var functor = sp.term as Symbol;
            if (functor == null)
            {
                if (sp.term.Equals(true))
                    functor = Symbol.True;
                else if (sp.term.Equals(false))
                    functor = Symbol.Intern("false");
                else
                    throw new SyntaxErrorException(
                    sp.term,
                    "Invalid functor for compound term: " + Term.ToStringInPrologFormat(sp.term));
            }
            tterm = new Structure(functor, args);
            while (arty != 0)
            {
                args[--arty] = pStack.term;
                sp = pStack.down;
                //BFREE(pStack);
                pStack = sp.down;
                //BFREE(sp); 
            }
            pStack.priority = 0;
            pStack.tokentype = TERMTYPE;
            pStack.specifier = TERM;
            pStack.term = tterm;
            return true;
        }

        private Object separator2atom(int tok)
        {
            switch (tok)
            {
                case OPEN_LIST_TOKEN:
                    return Symbol.Intern("[");
                case CLOSE_LIST_TOKEN:
                    return Symbol.Intern("]");
                case HEAD_TAIL_SEPARATOR_TOKEN:
                    return Symbol.Intern("|");
                case OPEN_CT_TOKEN:
                case OPEN_TOKEN:
                    return Symbol.Intern("(");
                case CLOSE_TOKEN:
                    return Symbol.Intern(")");
                case OPEN_CURLY_TOKEN:
                    return Symbol.Intern("{");
                case CLOSE_CURLY_TOKEN:
                    return Symbol.Intern("}");
                case END_TOKEN:
                    return Symbol.Intern(".");
                case COMMA_TOKEN:
                    return Symbol.Intern(",");
            }
            return null;
        }

        private bool reducebrackets()
            /*
     *   6.3.4.1
     *   term = open, term, close ;
     *   term = copen ct, term, close ;
     */
        {
            StackFrame sp;

            if (pStack == null) return false;
            reduce(1300);
            /* anything can between brackets */
            sp = pStack.down;
            if (sp == null) return false;
            if (sp.tokentype == OPEN_TOKEN || sp.tokentype == OPEN_CT_TOKEN)
            {
                pStack.down = sp.down;
                //BFREE(sp);
                if (pStack.term == null) pStack.term = separator2atom(pStack.tokentype);
                pStack.specifier = TERM;
                pStack.tokentype = TERMTYPE;
                pStack.priority = 0;
                return true;
            }
            else return false;
        }

        private bool reducecurly()
            /*
     *   term = open curly, term, close curly ;
     */
        {
            if (pStack == null) return false;
            if (pStack.tokentype == OPEN_CURLY_TOKEN)
            {
                pStack.specifier = TERM;
                pStack.tokentype = NAME_TOKEN; /* behaves like atom 6.3.1.3 */
                pStack.priority = 0;
                pStack.term = Symbol.Intern("{}");
                return true;
            }
            else
            {
                reduce(1300);
                /* anything can between curly brackets */
                StackFrame sp = pStack.down;
                if (sp == null) return false;
                if (sp.tokentype == OPEN_CURLY_TOKEN)
                {
                    Object tterm = this.pStack.term ?? this.separator2atom(this.pStack.tokentype);

                    pStack.down = sp.down;
                    //BFREE(sp);
                    pStack.tokentype = TERMTYPE;
                    pStack.priority = 0;
                    pStack.specifier = TERM;
                    pStack.term = new Structure("{}", tterm);
                    return true;
                }
                else return false;
            }
        }

#if DEBUGPARSER
        private void showspecifier(int spec)
        {
            if ((spec & FX) != 0) printf("   fx");
            if ((spec & FY) != 0) printf("   fy");
            if ((spec & XF) != 0) printf("   xf");
            if ((spec & YF) != 0) printf("   yf");
            if ((spec & XFX) != 0) printf("  xfx");
            if ((spec & YFX) != 0) printf("  yfx");
            if ((spec & XFY) != 0) printf("  xfy");
            if (islterm(spec)) printf("lterm");
            else if (isterm(spec)) printf(" term");
            if ((spec & DELIMITER) != 0) printf("  del");
        }

        private void showstackelement(StackFrame sp)
        {
            Console.Write("| {0:####}: ", sp.priority);
            Console.Write(" {0:XXXX} ", sp.specifier);
            showspecifier(sp.specifier);
            printf("   ");
            if (sp.term != null)
            {
                //writeterm(stdout, sp.term, 1 + 4);
                puts("");
            }
            else
            {
                switch (sp.tokentype)
                {
                    case COMMA_TOKEN:
                        printf(",\n");
                        break;
                    case OPEN_LIST_TOKEN:
                        printf("[\n");
                        break;
                    case CLOSE_LIST_TOKEN:
                        printf("]\n");
                        break;
                    case HEAD_TAIL_SEPARATOR_TOKEN:
                        printf("|\n");
                        break;
                    case OPEN_CURLY_TOKEN:
                        printf("{\n");
                        break;
                    case CLOSE_CURLY_TOKEN:
                        printf("}\n");
                        break;
                    case OPEN_CT_TOKEN:
                        printf("(\n");
                        break;
                    case OPEN_TOKEN:
                        printf("(\n");
                        break;
                    case CLOSE_TOKEN:
                        printf(")\n");
                        break;
                    case END_TOKEN:
                        printf(".\n");
                        break;
                }
            }
        }

        private void showstack()
        {
            StackFrame sp = pStack;
            puts("");
            printf("=====================\n");
            while (sp != null)
            {
                showstackelement(sp);
                sp = sp.down;
            }
            printf("=====================\n");
        }

        void showerror()
        {}
#endif
        #endregion

        #region Error handling
        void SyntaxError(string message)
        {
            throw new SyntaxErrorException(CaptureStack(), message, inputReader);
        }

        string CaptureStack()
        {
            List<object> result = new List<object>();
            if (this.TokenString != "")
                result.Add(this.TokenString);
            StackFrame s = pStack;
            while (s != null)
            {
                object item;
                switch(s.tokentype)
                {
                    case EOFFILE:
                        item = "end_of_file";
                        break;

                    case OPEN_TOKEN:
                    case OPEN_CT_TOKEN:
                        item = '(';
                        break;


                    case CLOSE_TOKEN:
                        item = ')';
                        break;

                    case OPEN_LIST_TOKEN:
                        item = '[';
                        break;

                    case CLOSE_LIST_TOKEN:
                        item = ']';
                        break;

                    case OPEN_CURLY_TOKEN:
                        item = '{';
                        break;

                    case CLOSE_CURLY_TOKEN:
                        item = '}';
                        break;

                    case HEAD_TAIL_SEPARATOR_TOKEN: 
                        item = '|';
                        break;

                    case COMMA_TOKEN:
                        item = ',';
                        break;

                    case END_TOKEN:
                        item = '.';
                        break;

                    default:
                        item = s.term;
                        break;
                }
                result.Insert(0, item);  // okay, so this is technically quadratic, but the stack shouldn't be deep, and this only runs when syntax errors are thrown
                s = s.down;
            }
            StringWriter sw = new StringWriter();
            ISOPrologWriter w = new ISOPrologWriter(sw);
            bool first = true;
            foreach (var t in result)
            {
                if (first)
                    first = false;
                else
                    w.WriteString(" ");
                w.Write(t);
            }

            return sw.ToString();
        }
        #endregion

#if DEBUGPARSER
        #region Icky C compatability

        static private void puts(string s)
        {
            Console.Write(s);
        }

        #endregion
#endif
    }
}