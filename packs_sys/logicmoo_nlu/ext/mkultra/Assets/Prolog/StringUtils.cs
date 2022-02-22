using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Prolog
{
    public static class StringUtils
    {
        static readonly char[] Vowels = { 'a', 'e', 'i', 'o', 'u' };

        public static string PluralForm(string singularForm)
        {
            switch (singularForm[singularForm.Length - 1])
            {
                case 's':
                case 'o':
                    return singularForm + "es";

                case 'f':
                    return singularForm.Substring(0, singularForm.Length - 1) + "ves";


                case 'y':
                    var secondToLast = singularForm[singularForm.Length - 2];
                    if (Vowels.Contains(secondToLast))
                    {
                        return singularForm + "s";
                    }
                    return singularForm.Substring(0, singularForm.Length - 1) + "ies";

                default:
                    return singularForm + "s";
            }
        }

        public static string LastWordOf(string phrase)
        {
            var space = phrase.LastIndexOf(' ');
            if (space < 0)
                return phrase;
            return phrase.Substring(space + 1);
        }

    }
}
