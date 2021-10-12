using System;

namespace Prolog
{
    public struct PredicateIndicator
    {
        public readonly Symbol Functor;
        public readonly int Arity;

        public PredicateIndicator(Symbol functor, int arity)
        {
            this.Functor = functor;
            this.Arity = arity;
        }

        public static PredicateIndicator FromExpression(object expression)
        {
            var s = Term.Deref(expression) as Structure;
            if (s == null
                || (!s.IsFunctor(Symbol.Slash, 2) && !s.IsFunctor(Symbol.SlashSlash, 2))
                || !(s.Argument(0) is Symbol)
                || !(s.Argument(1) is int))
                throw new ArgumentException("Predicate indicator should be of the form functor/arity, but got "+ISOPrologWriter.WriteToString(expression));
            return new PredicateIndicator((Symbol)s.Argument(0), (int)s.Argument(1));
        }

        public PredicateIndicator(Structure s) : this(s.Functor, s.Arity) { }

        public static bool operator==(PredicateIndicator a, PredicateIndicator b)
        {
            return a.Functor == b.Functor && a.Arity == b.Arity;
        }

        public static bool operator !=(PredicateIndicator a, PredicateIndicator b)
        {
            return a.Functor != b.Functor || a.Arity != b.Arity;
        }

        public override int GetHashCode()
        {
            return Functor.GetHashCode() ^ Arity;
        }

        public override bool Equals(object obj)
        {
            if (obj is PredicateIndicator)
            {
                var o = (PredicateIndicator)obj;
                return this.Functor == o.Functor && this.Arity == o.Arity;
            }
            return false;
        }

        public override string ToString()
        {
            return string.Format("{0}/{1}", Functor.Name, Arity);
        }
    }
}
