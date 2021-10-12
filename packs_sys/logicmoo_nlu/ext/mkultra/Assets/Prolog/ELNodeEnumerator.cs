namespace Prolog
{
    public abstract class ELNodeEnumerator
    {
        public ELNode Current;

        public abstract bool MoveNext();

        public abstract bool BindsVar(LogicVariable v);
    }
}
