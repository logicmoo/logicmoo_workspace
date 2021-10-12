using System;

namespace Prolog
{
    class GoalException : Exception
    {
        public object Goal { get; private set; }
        public GoalException(object goal, string message) : base(string.Format("{0}: {1}", message, ISOPrologWriter.WriteToString(goal)))
        {
            Goal = goal;
        }
    }
}
