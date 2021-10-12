using UnityEngine;

namespace Prolog
{
    class PrologConsole : Northwestern.UnityUtils.Console
    {
#pragma warning disable 649
        public GameObject DefaultGameObject;
#pragma warning restore 649

        private Repl repl;

        internal override void Start()
        {
            base.Start();
            repl = new Repl {
                       Output = Out,
                       CurrentGameObject = DefaultGameObject??gameObject,
                       OnChangeKB = KBChanged
                   };
            WindowTitle = "Prolog console: " + repl.CurrentKnowledgeBase.Name;
            Prolog.TraceOutput = Out;
            PrologChecker.Check();
        }

        void KBChanged(KnowledgeBase newKB)
        {
            var eli = FindObjectOfType<ELInspector>();
            if (eli != null)
                eli.SetKB(newKB);
            WindowTitle = "Prolog console: " + newKB.Name;
        }

        protected override void Run(string command)
        {

            if (command != ";")
                Out.Write("?- ");
            Out.WriteLine(command);
            repl.ProcessCommandLine(command);
        }

        protected override bool OmitCommandFromHistory(string command)
        {
            // Ignore requests for subsequent solutions
            return command.Trim() == ";";
        }
    }
}
