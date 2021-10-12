using System;

using UnityEngine;

namespace Prolog
{
    /// <summary>
    /// A wrapper for a Prolog KnowledgeBase as a Unity Component.
    /// </summary>
    class KB : MonoBehaviour
    {
        private const string GlobalKBGameObjectName = "GlobalKB";

        public string[] SourceFiles = new string[0];

        private KnowledgeBase kb;

        private static bool globalKBInitialized;

        public KnowledgeBase KnowledgeBase
        {
            get
            {
                if (kb == null)
                    this.MakeKB();

                return kb; 
            }
        }

        /// <summary>
        /// True if this is the KB object for the global Prolog knowledgebase.
        /// </summary>
        public bool IsGlobal
        {
            get
            {
                return name == GlobalKBGameObjectName;
            }
        }

        public static KB Global
        {
            get
            {
                return GameObject.Find(GlobalKBGameObjectName).GetComponent<KB>();
            }
        }

        internal void Awake()
        {
            if (IsGlobal)
            {
                if (globalKBInitialized)
                    Debug.LogError("There appear to be multiple KB components for the Global Prolog knowledgebase.");
                else
                    globalKBInitialized = true;
                this.MakeKB();
            }
        }

        internal void Start()
        {
            if (kb == null)
                MakeKB();
        }

        private void MakeKB()
        {
            var parent = transform.parent;
            KB parentKB = null;
            if (parent != null)
            {
                parentKB = parent.GetComponent<KB>();
            }

            kb = IsGlobal?
                KnowledgeBase.Global
                : new KnowledgeBase(
                    gameObject.name,
                    gameObject,
                    parentKB == null ? GameObject.Find(GlobalKBGameObjectName).KnowledgeBase() : parentKB.KnowledgeBase);

            // Add UID counter.
            ELNode.Store(kb.ELRoot/Symbol.Intern("next_uid")%0);

            try
            {
                foreach (var file in SourceFiles)
                    kb.Consult(file);
            }
            catch (Exception)
            {
                Debug.Break(); // Pause the game
                throw;
            }
        }
    }
}
