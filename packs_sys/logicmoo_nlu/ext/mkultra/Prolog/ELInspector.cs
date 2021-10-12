using System;
using System.Collections.Generic;
using System.Text;

using UnityEngine;

namespace Prolog
{
    class ELInspector : MonoBehaviour
    {
        #region Editor-configurable properties
        public Rect WindowRect = new Rect(0, 0, 640, 480);
        public bool ShowInspector = true;
        public GUIStyle Style = new GUIStyle();
        public KeyCode ActivationKey = KeyCode.F2;	//Key used to show/hide inspector
        #endregion

        #region Private fields and properties
        private string WindowTitle { get; set; }
        private ELNode root;

        /// <summary>
        /// Nodes to display the children of
        /// </summary>
        private readonly HashSet<ELNode> displayChildren = new HashSet<ELNode>(); 
        // ReSharper disable once InconsistentNaming
        private int ID;
        private Vector2 scrollPosition;
        // ReSharper disable once InconsistentNaming
        protected static int IDCount = typeof(ELInspector).GetHashCode();

        /// <summary>
        /// Total height of the dumped EL database
        /// </summary>
        private float viewHeight;
        #endregion

        internal void Start()
        {
            this.SetKB(this.GetComponent<PrologConsole>().DefaultGameObject.KnowledgeBase());
            ID = IDCount++;
            viewHeight = WindowRect.height;
        }

        public void SetKB(KnowledgeBase kb)
        {
            root = kb.ELRoot;
            displayChildren.Add(root);
            WindowTitle = kb.Name + " KB";
        }

        private bool mouseClicked;
        private float mouseClickY;
        internal void OnGUI()
        {
            if (this.ShowInspector)
            {
                this.WindowRect = GUI.Window(ID, this.WindowRect, this.DrawWindow, WindowTitle);
            }

            switch (Event.current.type)
            {
                case EventType.mouseDown:
                    mouseClicked = true;
                    mouseClickY = Event.current.mousePosition.y - WindowRect.y;
                    break;

                case EventType.KeyUp:
                    if (Event.current.keyCode == ActivationKey)
                    {
                        this.ShowInspector = !this.ShowInspector;
                    }
                    break;
            }
        }

        // ReSharper disable once InconsistentNaming
        private void DrawWindow(int windowID)
        {
            //Console Window
            GUI.DragWindow(new Rect(0, 0, this.WindowRect.width, 20));
            //Scroll Area
            scrollPosition = 
                GUI.BeginScrollView(
                    new Rect(0, 0, WindowRect.width, WindowRect.height),
                    scrollPosition,
                    new Rect(0, 0, WindowRect.width, viewHeight), false, true);
            mouseClickY += scrollPosition.y;
            viewHeight = Math.Max(
                viewHeight,
                this.RenderAt(root, 0, 0));
            GUI.EndScrollView();
            mouseClicked = false;
        }

        StringBuilder stringBuilder = new StringBuilder();
        private float RenderAt(ELNode node, float x, float y)
        {
            stringBuilder.Length = 0;
            var go = node.Key as GameObject;
            stringBuilder.Append(go != null ? 
                ('$'+go.name) 
                : (node.Key == null? 
                    "null"
                    : node.Key.ToString()));
            stringBuilder.Append(node.ModeString);
            var suppressChildren = node.Children.Count > 1 && !displayChildren.Contains(node);
            if (suppressChildren)
                stringBuilder.Append(" ...");
            var key = new GUIContent(stringBuilder.ToString());
            var size = Style.CalcSize(key);
            if (mouseClicked && mouseClickY >= y && mouseClickY < y + size.y)
                ToggleNode(node);
            GUI.Label(new Rect(x, y, size.x, size.y), key, Style);
            x += size.x;
            if (node.Children.Count == 0 || suppressChildren)
                y += size.y;
            else
                foreach (var child in node.Children)
                {
                    y = this.RenderAt(child, x, y);
                }
            return y;
        }

        private void ToggleNode(ELNode node)
        {
            if (displayChildren.Contains(node))
                displayChildren.Remove(node);
            else
            {
                displayChildren.Add(node);
            }
        }
    }
}
