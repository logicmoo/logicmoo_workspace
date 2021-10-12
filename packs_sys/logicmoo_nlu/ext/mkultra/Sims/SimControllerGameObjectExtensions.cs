using System;
using UnityEngine;
using Prolog;

public static class SimControllerGameObjectExtensions
{
        /// <summary>
        /// Queues the specified event for the character to process.
        /// </summary>
        /// <param name="character">Character to inform</param>
        /// <param name="ev">The event that occurred.</param>
        public static void QueueEvent(this GameObject character, Structure ev)
        {
            var sim = character.GetComponent<SimController>();
            if (sim == null)
                throw new Exception("Attempt to queue event on a game object that is not a character: "+character.name);
            sim.QueueEvent(ev);
        }

        /// <summary>
        /// True if the object is a character.
        /// </summary>
        public static bool IsCharacter(this GameObject o)
        {
            return o.GetComponent<SimController>() != null;
        }

        /// <summary>
        /// True if the object is a character.
        /// </summary>
        public static bool IsProp(this GameObject o)
        {
            return o.GetComponent<DockingRegion>() != null;
        }
}
