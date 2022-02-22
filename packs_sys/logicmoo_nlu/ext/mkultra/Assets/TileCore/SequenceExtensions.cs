using System;
using System.Collections.Generic;

/// <summary>
/// Extension methods, i.e. methods for other classes that are added as an afterthought.
/// </summary>
public static class SequenceExtensions
{
    /// <summary>
    /// Returns the element of list with the maximum score
    /// </summary>
    /// <typeparam name="T">The type of element in the list</typeparam>
    /// <param name="list">The list to search</param>
    /// <param name="score">A method that returns the score of a given list element</param>
    /// <returns>The element with the maximal score.  That is, not the score, but the element itself.</returns>
    public static T ArgMax<T>(this List<T> list, Func<T, float> score)
    {
        if (list.Count == 0)
        {
            UnityEngine.Debug.LogWarning("ArgMax called on empty list");
        }
        T best = list[0];
        float bestScore = score(best);
        for (int i = 1; i < list.Count; i++)
        {
            var e = list[i];
            var eScore = score(e);
            if (eScore > bestScore)
            {
                best = e;
                bestScore = eScore;
            }
        }
        return best;
    }

    /// <summary>
    /// Returns the element of list with the minimum score
    /// </summary>
    /// <typeparam name="T">The type of element in the list</typeparam>
    /// <param name="list">The list to search</param>
    /// <param name="score">A method that returns the score of a given list element</param>
    /// <returns>The element with the minimal score.  That is, not the score, but the element itself.</returns>
    public static T ArgMin<T>(this List<T> list, Func<T, float> score)
    {
        System.Diagnostics.Debug.Assert(list.Count>0, "ArgMin called on empty list");
        T best = list[0];
        float bestScore = score(best);
        for (int i = 1; i < list.Count; i++)
        {
            var e = list[i];
            var eScore = score(e);
            if (eScore < bestScore)
            {
                best = e;
                bestScore = eScore;
            }
        }
        return best;
    }
}