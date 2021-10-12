using System;
using Prolog;
using UnityEngine;

internal class ConfigurationOptions : MonoBehaviour
{
    public string Subtree="configuration";
    public ConfigurationOption[] Options= {};

    internal void Awake()
    {
        var kb = this.GetComponent<KB>();
        foreach (var option in Options)
            if (option.Selected)
            {
                kb.IsTrue(ISOPrologReader.Read(string.Format("assert(/{0}/{1}).",Subtree, option.Name)));
            }
    }

    [Serializable]
    public class ConfigurationOption
    {
        public Boolean Selected=false;
        public string Name="";
    }
}
