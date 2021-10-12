using System;
using System.IO;
using UnityEngine;
using UnityEditor.Callbacks;
using UnityEditor;

public class CopyResourcesPostprocessor
{
    [PostProcessBuild]
    public static void OnPostprocessBuild(BuildTarget target, string pathToBuiltProject)
    {
        var sourceDirectory = Application.dataPath;
        string targetDirectory;

        switch (target)
        {
            case BuildTarget.StandaloneWindows:
            case BuildTarget.StandaloneWindows64:
                targetDirectory = Path.ChangeExtension(pathToBuiltProject, null) + "_data";
                break;

            case BuildTarget.StandaloneOSXIntel:
                targetDirectory = pathToBuiltProject + "/Contents";
                break;

            default:
                throw new Exception("Don't know how to build Prolog code for target: "+target);
        }
        var copyResourcesPostprocessor = new CopyResourcesPostprocessor(sourceDirectory, targetDirectory);
        copyResourcesPostprocessor.CopyDirectory(sourceDirectory);
    }

    private readonly string sourcePrefix;

    private readonly string targetPrefix;

    static readonly string[] ResourceExtensions = { ".csv", ".prolog" };

    public CopyResourcesPostprocessor(string sourcePrefix, string targetPrefix)
    {
        this.sourcePrefix = sourcePrefix;
        this.targetPrefix = targetPrefix;
    }

    public string MapPath(string fileIn)
    {
        return fileIn.Replace(this.sourcePrefix, this.targetPrefix);
    }

    public void CopyDirectory(string fromPath)
    {
        Directory.CreateDirectory(this.MapPath(fromPath));

        foreach (var fileIn in Directory.GetFiles(fromPath))
            if (ArrayUtility.Contains(ResourceExtensions, Path.GetExtension(fileIn)))
            {
                File.Copy(fileIn, this.MapPath(fileIn));
            }

        foreach (var directoryIn in Directory.GetDirectories(fromPath))
            this.CopyDirectory(directoryIn);
    }
}
