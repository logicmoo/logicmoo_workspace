using System;
using System.Collections.Generic;
using System.IO;

namespace Prolog
{
    class SourceFileTracker
    {
        readonly Dictionary<string, DateTime> files = new Dictionary<string, DateTime>();

        public void NoteFile(string path)
        {
            files[path] = File.GetLastWriteTime(path);
        }

        public List<string> OutOfDateFiles
        {
            get
            {
                // Collections can't be iterated over while we're mutating them so we have to precompute the list of out of date files, then return them.
                var modifiedFiles = new List<string>();
                foreach (var pair in files)
                    if (File.GetLastWriteTime(pair.Key) > pair.Value)
                        modifiedFiles.Add(pair.Key);
                return modifiedFiles;
            }
        }

        public bool Contains(string path)
        {
            return files.ContainsKey(path);
        }
    }
}
