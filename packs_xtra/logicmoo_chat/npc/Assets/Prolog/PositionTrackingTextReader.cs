using System;
using System.IO;
using System.Text;

namespace Prolog
{
    class PositionTrackingTextReader : TextReader
    {
        public PositionTrackingTextReader(TextReader reader, string filename)
        {
            subreader = reader;
            File = filename;
            Line = 1;
            currentLine = new StringBuilder();
        }

        private readonly TextReader subreader;
        readonly StringBuilder currentLine;
        private string errorLine;

        public int Line { get; private set; }
        public int Column { get; private set; }
        public string File { get; private set; }

        /// <summary>
        /// The current line at the time a syntax error was found;
        /// </summary>
        public string ErrorLine
        {
            get
            {
                if (errorLine == null)
                {
                    // Finish off the current line
                    currentLine.Append(subreader.ReadLine());
                    errorLine = currentLine.ToString();
                }
                return errorLine;
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA2204:Literals should be spelled correctly", MessageId = "ErrorLine")]
        public override int Read()
        {
            if (errorLine != null)
                throw new InvalidOperationException("Attempt to read after ErrorLine invoked.");
            int c = subreader.Read();
            if (c=='\n')
            {
                Line += 1;
                Column = 0;
                currentLine.Length = 0;
            }
            else if (c>=0)
            {
                currentLine.Append((char) c);
                Column += 1;
            }

            return c;
        }

        public override int Peek()
        {
            return subreader.Peek();
        }
    }
}
