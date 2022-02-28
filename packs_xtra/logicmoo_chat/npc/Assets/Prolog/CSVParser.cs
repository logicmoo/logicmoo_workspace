using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Prolog
{
    // ReSharper disable once InconsistentNaming
    class CSVParser
    {
        public CSVParser(Symbol functor, char delimiter, PositionTrackingTextReader reader)
        {
            this.functor = functor;
            this.delimiter = delimiter;
            this.reader = reader;
        }

        struct ColumnFormat
        {
            public enum FormatType
            {
                // This is just a generic prolog expression (default)
                PrologExpression,
                // This is a string - wrap it in double quotes
                String,
                // This is a list of prolog expressions - wrap it in  [ ]
                List,
                // This is a list of prolog expressions, but separated by spaces rather than commas
                WordList
            }
            private readonly FormatType type;

            private readonly string prefix;

            public ColumnFormat(FormatType type, string prefix)
                : this()
            {
                this.type = type;
                this.prefix = prefix;
            }

            public void AppendFormatted(StringBuilder b, string item)
            {
                switch (type) {
                    case FormatType.String:
                    b.Append('"');
                    b.Append(item);
                    b.Append('"');
                        break;

                    case FormatType.List:
                        b.Append('[');
                        b.Append(item);
                        b.Append(']');
                        break;

                    case FormatType.WordList:
                        b.Append('[');
                        b.Append(item.Trim().Replace(' ', ','));
                        b.Append(']');
                        break;

                    default:
                    if (this.prefix != null)
                        b.Append(this.prefix);

                    b.Append((item.Trim()=="")?"null":item);
                        break;
                }
            }
        }

        readonly List<ColumnFormat> columnFormats = new List<ColumnFormat>();

        /// <summary>
        /// Number of columns in the spreadsheet.
        /// </summary>
        public int Arity
        {
            get
            {
                return columnFormats.Count;
            }
        }

        private readonly Symbol functor;

        private readonly char delimiter;

        private readonly PositionTrackingTextReader reader;

        private readonly StringBuilder itemBuffer = new StringBuilder();

        private readonly StringBuilder factBuffer = new StringBuilder();

        private int rowNumber = 1;

        const string PrefixHeader = "(prefix: ";

        // ReSharper disable once InconsistentNaming
        public void Read(Action<int, Structure> rowHandler)
        {

            var row = 1;
            try
            {
                this.ReadHeaderRow();
                row++;
                while (reader.Peek() >= 0)
                {
                    if (reader.Peek() == '%')
                        SkipLine(); // Skip comment lines
                    else
                        rowHandler(row, this.ReadFactRow());
                    row++;
                }
            }
            catch (InferenceStepsExceededException e)
            {
                Repl.RecordExceptionSourceLocation(e, row);
                throw;
            }
            catch (Exception e)
            {
                var wrapper = new PrologError(e,
                    string.Format("{0} row {1}",
                                  Path.GetFileName(Prolog.CurrentSourceFile),
                                  row));
                UnityEngine.Debug.LogException(wrapper);
                Repl.RecordExceptionSourceLocation(e, row);
                throw wrapper;
            }
        }

        void SkipLine()
        {
            int c;
            do
            {
                c = reader.Read();
            }
            while (c != '\r');
        }

        void ReadHeaderRow()
        {
            this.ReadRow(item => this.columnFormats.Add(DecodeFormat(item)));
        }

        private ColumnFormat DecodeFormat(string headerItem)
        {
            if (headerItem.EndsWith(")"))
            {
                if (headerItem.EndsWith("(string)"))
                    return new ColumnFormat(ColumnFormat.FormatType.String, null);
                if (headerItem.EndsWith("(list)"))
                    return new ColumnFormat(ColumnFormat.FormatType.List, null);
                if (headerItem.EndsWith("(word list)"))
                    return new ColumnFormat(ColumnFormat.FormatType.WordList, null);

                // ReSharper disable once StringIndexOfIsCultureSpecific.1
                var prefixSpec = headerItem.IndexOf(PrefixHeader);
                if (prefixSpec >= 0)
                {
                    var prefixStart = prefixSpec + PrefixHeader.Length;
                    return new ColumnFormat(
                        ColumnFormat.FormatType.PrologExpression, 
                        headerItem.Substring(prefixStart, headerItem.Length - (prefixStart + 1)));
                }
            }
            return new ColumnFormat(ColumnFormat.FormatType.PrologExpression, null);
        }

        Structure ReadFactRow()
        {
            bool firstColumn = true;
            int argument = 0;
            factBuffer.Length = 0;
            factBuffer.Append(functor.Name);
            factBuffer.Append('(');
            this.ReadRow(
                item =>
                {
                    if (firstColumn)
                        firstColumn = false;
                    else
                        factBuffer.Append(", ");
                    if (argument>=Arity)
                        throw new Exception("Too many columns in row "+rowNumber);
                    columnFormats[argument].AppendFormatted(this.factBuffer, item);
                    argument += 1;
                });
            if (argument != Arity)
                throw new Exception("Too few columns in row "+rowNumber);
            factBuffer.Append(").");
            return (Structure)ISOPrologReader.Read(factBuffer.ToString());
        }

        private void ReadRow(Action<string> itemHandler)
        {
            int peek = reader.Peek();
            while (peek >= 0)
            {
                if (peek == '\r' || peek == '\n')
                {
                    // end of line - swallow cr and/or lf
                    reader.Read();
                    if (peek == '\r')
                    {
                        // Swallow LF if CRLF
                        peek = reader.Peek();
                        if (peek == '\n')
                            reader.Read();
                    }
                    rowNumber++;
                    return;
                }
                if (peek == this.delimiter)
                    // Skip over delimiter
                    this.reader.Read();

                itemHandler(ReadItem(this.reader, this.delimiter, this.itemBuffer));
                peek = reader.Peek();
            }
        }

        static string ReadItem(TextReader reader, char delimiter, StringBuilder stringBuilder)
        {
            bool quoted = false;
            stringBuilder.Length = 0;
            int peek = (char)reader.Peek();
            if (peek == delimiter)
                return "";
            if (peek == '\"')
            {
                quoted = true;
                reader.Read();
            }
        getNextChar:
            peek = reader.Peek();
            if (peek < 0)
                goto done;
            if (quoted && peek == '\"')
            {
                reader.Read();  // Swallow quote
                if ((char)reader.Peek() == '\"')
                {
                    // It was an escaped quote
                    reader.Read();
                    stringBuilder.Append('\"');
                    goto getNextChar;
                }
                // It was the end of the item
                // ReSharper disable RedundantJumpStatement
                goto done;
                // ReSharper restore RedundantJumpStatement
            }
            if (!quoted && (peek == delimiter || peek == '\r' || peek == '\n'))
                // ReSharper disable RedundantJumpStatement
                goto done;
            // ReSharper restore RedundantJumpStatement
            stringBuilder.Append((char)peek);
            reader.Read();
            goto getNextChar;
        //System.Diagnostics.Debug.Assert(false, "Line should not be reachable.");
        done:
            return stringBuilder.ToString();
        }
    }
}
