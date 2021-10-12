// Copyright 2007, 2008, 2009, 2010, 2011 Ian Horswill
// This file is part of Twig.
//
// Twig is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as 
// published by the Free Software Foundation, either version 3 of
//  the License, or (at your option) any later version.
//
// Twig is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Twig.  If not, see <http://www.gnu.org/licenses/>.

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Prolog
{
    /// <summary>
    /// Creates documentation for the Twig system
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1063:ImplementIDisposableCorrectly"),
     System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design",
         "CA1001:TypesThatOwnDisposableFieldsShouldBeDisposable")]
    public class Manual : IDisposable
    {
        private Manual(string path)
        {
            output = File.CreateText(path);
        }

        /// <summary>
        /// Release resources associated with output stream.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA1816:CallGCSuppressFinalizeCorrectly"),
         System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1063:ImplementIDisposableCorrectly")]
        public void Dispose()
        {
            output.Dispose();
        }

        private readonly TextWriter output;

        private static readonly string[] ManualSections = {
                                                              "h2|Flow control|Prolog",
                                                              "h2|Meta-logical predicates|Prolog",
                                                              "h2|All solutions predicates|Prolog",
                                                              "h2|Arithmetic|Prolog",
                                                              "h2|Comparisons|Prolog",
                                                              "h2|List predicates|Prolog",
                                                              "h2|Term manipulation|Prolog",
                                                              "h2|Type predicates|Prolog",
                                                              "h2|Constraint programming|Prolog",
                                                              "h2|.NET interoperation|Prolog",
                                                              "h2|Declarations|Prolog",
                                                              "h2|Definite clause grammars|Prolog",
                                                              "h2|Loading code|Prolog",
                                                              "h2|Database manipulation|Prolog",
                                                              "h2|eremic logic|Prolog",
                                                              "h2|Other Predicates|Prolog"
                                                          };

        private static readonly Type[] ClassHierarchyRoots = {
                                                               
                                                             };

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance",
            "CA1810:InitializeReferenceTypeStaticFieldsInline")]
        static Manual()
        {
            foreach (var s in ManualSections)
                SectionTable[s.Split('|')[1].ToLower()] = new List<Delegate>();
        }

        internal static Dictionary<string, List<Delegate>> SectionTable = new Dictionary<string, List<Delegate>>();

        internal static void AddToSection(string name, Delegate d)
        {
            if (SectionTable.ContainsKey(name))
                SectionTable[name].Add(d);
            else
                throw new ArgumentException("Unknown manual section: " + name);
        }

        internal static void AddToSections(string sections, Delegate d)
        {
            string[] sectionNames = sections.Split(',');
            foreach (var name in sectionNames)
                AddToSection(name, d);
        }

        /// <summary>
        /// Returns the text of the manual
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return output.ToString();
        }

        private delegate void Thunk();

        /// <summary>
        /// Output the manual entry for the specified procedure or macro.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1804:RemoveUnusedLocals",
            MessageId = "ignore"),
         System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming",
             "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "d")]
        public void WriteEntry(Delegate d, string language)
        {
            Tag("dt", () => Span("dullCode", delegate
                                                 {
                                                     if (language == "Prolog")
                                                     {
                                                         Span("procedureName", Htmlify(DelegateUtils.NamedProcedureTable[d]));
                                                         var args = d.Arglist();
                                                         bool hasArg = false;
#pragma warning disable 414, 168, 219
                                                         // ReSharper disable UnusedVariable
                                                         foreach (var ignore in args)
                                                         // ReSharper restore UnusedVariable
#pragma warning restore 414, 168, 219
                                                         {
                                                             hasArg = true;
                                                             break;
                                                         }
                                                         if (hasArg)
                                                             WriteArglist(d.Arglist(), true, true);
                                                     }
                                                     else
                                                     {
                                                         OpenParen();
                                                         Span("procedureName", Htmlify(DelegateUtils.NamedProcedureTable[d]));
                                                         WriteArglist(d.Arglist(), false, false);
                                                     }
                                                 }));
            Tag("dd", Htmlify(d.Documentation()));
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1304:SpecifyCultureInfo",
            MessageId = "System.String.ToLower"),
         System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void WriteLanguageSection(string sectionInfo, string language)
        {
            string[] info = sectionInfo.Split('|');
            string sectionName = info[1];
            string sectionType = info[0];
            string lang = info[2];
            if (lang == language)
            {
                List<Delegate> entries = SectionTable[sectionName.ToLower()];
                //entries.Sort((d1, d2) => Writer.namedProcedureTable[d1].CompareTo(Writer.namedProcedureTable[d2]));
                Section(sectionType, sectionName);

                Tag("dl", delegate
                              {
                                  foreach (var d in entries)
                                      WriteEntry(d, language);
                              });
            }
        }

        /// <summary>
        /// Build the actual manual text
        /// </summary>
        private void WriteBody(Thunk tocWriter, Thunk sectionWriter)
        {
#if ClassDocumentation
            BuildSubclassTable();
#endif
            tocWriter();
            sectionWriter();
            //WriteLispTOC();
            //WriteClassHierarchyTOC();
            //WriteLispSections();
            //WriteClassHierarchySections();
        }

        private void WriteClassHierarchySections()
        {
#if ClassDocumentation
            //output.Write("<a name=\"Class hierarchy\"></a>");
            foreach (Type t in classHierarchyRoots)
                WriteClassHierarchy(t);
            DocumentRemainingClasses();
#endif
        }

        private void WriteAll(string title, Thunk tocWriter, Thunk sectionWriter)
        {
            output.Write(Header, title);
            Section("h1", title);
            WriteBody(tocWriter, sectionWriter);
            output.Write(Footer);
        }

        private void Close()
        {
            output.Close();
        }

        /// <summary>
        /// Writes the Lisp manual to the specified path.
        /// </summary>
        private static void WriteLanguageManualToFile(string language, string title, string path)
        {
            using (var m = new Manual(path))
            {
// ReSharper disable AccessToDisposedClosure
                m.WriteAll(title, () => m.WriteLanguageTOC(language), () => m.WriteLanguageSections(language));
// ReSharper restore AccessToDisposedClosure
                m.Close();
            }
        }

        /// <summary>
        /// Writes the Class manual to the specified path.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage",
            "CA2202:Do not dispose objects multiple times")]
        private static void WriteClassManualToFile(string path)
        {
            using (var m = new Manual(path))
            {
                m.WriteAll("Twig Class Reference Manual", m.WriteClassHierarchyTOC, m.WriteClassHierarchySections);
                m.Close();
            }
        }

        /// <summary>
        /// Writes all manual files to the specified directory
        /// </summary>
        /// <param name="directory"></param>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA1806:DoNotIgnoreMethodResults",
            MessageId = "Twig.MinimaLisp.KnowledgeBase")]
        public static void WriteManual(string directory)
        {
            // ReSharper disable once ObjectCreationAsStatement
            new KnowledgeBase("foo", null); // Force InstallPrimitives() to run.
            WriteLanguageManualToFile("Lisp", "Twig Lisp Reference", directory + "\\Twig Lisp Reference.htm");
            WriteLanguageManualToFile("GRL", "Signal Language Reference", directory + "\\Signal Language Reference.htm");
            WriteLanguageManualToFile("Prolog", "Twig Prolog Reference", directory + "\\Twig Prolog Reference.htm");
            WriteClassManualToFile(directory + "\\Twig Class Reference.htm");
        }


        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        // ReSharper disable once InconsistentNaming
        private void WriteLanguageTOC(string language)
        {
            int currentLevel = 1;
            //Tag("h2", "Table of contents");
            foreach (var sectionInfo in ManualSections)
            {
                string[] info = sectionInfo.Split('|');
                string sectionName = info[1];
                string sectionType = info[0];
                string lang = info[2];
                if (lang == language)
                {
                    int sectionLevel = sectionType[1] - '0';

                    while (currentLevel != sectionLevel)
                    {
                        if (currentLevel < sectionLevel)
                        {
                            output.WriteLine("<ul>");
                            currentLevel++;
                        }
                        else
                        {
                            output.WriteLine("</ul>");
                            currentLevel--;
                        }
                    }

                    WriteTOCEntry(sectionName);
                }
            }
            while (currentLevel-- > 1)
                output.WriteLine("</ul>");
        }

        // ReSharper disable once InconsistentNaming
        private void WriteClassHierarchyTOC()
        {
            //WriteTOCEntry("Class hierarchy");
            Tag("ul", delegate
                          {
                              foreach (var t in ClassHierarchyRoots)
                                  WriteTOCEntry(t.Name + " hierarchy");
                              WriteTOCEntry("Other classes");
                          });
            output.Write("</ul>");
        }

        // ReSharper disable once InconsistentNaming
        private void WriteTOCEntry(string sectionName)
        {
            Tag("li",
                () => Tag("a", string.Format("href=\"#{0}\"", sectionName), () => output.Write(sectionName)));
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void WriteLanguageSections(string language)
        {
// ReSharper disable UnusedVariable
            foreach (var s in ManualSections)
// ReSharper restore UnusedVariable
            {
                WriteLanguageSection(s, language);
                output.WriteLine();
            }
        }


        //private readonly Dictionary<Type, List<Type>> subclasses = new Dictionary<Type, List<Type>>();
        //private readonly Dictionary<Type, bool> areClassMembersDocumented = new Dictionary<Type, bool>();
        //private readonly Dictionary<Type, bool> documentationAlreadyPrinted = new Dictionary<Type, bool>();

        //[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        //private void AddSubclass(Type parent, Type subclass)
        //{
        //    if (parent == null)
        //        return;
        //    if (!subclasses.ContainsKey(parent))
        //        subclasses[parent] = new List<Type>();
        //    subclasses[parent].Add(subclass);
        //}

#if ClassDocumentation
        private static readonly Assembly twigAssembly = Assembly.GetAssembly(typeof (PhysicalObject));

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void BuildSubclassTable()
        {
            foreach (Type t in twigAssembly.GetTypes())
                if (t.GetDocumentation() != null)
                {
                    if (!t.IsAbstract)
                        // Force generation of class documentation if it isn't abstract, since we at least want to show the constructors.
                        areClassMembersDocumented[t] = true;
                    else
                    {
                        foreach (
                            var member in
                                t.GetMembers(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance |
                                             BindingFlags.Static))
                        {
                            if (member.GetDocumentation() != null)
                            {
                                areClassMembersDocumented[t] = true;
                                break;
                            }
                        }
                    }
                    AddSubclass(t.BaseType, t);
                }
        }

        private bool MembersAreDocumented(Type t)
        {
            bool v;
            if (areClassMembersDocumented.TryGetValue(t, out v))
                return v;
            return false;
        }

        private static string ClassBookmark(Type t)
        {
            return "Class " + t.Name;
        }

        private string ClassReference(Type t)
        {
            if (MembersAreDocumented(t))
            {
                return string.Format("<a href=\"#{0}\">{1}</a>", ClassBookmark(t), TypeName(t));
            }
            if (t.Namespace != null && t.Namespace.StartsWith("Microsoft.Xna"))
                return MSDNReference(t);
            return TypeName(t);
        }

        private static string MSDNReference(Type t)
        {
            System.Diagnostics.Debug.Assert(t.Namespace != null, "t.Namespace != null");
            return string.Format("<a href=\"http://msdn.microsoft.com/en-us/library/{0}.{1}.aspx\">{2}</a>",
                                 t.Namespace.ToLower(), t.Name.ToLower(), t.Name);
        }

        private string TypeName(Type t)
        {
            if (t == typeof (float))
                return "float";
            if (t == typeof (double))
                return "double";
            if (t == typeof (int))
                return "int";
            if (t == typeof (long))
                return "long";
            if (t == typeof (bool))
                return "bool";
            if (t == typeof (string))
                return "string";
            if (t == typeof (object))
                return "object";
            if (t == typeof (void))
                return "void";
            if (t.IsGenericType)
            {
                StringBuilder b = new StringBuilder();
                string[] names = t.GetGenericTypeDefinition().Name.Split('`');
                b.Append(names[0]);
                b.Append("&lt;");
                bool first = true;
                foreach (var a in t.GetGenericArguments())
                {
                    if (!first)
                        b.Append(",");
                    first = false;
                    b.Append(TypeName(a));
                }
                b.Append("&gt;");
                return b.ToString();
            }
            return t.Name;
        }

        private void WriteSubhierarchy(Type t)
        {
            Tag("li", delegate
                          {
                              Span("dullCode", ClassReference(t));
                              string documentation = t.GetDocumentation();
                              if (documentation != null)
                              {
                                  output.Write("<br>");
                                  output.Write(documentation);
                              }
                          });
            if (subclasses.ContainsKey(t))
            {
                Tag("ul", delegate
                              {
                                  foreach (Type c in subclasses[t])
                                      WriteSubhierarchy(c);
                              });
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void WriteClassHierarchy(Type t)
        {
            Section("h1", t.Name + " hierarchy", t.Name + " hierarchy");
            Tag("ul", () => WriteSubhierarchy(t));
            DocumentSubhierarchy(t);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void DocumentSubhierarchy(Type t)
        {
            if (MembersAreDocumented(t))
                DocumentClass(t);
            if (subclasses.ContainsKey(t))
            {
                foreach (Type c in subclasses[t])
                    DocumentSubhierarchy(c);
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode"),
         System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")
        ]
        private void DocumentClass(Type t)
        {
            if (documentationAlreadyPrinted.ContainsKey(t))
                return;
            documentationAlreadyPrinted[t] = true;
            Section("h2", t.Name + " class", ClassBookmark(t));
            Span("argument", delegate
                                 {
                                     if (t.IsEnum)
                                     {
                                         output.Write("Enumerated type");
                                     }
                                     else if (t.IsValueType)
                                     {
                                         output.Write("Value type");
                                     }
                                     else
                                     {
                                         output.Write("Superclasses: ");
                                         Type p = t.BaseType;
                                         output.Write(ClassReference(p));
                                         System.Diagnostics.Debug.Assert(p != null, "p != null");
                                         p = p.BaseType;
                                         while (p != null && p != typeof (object))
                                         {
                                             output.Write(", ");
                                             output.Write(ClassReference(p));
                                             p = p.BaseType;
                                         }
                                         List<Type> subs;
                                         if (subclasses.TryGetValue(t, out subs) && subs.Count > 0)
                                         {
                                             output.Write("<br>Subclasses: ");
                                             for (int i = 0; i < subs.Count; i++)
                                             {
                                                 if (i > 0)
                                                     output.Write(", ");
                                                 output.Write(ClassReference(subs[i]));
                                             }
                                         }
                                     }
                                 });

            string doc = t.GetDocumentation();
            if (doc != null)
                Tag("p", doc);
            if (t.IsSubclassOf(typeof (PhysicalObject)))
            {
                DocumentMembers(
                    t.GetConstructors(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance |
                                      BindingFlags.Static),
                    "Constructors", !t.IsAbstract,
                    delegate(ConstructorInfo c)
                        {
                            ParameterInfo[] p = c.GetParameters();
                            if (p.Length >= 2 && p[1].ParameterType == typeof (string))
                            {
                                Span("dullCode", "(define-twig-object ");
                                Span("argument", "name");
                                Span("dullCode", " " + t.Name + ((p.Length > 2) ? " " : ""));
                                Span("argument",
                                     delegate
                                         {
                                             for (int i = 2; i < p.Length; i++)
                                             {
                                                 output.Write(" &nbsp;&nbsp;");
                                                 output.Write(p[i].Name);
                                                 output.Write(":");
                                                 output.Write(ClassReference(p[i].ParameterType));
                                             }
                                         });
                            }
                            else
                            {
                                Span("dullCode", "(new-component ");
                                Span("dullCode", t.Name + ((p.Length > 1) ? " " : ""));
                                Span("argument",
                                     delegate
                                         {
                                             for (int i = 1; i < p.Length; i++)
                                             {
                                                 output.Write(" &nbsp;&nbsp;");
                                                 output.Write(p[i].Name);
                                                 output.Write(":");
                                                 output.Write(ClassReference(p[i].ParameterType));
                                             }
                                         });
                            }
                            Span("dullCode", ")");
                        });
            }
            else if (t.IsSubclassOf(typeof (TwigGameComponent)))
            {
                DocumentMembers(
                    t.GetConstructors(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance |
                                      BindingFlags.Static),
                    "Constructors", !t.IsAbstract,
                    delegate(ConstructorInfo c)
                        {
                            ParameterInfo[] p = c.GetParameters();
                            Span("dullCode", "(new-component " + t.Name + ((p.Length > 1) ? " " : ""));
                            Span("argument",
                                 delegate
                                     {
                                         for (int i = 1; i < p.Length; i++)
                                         {
                                             output.Write(" &nbsp;&nbsp;");
                                             output.Write(p[i].Name);
                                             output.Write(":");
                                             output.Write(ClassReference(p[i].ParameterType));
                                         }
                                     });
                            Span("dullCode", ")");
                        });
            }
            else if (!t.IsEnum)
            {
                DocumentMembers(
                    t.GetConstructors(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance |
                                      BindingFlags.Static),
                    "Constructors", !t.IsAbstract,
                    delegate(ConstructorInfo c)
                        {
                            Span("dullCode",
                                 delegate
                                     {
                                         output.Write("(new ");
                                         output.Write(t.Name);
                                     });
                            Span("argument",
                                 delegate
                                     {
                                         ParameterInfo[] p = c.GetParameters();
                                         foreach (ParameterInfo t1 in p)
                                         {
                                             output.Write(" &nbsp;&nbsp;");
                                             output.Write(t1.Name);
                                             output.Write(":");
                                             output.Write(ClassReference(t1.ParameterType));
                                         }
                                     });
                            Span("dullCode", ")");
                        });
            }
            DocumentMembers(
                t.GetFields(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance |
                            BindingFlags.Static),
                t.IsEnum ? "Values" : "Fields", t.IsEnum,
                delegate(FieldInfo f)
                    {
                        if (f.IsStatic)
                            Span("dullCode", t.Name + "." + f.Name);
                        else
                            Span("dullCode", f.Name);
                        if (!t.IsEnum)
                            Span("argument", " : " + ClassReference(f.FieldType));
                    });
            DocumentMembers(
                t.GetProperties(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance |
                                BindingFlags.Static),
                "Properties", t.IsEnum,
                delegate(PropertyInfo p)
                    {
                        if (p.GetAccessors()[0].IsStatic)
                            Span("dullCode", t.Name + "." + p.Name);
                        else
                            Span("dullCode", p.Name);
                        Span("argument", " : " + ClassReference(p.PropertyType));
                    });
            DocumentMembers(
                t.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance |
                             BindingFlags.Static),
                "Methods", false,
                delegate(MethodInfo m)
                    {
                        Span("dullCode", "(");
                        if (m.IsStatic)
                            Span("dullCode", t.Name);
                        else
                            Span("argument", t.Name.ToLower());
                        Span("dullCode", "." + m.Name);
                        Span("argument",
                             delegate
                                 {
                                     ParameterInfo[] p = m.GetParameters();
                                     foreach (ParameterInfo t1 in p)
                                     {
                                         output.Write(" &nbsp;&nbsp;");
                                         output.Write(t1.Name);
                                         output.Write(":");
                                         output.Write(ClassReference(t1.ParameterType));
                                     }
                                 });
                        Span("dullCode", ")");
                        output.Write(" : ");
                        Span("argument", ClassReference(m.ReturnType));
                    });
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void DocumentRemainingClasses()
        {
            List<Type> remainingClasses = new List<Type>();
            foreach (var pair in areClassMembersDocumented)
                if (!documentationAlreadyPrinted.ContainsKey(pair.Key))
                    remainingClasses.Add(pair.Key);
// ReSharper disable StringCompareIsCultureSpecific.1
            remainingClasses.Sort((t1, t2) => string.Compare(t1.Name, t2.Name));
// ReSharper restore StringCompareIsCultureSpecific.1
            Section("h1", "Other classes");
            foreach (var c in remainingClasses)
                DocumentClass(c);
        }

        private delegate void MemberFormatter<in T>(T member);

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode"),
         System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1307:SpecifyStringComparison",
             MessageId = "System.String.Compare(System.String,System.String)")]
        private void DocumentMembers<T>(T[] members, string heading, bool forceDocumentation, MemberFormatter<T> format)
            where T : MemberInfo
        {
            bool foundOne = false;
            foreach (var m in members)
                if (forceDocumentation || m.GetDocumentation() != null)
                {
                    foundOne = true;
                    break;
                }

            if (foundOne)
            {
                Section("h3", heading, null);
                List<T> mems = new List<T>(members);
// ReSharper disable StringCompareIsCultureSpecific.1
                mems.Sort((m1, m2) => (string.Compare(m1.Name, m2.Name)));
// ReSharper restore StringCompareIsCultureSpecific.1
                Tag("dl", delegate
                              {
                                  foreach (var m in mems)
                                  {
                                      string doc = m.GetDocumentation();
                                      if (m.Name != "value__" && (forceDocumentation || doc != null))
                                      {
// ReSharper disable AccessToModifiedClosure
                                          Tag("dt", () => format(m));
// ReSharper restore AccessToModifiedClosure
                                          if (doc != null)
                                              Tag("dd", doc);
                                      }
                                  }
                              });
            }
        }
#endif

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void Section(string sectionType, string sectionName)
        {
            Section(sectionType, sectionName, sectionName);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        private void Section(string sectionType, string sectionName, string bookmarkName)
        {
            if (bookmarkName != null)
                Tag(sectionType, () => Tag("a", string.Format("name=\"{0}\"", bookmarkName), sectionName));
            else
                Tag(sectionType, sectionName);
        }


        private void Span(string style, Thunk p)
        {
            Tag("span", string.Format("class=\"{0}\"", style), p);
        }

        private void Span(string style, string s)
        {
            Tag("span", string.Format("class=\"{0}\"", style), () => output.Write(s));
        }

        private void Tag(string name, Thunk p)
        {
            output.Write("<{0}>", name);
            p();
            output.Write("</{0}>", name);
        }

        private void Tag(string name, string content)
        {
            Tag(name, () => output.Write(content));
        }

        private void Tag(string name, string args, string content)
        {
            Tag(name, args, () => output.Write(content));
        }

        private void Tag(string name, string args, Thunk p)
        {
            output.Write("<{0} {1}>", name, args);
            p();
            output.Write("</{0}>", name);
        }

        private void Space(bool needComma)
        {
            output.Write(needComma ? ",&nbsp;" : "&nbsp;");
        }

        private void WriteArglist(IEnumerable<object> args, bool printOpenParen, bool printComma)
        {
            bool needSpace = !printOpenParen;
            if (printOpenParen)
                OpenParen();
            foreach (object arg in args)
            {
                if (needSpace)
                    Space(printComma);
                needSpace = true;
                var s = arg as Symbol;
                var str = arg as string;
                if (s != null)
                    str = s.Name;
                var sublist = arg as IEnumerable<object>;
                if (str != null)
                    Span("argument", Htmlify(str));
                else if (sublist != null)
                    WriteArglist(sublist, true, false);
                else
                    throw new ArgumentException("Improper data type in argument list");
            }
            CloseParen();
        }

        private static string Htmlify(string str)
        {
            if (str == "...")
                return "&hellip;";
            return str.Replace(">", "&gt;").Replace("<", "&lt;");
        }

        private void OpenParen()
        {
            //Span("dullCode", "(");
            output.Write("(");
        }

        private void CloseParen()
        {
            //Span("dullCode", ")");
            output.Write(")");
        }

        #region Header and footer

        private const string Header =
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n<meta content=\"en-us\" http-equiv=\"Content-Language\" />\n<meta content=\"text/html; charset=utf-8\" http-equiv=\"Content-Type\" />\n<title>{0}</title>\n<style type=\"text/css\">\n.dullCode {{\nfont-family: \"Courier New\", Courier, monospace;\n}}\n.procedureName {{\nfont-family: \"Courier New\", Courier, monospace;\nfont-weight:bold;\n}}\n.argument {{\nfont-family: \"Times New Roman\", Times, serif;\nfont-style:italic;\n}}\n</style>\n</head>\n<body>\n";

        private const string Footer = "</body>\n</html>\n";

        #endregion

        #region Emacs autocomplete file generator
        /// <summary>
        /// Writes the Emacs AutoCompleteMode config files
        /// </summary>
        public static void WriteEmacsAutoCompleteFile()
        {
            using (var m = new Manual("documentation.el"))
            {
                m.WriteAutoComplete("Lisp");
                m.WriteAutoComplete("Prolog");
            }
        }

        void WriteAutoComplete(string language)
        {
            string tableName = "twig-" + language.ToLower() + "-auto-complete-table";
            output.WriteLine("(defvar {0} (make-hash-table :test 'equal))", tableName);
            foreach (var sections in ManualSections)
            {
                string[] info = sections.Split('|');
                string sectionName = info[1];
                string lang = info[2];
                if (lang == language)
                {
                    foreach (Delegate proc in SectionTable[sectionName.ToLower()])
                    {
                        string procedureName = DelegateUtils.NamedProcedureTable[proc];
                        output.WriteLine(
                            "(puthash \"{0}\" '((procedure . \"{1}\") (arguments . ({2})) (docstring . \"{3}\")) {4})",
                            procedureName, procedureName,
                            FormatArgumentsForEmacs(proc.Arglist()),
                            proc.Documentation(),
                            tableName);
                    }
                }
            }
        }

        private string FormatArgumentsForEmacs(IEnumerable<object> arglist)
        {
            var s = new StringBuilder();
            foreach (var arg in arglist)
            {
                s.AppendFormat("\"{0}\" ", arg);
            }
            return s.ToString();
        }
        #endregion
    }
}