/*  $Id$
*  
*  Project: Swicli.Library - Two Way Interface for .NET and MONO to SWI-Prolog
*  Author:        Douglas R. Miles
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.com
*  Copyright (C):  2010-2012 LogicMOO Developement
*
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*********************************************************/

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Xml.Serialization;
using SbsSW.SwiPlCs;
using PlTerm = SbsSW.SwiPlCs.PlTerm;
using XElement = System.Xml.XmlNode;
using XDocument = System.Xml.XmlDocument;

namespace Swicli.Library
{
    public partial class PrologCLR
    {

        /// <summary>
        /// Retreves the ClazzSpec Fields and Properties into a MemberSpecList
        /// </summary>
        /// <param name="clazzSpec"></param>
        /// <param name="memberSpecs"></param>
        /// <returns></returns>
        [PrologVisible]
        static public bool cliPropsForType(PlTerm clazzSpec, PlTerm memberSpecList)
        {
            Type type = GetType(clazzSpec);
            var props = GetPropsForTypes(type);
            var value = props.Key.ToArray();
            int len = value.Length;
            var termv = ATOM_NIL;
            for (int i = len - 1; i >= 0; i--)
            {
                termv = PlC(".", ToProlog((value[i].Name)), termv);
            }
            var value2 = props.Value.ToArray();
            len = value2.Length;
            for (int i = len - 1; i >= 0; i--)
            {
                termv = PlC(".", ToProlog((value2[i].Name)), termv);
            }
            return memberSpecList.Unify(termv);
        }

        static readonly Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>> PropForTypes = new Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>>();

        private static KeyValuePair<List<PropertyInfo>, List<FieldInfo>> GetPropsForTypes(Type t)
        {
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> kv;

            if (PropForTypes.TryGetValue(t, out kv)) return kv;

            lock (PropForTypes)
            {
                if (!PropForTypes.TryGetValue(t, out kv))
                {
                    kv = new KeyValuePair<List<PropertyInfo>, List<FieldInfo>>(new List<PropertyInfo>(),
                                                                               new List<FieldInfo>());
                    var ta = t.GetCustomAttributes(typeof(XmlTypeAttribute), false);
                    bool specialXMLType = false;
                    if (ta != null && ta.Length > 0)
                    {
                        XmlTypeAttribute xta = (XmlTypeAttribute)ta[0];
                        specialXMLType = true;
                    }
                    HashSet<string> lowerProps = new HashSet<string>();
                    BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
                    foreach (
                        PropertyInfo o in t.GetProperties(flags))
                    {
                        if (o.CanRead)
                        {

                            if (o.Name.StartsWith("_")) continue;
                            if (o.DeclaringType == typeof(Object)) continue;
                            if (!lowerProps.Add(o.Name.ToLower())) continue;
                            if (o.GetIndexParameters().Length > 0)
                            {
                                continue;
                            }
                            if (specialXMLType)
                            {
                                var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                                if (use == null || use.Length < 1) continue;
                            }
                            kv.Key.Add(o);

                        }
                    }
                    foreach (FieldInfo o in t.GetFields(flags))
                    {
                        if (o.Name.StartsWith("_")) continue;
                        if (o.DeclaringType == typeof(Object)) continue;
                        if (!lowerProps.Add(o.Name.ToLower())) continue;
                        if (specialXMLType)
                        {
                            var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                            if (use == null || use.Length < 1) continue;
                        }
                        kv.Value.Add(o);
                    }
                }
                return kv;
            }
        }

        /// <summary>
        /// Retreves the clazzOrInstance Members into MemberSpec List
        /// 
        /// used by cli_memb/2
        /// </summary>
        /// <param name="clazzOrInstance"></param>
        /// <param name="membersSpecListOut"></param>
        /// <returns></returns>
        [PrologVisible]
        static public bool cliMembers(PlTerm clazzOrInstance, PlTerm membersSpecListOut)
        {
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            MemberInfo[] members = c.GetMembers(BindingFlagsALL);
            List<PlTerm> list = new List<PlTerm>();
            string cname = c.Name;
            List<MemberInfo> exclude = new List<MemberInfo>();
            int ordinal = 0;
            foreach (var info in c.GetFields(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetProperties(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetMethods(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetConstructors(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetEvents(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }

            foreach (MemberInfo info in members)
            {
                break;
                try
                {
                    if (exclude.Contains(info)) continue;
                }
                catch (Exception e)
                {
                    Embedded.Debug("Warn exclude.Contains {0}: {1}", info, e);
                    continue;
                }
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }

            return membersSpecListOut.Unify(ToPlArray(list.ToArray()));
        }

        private static void AddMemberToList(MemberInfo info, List<PlTerm> list, string cname, int ordinal)
        {

            PlTerm memb = MemberTerm(info, cname, ordinal);
            if (memb.TermRef != 0) list.Add(memb);
        }

        private static PlTerm MemberTerm(MemberInfo info, string cname, int ordinal)
        {
            string mn = info.Name;
            switch (info.MemberType)
            {
                case MemberTypes.Constructor:
                    {
                        var fi = (ConstructorInfo)info;
                        var mi = fi;
                        return PlC("c", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   ToPlListParams(fi.GetParameters()),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)),
                                   ToProlog(info));
                    }
                    break;
                case MemberTypes.Event:
                    {
                        var fi = (EventInfo)info;
                        MethodInfo mi = (fi.GetRaiseMethod() ??
                                         (fi.EventHandlerType != null ? fi.EventHandlerType.GetMethod("Invoke") : null) ??
                                         fi.GetAddMethod() ?? fi.GetRemoveMethod());
                        ParameterInfo[] parme = GetParmeters(fi);
                        return PlC("e", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.EventHandlerType),
                                   ToPlListParams(parme),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)),
                                   ToProlog(info));
                    }
                    break;
                case MemberTypes.Field:
                    {
                        var fi = (FieldInfo)info;
                        var mi = fi;
                        return PlC("f", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.FieldType),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)),
                                   ToProlog(info));
                    }
                    break;
                case MemberTypes.Method:
                    {
                        var fi = (MethodInfo)info;
                        var mi = fi;
                        return PlC("m", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.ReturnParameter.ParameterType),
                                   ToPlListParams(fi.GetParameters()),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)),
                                   ToProlog(info));
                    }
                    break;
                case MemberTypes.Property:
                    {
                        var fi = (PropertyInfo)info;
                        MethodInfo mi = (fi.CanRead ? fi.GetGetMethod(true) : fi.GetSetMethod(true));
                        return PlC("p", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.PropertyType),
                                   ToPlListParams(fi.GetIndexParameters()),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),
                                   AFlag(fi.CanRead, "CanRead"),
                                   AFlag(fi.CanWrite, "CanWrite"),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)),
                                   ToProlog(info));
                    }
                    break;
                case MemberTypes.TypeInfo:
                    break;
                case MemberTypes.Custom:
                    break;
                case MemberTypes.NestedType:
                    break;
                case MemberTypes.All:
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            return default(PlTerm);
        }

        public static PlTerm PlC(string decl, params PlTerm[] plTerms)
        {
            return PlTerm.PlCompound(decl, plTerms);
        }
        public static PlTerm PlC(string decl, PlTermV termV)
        {
            return PlTerm.PlCompound(decl, termV);
        }

        private static PlTerm AFlag(bool tf, string name)
        {
            PlTerm plTermPlAtom = PlTerm.PlAtom(tf ? "true" : "false");
            return PlC(name, plTermPlAtom);
        }
        private static PlTerm AFlag(bool tf)
        {
            PlTerm plTermPlAtom = PlTerm.PlAtom(tf ? "true" : "false");
            return plTermPlAtom;
        }

        static public Dictionary<Assembly, XElement> AssmblyXDoics = new Dictionary<Assembly, XElement>();

        public static XElement GetXmlDocMembers(Assembly typeAssembly)
        {
            XElement ele;
            lock (AssmblyXDoics)
                if (!AssmblyXDoics.TryGetValue(typeAssembly, out ele))
                {
                    try
                    {
                        AssmblyXDoics[typeAssembly] = ele = GetXmlDocMembers0(typeAssembly);
                    }
                    catch (Exception e)
                    {
                        Embedded.Debug("Cannot doc {0} {1}", typeAssembly, e);
                        AssmblyXDoics[typeAssembly] = ele = null;
                    }
                }
            return ele;
        }

        public static XElement GetXmlDocMembers0(Assembly typeAssembly)
        {
            var file = GetXmlDocFile(typeAssembly);
            if (file == null) return null;
            XDocument f = new XDocument();
            f.Load(file.FullName);
            if (f.FirstChild == null) return null;
            foreach (XElement mn in f.ChildNodes)
            {
                if (mn.Name.ToLower() == "members") return mn;
                foreach (XElement mn0 in mn.ChildNodes)
                {
                    if (mn0.Name.ToLower() == "members") return mn0;
                }
            }
            return null;
        }

        public static XElement GetXmlDocMembers(Type type)
        {
            return GetXmlDocMembers(type.Assembly);
        }


        private static FileInfo GetXmlDocFile(Assembly assembly)
        {
            string assemblyDirPath = Path.GetDirectoryName(assembly.Location);
            string fileName = String.Format("{0}.xml", Path.GetFileNameWithoutExtension(assembly.Location).ToLower());
            foreach (string file in Directory.GetFiles(assemblyDirPath))
            {
                if (Path.GetFileName(file).ToLower().Equals(fileName))
                {
                    return new FileInfo(file);
                }
            }
            return null;
        }

        static string GetMemberId(MemberInfo member)
        {
            char memberKindPrefix = GetMemberPrefix(member);
            string memberName = GetMemberFullName(member);
            return memberKindPrefix + ":" + memberName;
        }

        static char GetMemberPrefix(MemberInfo member)
        {
            return member.GetType().Name
              .Replace("Runtime", "")[0];
        }

        static string GetMemberFullName(MemberInfo member)
        {
            string memberScope = "";
            if (member.DeclaringType != null)
                memberScope = GetMemberFullName(member.DeclaringType);
            else if (member is Type)
                memberScope = ((Type)member).Namespace;

            memberScope = memberScope + "." + member.Name;
            if (member is MethodBase)
            {
                memberScope += "(";
                var mi = member as MethodBase;
                var ps = mi.GetParameters();
                bool needsComma = false;
                foreach (ParameterInfo info in ps)
                {
                    if (needsComma)
                    {
                        memberScope += ",";
                    }
                    else
                    {
                        needsComma = true;
                    }
                    memberScope += "" + info.ParameterType;
                }
                memberScope += ")";
            }
            return memberScope;
        }

        /// <summary>
        /// =Attempt to find assembly doc of member
        /// </summary>
        /// <param name="memb"></param>
        /// <param name="doc"></param>
        /// <param name="xml"></param>
        /// <returns></returns>
        [PrologVisible]
        static public bool cliMemberDoc(PlTerm membIn, PlTerm docOut, PlTerm xmlOut)
        {
            var mi = GetInstance(membIn) as MemberInfo;
            if (mi != null)
            {
                XElement xmls = GetDocString(mi);
                return xmlOut.Unify(ToProlog(xmls)) && docOut.Unify(PlTerm.PlString(xmls == null ? "" : xmls.InnerXml));
            }
            return true;
        }

        static public XElement GetDocString(MemberInfo memberInfo)
        {
            var docMembers = GetXmlDocMembers(memberInfo.DeclaringType.Assembly);
            return GetDocString(docMembers, memberInfo);
        }

        public static XElement GetDocString(XElement docMembers, MemberInfo info)
        {
            if (docMembers == null) return null;
            string memberId = GetMemberId(info).ToLower();
            foreach (XElement e in docMembers.ChildNodes)
            {
                var anme = e.Attributes["name"];
                if (anme != null)
                {
                    string matchWith = anme.Value.ToLower();
                    if (matchWith.StartsWith(memberId))
                    {
                        return e;
                    }
                }
            }
            return null;
        }

    }


    public class CycTypeInfo
    {
        readonly public PlTerm cycFort;
        readonly Type CType;
        public static Dictionary<Type, CycTypeInfo> typeFort = new Dictionary<Type, CycTypeInfo>();

        public bool IsEnum
        {
            get { return CType.IsEnum; }
        }

        public bool IsBitFlags
        {
            get; set;
        }

        public CycTypeInfo(PlTerm fort, Type type)
        {
            cycFort = fort;
            CType = type;
            typeFort[type] = this;
            if (type.IsEnum)
            {
                SetupEnum();
            }
            else if (type.IsInterface)
            {
                SetupType("Interface");
            }
            else if (type.IsClass)
            {
                SetupType("Class");
            }
        }

        private void SetupEnum()
        {
            
          /*  SimCyclifier simCyclifier = SimCyclifier.Master;
            var docMembers = SimCyclifier.GetXmlDocMembers(CType);
            simCyclifier.assertIsa(cycFort, C("Collection"));
            simCyclifier.assertIsa(cycFort, C("SimEnumCollection"));
            String ele = SimCyclifier.GetDocString(docMembers, CType);
            simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, "The sim enum for " + CType);
            if (!String.IsNullOrEmpty(ele))
            {
                simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, ele);
            }*/
            if (CType.IsEnum)
            {
                IsBitFlags = IsFlagType(CType);
                if (IsBitFlags)
                {
                 //   simCyclifier.assertIsa(cycFort, C("SimEnumBitFlagsCollection"));
                }
                foreach (FieldInfo fort in CType.GetFields(BindingFlags.Public | BindingFlags.Static))
                {
//                    Enum ev = (Enum) fort.GetValue(null);
  //                  var tc= ev.GetTypeCode();
                    string v = string.Format("{0}-{1}", CType.Name, fort.Name);
                    PlTerm cv = C(v);
                   /* simCyclifier.assertIsa(cv, C("Collection"));
                    simCyclifier.assertVocabGafNow(C("genls"), cv, cycFort);
                    simCyclifier.assertVocabGaf(CycAccess.comment, cv, "The sim enum value for: " + fort);
                    MemberInfo mi = fort;
                    simCyclifier.DocQueue.Enqueue(() =>
                                                      {
                                                          ele = SimCyclifier.GetDocString(docMembers, mi);
                                                          if (!String.IsNullOrEmpty(ele))
                                                          {
                                                              simCyclifier.assertVocabGaf(CycAccess.comment, cv, ele);
                                                          }
                                                      });*/
                }
            }
        }

        public static bool IsFlagType(Type type)
        {
            object[] attributes = type.GetCustomAttributes(typeof (FlagsAttribute), true);
            if (attributes != null && attributes.Length > 0)
            {
                return true;
            }
            /* if (type == typeof(OpenMetaverse.PrimFlags))
             {
                 return true;   
             }*/
            return false;
        }

        private void SetupType(String s)
        {
           // SimCyclifier simCyclifier = SimCyclifier.Master;
            //var docMembers = SimCyclifier.GetXmlDocMembers(CType);
            //simCyclifier.assertIsa(cycFort, C("Collection"));
            //simCyclifier.assertIsa(cycFort, C("Sim" + s + "Collection"));
            //String ele = SimCyclifier.GetDocString(docMembers, CType);
            //simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, "The sim " + s + " for " + CType);
            //if (!String.IsNullOrEmpty(ele))
            //{
              //  simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, ele);
            //}
        }

        private PlTerm C(string collection)
        {
            return PrologCLR.C(collection);
        }
    }
}