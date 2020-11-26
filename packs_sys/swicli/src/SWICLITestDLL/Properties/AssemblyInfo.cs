/*********************************************************
* 
*  Author:        Uwe Lesta
*  Copyright (C): 2008, Uwe Lesta SBS-Softwaresysteme GmbH
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

using System;           // CLSCompliant
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security.Permissions;

// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//[assembly: AssemblyTitle("SWI-Pl-cs2 - A .NET interface to SWI-Prolog ©")]
[assembly: AssemblyTitle("SWICLITestDLL - A .NET test interface to SWI-Prolog ©")]
[assembly: AssemblyDescription(".NET test interface to SWI-Prolog ©")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("SBS-Softwaresysteme written by Uwe Lesta/Douglas Miles")]
//[assembly: AssemblyProduct("SWI-Pl-cs2")]
[assembly: AssemblyProduct("SWICLITestDLL")]
[assembly: AssemblyCopyright("Copyright © 2010 under LGPL 2.1")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

// Setting ComVisible to false makes the types in this assembly not visible 
// to COM components.  If you need to access atom type in this assembly from 
// COM, set the ComVisible attribute to true on that type.
[assembly: ComVisible(false)]

// The following GUID is for the ID of the typelib if this project is exposed to COM
[assembly: Guid("5782cb18-d94e-4fba-9f25-410bf962b666")]

// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version 
//      Build Number
//      Revision
//
// You can specify all the values or you can default the Revision and Build Numbers 
// by using the '*' as shown below:
//[assembly: AssemblyVersion("1.1.5990.0")]
//[assembly: AssemblyFileVersion("1.1.5990.0")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

// Not internal tests
//[assembly: InternalsVisibleTo("NUnitTest")]


// to test internal methods
// see: "Using InternalsVisibleTo Attribute with Strong Named Assemblies"
// http://weblogs.asp.net/bhouse/archive/2006/04/17/443100.aspx

// To use the InternalsVisibleTo Attribute:
// 1.  Run sn.exe -p MyStrongNameKey.snk MyStrongNameKey.PublicKey
// This will extract to public key to a file with the .PublicKey extension.  
// (I hate using the .pub extension because it is seen as a Microsoft Publisher file…)
// 2.  run sn.exe -tp MyStrongNameKey.PublicKey
// This will display your public key for you.  Copy this key. 




//[assembly: InternalsVisibleTo("TestSwiPl, PublicKey=002400000480000094000000060200000024000052534131000400000100010083dc9537d3928376e3970266c16e2c02df12b991a1325c17f8ae92ac945a5a7922e2b3d2ad24eef3ee6f9b39a0523f72281cd3ab5f69c81e2903910e751d644d4c759f4b501e664dc0de71c0836c75710feba8997827c5058d8616fa1e1d21f26521275686cd0f161e7cf6300599129dd84eb90b9d2147d1de87551761c342bb")]

[assembly: CLSCompliant(true)]

[assembly:SecurityPermission(SecurityAction.RequestMinimum, UnmanagedCode = true)]