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
using System.Diagnostics;
//using java.lang;
#if USE_IKVM
using JavaClass = java.lang.Class;
using Type = System.Type;
#else
using JClass = System.Type;
using Type = System.Type;
#endif

namespace Swicli.Library
{
    public partial class PrologCLR
    {


        public static void cliStartDbg()
        {
             cliStartDbg(false, 5005);
        }
        public static void cliStartDbg(bool server, int port)
        {
            //ikvm.debugger.Debugger.MainStart(server, port);
			///System.Object[] paramz = new System.Object[]{server, port};
			Type.GetType("ikvm.debugger.Debugger").GetMethod("MainStart").Invoke(null,  new System.Object[]{server, port});
        }
        public static void cliStartJmx()
        {
            cliStartJmx(9999);
         //   ;
        }
        public static void cliStartJmx(int port9999)
        {

#if USE_IKVM
            if (port9999 <= 0)
            {
                try
                {
                    java.lang.Integer.parseInt(java.lang.System.getProperty("com.sun.management.jmxremote.port"));
                }
                catch (java.lang.NumberFormatException nfe)
                {
                    port9999 = 9999;
                }

            }

            java.lang.System.setProperty("com.sun.management.jmxremote", "true");
            java.lang.System.setProperty("com.sun.management.jmxremote.authenticate", "false");
            java.lang.System.setProperty("com.sun.management.jmxremote.ssl", "false");
            java.lang.System.setProperty("com.sun.management.jmxremote.port", "" + port9999);

			//sun.management.Agent.startAgent().
            Type.GetType("sun.management.Agent").GetMethod("startAgent").Invoke(null,  new System.Object[0]);

        }
#endif
    }
}


   

