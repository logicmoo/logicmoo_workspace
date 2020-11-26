
INTRO
-----

Thanks for using gSOAP!

SOAP Web Services is an emerging technology. SOAP enables cross-platform
development of networked applications. As can be expected by an emerging
technology, adoptation takes time and some SOAP implementations are
still in an infancy stage. This can lead to some frustration when trying
to adopt SOAP technology quickly. We hope that gSOAP relieves some of this
frustration.

The software is provided "as is", without any warranty. However, gSOAP
has received a lot of support from users and has been extensively tested
in the real world. We also continue to improve gSOAP and add new features.

DISCLAIMER:
WE TRY OUR BEST TO PROVIDE YOU WITH "REAL-WORLD" EXAMPLES BUT WE CANNOT
GUARANTEE THAT ALL CLIENT EXAMPLES CAN CONNECT TO THIRD PARTY WEB SERVICES
WHEN THESE SERVICES ARE DOWN OR HAVE BEEN REMOVED.

DOCS
----

See INSTALL.txt for installation instructions.

See soapdoc2.html or soapdoc2.pdf for the gSOAP documentation.

See license.pdf for the gSOAP public license (which is based on MPL1.1).

LIBS
----

Win32 build of clients and services requires winsock.dll.

To do this in Visual C++ 6.0, go to "Project", "settings", select the "Link"
tab (the project file needs to be selected in the file view) and add
"wsock32.lib" to the "Object/library modules" entry.

The Win32 distribution contains two MSVC++ project examples.
The custom build in VC++ 6.0 has been configured to invoke the gSOAP compiler
automatically. The VC++ projects can be converted to MSVC++ 7.0.

