update changeLog.txt in SwiPlCs and SwiPlCs-Documentation/Download

Versioning:
- change the version number in 
	- AssemblyInfo.cs
	- SwiCsPl.shfb
	- rename SwiPlCs-Documentation/Download/SwiPlCs_1.1.plv.pl.zip
	- add a new line in the table in SwiPlCs-Documentation/Download/index.htm
	- change version in SwiPlCs-Documentation/build_bin_package.bat
	
build and Test:
- run FxCop with SWI-cs.FxCop
- run all tests in solution
- run project HelloWorldDemo
- run Sandcastle help file builder with SWI-Pl-cs2\Help\SwiCsPl.shfb
- run SwiPlCs-Documentation/build_bin_package.bat
- [include the new file(s) into the project]
	- set properties 'build action' auf content

Publish:
- include the new zip file (SwiPlCs-Documentation\Download) into the project
- check and include new documentation html files into the project (SwiPlCs-Documentation\Generated\html)
- publish project SwiPlCs-Documentation
- publish Download/SwiPlCs_x.x.x.zip to http://gollem.swi.psy.uva.nl/twiki/pl/bin/view/Foreign/CSharpInterface 
- and to http://www.swi-prolog.org/contrib/CSharp.html

VSS Checkin