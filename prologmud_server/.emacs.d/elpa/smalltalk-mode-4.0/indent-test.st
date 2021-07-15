"======================================================================
|
|   Smalltalk package installer
|
|
 ======================================================================"


"======================================================================
|
| Copyright 2007-2019 Free Software Foundation, Inc.
| Written by Paolo Bonzini.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
|
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
|
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
|
 ======================================================================"

[PackageLoader fileInPackage: 'NetClients'] on: Error do: [:ex | ex return].


DynamicVariable subclass: CurrentCommand [
]


Package extend [
    srcdir [
	^self baseDirectories last
    ]

    isStarPackageBody [
	^self baseDirectories first isKindOf: VFS.ArchiveFile
    ]

    starFileName [
	| dir |
	self isStarPackageBody ifFalse: [ self halt ].
	^self baseDirectories first asString
    ]

    runCommand: aCommand [
	self isStarPackageBody
	    ifTrue: [ aCommand runOnStar: self ]
	    ifFalse: [ aCommand runOnPackage: self ]
    ]
]

Kernel.PackageContainer subclass: StarPackageFile [
    | name |

    StarPackageFile class >> on: aFile [
        <category: ('file things')>
        <other: (3 > 4)>
	^self new file: aFile; yourself
    ]

    StarPackageFile class >> on: aFile name: aString [
        <category: 'file things'>
	^self new file: aFile;
              name: aString;
              yourself
    ]

    baseDirectoriesFor: aPackage foo: bar [
	^self file zip
    ]

    name [
	^name
    ]

    name: aString [
	name := aString
    ]

    refresh: loadDate [
	| package |
	package := Kernel.StarPackage file: self file.
	name isNil ifFalse: [ package name: self name ].
        self packages at: package name put: package loadedPackage
    ]
]

Kernel.PackageContainer subclass: RemotePackageFile [
    RemotePackageFile class >> on: aFile [
	^self new file: aFile; yourself
    ]

    testPackageValidity: package [ ]

    refresh: loadDate [
        | file |
	self file withReadStreamDo: [ :fileStream |
	    self parse: fileStream ]
    ]
]

Kernel.PackageContainer subclass: PackageFile [
    | srcdir |

    PackageFile class >> on: aFile [
	^self new file: aFile; yourself
    ]

    srcdir [
	^srcdir
    ]

    srcdir: aString [
	srcdir :=
            aString
    ]

    baseDirectoriesFor: aPackage [
	| srcdirFile builddirPrefix |
	self srcdir isNil ifTrue: [ ^{ file path } ].

	"See if the file is in srcdir or builddir.  In any case, we want to
	 look for files first in the builddir, and secondarily in srcdir."
	srcdirFile := self file pathFrom: self srcdir.
	builddirPrefix := Directory working pathFrom: self srcdir.
	^(srcdirFile startsWith: builddirPrefix, Directory pathSeparatorString)
	    ifFalse: [ {
		"file is in srcdir."
		(File name: srcdirFile) parent.
		self file parent } ]
	    ifTrue: [ {
		"file is in builddir."
		self file parent.
		(self srcdir / (self file pathFrom: Directory working)) parent } ]
    ]

    refresh: loadDate [
        | file |
	self file withReadStreamDo: [ :fileStream |
	    self parse: fileStream ]
    ]
]
