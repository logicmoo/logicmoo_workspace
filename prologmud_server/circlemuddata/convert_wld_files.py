"""
Apologies in advanced for this hacked-together throw-away code.

This script goes through Circle MUD's data files and converts the data to an XML file of my own schema design.

Al Sweigart
al@inventwithpython.com

Released into public domain. Do whatever with it.
"""

import os, sys
from xml.sax.saxutils import escape

rootdir = sys.argv[1]

print('<rooms>')
for wldFile in os.listdir(rootdir):
    if not wldFile.endswith('.wld'):
        continue

    with open(os.path.join(rootdir, wldFile)) as fp:
        content = fp.readlines()

    content =  [line.strip() for line in content]

    readState = 'vNum'
    descArg = []
    lineNum = 0

    while lineNum < len(content):
        line = content[lineNum]

        if readState == 'vNum':
            if line == '$':
                break # reached end of file
            vNumArg = line[1:]
            nameArg = content[lineNum+1][:-1]
            readState = 'desc'
            lineNum += 2
        elif readState == 'desc':
            doneLineNum = lineNum
            while content[doneLineNum] != '~':
                doneLineNum += 1
            descArg = '\n'.join(content[lineNum:doneLineNum])
            lineNum = doneLineNum + 1
            readState = 'bitVector'
        elif readState == 'bitVector':
            zoneArg, bitVectorArg, sectorTypeArg = line.split()
            sectorTypeArg = {'0': 'inside',
                             '1': 'city',
                             '2': 'field',
                             '3': 'forest',
                             '4': 'hills',
                             '5': 'mountain',
                             '6': 'water_swim',
                             '7': 'water_noswim',
                             '8': 'underwater',
                             '9': 'flying'}[sectorTypeArg]
            lineNum += 1
            readState = 'exitAndDesc'
        elif readState == 'exitAndDesc':
            extraDescsArg = []
            exitsArg = []
            while content[lineNum] != 'S':
                if content[lineNum] == 'E':
                    doneLineNum = lineNum + 1
                    while content[doneLineNum] != '~':
                        doneLineNum += 1
                    extraDescsArg.append({'keywords': content[lineNum+1][:-1],
                                          'desc': '\n'.join(content[lineNum+2:doneLineNum])})
                    lineNum = doneLineNum + 1
                elif content[lineNum].startswith('D'):
                    exitDirection = {'0':'north',
                                 '1':'east',
                                 '2':'south',
                                 '3':'west',
                                 '4':'up',
                                 '5':'down'}[content[lineNum][1:2]]
                    doneLineNum = lineNum + 1
                    while content[doneLineNum] != '~':
                        doneLineNum += 1
                    exitDesc = '\n'.join(content[lineNum+1:doneLineNum])
                    exitKeywords = content[doneLineNum + 1][:-1].split()
                    exitDoorFlag, exitKeyNumber, exitRoomLinked = content[doneLineNum+2].split()
                    exitDoorFlag = {'0': 'nodoor',
                                    '1': 'normal',
                                    '2': 'pickproof'}[exitDoorFlag]
                    exitsArg.append({'direction': exitDirection,
                                     'desc': exitDesc,
                                     'keywords': exitKeywords,
                                     'type': exitDoorFlag,
                                     'keynum': exitKeyNumber,
                                     'roomlinked': exitRoomLinked})
                    lineNum = doneLineNum + 3

            # process this room
            attribs = []
            if 'a' in bitVectorArg: attribs.append('dark')
            if 'b' in bitVectorArg: attribs.append('death')
            if 'c' in bitVectorArg: attribs.append('nomob')
            if 'd' in bitVectorArg: attribs.append('indoors')
            if 'e' in bitVectorArg: attribs.append('peaceful')
            if 'f' in bitVectorArg: attribs.append('soundproof')
            if 'g' in bitVectorArg: attribs.append('notrack')
            if 'h' in bitVectorArg: attribs.append('nomagic')
            if 'i' in bitVectorArg: attribs.append('tunnel')
            if 'j' in bitVectorArg: attribs.append('private')
            if 'k' in bitVectorArg: attribs.append('godroom')
            if 'l' in bitVectorArg: attribs.append('house')
            if 'm' in bitVectorArg: attribs.append('house_crash')
            if 'n' in bitVectorArg: attribs.append('atrium')
            if 'o' in bitVectorArg: attribs.append('olc')
            if 'p' in bitVectorArg: attribs.append('bfs_mark')

            print('<room vnum="%s" name="%s" type="%s" zone="%s" %s>' % (vNumArg, nameArg, sectorTypeArg, zoneArg, ' '.join(['%s="true"' % (att) for att in attribs])))
            if descArg:
                print('  <desc>%s</desc>' % (escape(descArg)))
            for exitArg in exitsArg:
                cap = (exitArg['keywords'] or exitArg['desc']) and '>' or ' />'

                print('  <exit direction="%s" type="%s" keynum="%s" roomlinked="%s"%s' % (exitArg['direction'],
                                                                                          exitArg['type'],
                                                                                          exitArg['keynum'],
                                                                                          exitArg['roomlinked'],
                                                                                          cap))

                if exitArg['keywords']:
                    for k in exitArg['keywords']:
                        print('    <exitkeyword keyword="%s" />' % (k))
                if exitArg['desc']:
                    print('    <exitdesc>%s</exitdesc>' % (escape(exitArg['desc'])))
                if exitArg['keywords'] or exitArg['desc']:
                    print('  </exit>')

            for extraDescArg in extraDescsArg:
                if extraDescArg['keywords']:
                    cap = ''
                else:
                    cap = '</extradesc>'
                print('  <extradesc>%s%s' % (escape(extraDescArg['desc']), cap))
                if extraDescArg['keywords']:
                    for k in exitArg['keywords']:
                        print('    <extradesckeyword keyword="%s" />' % (k))
                    print('  </extradesc>')
            print('</room>\n\n')
            descArg = []
            readState = 'vNum'
            lineNum += 1
print('</rooms>')