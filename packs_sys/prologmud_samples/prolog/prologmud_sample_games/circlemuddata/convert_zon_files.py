"""
Apologies in advanced for this hacked-together throw-away code.

This script goes through Circle MUD's data files and converts the data to an XML file of my own schema design.

Al Sweigart
al@inventwithpython.com

Released into public domain. Do whatever with it.
"""

import os, re, sys
from xml.sax.saxutils import escape

extendedMobPat = re.compile('(.*?):(.*)')


allmobs = []
alldoors = []
allobjects = []
allremove = []

print('<zones>')
for zonFile in os.listdir(sys.argv[1]):
    try:
        if not zonFile.endswith('.zon'):# or zonFile != '120.zon':
            continue
        #print(zonFile)
        #import pdb; pdb.set_trace()
        with open(os.path.join(sys.argv[1], zonFile)) as fp:
            content = fp.readlines()

        content =  [line.strip() for line in content] # skip the first "CircleMUD v3.0 Shop File~" line

        lineNum = 0

        #import pdb; pdb.set_trace()
        vnumArg = content[lineNum][1:]; lineNum += 1
        while content[lineNum].startswith('*'): lineNum += 1
        zonenameArg = content[lineNum][:-1]; lineNum += 1
        while content[lineNum].startswith('*'): lineNum += 1
        startroomArg, endroomArg, lifespanArg, resetArg = content[lineNum].split(); lineNum += 1



        while lineNum < len(content): # read in commands
            line = content[lineNum]
            if line.startswith('*'):
                lineNum += 1
                continue

            if line == 'S':
                break # reached end of file

            line = content[lineNum].split()

            command = line[0]

            if command == 'M':
                #print('m %s' % lineNum)
                # add a mob
                # NOTE - I'm ignoring the if-flag for mobs
                allmobs.append( {'vnum': line[2], 'max': line[3], 'room': line[4], 'inv':[], 'equip':{}} )
            elif command == 'G':
                #print('g %s' % lineNum)
                allmobs[-1]['inv'].append({'vnum':line[2], 'max': line[3] })
            elif command == 'E':
                #print('e %s' % lineNum)
                allmobs[-1]['equip'][line[4]] = {'vnum': line[2], 'max': line[3]}
            elif command == 'O':
                #print('o %s' % lineNum)
                allobjects.append( {'vnum': line[2], 'max': line[3], 'room': line[4], 'contains':[]} )
            elif command == 'P':
                #print('p %s' % lineNum)
                for o in allobjects:
                    if o['vnum'] == line[2]:
                        o['contains'].append({'vnum': line[4], 'max':line[3]})
            elif command == 'D':
                #print('d %s' % lineNum)
                alldoors.append( {'room': line[2], 'exit': line[3], 'state': line[4]} )
            elif command == 'R':
                #print('r %s' % lineNum)
                allremove.append( {'room': line[2], 'vnum': line[3]} )
            lineNum += 1



            """
            <zone vnum="" name="" startroom="" endroom="" lifespan="" resetmode="">
                <mobs>
                    <mob vnum="" globalmax="" room="">
                        <equipped obj="" globalmax="" wornon="" />
                        <inventory obj="" globalmax="" />
                    </mob>
                </mobs>
                <objects>
                    <object vnum="" globalmax="" room="" />
                    <contains>
                        <object vnum="" globalmax="" />
                    </contains>
                </objects>
                <doors>
                    <door room="" exit="" state="open/closed/locked" />
                </doors>
                <cleanup>
                    <obj vnum="" room="" />
                </cleanup>
            </zone>

            """

        exitMap = {'0': 'north',
                   '1': 'east',
                   '2': 'south',
                   '3': 'west',
                   '4': 'up',
                   '5': 'down'}
        doorstateMap = {'0': 'open', '1':'closed', '2':'locked'}
        wornMap = {'0': 'light',
                   '1': 'rightfinger',
                   '2': 'leftfinger',
                   '3': 'neck1',
                   '4': 'neck2',
                   '5': 'body',
                   '6': 'head',
                   '7': 'legs',
                   '8': 'feet',
                   '9': 'hands',
                   '10': 'arms',
                   '11': 'shield',
                   '12': 'aboutbody',
                   '13': 'waist',
                   '14': 'rightwrist',
                   '15': 'leftwrist',
                   '16': 'wield',
                   '17': 'held'}
        resetMap = {'0': 'never', '1': 'afterdeserted', '2': 'asap'}

        print('<zone vnum="%s" name="%s" startroom="%s" endroom="%s" lifespan="%s" resetmode="%s">' % (vnumArg, escape(zonenameArg), startroomArg, endroomArg, lifespanArg, resetMap[resetArg]))
        if allmobs:
            print('  <mobs>')
            for m in allmobs:
                innertag = (not m['equip'] and not m['inv']) and ' /' or ''
                print('    <mob vnum="%s" globalmax="%s" room="%s"%s>' % (m['vnum'], m['max'], m['room'], innertag))
                for i in m['inv']:
                    print('      <inventory obj="%s" globalmax="%s" />' % (i['vnum'], i['max']))
                for k, v in m['equip'].items():
                    print('      <equipped obj="%s" globalmax="%s" wornon="%s" />' % (v['vnum'], v['max'], wornMap[k]))
                if not innertag:
                    print('    </mob>')
            print('  </mobs>')
        if allobjects:
            print('  <objects>')
            for o in allobjects:
                innertag = (not o['contains']) and ' /' or ''
                print('    <object vnum="%s" globalmax="%s" room="%s"/>' % (o['vnum'], o['max'], o['room']))

                if o['contains']:
                    print('    <contains>')
                    for c in o['contains']:
                        print('        <object vnum="%s" globalmax="%s" />' % (c['vnum'], c['max']))
                    print('    </contains>')
            print('  </objects>')
        if alldoors:
            print('  <doors>')
            for d in alldoors:
                print('    <door room="%s" exit="%s" state="%s" />' % (d['room'], exitMap[d['exit']], doorstateMap[d['state']]))
            print('  </doors>')
        print('</zone>')
    except:
        print('LINENUM=%s LINE: %s' % (lineNum, content[lineNum]))
        raise

print('</zones>')