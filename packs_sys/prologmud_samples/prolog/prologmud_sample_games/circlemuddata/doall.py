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

print('<mobs>')
for mobFile in os.listdir(sys.argv[1]):
    if not mobFile.endswith('.mob'): # or mobFile != '33.mob':
        continue

    with open(os.path.join(sys.argv[1], mobFile)) as fp:
        content = fp.readlines()

    content =  [line.strip() for line in content]

    readState = 'vNum'
    lineNum = 0

    while lineNum < len(content):
        line = content[lineNum]

        if readState == 'vNum':
            if line == '$':
                break # reached end of file
            vNumArg = line[1:]
            aliasArg = content[lineNum+1][:-1].split()
            shortDescArg = content[lineNum+2][:-1]
            readState = 'longdesc'
            lineNum += 3
        elif readState == 'longdesc':
            doneLineNum = lineNum
            while content[doneLineNum] != '~':
                doneLineNum += 1
            longDescArg = '\n'.join(content[lineNum:doneLineNum])
            lineNum = doneLineNum + 1
            readState = 'detaileddesc'
        elif readState == 'detaileddesc':
            doneLineNum = lineNum
            while content[doneLineNum] != '~':
                doneLineNum += 1
            detailedDescArg = '\n'.join(content[lineNum:doneLineNum])
            lineNum = doneLineNum + 1
            readState = 'bitVector'

        elif readState == 'bitVector':
            actionBitVectorArg, affectBitVectorArg, alignmentArg, typeArg = content[lineNum].split()

            lineNum += 1
            readState = 'level'
        elif readState == 'level':
            levelArg, thacoArg, acArg, maxhpArg, bareHandDmgArg = content[lineNum].split()
            goldArg, xpArg = content[lineNum + 1].split()
            loadArg, defaultPosArg, sexArg = content[lineNum + 2].split()
            lineNum += 3

            extendedMobArg = {}
            if not content[lineNum].startswith('#'):
                # this is the extended mob format.

                while content[lineNum] not in ('E', '$'):
                    mo = extendedMobPat.match(content[lineNum])
                    extendedMobArg[mo.group(1).strip()] = mo.group(2).strip()
                    lineNum += 1
                if content[lineNum] != '$':
                    lineNum += 1



            # process this mob
            actionAttribs = []
            if 'a' in actionBitVectorArg: actionAttribs.append('special')
            if 'b' in actionBitVectorArg: actionAttribs.append('sentinel')
            if 'c' in actionBitVectorArg: actionAttribs.append('scavenger')
            if 'd' in actionBitVectorArg: actionAttribs.append('isnpc')
            if 'e' in actionBitVectorArg: actionAttribs.append('aware')
            if 'f' in actionBitVectorArg: actionAttribs.append('aggressive')
            if 'g' in actionBitVectorArg: actionAttribs.append('stayzone')
            if 'h' in actionBitVectorArg: actionAttribs.append('wimpy')
            if 'i' in actionBitVectorArg: actionAttribs.append('aggrevil')
            if 'j' in actionBitVectorArg: actionAttribs.append('aggrgood')
            if 'k' in actionBitVectorArg: actionAttribs.append('aggrneutral')
            if 'l' in actionBitVectorArg: actionAttribs.append('memory')
            if 'm' in actionBitVectorArg: actionAttribs.append('helper')
            if 'n' in actionBitVectorArg: actionAttribs.append('nocharm')
            if 'o' in actionBitVectorArg: actionAttribs.append('nosummon')
            if 'p' in actionBitVectorArg: actionAttribs.append('nosleep')
            if 'q' in actionBitVectorArg: actionAttribs.append('nobash')
            if 'r' in actionBitVectorArg: actionAttribs.append('noblind')

            affectAttribs = []
            if 'a' in affectBitVectorArg: affectAttribs.append('blind')
            if 'b' in affectBitVectorArg: affectAttribs.append('invisible')
            if 'c' in affectBitVectorArg: affectAttribs.append('detectalign')
            if 'd' in affectBitVectorArg: affectAttribs.append('detectinvis')
            if 'e' in affectBitVectorArg: affectAttribs.append('detectmagic')
            if 'f' in affectBitVectorArg: affectAttribs.append('senselife')
            if 'g' in affectBitVectorArg: affectAttribs.append('waterwalk')
            if 'h' in affectBitVectorArg: affectAttribs.append('sanctuary')
            if 'i' in affectBitVectorArg: affectAttribs.append('group')
            if 'j' in affectBitVectorArg: affectAttribs.append('curse')
            if 'k' in affectBitVectorArg: affectAttribs.append('infravision')
            if 'l' in affectBitVectorArg: affectAttribs.append('poison')
            if 'm' in affectBitVectorArg: affectAttribs.append('protectevil')
            if 'n' in affectBitVectorArg: affectAttribs.append('protectgood')
            if 'o' in affectBitVectorArg: affectAttribs.append('sleep')
            if 'p' in affectBitVectorArg: affectAttribs.append('notrack')
            if 's' in affectBitVectorArg: affectAttribs.append('sneak')
            if 't' in affectBitVectorArg: affectAttribs.append('hide')
            if 'v' in affectBitVectorArg: affectAttribs.append('charm')

            """
            <mob vnum="" alignment="" type="" level="" thac0="" ac="" maxhp="" barehanddmg="" gold="" xp="" loadposition="" defaultposition="" sex="">
                <alias name="" />
                <shortdesc>short description</shortdesc>
                <longdesc>long description</longdesc>
                <detaileddesc>detailed description</detaileddesc>
                <action sentinel="true" ... />
                <affection blind="true" ... />
                <extended barehandattack="" str="" ... />
            </mob>
            """

            pos = {'0': 'dead',
                       '1': 'mortallywounded',
                       '2': 'incapacitated',
                       '3': 'stunned',
                       '4': 'sleeping',
                       '5': 'resting',
                       '6': 'sitting',
                       '7': 'fighting',
                       '8': 'standing'}
            loadArg = pos[loadArg]
            defaultPosArg = pos[defaultPosArg]

            sexArg = {'0': 'neutral',
                      '1': 'male',
                      '2': 'female'}[sexArg]


            print('<mob vnum="%s" alignment="%s" type="%s" level="%s" thac0="%s" ac="%s" maxhp="%s" barehanddmg="%s" gold="%s" xp="%s" loadposition="%s" defaultposition="%s" sex="%s">' % (vNumArg,
                alignmentArg, typeArg, levelArg, thacoArg, acArg, maxhpArg, bareHandDmgArg, goldArg, xpArg, loadArg, defaultPosArg, sexArg))

            for alias in aliasArg:
                print('  <alias name="%s" />' % escape(alias))

            if shortDescArg:
                print('  <shortdesc>%s</shortdesc>' % escape(shortDescArg))
            if longDescArg:
                print('  <longdesc>%s</longdesc>' % escape(longDescArg))
            if detailedDescArg:
                print('  <detaileddesc>%s</detaileddesc>' % escape(detailedDescArg))

            if actionAttribs:
                print('  <action %s />' % (' '.join(['%s="true"' % x for x in actionAttribs])))

            if affectAttribs:
                print('  <affection %s />' % (' '.join(['%s="true"' % x for x in affectAttribs])))

            if extendedMobArg:
                if 'BareHandAttack' in extendedMobArg:
                    extendedMobArg['BareHandAttack'] = {'0': 'hit/hits',
                                                        '1': 'sting/stings',
                                                        '2': 'whip/whips',
                                                        '3': 'slash/slashes',
                                                        '4': 'bite/bites',
                                                        '5': 'bludgeon/bludgeons',
                                                        '6': 'crush/crushes',
                                                        '7': 'pound/pounds',
                                                        '8': 'claw/claws',
                                                        '9': 'maul/mauls',
                                                        '10': 'thrash/thrashes',
                                                        '11': 'pierce/pierces',
                                                        '12': 'blast/blasts',
                                                        '13': 'punch/punches',
                                                        '14': 'stab/stabs'}[extendedMobArg['BareHandAttack']]
                print('  <extended %s />' % (' '.join(['%s="%s"' % (k.lower(), v) for k, v in extendedMobArg.items()])))

            print('</mob>\n\n')
            readState = 'vNum'
print('</mobs>')


extendedMobPat = re.compile('(.*?):(.*)')

print('<objects>')

for objFile in os.listdir(sys.argv[1]):
    if not objFile.endswith('.obj'):# or objFile != '120.obj':
        continue
    #import pdb; pdb.set_trace()
    with open(os.path.join(sys.argv[1], objFile)) as fp:
        content = fp.readlines()

    content =  [line.strip() for line in content]

    readState = 'vNum'
    lineNum = 0

    while lineNum < len(content):
        line = content[lineNum]

        if readState == 'vNum':
            if line == '$':
                break # reached end of file
            vNumArg = line[1:]
            aliasArg = content[lineNum+1][:-1].split()
            shortDescArg = content[lineNum+2][:-1]
            #if shortDescArg == 'a scroll of recall':
            #    pass#import pdb; pdb.set_trace()
            longDescArg = content[lineNum+3][:-1]
            actionDescArg = content[lineNum+4][:-1]
            readState = 'bitVector'
            lineNum += 5
        #elif readState == 'longdesc':
        #    doneLineNum = lineNum
        #    while content[doneLineNum] != '~':
        #        doneLineNum += 1
        #    longDescArg = '\n'.join(content[lineNum:doneLineNum])
        #    lineNum = doneLineNum + 1
        #    readState = 'actiondesc'
        #elif readState == 'actiondesc':
        #    doneLineNum = lineNum
        #    while content[doneLineNum] != '~':
        #        doneLineNum += 1
        #    actionDescArg = '\n'.join(content[lineNum:doneLineNum])
        #    lineNum = doneLineNum + 1
        #    readState = 'bitVector'

        elif readState == 'bitVector':
            typeFlagArg, effectsBitVectorArg, wearBitVectorArg = content[lineNum].split()
            value0Arg, value1Arg, value2Arg, value3Arg = content[lineNum+1].split()
            weightArg, costArg, rentArg = content[lineNum+2].split()
            lineNum += 3
            readState = 'extradesc'
        elif readState == 'extradesc':
            extendedArg = []
            affectArg = {}
            while not content[lineNum].startswith('#') and not content[lineNum].startswith('$'):
                # this is the extended mob format.
                if content[lineNum] == 'E':
                    lineNum += 1
                    #import pdb; pdb.set_trace()
                    doneLineNum = lineNum
                    while content[doneLineNum] != '~':
                        doneLineNum += 1
                    extendedArg.append( (content[lineNum][:-1].split(), '\n'.join(content[lineNum+1:doneLineNum])) ) # keywords list, desc string
                    lineNum = doneLineNum + 1

                elif content[lineNum] == 'A':
                    lineNum += 1
                    affectline = content[lineNum].split()
                    affectArg[affectline[0]] = affectline[1]
                    lineNum += 1


            # process this obj
            typeFlagArg = {'1': 'light',
                           '2': 'scroll',
                           '3': 'wand',
                           '4': 'staff',
                           '5': 'weapon',
                           '6': 'fireweapon',
                           '7': 'missile',
                           '8': 'treasure',
                           '9': 'armor',
                           '10': 'potion',
                           '11': 'worn',
                           '12': 'other',
                           '13': 'trash',
                           '14': 'trap',
                           '15': 'container',
                           '16': 'note',
                           '17': 'drinkcontainer',
                           '18': 'key',
                           '19': 'food',
                           '20': 'money',
                           '21': 'pen',
                           '22': 'boat',
                           '23': 'fountain'}[typeFlagArg]

            effectAttribs = []
            if 'a' in effectsBitVectorArg: effectAttribs.append('glow')
            if 'b' in effectsBitVectorArg: effectAttribs.append('hum')
            if 'c' in effectsBitVectorArg: effectAttribs.append('norent')
            if 'd' in effectsBitVectorArg: effectAttribs.append('nodonate')
            if 'e' in effectsBitVectorArg: effectAttribs.append('noinvis')
            if 'f' in effectsBitVectorArg: effectAttribs.append('invis')
            if 'g' in effectsBitVectorArg: effectAttribs.append('cantenchant')
            if 'h' in effectsBitVectorArg: effectAttribs.append('nodrop')
            if 'i' in effectsBitVectorArg: effectAttribs.append('bless')
            if 'j' in effectsBitVectorArg: effectAttribs.append('antigood')
            if 'k' in effectsBitVectorArg: effectAttribs.append('antievil')
            if 'l' in effectsBitVectorArg: effectAttribs.append('antineutral')
            if 'm' in effectsBitVectorArg: effectAttribs.append('antimagicuser')
            if 'n' in effectsBitVectorArg: effectAttribs.append('anticleric')
            if 'o' in effectsBitVectorArg: effectAttribs.append('antithief')
            if 'p' in effectsBitVectorArg: effectAttribs.append('antiwarrior')
            if 'q' in effectsBitVectorArg: effectAttribs.append('nosell')

            wearAttribs = []
            if wearBitVectorArg.isdigit():
                wearBitVectorArg = int(wearBitVectorArg)
                # on a side note, why did they use a bit vector for this? Can you wear a piece of armor on your feet and head?
                # It seems like they just needed the "take" bit to be set or not. I'm not sure why a piece of armor wouldn't be takeable,
                # or why they make "takeable" specific to armors instead of all items.
                if wearBitVectorArg >= 16384:
                    wearBitVectorArg -= 16384
                    wearAttribs.append('hold')
                if wearBitVectorArg >= 8192:
                    wearBitVectorArg -= 8192
                    wearAttribs.append('wield')
                if wearBitVectorArg >= 4096:
                    wearBitVectorArg -= 4096
                    wearAttribs.append('wrist')
                if wearBitVectorArg >= 2048:
                    wearBitVectorArg -= 2048
                    wearAttribs.append('waist')
                if wearBitVectorArg >= 1024:
                    wearBitVectorArg -= 1024
                    wearAttribs.append('about')
                if wearBitVectorArg >= 512:
                    wearBitVectorArg -= 512
                    wearAttribs.append('shield')
                if wearBitVectorArg >= 256:
                    wearBitVectorArg -= 256
                    wearAttribs.append('arms')
                if wearBitVectorArg >= 128:
                    wearBitVectorArg -= 128
                    wearAttribs.append('hands')
                if wearBitVectorArg >= 64:
                    wearBitVectorArg -= 64
                    wearAttribs.append('feet')
                if wearBitVectorArg >= 32:
                    wearBitVectorArg -= 32
                    wearAttribs.append('legs')
                if wearBitVectorArg >= 16:
                    wearBitVectorArg -= 16
                    wearAttribs.append('head')
                if wearBitVectorArg >= 8:
                    wearBitVectorArg -= 8
                    wearAttribs.append('body')
                if wearBitVectorArg >= 4:
                    wearBitVectorArg -= 4
                    wearAttribs.append('neck')
                if wearBitVectorArg >= 2:
                    wearBitVectorArg -= 2
                    wearAttribs.append('finger')
                if wearBitVectorArg < 1:
                    #wearBitVectorArg -= 1
                    wearAttribs.append('canttake')
            else:
                if 'a' in wearBitVectorArg: wearAttribs.append('takeable')
                if 'b' in wearBitVectorArg: wearAttribs.append('finger')
                if 'c' in wearBitVectorArg: wearAttribs.append('neck')
                if 'd' in wearBitVectorArg: wearAttribs.append('body')
                if 'e' in wearBitVectorArg: wearAttribs.append('head')
                if 'f' in wearBitVectorArg: wearAttribs.append('legs')
                if 'g' in wearBitVectorArg: wearAttribs.append('feet')
                if 'h' in wearBitVectorArg: wearAttribs.append('hands')
                if 'i' in wearBitVectorArg: wearAttribs.append('arms')
                if 'j' in wearBitVectorArg: wearAttribs.append('shield')
                if 'k' in wearBitVectorArg: wearAttribs.append('about')
                if 'l' in wearBitVectorArg: wearAttribs.append('waist')
                if 'm' in wearBitVectorArg: wearAttribs.append('wrist')
                if 'n' in wearBitVectorArg: wearAttribs.append('wield')
                if 'o' in wearBitVectorArg: wearAttribs.append('hold')

            """
            <object vnum="" type="" value0="" value1="" value2="" value3="" weight="" cost="" rent="">
                <alias name="" />
                <shortdesc>description</shortdesc>
                <longdesc>description</longdesc>
                <actiondesc>description</actiondesc>
                <effects glow="true" ... />
                <wear takeable="true" ... />
                <typespecific charge="42" ... />
                <extradesc><keyword="" />description</extradesc>
                <affect type="" value="" />
            </object>
            """

            valueDefs = {'light': [None, None, 'capacity', None],
                         'scroll': ['level', 'spell1', 'spell2', 'spell3'],
                         'wand': ['level', 'capacity', 'remaining', 'spell'],
                         'staff': ['level', 'capacity', 'remaining', 'spell'],
                         'weapon': [None, 'numdice', 'sizedice', 'damagetype'],
                         'fireweapon': [None, None, None, None],
                         'missile': [None, None, None, None],
                         'treasure': [None, None, None, None],
                         'armor': ['ac', None, None, None],
                         'potion': ['level', 'spell1', 'spell2', 'spell3'],
                         'worn': [None, None, None, None],
                         'other': [None, None, None, None],
                         'trash': [None, None, None, None],
                         'trap': [None, None, None, None],
                         'container': ['capacity', 'containertype', 'keynum', None],
                         'note': ['language', None, None, None],
                         'drinkcontainer': ['capacity', 'remaining', 'drinktype', 'ispoisoned'],
                         'key': [None, None, None, None],
                         'food': ['filling', None, None, 'ispoisoned'],
                         'money': ['amount', None, None, None],
                         'pen': [None, None, None, None],
                         'boat': [None, None, None, None],
                         'fountain': ['capacity', 'remaining', 'drinktype', 'ispoisoned']}
            drinkTypeMap = {'0': 'water',
                         '1': 'beer',
                         '2': 'wine',
                         '3': 'ale',
                         '4': 'darkale',
                         '5': 'whisky',
                         '6': 'lemonade',
                         '7': 'firebreath',
                         '8': 'localspecial',
                         '9': 'slime',
                         '10': 'milk',
                         '11': 'tea',
                         '12': 'coffee',
                         '13': 'blood',
                         '14': 'saltwater',
                         '15': 'clearwater'}
            damagetypeMap = {
                  '0':    'hit/hits',
                  '1':    'sting/stings',
                  '2':    'whip/whips',
                  '3':    'slash/slashes',
                  '4':    'bite/bites',
                  '5':    'bludgeon/bludgeons',
                  '6':    'crush/crushes',
                  '7':    'pound/pounds',
                  '8':    'claw/claws',
                  '9':    'maul/mauls',
                  '10':   'thrash/thrashes',
                  '11':   'pierce/pierces',
                  '12':   'blast/blasts',
                  '13':   'punch/punches',
                  '14':   'stab/stabs'}

            affectTypes = {
                '0': 'none',
                '1': 'strength',
                '2': 'dexterity',
                '3': 'intelligence',
                '4': 'wisdom',
                '5': 'constitution',
                '6': 'charisma',
                '7': 'class',
                '8': 'level',
                '9': 'age',
                '10': 'charweight',
                '11': 'charheight',
                '12': 'mana',
                '13': 'hit',
                '14': 'move',
                '15': 'gold',
                '16': 'experience',
                '17': 'ac',
                '18': 'hitroll',
                '19': 'damageroll',
                '20': 'saveparalysis',
                '21': 'saverods',
                '22': 'savepetrification',
                '23': 'savebreath',
                '24': 'savespell'}

            spellNumbers = {'28': 'heal', '29': 'invisible', '26': 'fireball', '32': 'magic missile', '24': 'enchant weapon', '25': 'energy drain', '23': 'earthquake', '27': 'harm', '20': 'detect magic', '21': 'detect poison', '22': 'dispel evil', '49': 'group recall', '46': 'dispel good', '47': 'group armor', '44': 'sense life', '45': 'animate dead', '42': 'word of recall', '43': 'remove poison', '40': 'summon', '41': 'ventriloquate', '1': 'armor', '3': 'bless', '2': 'teleport', '5': 'burning hands', '4': 'blindness', '7': 'charm', '6': 'call lightning', '9': 'clone', '8': 'chill touch', '201': 'identify', '39': 'strength', '12': 'create food', '11': 'control weather', '10': 'color spray', '13': 'create water', '38': 'sleep', '15': 'cure critic', '14': 'cure blind', '17': 'curse', '16': 'cure light', '19': 'detect invis', '18': 'detect align', '31': 'locate object', '30': 'lightning bolt', '51': 'waterwalk', '36': 'sanctuary', '35': 'remove curse', '34': 'prot from evil', '33': 'poison', '37': 'shocking grasp', '48': 'group heal', '50': 'infravision'}

            #print('<object vnum="%s" type="%s" value0="%s" value1="%s" value2="%s" value3="%s" weight="%s" cost="%s" rent="%s">' % (vNumArg, typeFlagArg,
            #    value0Arg, value1Arg, value2Arg, value3Arg, weightArg, costArg, rentArg))
            print('<object vnum="%s" type="%s" weight="%s" cost="%s" rent="%s">' % (vNumArg, typeFlagArg,
                weightArg, costArg, rentArg))

            if shortDescArg:
                print('  <shortdesc>%s</shortdesc>' % escape(shortDescArg))
            if longDescArg:
                print('  <longdesc>%s</longdesc>' % escape(longDescArg))
            if actionDescArg:
                print('  <actiondesc>%s</actiondesc>' % escape(actionDescArg))


            if effectAttribs:
                print('  <effects %s />' % (' '.join(['%s="true"' % x for x in effectAttribs])))

            if wearAttribs:
                print('  <wear %s />' % (' '.join(['%s="true"' % x for x in wearAttribs])))

            if valueDefs[typeFlagArg] != [None, None, None, None]:
                #import pdb; pdb.set_trace()
                typespecificArg = {}
                for i in range(4):
                    key = valueDefs[typeFlagArg][i]
                    value = (value0Arg, value1Arg, value2Arg, value3Arg)[i]
                    if key is not None:
                        if key == 'damagetype':
                            value = damagetypeMap[value]
                        elif key == 'containertype':
                            value = int(value)
                            if value >= 8:
                                typespecificArg['locked'] = 'true'
                                value -= 8
                            if value >= 4:
                                typespecificArg['closed'] = 'true'
                                value -= 4
                            if value >= 2:
                                typespecificArg['pickproof'] = 'true'
                                value -= 2
                            if value >= 1:
                                typespecificArg['closeable'] = 'true'
                                value -= 1
                            key = None
                        elif key == 'keynum' and (value == '0' or value == '-1'):
                            key = None
                        elif key == 'ispoisoned':
                            if value == '0':
                                key = None # don't put in "ispoisoned" attribute if not poisoned.
                            else:
                                value = 'true'
                        elif key in ('spell1', 'spell2', 'spell3', 'spell'):
                            if value == '-1':
                                key = None
                            else:
                                value = spellNumbers[value]
                        elif key == 'drinktype':
                            value = drinkTypeMap[value]

                        if key is not None:
                            typespecificArg[key] = value


                print('  <typespecific %s />' % ' '.join(['%s="%s"' % (k,v) for k, v in typespecificArg.items()]))





            for arg in extendedArg:
                print('  <extradesc>')
                for keyword in arg[0]:
                    print('    <keyword>%s</keyword>' % keyword)
                print('    %s</extradesc>' % escape(arg[1]))


            for k, v in affectArg.items():
                print('  <affect type="%s" value="%s" />' % (affectTypes[k], v))

            print('</object>\n\n')
            readState = 'vNum'
print('</objects>')

extendedMobPat = re.compile('(.*?):(.*)')

print('<shops>')

for shpFile in os.listdir(sys.argv[1]):
    if not shpFile.endswith('.shp'):# or shpFile != '120.shp':
        continue
    #import pdb; pdb.set_trace()
    with open(os.path.join(sys.argv[1], shpFile)) as fp:
        content = fp.readlines()

    content =  [line.strip() for line in content][1:] # skip the first "CircleMUD v3.0 Shop File~" line

    lineNum = 0

    while lineNum < len(content):
        line = content[lineNum]

        if line == '$~':
            break # reached end of file
        vNumArg = line[1:-1]
        lineNum += 1

        forSaleVNumArg = []
        # read in the items for sale
        while content[lineNum] != '-1':
            forSaleVNumArg.append(content[lineNum])
            lineNum += 1
        lineNum += 1 # skip "-1" line


        profitWhenBuyingArg = content[lineNum]
        lineNum += 1
        profitWhenSellingArg = content[lineNum]
        lineNum += 1

        buyTypeArg = []
        while content[lineNum] != '-1':
            if content[lineNum] == 'LIQ CONTAINER':
                buyTypeArg.append('drinkcontainer')
            else:
                buyTypeArg.append(content[lineNum].lower())
            lineNum += 1
        lineNum += 1 # skip "-1" line

        playertobuydoesnotexistArg = content[lineNum][3:-1]; lineNum += 1
        playertoselldoesnotexistArg = content[lineNum][3:-1]; lineNum += 1
        shopdoesnotbuyArg = content[lineNum][3:-1]; lineNum += 1
        shopcannotaffordArg = content[lineNum][3:-1]; lineNum += 1
        playercannotaffordArg = content[lineNum][3:-1]; lineNum += 1
        shopsolditemArg = content[lineNum][3:-1]; lineNum += 1
        shopboughtitemArg = content[lineNum][3:-1]; lineNum += 1
        temperArg = content[lineNum]; lineNum += 1

        if shopboughtitemArg == 'Oops - %d a minor bug - please report!':
            shopboughtitemArg = ''

        if temperArg == '-1':
            temperArg = ''
        elif temperArg == '0':
            temperArg = 'The shopkeeper pukes on the player.'
        elif temperArg == '1':
            temperArg = 'The shopkeeper smokes his joint.'

        bitvector = content[lineNum]; lineNum += 1
        willFightArg = bitvector in ('1', '3')
        willBankArg =  bitvector in ('2', '3')

        shopkeeperMobArg = content[lineNum]; lineNum += 1
        wontdealwithArg = content[lineNum]; lineNum += 1

        shopRoomsArg = []
        while content[lineNum] != '-1':
            shopRoomsArg.append(content[lineNum])
            lineNum += 1
        lineNum += 1 # skip "-1" line

        open1Arg = content[lineNum]; lineNum += 1
        close1Arg = content[lineNum]; lineNum += 1
        open2Arg = content[lineNum]; lineNum += 1
        close2Arg = content[lineNum]; lineNum += 1



        """
        <shop shopnum="" profitwhenselling="" profitwhenbuying="" fights="true" banks="true" shopkeepervnum="" roomvnum="" open1="" close1="" open2="" close2="">
            <forsale>
                <item vnum="" />
            </forsale>
            <willbuy light="true" scroll="true" ...>
                <willbuyname>name</willbuyname>
            </willbuy>
            <messages>
                <playercantbuy>message</playercantbuy>
                <playercantsell>message</playercantsell>
                <shopdoesnotbuy>message</shopdoesnotbuy>
                <shopcantafford>message</shopcantafford>
                <playercantafford>message</playercantafford>
                <shopsolditem>message</shopsolditem>
                <shopboughtitem>message</shopboughtitem>
                <temper>message</temper>
            </messages>
            <wontdealwith good="true" evil="true" ... />
            <room vnum=""/>
            <room vnum=""/>
            <room vnum=""/>
        </shop>
        """

        # don't show open2 and close2 if they are both 0
        if open2Arg == '0' and close2Arg == '0':
            open2Arg = ''
            close2Arg = ''
        else:
            open2Arg = 'open2="%s"' % (open2Arg)
            close2Arg = 'close2="%s"' % (close2Arg)

        willFightArg = willFightArg and ' fights="true"' or ''
        willBankArg = willBankArg and ' banks="true"' or ''


        print('<shop shopnum="%(shopnum)s" profitwhenselling="%(sellprofit)s" profitwhenbuying="%(buyprofit)s"%(fights)s%(banks)s shopkeepervnum="%(shopkeeper)s" open1="%(open1)s" close1="%(close1)s" %(open2)s %(close2)s>' % {
            'shopnum': vNumArg,
            'sellprofit': profitWhenSellingArg,
            'buyprofit': profitWhenBuyingArg,
            'shopkeeper': shopkeeperMobArg,
            'fights': willFightArg,
            'banks': willBankArg,
            'open1': open1Arg,
            'close1': close1Arg,
            'open2': open2Arg,
            'close2': close2Arg})

        print('  <forsale>')
        for arg in forSaleVNumArg:
            print('    <item vnum="%s" />' % (arg))
        print('  </forsale>')

        print('  <willbuy %s />' % (' '.join(['%s="true"' % (x) for x in buyTypeArg])))

        print('  <messages>')
        print('    <playercantbuy>%s</playercantbuy>' % (playertobuydoesnotexistArg))
        print('    <playercantsell>%s</playercantsell>' % (playertoselldoesnotexistArg))
        print('    <shopdoesnotbuy>%s</shopdoesnotbuy>' % (shopdoesnotbuyArg))
        print('    <shopcantafford>%s</shopcantafford>' % (shopcannotaffordArg))
        print('    <playercantafford>%s</playercantafford>' % (playercannotaffordArg))
        print('    <shopsolditem>%s</shopsolditem>' % (shopsolditemArg))
        print('    <shopboughtitem>%s</shopboughtitem>' % (shopboughtitemArg))
        print('    <temper>%s</temper>' % (temperArg))
        print('  </messages>')

        wontdealattr = []
        wontdealwithArg = int(wontdealwithArg)
        if wontdealwithArg >= 64:
            wontdealwithArg -= 64
            wontdealattr.append('warrior')
        if wontdealwithArg >= 32:
            wontdealwithArg -= 32
            wontdealattr.append('thief')
        if wontdealwithArg >= 16:
            wontdealwithArg -= 16
            wontdealattr.append('cleric')
        if wontdealwithArg >= 8:
            wontdealwithArg -= 8
            wontdealattr.append('magicuser')
        if wontdealwithArg >= 4:
            wontdealwithArg -= 4
            wontdealattr.append('neutral')
        if wontdealwithArg >= 2:
            wontdealwithArg -= 2
            wontdealattr.append('evil')
        if wontdealwithArg >= 1:
            wontdealwithArg -= 1
            wontdealattr.append('good')

        if wontdealattr:
            print('  <wontdealwith %s />' % (' '.join(['%s="true"' % (x) for x in wontdealattr])))

        print('  <rooms>')
        for arg in shopRoomsArg:
            print('    <room vnum="%s" />' % (arg))
        print('  </rooms>')

        print('</shop>')
print('</shops>')


print('<rooms>')
for wldFile in os.listdir(sys.argv[1]):
    if not wldFile.endswith('.wld'):
        continue

    with open(os.path.join(sys.argv[1], wldFile)) as fp:
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

            print('<room vnum="%s" name="%s" type="%s" zone="%s" %s>' % (vNumArg, escape(nameArg), sectorTypeArg, zoneArg, ' '.join(['%s="true"' % (att) for att in attribs])))
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