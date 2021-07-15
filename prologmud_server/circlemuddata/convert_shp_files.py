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

        sys.stdout.flush()
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

        sys.stdout.flush()
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
        sys.stdout.flush()
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
        sys.stdout.flush()
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
        sys.stdout.flush()
        for arg in shopRoomsArg:
            print('    <room vnum="%s" />' % (arg))
        print('  </rooms>')
        sys.stdout.flush()
        print('</shop>')
print('</shops>')