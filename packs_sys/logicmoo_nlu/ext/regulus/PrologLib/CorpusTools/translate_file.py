import nltk
from nltk.book import *

import json
import urllib
import codecs

def main():
    file = "C:/cygwin/home/ACCEPT/MT/GTFeb2012/symc_bip_01_devtest_full.en"
    #file = "C:/cygwin/home/ACCEPT/MT/GTFeb2012/symc_bip_01_devtest_small.en"
    f = codecs.open(file, encoding='utf-16')
    i = 0
    for line in f:
        line1 = str(line.strip())
        i = i + 1
        translated = translate_text(line1, 'en', 'fr')
        tagged = nltk.pos_tag(nltk.word_tokenize(line1))
        print i
        print line1
        print tagged
        print translated

def translate_text(input_text, source, target):
    url = "http://accept:motelone@accept.statmt.org/demo/translate.php"
    params = urllib.urlencode({'system' : 'sb', 'v' : '1.0', 'ie' : 'UTF8', \
            'langpair' : '%s|%s' % (source,target), 'q' : input_text})
    f = urllib.urlopen(url,params)
    response = json.loads(f.readline())
    return response['responseData']['translatedText']


        
    
    
    
    
    
                          

    

    
    

