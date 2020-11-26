
import json
import urllib
import codecs
import csv

# Sample call
def main():
    infile = "C:/cygwin/home/ACCEPT/MT/Comparison/test_fr.csv"
    outfile = "C:/cygwin/home/ACCEPT/MT/Comparison/test_fr_en.csv"
    inlang = 'fr'
    outlang = 'en'
    translate_csv(infile, outfile, inlang, outlang)

def translate_csv(infile, outfile, inlang, outlang):
    csvReader = csv.reader(open(infile, 'rb'), delimiter='\t', quotechar='"')
    csvWriter = csv.writer(open(outfile, 'wb'), delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    i = 0
    print '--- Start translating using ' + inlang + ' --> ' + outlang + ' server'
    for row in csvReader:
        if len(row) != 2:
            print '*** Warning: row does not contain two elements: ' + row
        else:
            source0 = row[0]
            source1 = row[1]
            trans0 = translate_text(source0, inlang, outlang).encode('utf-8')       
            if source0 == source1:
                trans1 = trans0
            else:
                trans1 = translate_text(row[1], inlang, outlang).encode('utf-8')       
            outrow = (source0, source1, trans0, trans1)
            #print ', '.join(outrow)
            i += 1
            print 'Translated row ' + str(i)
            csvWriter.writerow(outrow)
    print '--- Finished'

def translate_text(input_text, source, target):
    url = "http://accept:motelone@accept.statmt.org/demo/translate.php"
    params = urllib.urlencode({'system' : 'sb', 'v' : '1.0', 'ie' : 'UTF8', \
            'langpair' : '%s|%s' % (source,target), 'q' : input_text})
    f = urllib.urlopen(url,params)
    response = json.loads(f.readline())
    return response['responseData']['translatedText']


        
    
    
    
    
    
                          

    

    
    

