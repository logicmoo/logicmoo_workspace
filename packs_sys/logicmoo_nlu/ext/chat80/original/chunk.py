from nltk.tag import pos_tag
from nltk.tokenize import word_tokenize
from nltk.corpus import brown
from nltk.tag import UnigramTagger

import sys
import select

if select.select([sys.stdin,],[],[],0.0)[0]:
 for line in sys.stdin:
  print(pos_tag(word_tokenize(line)))
else:
 print(pos_tag(word_tokenize(' '.join(sys.argv[1:]))))
#test_sent = ' '.join(sys.argv[1:]
#unigram_tagger = UnigramTagger(brown.tagged_sents(categories='news')[:500])
#for tok, tag in unigram_tagger.tag(test_sent):
# print("({}, {}), ".format(tok, tag))



