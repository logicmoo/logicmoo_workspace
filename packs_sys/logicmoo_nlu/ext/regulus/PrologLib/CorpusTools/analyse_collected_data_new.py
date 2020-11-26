import nltk
import codecs
import os
import glob
import re
import math
from nltk.tokenize import PunktWordTokenizer

# Code for extracting representative sentences from the Symantec data. This assumes
# that you have previously cleaned them up using the routine in read_forum_data.py.
#
# The main routine is
#
# extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples)
#
# The arguments as as follows:
#
# all_sents_file.                    The input file: you create this by doing a 'cat' of all the files created by read_forum_data.py.
# ordered_corpus_file                Output file: all sentences, ordered by length
# representative_ordered_corpus_file Output file: representative corpus
# range_of_lengths                   List of two numbers, giving the range of sentence lengths required
# n_examples                         Approximate number of examples to generate
#
# The representative corpus is chosen by picking a number of examples for each sentence length proportional to the frequency with
# which that length occurs in the original corpus. "Representative" is defined in terms of weighted frequency of unigrams and bigrams
#
# There are a number of sample calls below.
#
# You need to install the NLTK package to get the word tokenizer.

def forum1():
    all_sents_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt'
    ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/cleaned_ordered_sents.txt'
    representative_ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/representative_cleaned_sents.txt'
    range_of_lengths = (5, 35)
    n_examples = 1000
    extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples)

def tm1():
    all_sents_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/TM/symc_bip_07_en_fr_small.fr'
    ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/TM/cleaned_ordered_sents.txt'
    representative_ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/TM/representative_cleaned_sents.txt'
    range_of_lengths = (5, 35)
    n_examples = 10000
    extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples)

def forum2():
    all_sents_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt'
    ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/cleaned_ordered_sents_long.txt'
    representative_ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/representative_cleaned_sents_long2.txt'
    range_of_lengths = (20, 39)
    n_examples = 5000
    extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples)

def forum3():
    all_sents_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt'
    ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/cleaned_ordered_sents3.txt'
    representative_ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/GTFeb2012/CleanFrenchVersions/representative_cleaned_sents3.txt'
    range_of_lengths = (5, 35)
    n_examples = 10000
    extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples)

def new_forum1():
    all_sents_file = 'C:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/all_files_v2.txt'
    ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/cleaned_ordered_sents.txt'
    representative_ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/representative_cleaned_sents.txt'
    range_of_lengths = (5, 35)
    n_examples = 10000
    extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples)

def new_forum_1000():
    all_sents_file = 'C:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/all_files_v2.txt'
    ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/cleaned_ordered_sents.txt'
    representative_ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/representative_cleaned_sents_1000.txt'
    range_of_lengths = (5, 35)
    n_examples = 1025
    extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples)

def new_forum_1000_random():
    all_sents_file = 'C:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/all_files_v2.txt'
    representative_ordered_corpus_file = 'c:/cygwin/home/ACCEPT/MT/ForumDec2012/CleanFrenchVersions/representative_cleaned_sents_1000_random.txt'
    range_of_lengths = (5, 35)
    n_examples = 1025
    extract_random_sents(all_sents_file, representative_ordered_corpus_file, range_of_lengths, n_examples)

unigram_weight = 1.0
bigram_weight = 3.0

def extract_representative_sents(all_sents_file, ordered_corpus_file, representative_ordered_corpus_file, range_of_lengths, n_examples):
    fdist = get_freqs(all_sents_file)
    tagged_lines = get_tagged_lines(all_sents_file, fdist)
    
    dump_data(ordered_corpus_file, tagged_lines, 'all', 'all')
    
    dump_data(representative_ordered_corpus_file, tagged_lines, range_of_lengths, n_examples)

def extract_random_sents(all_sents_file, random_corpus_file, range_of_lengths, n_examples):
    fdist = 'none'
    tagged_lines = get_tagged_lines(all_sents_file, fdist)
        
    dump_data(random_corpus_file, tagged_lines, range_of_lengths, n_examples)

def get_freqs(all_sents_file):
    print "Calculating word and bigram frequencies..."
    f = codecs.open(all_sents_file, encoding='utf-8')
    raw = f.read()
    lcase_words = tokenize_and_clean(raw)
    print "Done"
    print "Number of tokens: " + str(len(lcase_words))
    print "Number of types: " + str(len(set(lcase_words)))   
    return nltk.FreqDist(lcase_words + nltk.bigrams(lcase_words))

def dump_data(outfile, tagged_lines, range_of_lengths, n_examples):
    f = codecs.open(outfile, 'w', encoding='utf-8')
    total = count_words_in_tagged_lines(tagged_lines)
    all_lengths = set(lengths_for_tagged_lines(tagged_lines))
    total_lines = count_unique_lines_for_range(tagged_lines, range_of_lengths)
    print('Require {0} lines from {1} unique lines in range'.format(n_examples, total_lines))
    if range_of_lengths == 'all':
        print('Using all data')
        proportion_to_use = 1
    elif n_examples > total_lines:
        print('Warning: not enough lines to produce {0} examples'.format(n_examples))
        proportion_to_use = 1
    else:
        proportion_to_use = float(n_examples) / float(total_lines)
    cumulative = 0.0
    for length in all_lengths:
            cumulative = dump_data_for_length(length, f, tagged_lines, total, range_of_lengths, proportion_to_use, cumulative)
    f.close()
    print('Printed data to {0}'.format(outfile))

def dump_data_for_length(length, f, tagged_lines, total, range_of_lengths, proportion_to_use, cumulative):
    relevant_tagged_lines = [line for line in tagged_lines if line[1] == length]
    n_lines = len(relevant_tagged_lines)
    word_count_for_this_length = math.fsum(lengths_for_tagged_lines(relevant_tagged_lines))
    pc_for_this_length = ( 100.0 * word_count_for_this_length ) / total
    new_cumulative = cumulative + pc_for_this_length

    sorted_lines = sorted(relevant_tagged_lines, key=lambda x: x[2], reverse=True)
    # Consider items identical if cleaned sentence words are identical (e.g. ignore extra spaces)
    uniqued_sorted_lines = remove_dups_keeping_order(sorted_lines, lambda x: str(tokenize_and_clean(x[0])))
    n_unique_lines = len(uniqued_sorted_lines)
    n_examples = int(n_unique_lines * proportion_to_use)
    
    if not number_in_range(length, range_of_lengths):
        print('#--- Skipping {0} sentences of length {1} ({2:.2f}%, {3:.2f}%)'.format(n_lines, length, pc_for_this_length, new_cumulative))
        f.write('\n#--- Skipping {0} sentences of length {1} ({2:.2f}%, {3:.2f}%)'.format(n_lines, length, pc_for_this_length, new_cumulative))
    else:
        print('#--- {0} sentences ({1} unique) of length {2} ({3:.2f}%, {4:.2f}%). {5} retained.'.format(n_lines, n_unique_lines, length, pc_for_this_length, new_cumulative, n_examples))
        f.write('\n#--- {0} sentences ({1} unique) of length {2} ({3:.2f}%, {4:.2f}%). {5} retained.'.format(n_lines, n_unique_lines, length, pc_for_this_length, new_cumulative, n_examples))
        for line in all_or_first_n(n_examples, uniqued_sorted_lines):
            f.write('\n' + line[0])
            #f.write('\n' + line[0] + ' ({0} chars)'.format(len(line[0])))
    return new_cumulative

def count_unique_lines_for_range(tagged_lines, range_of_lengths):
    relevant_tagged_lines = [line for line in tagged_lines if number_in_range(line[1], range_of_lengths)]
    uniqued_lines = remove_dups_keeping_order(relevant_tagged_lines, lambda x: str(tokenize_and_clean(x[0])))
    n_unique_lines = len(uniqued_lines)
    return n_unique_lines

def count_words_in_tagged_lines(tagged_lines):
    return math.fsum([item[1] for item in tagged_lines])

def lengths_for_tagged_lines(tagged_lines):
    return sorted([item[1] for item in tagged_lines])

def get_tagged_lines(all_sents_file, fdist):
    f = codecs.open(all_sents_file, encoding='utf-8')
    lines = f.readlines()
    print('\nRead file {0} ({1} lines)\n'.format(all_sents_file, len(lines)))
    f.close()
    return [tag_line(line, fdist) for line in lines]

def tag_line(line, fdist):
    cleaned_line = clean_raw_line(line)
    lcase_words = tokenize_and_clean(cleaned_line)
    length = len(lcase_words)
    if fdist == 'none':
        av_freq = 0.0
    else:
        av_freq = get_av_freq(lcase_words, fdist)
    return (cleaned_line, length, av_freq)

# Give bigrams higher weight since they're more informative
def get_av_freq(words, fdist):
    return unigram_weight * get_av_freq_words(words, fdist) + bigram_weight * get_av_freq_bigrams(words, fdist)

def get_av_freq_words(words, fdist):
    return geometric_average_of_freqs(words, fdist)

def get_av_freq_bigrams(words, fdist):
    return geometric_average_of_freqs(nltk.bigrams(words), fdist)

def geometric_average_of_freqs(words, fdist):
    length = len(words)
    logfreqs = [ math.log(safe_freq(word, fdist)) for word in words ]
    if length == 0:
        return 1
    else:
        return math.exp( math.fsum(logfreqs) / length )

def safe_freq(word, fdist):
    if fdist.has_key(word):
        return fdist[word]
    else:
        return 1

cleaning_conversions = [(r'\bRe :', ''), (r'\bRe  :', ''), (r'^( )*', ''), (r'\n', '')]
#cleaning_conversions = [(r'\n', '')]

def clean_raw_line(str):
    for (instr, outstr) in cleaning_conversions:
        str = re.sub(instr, outstr, str)
    return str

def tokenize_and_clean(raw):
    words = PunktWordTokenizer().tokenize(raw) 
    return [clean_word(w) for w in words]    

def clean_word(w):
    return re.sub(r'^\'', '', w.lower())

def number_in_range(length, range_of_lengths):
    if range_of_lengths == 'all':
        return True
    else:
        return ( range_of_lengths[0] <= length and length <= range_of_lengths[1] )

def all_or_first_n(n, l):
    if n == 'all':
        return l
    else:
        return l[:n]

# Found this useful function on the web. It is supposed to be the most efficient way to do it.
def remove_dups_keeping_order(seq, idfun=None): 
   if idfun is None:
       def idfun(x): return x
   seen = {}
   result = []
   for item in seq:
       marker = idfun(item)
       # in old Python versions:
       # if seen.has_key(marker)
       # but in new ones:
       if marker in seen: continue
       seen[marker] = 1
       result.append(item)
   return result


    

    

