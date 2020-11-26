import nltk
import codecs
import os
import glob
import re

# Code for pre-processing raw Symantec data. The main routine is
#
# read_symantec_dir(root_dir, sub_dir, new_sub_dir)
#
# This assumes that the data is in the directory root_dir/sub_dir. It
# reads each file, cleans it up, splits it into sentences, and writes out the
# result to root_dir/new_sub_dir.
#
# main() gives you a sample call.
#
# You need to install the NLTK package to get the sentence tokenizer.

sent_tokenizer=nltk.data.load('tokenizers/punkt/english.pickle')

def main():
    root_dir = 'C:/cygwin/home/ACCEPT/MT/ForumDec2012/'
    sub_dir = 'RawFrench'
    new_sub_dir = 'CleanFrench'
    
    read_symantec_dir(root_dir, sub_dir, new_sub_dir)

def read_symantec_dir(root_dir, sub_dir, new_sub_dir):
    path = root_dir + sub_dir
    for infile in glob.glob( path + '/*' ):
        if ok_file_name(infile):
            print "\nREADING FILE: " + infile
            outfile = re.sub(sub_dir, new_sub_dir, infile)
            file_contents = read_symantec_file(infile)
            print_unicode_string_list_to_file(file_contents, outfile)
            print "\nWRITTEN FILE: " + outfile
        else:
            print "\nBAD FILE: " + infile
            
def ok_file_name(file):
    return (re.sub(r'[/\\:_-]', '', file)).isalnum()

def read_symantec_file(file):
    f = codecs.open(file, encoding='utf-8')
    raw = f.read()
    split = raw.split('<P>')
    sents = [ sent
              for raw_component in split for sent in clean_and_sent_tokenize(raw_component) if len(sent) > 0
            ]
    return sents

def clean_and_sent_tokenize(str):
    clean_str = convert_non_word_strings(convert_html_strings(nltk.clean_html(str)))
    return sent_tokenizer.tokenize(clean_str)                                     

# &gt;    -> >
# &lt;    -> <
# &amp;   -> &
# &quot;  -> "

html_conversions = [("&gt;", ">"), ("&lt;", "<"), ("&amp;", "&"), ("&quot;", "\"")]

def convert_html_strings(str):
    for (instr, outstr) in html_conversions:
        str = re.sub(instr, outstr, str)
    return str

# http(s)://...    -> <URL>
# ftp://...        -> <FTP>

non_word_conversions = [(r'\bhttp(\S)*//(\S)*\b', '<URL>'), (r'\bftp://(\S)*\b', '<FTP>')]

def convert_non_word_strings(str):
    for (instr, outstr) in non_word_conversions:
        str = re.sub(instr, outstr, str)
    return str

def print_unicode_string_list_to_file(list, file):
    f = codecs.open(file, 'w', encoding='utf-8')
    for item in list:
        item1 = re.sub(r'\n', ' ', item)
        f.write('\n' + item1)
    f.close()



    
    

