import nltk
import codecs
import os
import glob
import re
import math

accept_source_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/ForFreEngTraining/FreUnicodeRaw.txt'
accept_pre_edited_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/ForFreEngTraining/FreUnicodePreEdited.txt'
accept_google_translated_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/ForFreEngTraining/EngUnicodeGoogle.txt'
accept_bilingual_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/ForFreEngTraining/1000Bilingual.txt'
accept_postedited_bilingual_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/ForFreEngTraining/1000BilingualPosteditedUnicode.txt'
accept_postedited_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/ForFreEngTraining/EngUnicodePostedited.txt'
accept_review_file = 'C:/cygwin/home/ACCEPT/MT/GTFeb2012/ForFreEngTraining/FreEngReview.txt'

def main():
    merge_files_for_bilingual_editing(accept_source_file, accept_google_translated_file, accept_bilingual_file)

def extract_postedited():
    extract_post_edited(accept_postedited_bilingual_file, accept_postedited_file)

def extract_review():
    merge_files_for_review(accept_source_file, accept_pre_edited_file, accept_google_translated_file, accept_postedited_file, accept_review_file)

def merge_files_for_bilingual_editing(source_file, target_file, bilingual_file):
    source_lines = read_file_to_lines(source_file)
    target_lines = read_file_to_lines(target_file)
    len_source = len(source_lines)
    len_target = len(target_lines)
    if not len_source == len_target:
        print('\nError: source file has {0} lines and target file has {1} lines\n'.format(len_source, len_target))
    else:
        merged_lines = merge_lines(source_lines, target_lines, len_source)
        write_out_lines_to_file(merged_lines, bilingual_file)
        print('\nBilingual editing file {0} lines created: {1}\n'.format(len_source, bilingual_file))

def merge_files_for_review(source_file, pre_edited_file, google_translated_file, postedited_file, review_file):
    lines1 = read_file_to_lines(source_file)
    lines2 = read_file_to_lines(pre_edited_file)
    lines3 = read_file_to_lines(google_translated_file)
    lines4 = read_file_to_lines(postedited_file)
    if not len(lines1) == len(lines2) == len(lines3) == len(lines4):
        print('\nError: files have different lengths: {0}\n'.format((len(lines1), len(lines2), len(lines3), len(lines4))))
    else:
        merged_lines = merge_lines_for_review(lines1, lines2, lines3, lines4, len(lines1))
        write_out_lines_to_file(merged_lines, review_file)
        print('\nReview file {0} lines created: {1}\n'.format(len(merged_lines), review_file))

def extract_post_edited(postedited_bilingual_file, postedited_file):
    bilingual_lines = read_file_to_lines(postedited_bilingual_file)
    print('\nRead postedited file ({0} lines): {1}\n'.format(len(bilingual_lines), postedited_bilingual_file))
    
    postedited_lines = extract_postedited_lines(bilingual_lines)
    write_out_lines_to_file(postedited_lines, postedited_file)
    print('\nPostedited file {0} lines created: {1}\n'.format(len(postedited_lines), postedited_file))

def extract_postedited_lines(bilingual_lines):
    length = len(bilingual_lines)
    len3 = length / 3
    out = []
    for i in range(len3):
        out.append(bilingual_lines[( 3 * i ) + 1])
    return out        

def read_file_to_lines(file):
    f = codecs.open(file, encoding='utf-16LE')
    lines = f.readlines()
    f.close()
    return lines

def merge_lines(l1, l2, length):
    out = []
    for i in range(length - 1):
        elt1 = l1[i]
        elt2 = l2[i]
        out.append(merge_items_for_bilingual_editing(elt1, elt2))
    return out

def merge_lines_for_review(l1, l2, l3, l4, length):
    out = []
    for i in range(length - 1):
        if not canonical_for_comparison(l1[i]) == canonical_for_comparison(l2[i]):
            out.append(unicode('PRE-EDITED\n'))
        if not canonical_for_comparison(l3[i]) == canonical_for_comparison(l4[i]):
            out.append(unicode('POST-EDITED\n'))
        out.append(l1[i])
        out.append(l2[i])
        out.append(l3[i])
        out.append(l4[i])
        out.append(unicode('\n'))
    return out

def merge_items_for_bilingual_editing(elt1, elt2):
    return elt1 + elt2
        
def write_out_lines_to_file(lines, file):
    f = codecs.open(file, 'w', encoding='utf-16LE')
    for item in lines:
        f.write(item)
    f.close()

def canonical_for_comparison(str):
    out = ''
    for char in str.lower():
        if char.isalnum() or char in unicode('. ') or char in '. ':
            out = out + char
    return out

    
    
                


    

    

