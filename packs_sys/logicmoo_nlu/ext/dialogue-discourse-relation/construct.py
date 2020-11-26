import pandas as pd
import re, glob, nltk, sys, os
import nltk

# Connective cuewords are defined in paper 
CUEWORDS_COMPARISON = set(['but', 'however', 'although', 'by contrast'])
CUEWORDS_CONTINGENCY = set(['because', 'so', 'thus', 'as a result', 'consequently', 'therefore'])
CUEWORDS_EXPANSION = set(['also', 'for example', 'in addition', 'instead', 'indeed', 'moreover', 'for instance', 'in fact', 'furthermore', 'or', 'and'])
CUEWORDS_TEMPORAL = set(['then', 'previously', 'earlier', 'later', 'after', 'before'])

# the following two list should match the sequence between each other
CUEWORDS_ALL = [CUEWORDS_COMPARISON, CUEWORDS_CONTINGENCY, CUEWORDS_EXPANSION, CUEWORDS_TEMPORAL]
RELATION_NAMES = ['comparison', 'contingency', 'expansion', 'temporal']

def identify_pairs(utt_text, utt_text_prev="", show=True):
    results = []
    utt_text = utt_text.lower()

    # Divide the text to individual sentence, and deal with sentence separately
    text_lst = nltk.sent_tokenize(utt_text)
    # Load previous turn utterance, to provide info for argument in previous turn, if the connective appears at the beginning
    text_prev_lst = nltk.sent_tokenize(utt_text_prev)

    # Pick paris for each text in text list
    for text_count, text in enumerate(text_lst):
        text_pos = nltk.pos_tag(nltk.word_tokenize(text))
        for relation_idx, cuewords_lst in enumerate(CUEWORDS_ALL):
            for cue_this in cuewords_lst:
                # find all appears of connectives
                iter = re.finditer(r"\b%s\b" % cue_this, text)
                indices = [m.start(0) for m in iter]

                if len(indices) > 0:
                    # if do exist a connective of this kind of relation
                    if show:
                        print (text)
                        print (cue_this)
                        print (indices)
                    for connective_index in indices:
                        # if the connective is in the middle
                        if connective_index == 0:
                            if text_count > 0:
                                arg1 = text_lst[text_count - 1]
                            elif len(text_prev_lst) > 1:
                                arg1 = text_prev_lst[-1]
                            else:
                                arg1 = ''
                            arg2 = text
                            type_this = 'begin'
                            if show:
                                print (type_this)
                        else:
                            # for each detected connectives
                            arg1 = text[0:connective_index]
                            arg2 = text[connective_index:]
                            type_this = 'mid'
                            if show:
                                print (type_this)

                        # Begin: Rules to filter out not common connectives cases
                        # find the place of the connective
                        select_flag = True
                        # remove connectives
                        arg2 = (arg2[len(cue_this):]).strip()
                        try:
                            # Filter 1: pos tag of the connective words
                            if type_this == 'mid':
                                connective_pos = text_pos[len(nltk.word_tokenize(arg1))][1]
                                if show:
                                    print (text_pos[len(nltk.word_tokenize(arg1))])
                            else:
                                connective_pos = text_pos[0][1]
                                if show:
                                    print (text_pos[0])
                            if connective_pos not in ['IN', 'CC']:
                                select_flag = False
                            # Filter 2: length of arguments
                            if len(nltk.word_tokenize(arg1)) < 3 or len(nltk.word_tokenize(arg2)) < 4:
                                select_flag = False
                            if cue_this == 'and' and (len(nltk.word_tokenize(arg1)) < 12 or len(nltk.word_tokenize(arg2)) < 12):
                                select_flag = False
                            if cue_this == 'and' and type_this == 'begin':
                                select_flag = False
                            if cue_this == 'or' and (len(nltk.word_tokenize(arg1)) < 12 or len(nltk.word_tokenize(arg2)) < 12):
                                select_flag = False
                            if cue_this == 'after' and (len(nltk.word_tokenize(arg1)) < 12 or len(nltk.word_tokenize(arg2)) < 12):
                                select_flag = False
                            if cue_this == 'before' and (len(nltk.word_tokenize(arg1)) < 6 or len(nltk.word_tokenize(arg2)) < 6):
                                select_flag = False
                        except:
                            select_flag = False
                        # End of rules

                        if select_flag:
                            # save to results dictionary list
                            result = {}
                            result['arg1'] = arg1
                            result['arg2'] = arg2
                            result['relation'] = RELATION_NAMES[relation_idx]
                            result['connective'] = cue_this
                            result['type'] = type_this
                            result['original_utt'] = utt_text
                            result['original_utt_prev'] = utt_text_prev
                            results.append(result)
                            if show:
                                print ('->', RELATION_NAMES[relation_idx])
    return results

# Get pairs with cuewords
def get_pairs(df, header, output, show=True):
    data = pd.DataFrame(columns=header)
    list_utt_name = 'Answer.sentence'
    index = 0
    list_utt_range = 100
    prev_text = ""
    for i in range(1, len(df.index)):
    # for i in range(1, 5):
        for range_i in range(1, list_utt_range):
            col_name_this = '%s%s' % (list_utt_name, str(range_i))
            if col_name_this not in df.columns:
                # if there is not more utterances in this conversation, terminate
                break
            else:
                # for each utterance
                text = str(df.ix[i, col_name_this])
                results = identify_pairs(text, prev_text, show=show)
                # update previous text
                prev_text = text
                # save result to csv row
                for result in results:
                    for key, value in result.items():
                        data.loc[index, key] = value
                    index += 1

    data.to_csv(output, columns=header, index=False)



if __name__ == '__main__':
    '''
    Code for original edina corpus
    '''
    header = ['arg1', 'arg2', 'relation', 'connective', 'type', 'original_utt', 'original_utt_prev']
    
    for foldername in os.listdir('Edina/'):
        if os.path.isdir('Edina/%s' % foldername):
            print ('+++++ %s ...' % foldername)
            for filename in os.listdir('Edina/%s' % foldername):
                pathname = 'Edina/%s/%s' % (foldername, filename)
                df = pd.read_csv(pathname)

                output = 'Edina-DR2/Edina-DR/%s/pairs_%s' % (foldername, filename)
                if not os.path.exists('Edina-DR2/Edina-DR/%s' % foldername):
                    os.makedirs('Edina-DR2/Edina-DR/%s' % foldername)
                get_pairs(df, header, output, show=False)

    '''
    Code for debugging
    '''
    # identify_pairs('I love you but do you love me?', show=True)
    # identify_pairs('I love elephant a lot but I go to zoo to see them', show=True)
    # identify_pairs('I love cute small elephant a lot and I go to zoo to see the love ones them', show=True)
    # identify_pairs('This is Tom and Jerry', show=True)
    # identify_pairs("no kidding, right, he's our best scorer. hopefully he'll be back before that game.")