from sklearn.linear_model import LogisticRegression
from sklearn import model_selection
from sklearn.cross_validation import train_test_split
import numpy as np
import sklearn
import re, nltk, glob, random, sys, json, os
import pandas as pd
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import KFold
from sklearn.model_selection import cross_val_predict
import nltk
import ast

RELATION_NAMES = ['comparison', 'contingency', 'expansion', 'temporal']

'''
Functions for word pairs feature extraction
'''

def print_new(text, end='.'):
    sys.stdout.write(str(text) + end)
    sys.stdout.flush()

def get_stat(df, humanset=False):
    if humanset:
        y, uniques = pd.factorize(df['relation_human'])
    else:
        y, uniques = pd.factorize(df['relation']) # y is the vector of labels
    print ('Total samples: ', len(df.index))
    print ('Relation factorization: ', uniques)
    return y

def get_general_intent(intent_text_lst):
    return_lst = []
    for intent_str in intent_text_lst:
        general_intent = intent_str
        if 'info_request' in intent_str:
            general_intent = 'info_request'
        elif 'opinion_provide_pos_patterns' in intent_str:
            general_intent = 'opinion_provide_pos_patterns'
        elif 'opinion_provide_neg_patterns' in intent_str:
            general_intent = 'opinion_provide_neg_patterns'
        elif 'invoke_game' in intent_str:
            general_intent = 'invoke_game'
        elif 'opinion_request_pos_patterns' in intent_str:
            general_intent = 'opinion_request_pos_patterns'
        elif 'opinion_request_neg_patterns' in intent_str:
            general_intent = 'opinion_request_neg_patterns'
        return_lst.append(general_intent)
    return return_lst

def extract_features(df_all, feat_select, selected_feat_dialogue, selected_feat_dialogue_indirect):
    cols_all = list(df_all.columns) # all column names in csv file

    '''
    Dialogue Features
    '''
    if 'dialogue' in feat_select:
        # extract indirect dialogue feature to the main columns
        feat_lst_dialogue_gnode_replicated = []
        feat_lst_dialogue_intent_replicated = []
        for i in range(len(df_all.index)):
            if 'intents' in selected_feat_dialogue_indirect:
                all_intents = []
                try:
                    int_list_arg1 = ast.literal_eval(str(df_all.loc[i, 'arg1_intents']))
                    int_list_arg2 = ast.literal_eval(str(df_all.loc[i, 'arg2_intents']))
                except:
                    pass
                # convert this intents text to real type
                int_list_arg1 = get_general_intent(int_list_arg1)
                int_list_arg2 = get_general_intent(int_list_arg2)
                # extract all intents
                for intent_one in int_list_arg1:
                    all_intents.append('arg1_intent_%s' % intent_one)
                for intent_one in int_list_arg2:
                    all_intents.append('arg2_intent_%s' % intent_one)
                # assign 1 to all those one-hot feature
                for feat_this in all_intents:
                    df_all.loc[i, feat_this] = 1 # set data record this section to 1
                    feat_lst_dialogue_intent_replicated.append(feat_this) # add this gnode property name to the whole head list


            if 'gnode_entities' in selected_feat_dialogue_indirect:
                on_feat_lst = []
                try:
                    ent_list_arg1 = ast.literal_eval(str(df_all.loc[i, 'arg1_gnode_entities']))
                    # print (ent_list_arg1)
                    ent_list_arg2 = ast.literal_eval(str(df_all.loc[i, 'arg2_gnode_entities']))
                    # print (ent_list_arg2)
                except:
                    pass
                # extract all entities
                for ent in ent_list_arg1:
                    on_feat_lst.append('arg1_gnode_%s' % ent[0])
                for ent in ent_list_arg2:
                    on_feat_lst.append('arg2_gnode_%s' % ent[0])
                # assign 1 to all those one-hot feature
                for feat_this in on_feat_lst:
                    df_all.loc[i, feat_this] = 1 # set data record this section to 1
                    feat_lst_dialogue_gnode_replicated.append(feat_this) # add this gnode property name to the whole head list
            if 'actors' in selected_feat_dialogue_indirect:
                on_feat_lst = []
                try:
                    # actor_list_arg1 = ast.literal_eval(str(df_all.loc[i, 'arg1_actors']))
                    # print (actor_list_arg1)
                    j = json.loads(str(df_all.loc[i, 'arg1_actors']))
                    # print (j)
                    # actor_list_arg2 = ast.literal_eval(str(df_all.loc[i, 'arg2_actors']))
                    # print (actor_list_arg2)
                except:
                    pass
        feat_lst_dialogue_intent = list(set(feat_lst_dialogue_intent_replicated))
        print ('Intent features #', len(feat_lst_dialogue_intent))
        feat_lst_dialogue_gnode = list(set(feat_lst_dialogue_gnode_replicated))
        print ('Gnode features #', len(feat_lst_dialogue_gnode))

        # extract actor feature to the main columns
        print ('=> LOADING FEATURES: dialogue')
        cat_vars = selected_feat_dialogue
        for var in cat_vars:
            cat_list = 'var'+'_'+var
            cat_list = pd.get_dummies(df_all[var], prefix=var)
            data1 = df_all.join(cat_list)
            df_all = data1
        cat_vars = cols_all
        data_vars = df_all.columns.values.tolist()
        to_keep = [i for i in data_vars if i not in cat_vars]
        to_keep = to_keep + feat_lst_dialogue_gnode # combine head of existing properties and new gnode feature

    final_feat_lst = []
    if 'dialogue' in feat_select:
        final_feat_lst = final_feat_lst + to_keep
        print ('dialogue features #: ', len(to_keep))

    return df_all, final_feat_lst


if __name__ == '__main__':
    '''
    VARIABLE SETTINGS
    '''
    feat_select = ['dialogue']
    model_select = 'lr'

    # Setting dialogue features in use
    selected_feat_dialogue = ['arg1_dialogue_act', 'arg2_dialogue_act', 'arg1_sentiment', 'arg2_sentiment', 'arg1_topic', 'arg2_topic', 'arg1_cobot_topics', 'arg2_cobot_topics', 'topic_golden']
    selected_feat_dialogue_indirect = ['gnode_entities', 'intents']

    import_original_dataset = False
    # Option 1: use original folders
    if import_original_dataset:
        all_dfs = []
        for foldername in os.listdir('Edina-DR/Edina-DR_NLU/'):
            if os.path.isdir('Edina-DR/Edina-DR_NLU/%s' % foldername):
                print ('+++++ %s ...' % foldername)
                for filename in os.listdir('Edina-DR/Edina-DR_NLU/%s' % foldername):
                    pathname = 'Edina-DR/Edina-DR_NLU/%s/%s' % (foldername, filename)
                    df_this = pd.read_csv(pathname,index_col=None, header=0)
                    # add feature which is not existing in csv: the golden topic label in edina data
                    df_this['topic_golden'] = foldername
                    all_dfs.append(df_this)
        print ('All data frames: ', len(all_dfs))
        df_all = pd.concat(all_dfs, axis = 0, ignore_index = True) # df_all is all record in all files across folders

        y = get_stat(df_all)

        # Extract features
        df_all, final_feat_lst = extract_features(df_all, feat_select, selected_feat_dialogue, selected_feat_dialogue_indirect)

        print (final_feat_lst[0:20])
        X = df_all[final_feat_lst]
        X = X.fillna(0)
        kf = KFold(n_splits=10)
                            
        print ('=> data preparing done.')

        if model_select == 'lr':
            logreg = LogisticRegression(multi_class='ovr', class_weight='balanced')
            pipe = make_pipeline(StandardScaler(), logreg)

        y_pred = cross_val_predict(pipe, X, y, cv=10)
        print ("==========RESULT=========")
        confusion_matrix_print = confusion_matrix(y, y_pred)
        clas_report = classification_report(y, y_pred)
        print(confusion_matrix)
        print(clas_report)

    else:
        # Option 2: use separate processed dataset
        pathname = 'Edina-DR/Edina-DR_NLU_separate/'
        df_train = pd.read_csv("%strain.csv" % pathname,index_col=None, header=0)
        df_test = pd.read_csv("%stest_human-annotation.csv" % pathname,index_col=None, header=0)
        print ("--Stat for Train set: ")
        y_train = get_stat(df_train)
        print ("--Stat for Test set: ")
        y_test = get_stat(df_test, humanset=True)

        df_each_relation = []
        for i, relation in enumerate(RELATION_NAMES):
            df_each_relation.append(df_test.loc[df_test['relation_human'] == relation])

        # Print statistics for each relation
        for i, each_df in enumerate(df_each_relation):
            print ('processing relation: %s' % RELATION_NAMES[i])
            print (len(each_df.index))

        df_all = pd.concat([df_train, df_test], axis = 0, ignore_index = True) # df_all is all record in all files across folders

        df_all, final_feat_lst = extract_features(df_all, feat_select, selected_feat_dialogue, selected_feat_dialogue_indirect)
                            
        print ('=> data preparing done.')

        X = df_all[final_feat_lst+['relation']]
        X = X.fillna(0)
        X_train = X[:len(df_train.index)]
        X_test = X[len(df_train.index):len(df_train.index)+len(df_test.index)]

        # remove relation column
        X_train.drop(['relation'], axis = 1, inplace = True, errors = 'ignore')
        X_test.drop(['relation'], axis = 1, inplace = True, errors = 'ignore')

        print (X_train.shape)
        print (X_test.shape)

        print (y_train.shape)
        print (y_test.shape)

        logreg = LogisticRegression(multi_class='ovr', class_weight='balanced')

        logreg.fit(X_train, y_train)
        y_pred_train = logreg.predict(X_train)
        y_pred_test = logreg.predict(X_test)

        print ("==========TRAIN SET RESULT=========")
        confusion_matrix_print = confusion_matrix(y_train, y_pred_train)
        clas_report = classification_report(y_train, y_pred_train)
        print(confusion_matrix)
        print(clas_report)

        print ("==========TEST SET RESULT=========")
        confusion_matrix_print = confusion_matrix(y_test, y_pred_test)
        clas_report = classification_report(y_test, y_pred_test)
        print(confusion_matrix_print)
        print(clas_report)