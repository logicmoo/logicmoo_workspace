# Implicit Discourse Relation Identification for Open-domain Dialogues

## Dataset

The new proposed dataset are in the `Edina-DR` directory. The three folders under it are for:

* `Edina-DR`: Argument pairs extracted from Edina dataset
* `Edina-DR_NLU`: Argument pairs and dialogue features extracted Natural Language Understanding modules
* `Edina-DR_NLU_separate`: 400 samples randomly selected from the samples in `Edina-DR_NLU` and annotated by human, and corresponding training set
* `Edina-DR_NLU_separate_balanced`: 400 samples (100 for each relation) selected from the samples in `Edina-DR_NLU` and annotated by human, and corresponding training set

Note: training set is too large for `Edina-DR_NLU_separate` and `Edina-DR_NLU_separate_balanced`. The full version is available [here](https://drive.google.com/drive/folders/19VdM6rw6IKLuMw47Zbv_jr4k2bLOk-o0?usp=sharing).

The column names across all files in `Edina-DR` and `Edina-DR_NLU` stands for:

* `arg1`: the first arugment
* `arg2`: the second argument
* `relation`: the annotated discourse relation
* `connective`: the connective word originally lies between `arg1` and `arg2`
* `type`: whether the connective originally is at the beginning (`begin`) of a utterance or middle (`mid`) of a utterance
* `original_utt`: orginal full utterance where this argument pairs and discourse relation are extracted from
* `original_utt_prev`: the previous utterance of `original_utt`. If the connective is `begin`, then the first argument is extracted from the previous utternace
* `arg*_{dialogue_act, sentiment, topic, cobot_topics, intents}`: values for the names dialogue features
* `arg*_gnode_entities`: entities and their related properties using the entity detector
* `arg*_{gkg_entities, gkg_keywords, gkg_etype_list}`: the same information as in `arg*_gnode_entities` but separated according whether it's entities themselves, keywords or types
* `arg*_flow_topics`: topics detected by seperated topic classifier originally designed for invoking certain dialogue flows

The human annotation include:

* Discourse relation identified by expert annotater (`relation_human` field)
* Whether this pair of arugments do not hold a discourse relation due to grammar error (`error_human` field is `y`)

## Dataset Construction

1. Get original self-dialogue corpus Edina can be obtained [here](https://github.com/jfainberg/self_dialogue_corpus)
2. Rename the folder containing data to `Edina` and put on the root directory
3. Run the following command to generate the dataset with discourse relation labels

```
python construct.py
```

## Evaluation and Feature Selection

For the feature-based classifier using logistic regression, run the following command. Features used in the model can be selected by changing the lists `selected_feat_dialogue` and `selected_feat_dialogue_indirect`.

```
python eval.py
```

## Cite

```
@inproceedings{ma-etal-2019-implicit,
    title = "Implicit Discourse Relation Identification for Open-domain Dialogues",
    author = "Ma, Mingyu Derek  and
      Bowden, Kevin  and
      Wu, Jiaqi  and
      Cui, Wen  and
      Walker, Marilyn",
    booktitle = "Proceedings of the 57th Conference of the Association for Computational Linguistics",
    month = jul,
    year = "2019",
    address = "Florence, Italy",
    publisher = "Association for Computational Linguistics",
    url = "https://www.aclweb.org/anthology/P19-1065",
    pages = "666--672",
    abstract = "Discourse relation identification has been an active area of research for many years, and the challenge of identifying implicit relations remains largely an unsolved task, especially in the context of an open-domain dialogue system. Previous work primarily relies on a corpora of formal text which is inherently non-dialogic, i.e., news and journals. This data however is not suitable to handle the nuances of informal dialogue nor is it capable of navigating the plethora of valid topics present in open-domain dialogue. In this paper, we designed a novel discourse relation identification pipeline specifically tuned for open-domain dialogue systems. We firstly propose a method to automatically extract the implicit discourse relation argument pairs and labels from a dataset of dialogic turns, resulting in a novel corpus of discourse relation pairs; the first of its kind to attempt to identify the discourse relations connecting the dialogic turns in open-domain discourse. Moreover, we have taken the first steps to leverage the dialogue features unique to our task to further improve the identification of such relations by performing feature ablation and incorporating dialogue features to enhance the state-of-the-art model.",
}

@article{fainberg2018talking,
  title={Talking to myself: self-dialogues as data for conversational agents},
  author={Fainberg, Joachim and Krause, Ben and Dobre, Mihai and Damonte, Marco and Kahembwe, Emmanuel and Duma, Daniel and Webber, Bonnie and Fancellu, Federico},
  journal={arXiv preprint arXiv:1809.06641},
  year={2018}
}

@article{krause2017edina,
  title={Edina: Building an Open Domain Socialbot with Self-dialogues},
  author={Krause, Ben and Damonte, Marco and Dobre, Mihai and Duma, Daniel and Fainberg, Joachim and Fancellu, Federico and Kahembwe, Emmanuel and Cheng, Jianpeng and Webber, Bonnie},
  journal={Alexa Prize Proceedings},
  year={2017}
}
```