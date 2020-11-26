
#
# export.dir, if not set, points by default to the directory this regulus exporter (rex) file is located.
# you may use ${export.dir} in filenames below to specify the output location of the regulus files.
#
# export.dir=/home/isidoros/arbanitis/speech/MedSLT2/Core/Regulus


#
# enable/disable export of multilingual definitions
#
export.multilingual.definition=true

#
# this points to the file holding definitions for all languages
#
export.multilingual.definition.file=${export.dir}/coreLexicon-defs.regulus

#
# add some more flags to control what to write out to the multilingual definition file
#

########################################################################################################

#
# comma seperated list of languages to export
# this list is case sensitive
#
export.languages=English, Finnish, Japanese, Swedish

#
# where to write lexicon and definition files
# ${lang} gets substituted by each of the language names defined in "export.languages"
#
export.languages.definitions=${export.dir}/${lang}-definitions.regulus
export.languages.lexicals=${export.dir}/${lang}-lexicon.regulus

#
# set to true to include language-specific ignore_feature definitions
#
export.language.definitions.include.ignore_feature=true

#
# set to true to to include language-specific definitions in definitions file
#
export.language.definitions.include.language_specific_values=true

#
# set language encoding for languages listed in "export.languages"
# if not specified defaults to java system encoding
#
# Arab.encoding=cp1256

#
# if a lexical entry has a non-empty 'latin_transliteration' entry a macro is written out instead of the lexical entry.
#
export.transliteration.macro=a



########################################################################################################

#
# comma seperated list of features (protege slots) which should not get exported
# in feature definition files
#
# Slots "agr", "entry_type", "surface_form" and "sem" get special treatment by the exporter.
#
export.ignore.features=agr, entry_type, surface_form, sem, latin_transliteration

# uncomment this to get debug messages in protege's output trace
# debug=true
