logicmoo's LogicMOO WebUI Setup Scripts/Sources

Directory structure and shared global scripts


=========

````bash

git remote add origin https://github.com/logicmoo/logicmoo_webui.git

git push -u origin master --force

(
mkdir -p linuxOnly
cd linuxOnly
git clone https://github.com/friguzzi/bddem.git
git clone https://github.com/friguzzi/cplint.git
git clone https://github.com/friguzzi/cplint_r.git
git clone https://github.com/JanWielemaker/hdt.git
git clone https://github.com/friguzzi/lbfgs
git clone https://github.com/friguzzi/mpi.git
git clone https://github.com/ArnaudFadja/phil.git
git clone https://github.com/ArnaudFadja/phil_datasets.git
git clone https://github.com/facebook/rocksdb
git clone https://github.com/rzese/trill.git

# @TODO title('Integrative statistics with R'). keywords([statistics,'R',bioinformatics,'machine learning']). home( 'http://stoics.org.uk/~nicos/sware/real' ).
cd ..
)


(
mkdir -p node_modules
cd node_modules
git clone https://github.com/ajaxorg/ace
git clone https://github.com/Microsoft/monaco-editor
git clone https://github.com/LogtalkDotOrg/ace ace-logtalk
cd ..
)

% git subtree add --prefix ClioPatria https://github.com/logicmoo/ClioPatria-filessytem-and-clausedb.git master --squash
git clone https://github.com/logicmoo/swish.git
git clone https://github.com/logicmoo/lps_corner.git

git clone https://github.com/logicmoo/ClioPatria-filessytem-and-clausedb.git ClioPatria

(
cd ClioPatria/cpack

git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/accurator.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/amalgame.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/annotation_service.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/autocompletion.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/cloud.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/cluster_search.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/cluster_search_ui.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/command.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/cpack_repository.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/ecdemo.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/EDM.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/find_resource.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/foaf.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/foaf_user.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/isearch.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/jquery.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/media_cache.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/opmv.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/owl.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/pirates.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/prov.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/rda_gr2.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/rdf-mt.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/skos.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/skos_browser.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/statistics.git
#git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/swish.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/tag_matcher.git
#git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/trill_on_swish.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/versioned_graph.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/void.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/xmlrdf.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/yaz.git
git clone http://cliopatria.swi-prolog.org/git/cpack-mirrors/yui3.git
git clone https://github.com/ClioPatria/image_annotation
git clone https://github.com/ClioPatria/rdf_qa
git clone https://github.com/ClioPatria/webaccess
cd ../..
)



