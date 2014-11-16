#!/bin/bash

( echo -e "[foreign_test].\n p(2),q([1,5,3,4],2,D),r(5,F),s(G,H,I,J),t(G,H,I,J),aa(position(2,4),XX), io(_), io(2), sio(SIO), \+ sio(1), f(field(2,3,Sum)).\nhalt.\n" | \
    swipl -q -g "assertz(user:file_search_path(library,'../../../../../../src/main/packages/assertions/prolog')),assertz(user:file_search_path(library,'../../../../../../src/main/packages/xlibrary/prolog'))" )
