/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).

%:- op(100,xfx,' * ').
:- dynamic(fav/2).
:- discontiguous fav/2.
fav(A,B):- nonvar_or_ci(A),nonvar_or_ci(B), cls1,mmake, asserta(fav(A,B),Ref),!, call_cleanup(arc1(A),erase(Ref)).
%~ warn_skip(save_supertest(v('00576224'),'muarc_cache/00576224.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('00576224')))
%~ warn_skip(clear_saveable_test_info(v('423a55dc')))

:- discontiguous((fav_testcase/2,fav_testcase/3)).

fav_testcase(v('423a55dc')>trn+1,"

   _______________________       _______________________
  |                       |     |                       |
  |                       |     |                       |
  |                       |     |                       |
  |         6 6 6         |     | 6 6 6                 |
  |         6   6         |     |   6   6               |
  |         6   6         |     |     6   6             |
  |         6   6         |     |       6   6           |
  |         6 6 6         |     |         6 6 6         |
  |                       |     |                       |
  |                       |     |                       |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(11,10))          'Output' v('423a55dc') (grid(11,10))
",test_easy(findrot(_))).


%= fav(v('423a55dc'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/423a55dc.json']),filename(['/data/evaluation/423a55dc.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(5, 1) ']).
%~ warn_skip(save_supertest(v('423a55dc'),'muarc_cache/423a55dc.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('423a55dc')))
%~ warn_skip(clear_saveable_test_info(v('414297c0')))
fav_testcase(v('414297c0')>trn+1,"

   _________________________________________       _____________
  |                                         |     | 8 8 8 8 8 8 |
  |                                         |     | 8 2 8 8 8 8 |
  |                       2                 |     | 2 1 2 8 8 8 |
  |                     2 1 2               |     | 2 8 8 8 8 8 |
  |                     2           2   2   |     | 8 8 8 8 8 8 |
  |                                   3 2   |     | 8 8 8 8 8 8 |
  |   8 8 8 8 8 8                   2   2   |     | 8 8 2 8 2 8 |
  |   8 8 8 8 8 8                           |     | 8 8 8 4 2 8 |
  |   8 1 8 8 8 8                           |     | 8 8 2 2 2 8 |
  |   8 8 8 8 8 8         2   2             |       ¯¯¯¯¯¯¯¯¯¯¯¯¯
  |   8 8 8 8 8 8           4 2             |
  |   8 8 8 8 8 8         2 2 2             |
  |   8 8 8 8 8 8                           |
  |   8 8 8 4 8 8                           |
  |   8 8 8 8 8 8                           |
  |                                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(20,16))          'Output' v('414297c0') (grid(6,9))
",[indiv(colormass)]).


%= fav(v('414297c0'),[no_sol(i(complete),resize_grid(11,12,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([11,12]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/414297c0.json']),filename(['/data/evaluation/414297c0.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('414297c0'),'muarc_cache/414297c0.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('414297c0')))
%~ warn_skip(clear_saveable_test_info(v('40f6cd08')))
fav_testcase(v('40f6cd08')>trn+1,"

   _____________________________________________________________       _____________________________________________________________
  |                                                             |     |                                                             |
  |                                                             |     |                                                             |
  |       2 2 2 2 2 2 2 2 2 2 2                                 |     |       2 2 2 2 2 2 2 2 2 2 2                                 |
  |       2 2 2 2 2 2 2 2 2 2 2                                 |     |       2 2 2 2 2 2 2 2 2 2 2                                 |
  |       2 2 2 2 2 2 2 2 2 2 2                                 |     |       2 2 2 2 2 2 2 2 2 2 2                                 |
  |       6 6 6 6 6 6 6 6 2 2 2                                 |     |       6 6 6 6 6 6 6 6 2 2 2                                 |
  |       8 8 8 8 8 8 8 6 2 2 2                                 |     |       8 8 8 8 8 8 8 6 2 2 2                                 |
  |       8 8 8 8 8 8 8 6 2 2 2                                 |     |       8 8 8 8 8 8 8 6 2 2 2                                 |
  |       6 6 6 6 6 6 6 6 2 2 2           2 2 2 2 2 2 2 2 2     |     |       6 6 6 6 6 6 6 6 2 2 2           2 2 2 2 2 2 2 2 2     |
  |       2 2 2 2 2 2 2 2 2 2 2           2 2 2 2 2 2 2 2 2     |     |       2 2 2 2 2 2 2 2 2 2 2           2 2 2 2 2 2 2 2 2     |
  |       2 2 2 2 2 2 2 2 2 2 2           2 2 2 2 2 2 2 2 2     |     |       2 2 2 2 2 2 2 2 2 2 2           2 2 2 2 2 2 2 2 2     |
  |       2 2 2 2 2 2 2 2 2 2 2           2 2 2 2 2 2 2 2 2     |     |       2 2 2 2 2 2 2 2 2 2 2           6 6 6 6 6 6 2 2 2     |
  |                                       2 2 2 2 2 2 2 2 2     |     |                                       8 8 8 8 8 6 2 2 2     |
  |                                       2 2 2 2 2 2 2 2 2     |     |                                       6 6 6 6 6 6 2 2 2     |
  |                                       2 2 2 2 2 2 2 2 2     |     |                                       2 2 2 2 2 2 2 2 2     |
  |                                       2 2 2 2 2 2 2 2 2     |     |                                       2 2 2 2 2 2 2 2 2     |
  |                                       2 2 2 2 2 2 2 2 2     |     |                                       2 2 2 2 2 2 2 2 2     |
  |                                                             |     |                                                             |
  |                                                             |     |                                                             |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           8 8 8 8 8 8 8 8 8 8 8 8 6 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           8 8 8 8 8 8 8 8 8 8 8 8 6 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |
  |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |     |           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2                   |
  |                                                             |     |                                                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(30,30))          'Output' v('40f6cd08') (grid(30,30))
",[indiv(colormass),human(paste_subobjs)]).


%= fav(v('40f6cd08'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/40f6cd08.json']),filename(['/data/evaluation/40f6cd08.json']),-rotation_match,+shape_match,+mask_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('40f6cd08'),'muarc_cache/40f6cd08.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('40f6cd08')))
%~ warn_skip(clear_saveable_test_info(v('3f23242b')))
fav_testcase(v('3f23242b')>trn+1,"

   _____________________       _____________________
  |                     |     |                     |
  |                     |     |                     |
  |                     |     |     5 5 5 5 5       |
  |                     |     |     2   5   2       |
  |         3           |     |     2   3   2       |
  |                     |     |     2       2       |
  |                     |     | 2 2 8 8 8 8 8 2 2 2 |
  |                     |     |                     |
  |                     |     |                     |
  |                     |     |                     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,10))          'Output' v('3f23242b') (grid(10,10))
").


%= fav(v('3f23242b'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3f23242b.json']),filename(['/data/evaluation/3f23242b.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('3f23242b'),'muarc_cache/3f23242b.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3f23242b')))
%~ warn_skip(clear_saveable_test_info(v('3ee1011a')))
fav_testcase(v('3ee1011a')>trn+1,"

   _____________________________________________       _____________
  |                                             |     | 1 1 1 1 1 1 |
  |                                             |     | 1 3 3 3 3 1 |
  |                           1                 |     | 1 3 6 6 3 1 |
  |         3                 1                 |     | 1 3 6 6 3 1 |
  |         3                 1                 |     | 1 3 3 3 3 1 |
  |         3                 1                 |     | 1 1 1 1 1 1 |
  |         3                 1                 |       ¯¯¯¯¯¯¯¯¯¯¯¯¯
  |                           1                 |
  |                 6                           |
  |                 6                           |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
  |                                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(22,21))          'Output' v('3ee1011a') (grid(6,6))
").


%= fav(v('3ee1011a'),[no_sol(i(complete),resize_grid(5,5,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([5,5]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3ee1011a.json']),filename(['/data/evaluation/3ee1011a.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('3ee1011a'),'muarc_cache/3ee1011a.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3ee1011a')))
%~ warn_skip(clear_saveable_test_info(v('3ed85e70')))
fav_testcase(v('3ed85e70')>trn+1,"

   _____________________________________________________________       _____________________________________________________________
  | 3 3 3 3 3 3                                                 |     | 3 3 3 3 3 3                                                 |
  | 3 3 3 3 3 3                           2 2 2                 |     | 3 3 3 3 3 3           8 8 8 8 8       2 2 2                 |
  | 3 2 2 2 3 3             4 4 4         2 2 2                 |     | 3 2 2 2 3 3           8 4 4 4 8       2 1 2                 |
  | 3 2 1 2 3 3             4 4 4         2 2 2                 |     | 3 2 1 2 3 3           8 4 4 4 8       2 2 2         1 1 1   |
  | 3 2 2 2 3 3             4 4 4                         2     |     | 3 2 2 2 3 3           8 4 4 4 8                     1 2 1   |
  | 3 3 3 3 3 3                                                 |     | 3 3 3 3 3 3           8 8 8 8 8                     1 1 1   |
  | 3 3 3 3 3 3                                                 |     | 3 3 3 3 3 3                                                 |
  | 8 8 8 8 8 3                                                 |     | 8 8 8 8 8 3                                                 |
  | 8 4 4 4 8 3                                       1 1 1     |     | 8 4 4 4 8 3                         1 1 1         1 1 1     |
  | 8 4 4 4 8 3     8 8                   2           1 1 1     |     | 8 4 4 4 8 3     8 8     1 1 1       1 2 1         1 2 1     |
  | 8 4 4 4 8 3     8 8       2                       1 1 1     |     | 8 4 4 4 8 3     8 8     1 2 1       1 1 1         1 1 1     |
  | 8 8 8 8 8 3                                                 |     | 8 8 8 8 8 3             1 1 1                               |
  | 3 3 3 3 3 3               3 3 3                             |     | 3 3 3 3 3 3               3 3 3                             |
  | 3 3 3 3 3 3               3 3 3                             |     | 3 3 3 3 3 3               3 3 3                             |
  | 3 3 3 3 3 3               3 3 3                       8 8   |     | 3 3 3 3 3 3               3 3 3                       8 8   |
  | 3 3 3 3 3 3                                           8 8   |     | 3 3 3 3 3 3                         8 8 8 8 8         8 8   |
  | 3 3 3 3 3 3                           4 4 4                 |     | 3 3 3 3 3 3                         8 4 4 4 8               |
  | 3 3 3 3 3 3         1 1 1             4 4 4                 |     | 3 3 3 3 3 3         1 1 1           8 4 4 4 8               |
  | 3 3 3 3 3 3         1 1 1             4 4 4                 |     | 3 3 3 3 3 3         1 2 1           8 4 4 4 8               |
  | 3 1 1 1 3 3         1 1 1                                   |     | 3 1 1 1 3 3         1 1 1           8 8 8 8 8 1 1 1         |
  | 3 1 2 1 3 3                                     2           |     | 3 1 2 1 3 3                                   1 2 1         |
  | 3 1 1 1 3 3                                                 |     | 3 1 1 1 3 3                                   1 1 1         |
  | 3 3 3 3 3 3                                                 |     | 3 3 3 3 3 3                                                 |
  | 3 3 3 3 3 3                                                 |     | 3 3 3 3 3 3                                                 |
  | 3 3 3 3 3 3                             3 3 3               |     | 3 3 3 3 3 3                             3 3 3               |
  | 3 3 3 3 3 3           2 2 2             3 3 3       2 2 2   |     | 3 3 3 3 3 3           2 2 2             3 3 3       2 2 2   |
  | 3 3 3 3 3 3           2 2 2             3 3 3       2 2 2   |     | 3 3 3 3 3 3           2 1 2     1 1 1   3 3 3       2 1 2   |
  | 3 3 3 3 3 3           2 2 2       2                 2 2 2   |     | 3 3 3 3 3 3           2 2 2     1 2 1               2 2 2   |
  | 3 3 3 3 3 3                                                 |     | 3 3 3 3 3 3                     1 1 1                       |
  | 3 3 3 3 3 3                                                 |     | 3 3 3 3 3 3                                                 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(30,30))          'Output' v('3ed85e70') (grid(30,30))
").


%= fav(v('3ed85e70'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3ed85e70.json']),filename(['/data/evaluation/3ed85e70.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('3ed85e70'),'muarc_cache/3ed85e70.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3ed85e70')))
%~ warn_skip(clear_saveable_test_info(v('3d31c5b3')))
fav_testcase(v('3d31c5b3')>trn+1,"

   _____________       _____________
  | 5 5   5 5 5 |     | 5 5 4 5 5 5 |
  |   5   5   5 |     |   5 8 5 8 5 |
  |       5 5   |     | 2 4 2 5 5 4 |
  |   4 4   4   |       ¯¯¯¯¯¯¯¯¯¯¯¯¯
  |           4 |
  |   4   4   4 |
  | 2 2 2       |
  |   2 2   2   |
  | 2 2 2   2   |
  | 8   8 8 8 8 |
  |     8 8 8 8 |
  |       8     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(6,12))           'Output' v('3d31c5b3') (grid(6,3))
").


%= fav(v('3d31c5b3'),[no_sol(i(complete),resize_grid(6,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([6,3]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3d31c5b3.json']),filename(['/data/evaluation/3d31c5b3.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(6, 1) ']).
%~ warn_skip(save_supertest(v('3d31c5b3'),'muarc_cache/3d31c5b3.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3d31c5b3')))
%~ warn_skip(clear_saveable_test_info(v('3b4c2228')))
fav_testcase(v('3b4c2228')>trn+1,"

   ___________       _______
  |   3 3     |     | 1     |
  |   3 3     |     |       |
  |           |     |       |
  | 2 2     2 |       ¯¯¯¯¯¯¯
  | 2 2       |
  |       2 2 |
  |       2 2 |
   ¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(5,7))            'Output' v('3b4c2228') (grid(3,3))
").


%= fav(v('3b4c2228'),[no_sol(i(complete),resize_grid(3,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3b4c2228.json']),filename(['/data/evaluation/3b4c2228.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(5, 2) ']).
%~ warn_skip(save_supertest(v('3b4c2228'),'muarc_cache/3b4c2228.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3b4c2228')))
%~ warn_skip(clear_saveable_test_info(v('3a301edc')))
fav_testcase(v('3a301edc')>trn+1,"

   _____________________________       _____________________________
  |                             |     |                             |
  |                             |     |                             |
  |                             |     |     1 1 1 1 1 1 1           |
  |       3 3 3 3 3             |     |     1 3 3 3 3 3 1           |
  |       3 3 3 3 3             |     |     1 3 3 3 3 3 1           |
  |       3 3 1 3 3             |     |     1 3 3 1 3 3 1           |
  |       3 3 3 3 3             |     |     1 3 3 3 3 3 1           |
  |       3 3 3 3 3             |     |     1 3 3 3 3 3 1           |
  |                             |     |     1 1 1 1 1 1 1           |
  |                             |     |                             |
  |                             |     |                             |
  |                             |     |                             |
  |                             |     |                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(14,13))          'Output' v('3a301edc') (grid(14,13))
").


%= fav(v('3a301edc'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3a301edc.json']),filename(['/data/evaluation/3a301edc.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(5, 1) ']).
%~ warn_skip(save_supertest(v('3a301edc'),'muarc_cache/3a301edc.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3a301edc')))
%~ warn_skip(clear_saveable_test_info(v('3979b1a8')))
fav_testcase(v('3979b1a8')>trn+1,"

   ___________       _____________________
  | 2 3 3 3 2 |     | 2 3 3 3 2 2 5 3 2 5 |
  | 3 3 5 3 3 |     | 3 3 5 3 3 2 5 3 2 5 |
  | 3 5 5 5 3 |     | 3 5 5 5 3 2 5 3 2 5 |
  | 3 3 5 3 3 |     | 3 3 5 3 3 2 5 3 2 5 |
  | 2 3 3 3 2 |     | 2 3 3 3 2 2 5 3 2 5 |
   ¯¯¯¯¯¯¯¯¯¯¯      | 2 2 2 2 2 5 5 3 2 5 |
                    | 5 5 5 5 5 5 3 3 2 5 |
                    | 3 3 3 3 3 3 3 2 2 5 |
                    | 2 2 2 2 2 2 2 2 5 5 |
                    | 5 5 5 5 5 5 5 5 5 3 |
                     ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(5,5))            'Output' v('3979b1a8') (grid(10,10))
").


%= fav(v('3979b1a8'),[no_sol(i(complete),resize_grid(10,10,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([10,10]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3979b1a8.json']),filename(['/data/evaluation/3979b1a8.json']),-shape_match,-rotation_match,-mask_match,+color_match,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('3979b1a8'),'muarc_cache/3979b1a8.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3979b1a8')))
%~ warn_skip(clear_saveable_test_info(v('37d3e8b2')))
fav_testcase(v('37d3e8b2')>trn+1,"

   ___________________________________       ___________________________________
  |                                   |     |                                   |
  |     8 8 8 8 8 8       8 8 8 8 8   |     |     2 2 2 2 2 2       3 3 3 3 3   |
  |     8     8   8       8   8 8 8   |     |     2     2   2       3   3 3 3   |
  |     8     8 8 8       8 8 8   8   |     |     2     2 2 2       3 3 3   3   |
  |     8 8 8 8 8 8       8   8 8 8   |     |     2 2 2 2 2 2       3   3 3 3   |
  |                       8 8 8 8 8   |     |                       3 3 3 3 3   |
  |           8 8 8 8                 |     |           2 2 2 2                 |
  |           8     8     8 8 8 8     |     |           2     2     1 1 1 1     |
  |           8 8 8 8     8     8     |     |           2 2 2 2     1     1     |
  |           8 8   8     8 8 8 8     |     |           2 2   2     1 1 1 1     |
  |           8 8 8 8     8 8 8 8     |     |           2 2 2 2     1 1 1 1     |
  |                                   |     |                                   |
  |   8 8 8 8 8       8 8 8 8 8 8 8   |     |   2 2 2 2 2       7 7 7 7 7 7 7   |
  |   8 8 8   8       8   8 8 8   8   |     |   2 2 2   2       7   7 7 7   7   |
  |   8   8 8 8       8 8 8   8   8   |     |   2   2 2 2       7 7 7   7   7   |
  |   8 8 8 8 8       8   8 8 8 8 8   |     |   2 2 2 2 2       7   7 7 7 7 7   |
  |                   8 8 8 8 8 8 8   |     |                   7 7 7 7 7 7 7   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(17,17))          'Output' v('37d3e8b2') (grid(17,17))
").


%= fav(v('37d3e8b2'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/37d3e8b2.json']),filename(['/data/evaluation/37d3e8b2.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('37d3e8b2'),'muarc_cache/37d3e8b2.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('37d3e8b2')))
%~ warn_skip(clear_saveable_test_info(v('358ba94e')))
fav_testcase(v('358ba94e')>trn+1,"

   _______________________________________       ___________
  |                             8 8 8 8 8 |     | 8 8 8 8 8 |
  |   8 8 8 8 8                 8   8 8 8 |     | 8   8   8 |
  |   8   8   8     8 8 8 8 8   8 8 8   8 |     | 8 8 8 8 8 |
  |   8 8 8 8 8     8   8   8   8   8 8 8 |     | 8   8   8 |
  |   8   8 8 8     8 8 8 8 8   8 8 8 8 8 |     | 8 8 8 8 8 |
  |   8 8 8 8 8     8 8 8   8             |       ¯¯¯¯¯¯¯¯¯¯¯
  |                 8 8 8 8 8             |
  |                                       |
  |                         8 8 8 8 8     |
  |       8 8 8 8 8         8 8 8   8     |
  |       8   8   8         8   8 8 8     |
  |       8 8 8 8 8         8 8 8   8     |
  |       8   8   8         8 8 8 8 8     |
  |       8 8 8 8 8                       |
  |                                       |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(19,15))          'Output' v('358ba94e') (grid(5,5))
").


%= fav(v('358ba94e'),[no_sol(i(complete),resize_grid(5,5,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([5,5]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/358ba94e.json']),filename(['/data/evaluation/358ba94e.json']),-shape_match,-rotation_match,-mask_match,+color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('358ba94e'),'muarc_cache/358ba94e.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('358ba94e')))
%~ warn_skip(clear_saveable_test_info(v('351d6448')))
fav_testcase(v('351d6448')>trn+1,"

   ___________________________       ___________________________
  |                           |     |                           |
  | 1 1                       |     | 1 1 1 1 1 1 1 1 1 1       |
  |                           |     |                           |
  | 5 5 5 5 5 5 5 5 5 5 5 5 5 |       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
  |                           |
  | 1 1 1 1                   |
  |                           |
  | 5 5 5 5 5 5 5 5 5 5 5 5 5 |
  |                           |
  | 1 1 1 1 1 1               |
  |                           |
  | 5 5 5 5 5 5 5 5 5 5 5 5 5 |
  |                           |
  | 1 1 1 1 1 1 1 1           |
  |                           |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(13,15))          'Output' v('351d6448') (grid(13,3))
").


%= fav(v('351d6448'),[no_sol(i(complete),resize_grid(13,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([13,3]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/351d6448.json']),filename(['/data/evaluation/351d6448.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('351d6448'),'muarc_cache/351d6448.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('351d6448')))
%~ warn_skip(clear_saveable_test_info(v('34b99a2b')))
fav_testcase(v('34b99a2b')>trn+1,"

   ___________________       _________
  |   8     4 5   5   |     | 2 2 2   |
  |   8   8 4 5   5 5 |     | 2 2 2   |
  |   8   8 4       5 |     |   2     |
  |   8   8 4   5   5 |     |         |
  |       8 4     5   |     |     2 2 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(9,5))            'Output' v('34b99a2b') (grid(4,5))
").


%= fav(v('34b99a2b'),[no_sol(i(complete),resize_grid(4,5,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([4,5]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/34b99a2b.json']),filename(['/data/evaluation/34b99a2b.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('34b99a2b'),'muarc_cache/34b99a2b.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('34b99a2b')))
%~ warn_skip(clear_saveable_test_info(v('3490cc26')))
fav_testcase(v('3490cc26')>trn+1,"

   _________________________________________________________       _________________________________________________________
  |                                                         |     |                                                         |
  |   8 8                                                   |     |   8 8                                                   |
  |   8 8                                                   |     |   8 8                                                   |
  |                                                         |     |                                                         |
  |                                                         |     |                                                         |
  |                                                         |     |                                                         |
  |       8 8                   8 8                         |     |       8 8 7 7 7 7 7 7 7 7 7 8 8                         |
  |       8 8                   8 8                         |     |       8 8 7 7 7 7 7 7 7 7 7 8 8                         |
  |                                                         |     |       7 7                   7 7                         |
  |                                                         |     |       7 7                   7 7                         |
  |                                                         |     |       7 7                   7 7                         |
  |       8 8     8 8           8 8                         |     |       8 8 7 7 8 8           8 8                         |
  |       8 8     8 8           8 8                         |     |       8 8 7 7 8 8           8 8                         |
  |                                                         |     |               7 7           7 7                         |
  |                                                         |     |               7 7           7 7                         |
  |                                                 8 8     |     |               7 7           7 7                 8 8     |
  |               8 8                               8 8     |     |               8 8           7 7                 8 8     |
  |               8 8                                       |     |               8 8           7 7                         |
  |                                                         |     |               7 7           7 7                         |
  |                                                         |     |               7 7           7 7                         |
  |                                                         |     |               7 7           7 7                         |
  |     2 2       8 8           8 8           8 8           |     |     2 2 7 7 7 8 8           8 8 7 7 7 7 7 8 8           |
  |     2 2       8 8           8 8           8 8           |     |     2 2 7 7 7 8 8           8 8 7 7 7 7 7 8 8           |
  |                                                         |     |                                                         |
  |                                                         |     |                                                         |
  |                                                         |     |                                                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(28,26))          'Output' v('3490cc26') (grid(28,26))
").


%= fav(v('3490cc26'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3490cc26.json']),filename(['/data/evaluation/3490cc26.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('3490cc26'),'muarc_cache/3490cc26.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3490cc26')))
%~ warn_skip(clear_saveable_test_info(v('33b52de3')))
fav_testcase(v('33b52de3')>trn+1,"

   _______________________________________________       _______________________________________________
  |                                               |     |                                               |
  |             5 5 5   5 5 5   5 5 5   5 5 5     |     |             1 1 1   8 8 8   1 1 1   8 8 8     |
  |               5       5       5       5       |     |               1       8       1       8       |
  |   1 8 1 8   5 5 5   5 5 5   5 5 5   5 5 5     |     |   1 8 1 8   1 1 1   8 8 8   1 1 1   8 8 8     |
  |   8 8 1 1                                     |     |   8 8 1 1                                     |
  |   1 1 4 1   5 5 5   5 5 5   5 5 5   5 5 5     |     |   1 1 4 1   8 8 8   8 8 8   1 1 1   1 1 1     |
  |   1 1 4 4     5       5       5       5       |     |   1 1 4 4     8       8       1       1       |
  |             5 5 5   5 5 5   5 5 5   5 5 5     |     |             8 8 8   8 8 8   1 1 1   1 1 1     |
  |                                               |     |                                               |
  |             5 5 5   5 5 5   5 5 5   5 5 5     |     |             1 1 1   1 1 1   4 4 4   1 1 1     |
  |               5       5       5       5       |     |               1       1       4       1       |
  |             5 5 5   5 5 5   5 5 5   5 5 5     |     |             1 1 1   1 1 1   4 4 4   1 1 1     |
  |                                               |     |                                               |
  |             5 5 5   5 5 5   5 5 5   5 5 5     |     |             1 1 1   1 1 1   4 4 4   4 4 4     |
  |               5       5       5       5       |     |               1       1       4       4       |
  |             5 5 5   5 5 5   5 5 5   5 5 5     |     |             1 1 1   1 1 1   4 4 4   4 4 4     |
  |                                               |     |                                               |
  |                                               |     |                                               |
  |                                               |     |                                               |
  |                                               |     |                                               |
  |                                               |     |                                               |
  |                                               |     |                                               |
  |                                               |     |                                               |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(23,23))          'Output' v('33b52de3') (grid(23,23))
").


%= fav(v('33b52de3'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/33b52de3.json']),filename(['/data/evaluation/33b52de3.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('33b52de3'),'muarc_cache/33b52de3.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('33b52de3')))
%~ warn_skip(clear_saveable_test_info(v('3391f8c0')))
fav_testcase(v('3391f8c0')>trn+1,"

   _________________________       _________________________
  |                         |     |                         |
  |   2 2   2 2   2 2       |     |     3     3     3       |
  |   2     2     2         |     |   3 3   3 3   3 3       |
  |                         |     |                         |
  |         2 2     3       |     |           3   2 2       |
  |         2     3 3       |     |         3 3   2         |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(12,9))           'Output' v('3391f8c0') (grid(12,9))
").


%= fav(v('3391f8c0'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3391f8c0.json']),filename(['/data/evaluation/3391f8c0.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('3391f8c0'),'muarc_cache/3391f8c0.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3391f8c0')))
%~ warn_skip(clear_saveable_test_info(v('332efdb3')))
fav_testcase(v('332efdb3')>trn+1,"

   ___________       ___________
  |           |     | 1 1 1 1 1 |
  |           |     | 1   1   1 |
  |           |     | 1 1 1 1 1 |
  |           |     | 1   1   1 |
  |           |     | 1 1 1 1 1 |
   ¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(5,5))            'Output' v('332efdb3') (grid(5,5))
").


%= fav(v('332efdb3'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/332efdb3.json']),filename(['/data/evaluation/332efdb3.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('332efdb3'),'muarc_cache/332efdb3.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('332efdb3')))
%~ warn_skip(clear_saveable_test_info(v('32e9702f')))
fav_testcase(v('32e9702f')>trn+1,"

   _________________       _________________
  |                 |     | 5 5 5 5 5 5 5 5 |
  |     3 3 3 3     |     | 5 3 3 3 3 5 5 5 |
  |                 |     | 5 5 5 5 5 5 5 5 |
  |                 |     | 5 5 5 5 5 5 5 5 |
  |   3 3           |     | 3 3 5 5 5 5 5 5 |
  |                 |     | 5 5 5 5 5 5 5 5 |
  |                 |     | 5 5 5 5 5 5 5 5 |
  |                 |     | 5 5 5 5 5 5 5 5 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(8,8))            'Output' v('32e9702f') (grid(8,8))
").


%= fav(v('32e9702f'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/32e9702f.json']),filename(['/data/evaluation/32e9702f.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('32e9702f'),'muarc_cache/32e9702f.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('32e9702f')))
%~ warn_skip(clear_saveable_test_info(v('31d5ba1a')))
fav_testcase(v('31d5ba1a')>trn+1,"

   ___________       ___________
  | 9     9 9 |     | 6   6   6 |
  |           |     | 6 6 6     |
  |     9   9 |     | 6         |
  |     4 4   |       ¯¯¯¯¯¯¯¯¯¯¯
  | 4 4 4     |
  | 4   4   4 |
   ¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(5,6))            'Output' v('31d5ba1a') (grid(5,3))
").


%= fav(v('31d5ba1a'),[no_sol(i(complete),resize_grid(5,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([5,3]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/31d5ba1a.json']),filename(['/data/evaluation/31d5ba1a.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(5, 2) ']).
%~ warn_skip(save_supertest(v('31d5ba1a'),'muarc_cache/31d5ba1a.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('31d5ba1a')))
%~ warn_skip(clear_saveable_test_info(v('31adaf00')))
fav_testcase(v('31adaf00')>trn+1,"

   _____________________       _____________________
  |   5     5           |     |   5 1 1 5           |
  | 5 5       5 5   5   |     | 5 5 1 1   5 5   5   |
  |       5 5     5 5 5 |     | 1 1   5 5     5 5 5 |
  |     5   5 5     5   |     | 1 1 5   5 5 1 1 5   |
  |   5             5   |     |   5   1 1   1 1 5   |
  | 5   5     5 5 5   5 |     | 5   5 1 1 5 5 5   5 |
  |       5   5 5   5   |     | 1 1   5   5 5   5   |
  |     5   5 5 5       |     | 1 1 5   5 5 5       |
  | 5   5 5   5 5   5   |     | 5   5 5   5 5   5   |
  |               5     |     |               5     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,10))          'Output' v('31adaf00') (grid(10,10))
").


%= fav(v('31adaf00'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/31adaf00.json']),filename(['/data/evaluation/31adaf00.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('31adaf00'),'muarc_cache/31adaf00.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('31adaf00')))
%~ warn_skip(clear_saveable_test_info(v('319f2597')))
fav_testcase(v('319f2597')>trn+1,"

   _________________________________________       _________________________________________
  | 5 2 5 2 5 4 3 8 2 7 3 7 5 6 2 1 9 3 2 1 |     | 5 2 5 2 5 4     2 7 3 7 5 6 2 1 9 3 2 1 |
  | 1 2 3 6 5 2 2 5 8 9 8 5 8 7 9 2 6 5 5 5 |     | 1 2 3 6 5 2 2   8 9 8 5 8 7 9 2 6 5 5 5 |
  | 3 1 9 7 9 8 8 7 1 9 7 3 9 7 5 9 8 4 9 8 |     | 3 1 9 7 9 8     1 9 7 3 9 7 5 9 8 4 9 8 |
  | 5 7 7 8 3 4 4 4 4 6 2 9 4 3 6 8 4 6 7 1 |     | 5 7 7 8 3 4     4 6 2 9 4 3 6 8 4 6 7 1 |
  | 6 3 8 3 5 9 7 5 4 6 1 9 3 9 7 7 6 7 8 1 |     | 6 3 8 3 5 9     4 6 1 9 3 9 7 7 6 7 8 1 |
  | 6 2 4 9 8 3 9 1 4 8 9 9 6 5 9 1 9 7 5 7 |     | 6 2 4 9 8 3     4 8 9 9 6 5 9 1 9 7 5 7 |
  | 6 2 8 3 4 6 4 8 9 3 9 3 6 9 2 2 9 1 9 3 |     | 6 2 8 3 4 6     9 3 9 3 6 9 2 2 9 1 9 3 |
  | 2 4 7 7 8 1 4 7 6 2 8 9 8 8 7 4 8 4 9 1 |     | 2 4 7 7 8 1     6 2 8 9 8 8 7 4 8 4 9 1 |
  | 2 1 5 9 2 6 8 3 6 4 5 8 6 3 1 4 5 1 5 1 |     | 2 1 5 9 2 6     6 4 5 8 6 3 1 4 5 1 5 1 |
  | 9 4 9 5 1 2 8 2 1 4 2 9 9 6 1 9 9 7 2 1 |     | 9 4 9 5 1 2   2 1 4 2 9 9 6 1 9 9 7 2 1 |
  | 6 6 2 3 7 3 7 5 4 3 2 4 4 7 7 7 6 7 6 7 |     | 6 6 2 3 7 3     4 3 2 4 4 7 7 7 6 7 6 7 |
  | 2 4 3 1 4 8     9 6 3 2 4 4 8 7 2 9 4 2 |     | 2                     2         2     2 |
  | 3 5 7 8 2 4     1 6 4 7 4 7 2 3 9 4 5 2 |     |         2                   2         2 |
  | 8 1 4 3 9 6 9 9 8 5 4 3 5 2 6 8 9 9 4 8 |     | 8 1 4 3 9 6     8 5 4 3 5 2 6 8 9 9 4 8 |
  | 1 2 6 9 8 9 1 4 3 3 6 2 3 7 3 1 8 1 4 5 |     | 1 2 6 9 8 9     3 3 6 2 3 7 3 1 8 1 4 5 |
  | 3 8 4 4 4 9 6 1 6 7 9 4 2 6 2 9 3 1 5 1 |     | 3 8 4 4 4 9     6 7 9 4 2 6 2 9 3 1 5 1 |
  | 2 7 5 8 8 8 6 3 4 6 3 7 9 2 1 1 7 2 5 9 |     | 2 7 5 8 8 8     4 6 3 7 9 2 1 1 7 2 5 9 |
  | 2 1 7 2 1 3 5 5 3 6 2 8 3 6 9 5 5 9 8 4 |     | 2 1 7 2 1 3     3 6 2 8 3 6 9 5 5 9 8 4 |
  | 3 3 3 6 6 3 6 5 9 4 7 2 4 4 7 7 6 1 2 9 |     | 3 3 3 6 6 3     9 4 7 2 4 4 7 7 6 1 2 9 |
  | 2 5 8 9 7 9 7 2 3 2 2 6 6 7 9 8 9 1 1 6 |     | 2 5 8 9 7 9   2 3 2 2 6 6 7 9 8 9 1 1 6 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(20,20))          'Output' v('319f2597') (grid(20,20))
").


%= fav(v('319f2597'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/319f2597.json']),filename(['/data/evaluation/319f2597.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('319f2597'),'muarc_cache/319f2597.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('319f2597')))
%~ warn_skip(clear_saveable_test_info(v('3194b014')))
fav_testcase(v('3194b014')>trn+1,"

   _________________________________________       _______
  | 8 8 8 8 8   1   1 1 8 1 1 1           1 |     | 3 3 3 |
  | 1         8 1 1       8   5 5 5 5 5 8 1 |     | 3 3 3 |
  |           8 1     8 1 1 1 5 5 5 5 5 8   |     | 3 3 3 |
  | 1 8   1 8     8 8 8 8 1 8 5 5 5 5 5 1   |       ¯¯¯¯¯¯¯
  |   8   9 9 9 9 8           5 5 5 5 5 8   |
  | 8 1 8 9 9 9 9 8 1 1   1 1   8   8 8   8 |
  |       9 9 9 9   1 1 8 8 3 3 8 1 1     1 |
  | 8 1 1 8 1 8   1       3 3 3 1   8 1 8 8 |
  |   1 8 8 1 1   8 8 3 3 3 3 3 8     8 1   |
  |   1 1   1       8 3 3 3 3 3 1 1 8 8 1   |
  | 8   8   8         3 3 3 3 3 1 1 1   8 8 |
  |         8 1 1 1 1 3 3 3 3 3 1 1   1 8 1 |
  |   8 8   8 8 1 8   3 3 3 8 1 1           |
  |     8 8     8   1     1       8 1 1 1   |
  |     1   1   1 8 8 1     8   1   1 1     |
  |   4 4 4 4 4 8 4       1   8   8   1 8   |
  | 1 4 4 4 4 4 4 4   1 1   8         8 1 8 |
  | 1 4 4 4 4 4 1 1   1   1 1         1   8 |
  |   1       1 8 1   8   1     8     8 1   |
  | 8   1     1   8   1 1   1 8   8     1   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(20,20))          'Output' v('3194b014') (grid(3,3))
").


%= fav(v('3194b014'),[no_sol(i(complete),resize_grid(3,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/3194b014.json']),filename(['/data/evaluation/3194b014.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('3194b014'),'muarc_cache/3194b014.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('3194b014')))
%~ warn_skip(clear_saveable_test_info(v('310f3251')))
fav_testcase(v('310f3251')>trn+1,"

   _______       ___________________
  |       |     |   2     2     2   |
  |     6 |     |     6     6     6 |
  | 6     |     | 6     6     6     |
   ¯¯¯¯¯¯¯      |   2     2     2   |
                |     6     6     6 |
                | 6     6     6     |
                |   2     2     2   |
                |     6     6     6 |
                | 6     6     6     |
                 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(3,3))            'Output' v('310f3251') (grid(9,9))
").


%= fav(v('310f3251'),[no_sol(i(complete),resize_grid(6,6,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([6,6]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/310f3251.json']),filename(['/data/evaluation/310f3251.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('310f3251'),'muarc_cache/310f3251.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('310f3251')))
%~ warn_skip(clear_saveable_test_info(v('2f0c5170')))
fav_testcase(v('2f0c5170')>trn+1,"

   _____________________________________________       ___________
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |     |     4 4   |
  | 8 8 8 8 8 8 8 8 8 8 8           8 8 8 8 8 8 |     |   4 4 2 4 |
  | 8 8 8 8 8 8 8 8 8 8 8       2   8 8 8 8 8 8 |     |     4 4   |
  | 8 8 8 8 8 8 8 8 8 8 8           8 8 8 8 8 8 |     |           |
  | 8 8 8 8 8 8 8 8 8 8 8           8 8 8 8 8 8 |     |           |
  | 8 8 8 8 8 8 8 8 8 8 8           8 8 8 8 8 8 |       ¯¯¯¯¯¯¯¯¯¯¯
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8             8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8     4 4     8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8   4 4 2 4   8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8     4 4     8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(22,21))          'Output' v('2f0c5170') (grid(5,5))
").


%= fav(v('2f0c5170'),[no_sol(i(complete),resize_grid(9,9,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([9,9]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2f0c5170.json']),filename(['/data/evaluation/2f0c5170.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('2f0c5170'),'muarc_cache/2f0c5170.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2f0c5170')))
%~ warn_skip(clear_saveable_test_info(v('2c737e39')))
fav_testcase(v('2c737e39')>trn+1,"

   _________________________       _________________________
  |                         |     |                         |
  |                         |     |                         |
  |     3 3                 |     |     3 3                 |
  |     2 2                 |     |     2 2                 |
  |     2 5                 |     |     2 5                 |
  |                         |     |                         |
  |                         |     |             3 3         |
  |                         |     |             2 2         |
  |               5         |     |             2           |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(12,12))          'Output' v('2c737e39') (grid(12,12))
").


%= fav(v('2c737e39'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2c737e39.json']),filename(['/data/evaluation/2c737e39.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('2c737e39'),'muarc_cache/2c737e39.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2c737e39')))
%~ warn_skip(clear_saveable_test_info(v('2c0b0aff')))
fav_testcase(v('2c0b0aff')>trn+1,"

   _________________________________________________       ___________________
  |                                                 |     | 8 8 3 8 8 8 8 3 8 |
  |   3 8 8 3 8 8 8 3 8     3 8 8 8 3 8 8 8 3 8     |     | 8 3 3 3 8 8 3 3 3 |
  |   8 8 3 3 3 8 8 8 3     8 8 3 8 8 8 8 3 3 3     |     | 8 8 3 8 8 8 8 3 8 |
  |   8 8 8 3 8 8 8 8 8     8 3 3 3 8 8 8 8 3 8     |     | 3 8 8 8 3 8 8 8 8 |
  |   8 3 8 8 8 8 3 8 8     8 8 3 8 3 8 3 8 8 8     |     | 8 3 8 8 8 8 3 8 3 |
  |   8 8 8 3 8 3 3 3 8     8 8 8 8 8 8 8 8 3 3     |     | 3 3 3 8 8 3 3 3 8 |
  |   3 8 8 8 8 8 3 8 8     8 3 8 3 8 8 3 8 8 8     |     | 8 3 8 8 8 8 3 8 8 |
  |   8 8 3 8 8 8 8 3 8     8 8 8 8 8 3 3 3 8 8     |     | 8 8 8 8 3 8 8 8 8 |
  |                         8 8 3 8 8 8 3 8 8 8     |       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
  |                         8 8 8 8 8 8 8 8 3 8     |
  |                                                 |
  |                                                 |
  |   8 8 3 8 8 8 8 3 8                             |
  |   8 3 3 3 8 8 3 3 3   8 8 3 8 8 8 8 8 8 8 8     |
  |   8 8 3 8 8 8 8 3 8   8 8 8 3 8 8 8 8 3 3 8     |
  |   3 8 8 8 3 8 8 8 8   8 8 3 3 3 8 8 8 8 8 8     |
  |   8 3 8 8 8 8 3 8 3   8 8 8 3 8 8 8 3 8 8 3     |
  |   3 3 3 8 8 3 3 3 8   3 8 8 8 8 8 3 3 3 8 8     |
  |   8 3 8 8 8 8 3 8 8   8 8 3 8 3 8 8 3 8 8 8     |
  |   8 8 8 8 3 8 8 8 8   8 3 3 3 8 8 8 8 8 3 8     |
  |                       8 8 3 8 8 8 3 3 8 8 8     |
  |                                                 |
  |                                                 |
  |                                                 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(24,24))          'Output' v('2c0b0aff') (grid(9,8))
").


%= fav(v('2c0b0aff'),[no_sol(i(complete),resize_grid(8,7,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([8,7]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2c0b0aff.json']),filename(['/data/evaluation/2c0b0aff.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('2c0b0aff'),'muarc_cache/2c0b0aff.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2c0b0aff')))
%~ warn_skip(clear_saveable_test_info(v('2b01abd0')))
fav_testcase(v('2b01abd0')>trn+1,"

   _________________________       _________________________
  |             1           |     |             1           |
  |             1           |     |             1           |
  |             1   8   8   |     |     8   8   1   4   4   |
  |             1     8 4   |     |     4 8     1     4 8   |
  |             1   8 8 8   |     |     8 8 8   1   4 4 4   |
  |             1           |     |             1           |
  |             1           |     |             1           |
  |             1           |     |             1           |
  |             1           |     |             1           |
  |             1           |     |             1           |
  |             1           |     |             1           |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(12,11))          'Output' v('2b01abd0') (grid(12,11))
").


%= fav(v('2b01abd0'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2b01abd0.json']),filename(['/data/evaluation/2b01abd0.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('2b01abd0'),'muarc_cache/2b01abd0.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2b01abd0')))
%~ warn_skip(clear_saveable_test_info(v('2a5f8217')))
fav_testcase(v('2a5f8217')>trn+1,"

   ___________________       ___________________
  |                   |     |                   |
  |   1 1 1     1     |     |   6 6 6     7     |
  |   1   1     1 1   |     |   6   6     7 7   |
  |                   |     |                   |
  |               9 9 |     |               9 9 |
  |     1 1         9 |     |     9 9         9 |
  |       1   7       |     |       9   7       |
  | 6 6 6     7 7     |     | 6 6 6     7 7     |
  | 6   6             |     | 6   6             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(9,9))            'Output' v('2a5f8217') (grid(9,9))
").


%= fav(v('2a5f8217'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2a5f8217.json']),filename(['/data/evaluation/2a5f8217.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('2a5f8217'),'muarc_cache/2a5f8217.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2a5f8217')))
%~ warn_skip(clear_saveable_test_info(v('29700607')))
fav_testcase(v('29700607')>trn+1,"

   _________________________       _________________________
  |         5 3 8           |     |         5 3 8           |
  |                         |     |         5 3 8           |
  |                         |     |         5 3 8           |
  |                         |     |         5 3 8           |
  | 5                       |     | 5 5 5 5 5 3 8           |
  |                       8 |     |           3 8 8 8 8 8 8 |
  |                         |     |           3             |
  |                         |     |           3             |
  | 3                       |     | 3 3 3 3 3 3             |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(12,12))          'Output' v('29700607') (grid(12,12))
").


%= fav(v('29700607'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/29700607.json']),filename(['/data/evaluation/29700607.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('29700607'),'muarc_cache/29700607.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('29700607')))
%~ warn_skip(clear_saveable_test_info(v('292dd178')))
fav_testcase(v('292dd178')>trn+1,"

   ___________________       ___________________
  | 5 5 5 5 5 5 5 5 5 |     | 5 5 5 5 5 5 5 5 5 |
  | 5 5 5 5 5 5 5 5 5 |     | 5 5 5 5 5 5 5 5 5 |
  | 5 5 1 1 1 1 5 5 5 |     | 5 5 1 1 1 1 5 5 5 |
  | 5 5 1 5 5 1 5 5 5 |     | 5 5 1 2 2 1 5 5 5 |
  | 5 5 5 5 5 1 5 5 5 |     | 2 2 2 2 2 1 5 5 5 |
  | 5 5 1 1 1 1 5 5 5 |     | 5 5 1 1 1 1 5 5 5 |
  | 5 5 5 5 5 5 5 5 5 |     | 5 5 5 5 5 5 5 5 5 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(9,7))            'Output' v('292dd178') (grid(9,7))
").


%= fav(v('292dd178'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/292dd178.json']),filename(['/data/evaluation/292dd178.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('292dd178'),'muarc_cache/292dd178.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('292dd178')))
%~ warn_skip(clear_saveable_test_info(v('281123b4')))
fav_testcase(v('281123b4')>trn+1,"

   _______________________________________       _________
  |     8 8 3 5 5     3   9 9 9 3 4   4   |     | 4 9 9 9 |
  | 8 8 8 8 3   5   5 3   9   9 3 4   4   |     | 4 9 4 9 |
  | 8 8   8 3 5   5 5 3       9 3   4   4 |     | 8 4 5 9 |
  |   8 8   3       5 3 9     9 3         |     | 9 8 8 9 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(19,4))           'Output' v('281123b4') (grid(4,4))
").


%= fav(v('281123b4'),[no_sol(i(complete),resize_grid(4,4,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([4,4]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/281123b4.json']),filename(['/data/evaluation/281123b4.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(6, 1) ']).
%~ warn_skip(save_supertest(v('281123b4'),'muarc_cache/281123b4.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('281123b4')))
%~ warn_skip(clear_saveable_test_info(v('27f8ce4f')))
fav_testcase(v('27f8ce4f')>trn+1,"

   _______       ___________________
  | 7 7 1 |     | 7 7 1 7 7 1       |
  | 4 7 1 |     | 4 7 1 4 7 1       |
  | 3 3 7 |     | 3 3 7 3 3 7       |
   ¯¯¯¯¯¯¯      |       7 7 1       |
                |       4 7 1       |
                |       3 3 7       |
                |             7 7 1 |
                |             4 7 1 |
                |             3 3 7 |
                 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(3,3))            'Output' v('27f8ce4f') (grid(9,9))
").


%= fav(v('27f8ce4f'),[no_sol(i(complete),resize_grid(9,9,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([9,9]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/27f8ce4f.json']),filename(['/data/evaluation/27f8ce4f.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('27f8ce4f'),'muarc_cache/27f8ce4f.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('27f8ce4f')))
%~ warn_skip(clear_saveable_test_info(v('27a77e38')))
fav_testcase(v('27a77e38')>trn+1,"

   ___________       ___________
  | 3 6 4 2 4 |     | 3 6 4 2 4 |
  | 8 4 3 3 4 |     | 8 4 3 3 4 |
  | 5 5 5 5 5 |     | 5 5 5 5 5 |
  |           |     |           |
  |           |     |     4     |
   ¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(5,5))            'Output' v('27a77e38') (grid(5,5))
").


%= fav(v('27a77e38'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/27a77e38.json']),filename(['/data/evaluation/27a77e38.json']),-rotation_match,-mask_match,+shape_match,+color_match,second_most_used_color,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('27a77e38'),'muarc_cache/27a77e38.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('27a77e38')))
%~ warn_skip(clear_saveable_test_info(v('2753e76c')))
fav_testcase(v('2753e76c')>trn+1,"

   _________________________________       _________
  |                 4 4       2 2   |     | 2 2 2 2 |
  |       2 2 2     4 4       2 2   |     |   3 3 3 |
  |       2 2 2                     |     |     8 8 |
  |       2 2 2         2 2 2       |     |       4 |
  |                     2 2 2       |       ¯¯¯¯¯¯¯¯¯
  |             3 3     2 2 2       |
  | 8 8 8 8     3 3                 |
  | 8 8 8 8                 2 2 2 2 |
  | 8 8 8 8       3 3 3     2 2 2 2 |
  | 8 8 8 8       3 3 3     2 2 2 2 |
  |               3 3 3     2 2 2 2 |
  |       8 8                       |
  |       8 8                       |
  |                         3 3     |
  |                         3 3     |
  |                                 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(16,16))          'Output' v('2753e76c') (grid(4,4))
").


%= fav(v('2753e76c'),[no_sol(i(complete),resize_grid(3,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2753e76c.json']),filename(['/data/evaluation/2753e76c.json']),-shape_match,-rotation_match,-mask_match,+color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('2753e76c'),'muarc_cache/2753e76c.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2753e76c')))
%~ warn_skip(clear_saveable_test_info(v('2697da3f')))
fav_testcase(v('2697da3f')>trn+1,"

   _______________       _______________________
  |               |     |           4           |
  |               |     |         4   4         |
  |     4 4       |     |         4 4 4         |
  |   4   4 4     |     |           4           |
  |     4 4       |     |   4 4           4 4   |
  |               |     | 4   4 4       4 4   4 |
  |               |     |   4 4           4 4   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯      |           4           |
                        |         4 4 4         |
                        |         4   4         |
                        |           4           |
                         ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(7,7))            'Output' v('2697da3f') (grid(11,11))
").


%= fav(v('2697da3f'),[no_sol(i(complete),resize_grid(15,15,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([15,15]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2697da3f.json']),filename(['/data/evaluation/2697da3f.json']),-shape_match,-rotation_match,-mask_match,+color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('2697da3f'),'muarc_cache/2697da3f.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2697da3f')))
%~ warn_skip(clear_saveable_test_info(v('2685904e')))
fav_testcase(v('2685904e')>trn+1,"

   _____________________       _____________________
  | 8                   |     | 8                   |
  |                     |     |                     |
  |                     |     |                     |
  |                     |     |                     |
  |                     |     |                     |
  |                     |     |           1         |
  | 5 5 5 5 5 5 5 5 5 5 |     | 5 5 5 5 5 5 5 5 5 5 |
  |                     |     |                     |
  | 6 6 4 6 2 1 9 2 9 4 |     | 6 6 4 6 2 1 9 2 9 4 |
  |                     |     |                     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,10))          'Output' v('2685904e') (grid(10,10))
").


%= fav(v('2685904e'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2685904e.json']),filename(['/data/evaluation/2685904e.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(6, 1) ']).
%~ warn_skip(save_supertest(v('2685904e'),'muarc_cache/2685904e.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2685904e')))
%~ warn_skip(clear_saveable_test_info(v('256b0a75')))
fav_testcase(v('256b0a75')>trn+1,"

   _________________________________________________       _________________________________________________
  |   4           4         6             1     5   |     |   4           4     4 1 6 1 1 1 1     1     5   |
  |                     4                           |     |                     4 1 1 1 1 1 1               |
  |             7                                   |     |             7       1 1 1 1 1 1 1               |
  |   2     9                                       |     |   2     9           1 1 1 1 1 1 1               |
  |                     1 1       8 8               |     | 1 1 1 1 1 1 1 1 1 1 8 8 8 8 8 8 8 1 1 1 1 1 1 1 |
  |                     1           8               |     | 1 1 1 1 1 1 1 1 1 1 8 1 1 1 1 1 8 1 1 1 1 1 1 1 |
  |                                                 |     | 1 1 1 1 1 1 1 1 1 1 8 1 1 1 1 1 8 1 1 1 1 1 1 1 |
  |                                               2 |     | 1 1 1 1 1 1 1 1 1 1 8 1 1 1 1 1 8 1 1 1 1 1 1 2 |
  |                                       1         |     | 1 1 1 1 1 1 1 1 1 1 8 1 1 1 1 1 8 1 1 1 1 1 1 1 |
  |                                                 |     | 1 1 1 1 1 1 1 1 1 1 8 1 1 1 1 1 8 1 1 1 1 1 1 1 |
  |             2       8           8               |     | 2 2 2 2 2 2 2 1 1 1 8 1 1 1 1 1 8 1 1 1 1 1 1 1 |
  |                     8 8       8 8       7       |     | 1 1 1 1 1 1 1 1 1 1 8 8 8 8 8 8 8 1 1 1 7 7 7 7 |
  |       9                                         |     |       9             1 1 1 1 1 1 1               |
  |                                 7               |     |                     1 1 1 1 1 1 7               |
  |                                       5         |     |                     1 1 1 1 1 1 7     5         |
  |                           1                     |     |                     1 1 1 1 1 1 7               |
  |                                                 |     |                     1 1 1 1 1 1 7               |
  |           5         3                           |     |           5         3 1 1 1 1 1 7               |
  |                                               1 |     |                     3 1 1 1 1 1 7             1 |
  |                                                 |     |                     3 1 1 1 1 1 7               |
  |                           3                     |     |                     3 1 1 3 1 1 7               |
  |                               1               4 |     |                     3 1 1 3 1 1 7             4 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(24,22))          'Output' v('256b0a75') (grid(24,22))
").


%= fav(v('256b0a75'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/256b0a75.json']),filename(['/data/evaluation/256b0a75.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('256b0a75'),'muarc_cache/256b0a75.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('256b0a75')))
%~ warn_skip(clear_saveable_test_info(v('2546ccf6')))
fav_testcase(v('2546ccf6')>trn+1,"

   _____________________________________       _____________________________________
  |         2         2         2       |     |         2         2         2       |
  | 3   3 3 2         2         2       |     | 3   3 3 2 3 3   3 2         2       |
  |   3 3   2         2         2       |     |   3 3   2   3 3   2         2       |
  |     3   2         2         2       |     |     3   2   3     2         2       |
  | 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 |     | 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 |
  |     3   2   3     2         2       |     |     3   2   3     2         2       |
  |   3 3   2   3 3   2         2       |     |   3 3   2   3 3   2         2       |
  | 3   3 3 2 3 3   3 2         2       |     | 3   3 3 2 3 3   3 2         2       |
  |         2         2         2       |     |         2         2         2       |
  | 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 |     | 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 |
  |         2         2         2       |     |         2         2         2       |
  |         2   1     2     1   2       |     |         2   1     2     1   2       |
  |         2     1 1 2 1 1     2       |     |         2     1 1 2 1 1     2       |
  |         2     1   2   1     2       |     |         2     1   2   1     2       |
  | 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 |     | 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 |
  |         2         2   1     2       |     |         2     1   2   1     2       |
  |         2         2 1 1     2       |     |         2     1 1 2 1 1     2       |
  |         2         2     1   2       |     |         2   1     2     1   2       |
  |         2         2         2       |     |         2         2         2       |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(18,19))          'Output' v('2546ccf6') (grid(18,19))
").


%= fav(v('2546ccf6'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2546ccf6.json']),filename(['/data/evaluation/2546ccf6.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('2546ccf6'),'muarc_cache/2546ccf6.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2546ccf6')))
%~ warn_skip(clear_saveable_test_info(v('25094a63')))
fav_testcase(v('25094a63')>trn+1,"

   _____________________________________________________________       _____________________________________________________________
  | 6 8 6 8 8 8 8 6 6 3 8 6 2 3 8 1 2 8 1 3 8 3 3 6 6 1 2 6 2 2 |     | 6 8 6 8 8 8 8 6 6 3 8 6 2 3 8 1 2 8 1 3 8 3 3 6 6 1 2 6 2 2 |
  | 2 3 3 2 2 3 6 2 6 2 8 1 3 8 1 8 1 8 8 8 8 2 2 3 2 1 2 8 6 3 |     | 2 3 3 2 2 3 6 2 6 2 8 1 3 8 1 8 1 8 8 8 8 2 2 3 2 1 2 8 6 3 |
  | 2 3 8 2 3 6 6 6 6 6 6 6 6 6 3 8 2 6 6 2 8 3 8 8 3 2 3 3 3 2 |     | 2 3 8 2 3 4 4 4 4 4 4 4 4 4 3 8 2 6 6 2 8 3 8 8 3 2 3 3 3 2 |
  | 8 6 8 2 3 6 6 6 6 6 6 6 6 6 3 1 1 2 1 2 6 8 2 6 2 1 8 3 3 8 |     | 8 6 8 2 3 4 4 4 4 4 4 4 4 4 3 1 1 2 1 2 6 8 2 6 2 1 8 3 3 8 |
  | 6 8 6 2 3 6 6 6 6 6 6 6 6 6 3 3 3 8 1 1 2 1 8 3 8 2 3 6 8 2 |     | 6 8 6 2 3 4 4 4 4 4 4 4 4 4 3 3 3 8 1 1 2 1 8 3 8 2 3 6 8 2 |
  | 3 8 3 3 6 6 6 6 6 6 6 6 6 6 2 8 6 3 2 6 1 6 6 2 8 8 3 2 6 6 |     | 3 8 3 3 6 4 4 4 4 4 4 4 4 4 2 8 6 3 2 6 1 6 6 2 8 8 3 2 6 6 |
  | 2 6 3 2 8 6 6 6 6 6 6 6 6 6 8 3 8 3 3 6 3 1 8 8 1 2 3 1 8 8 |     | 2 6 3 2 8 4 4 4 4 4 4 4 4 4 8 3 8 3 3 6 3 1 8 8 1 2 3 1 8 8 |
  | 3 8 1 6 1 8 1 3 8 3 2 3 2 8 1 3 1 2 2 8 1 6 3 3 3 6 2 2 8 6 |     | 3 8 1 6 1 8 1 3 8 3 2 3 2 8 1 3 1 2 2 8 1 6 3 3 3 6 2 2 8 6 |
  | 8 3 3 8 3 8 2 2 8 8 8 8 8 1 1 6 3 3 6 2 2 6 1 3 3 6 3 1 3 3 |     | 8 3 3 8 3 8 2 2 8 8 8 8 8 1 1 6 3 3 6 2 2 6 1 3 3 6 3 1 3 3 |
  | 2 3 3 2 3 2 6 2 3 6 8 3 3 8 3 6 1 3 3 8 8 1 6 6 8 8 1 6 2 6 |     | 2 3 3 2 3 2 6 2 3 6 8 3 3 8 3 6 1 3 3 8 8 1 6 6 8 8 1 6 2 6 |
  | 3 6 3 3 3 2 3 6 1 6 3 8 2 8 2 3 2 6 3 6 6 8 3 6 6 1 6 8 8 6 |     | 3 6 3 3 3 2 3 6 1 6 3 8 2 8 2 3 2 6 3 6 6 8 3 6 6 1 6 8 8 6 |
  | 8 3 3 1 2 2 6 8 2 3 6 8 3 2 2 6 3 2 1 2 6 3 6 8 8 8 1 8 1 6 |     | 8 3 3 1 2 2 6 8 2 3 6 8 3 2 2 6 3 2 1 2 6 3 6 8 8 8 1 8 1 6 |
  | 1 8 8 1 6 6 8 2 8 2 1 2 8 8 1 8 2 8 3 8 3 3 8 8 2 3 3 3 3 3 |     | 1 8 8 1 6 6 8 2 8 2 1 2 8 8 1 8 2 8 3 8 3 3 8 8 2 3 3 3 3 3 |
  | 8 8 3 8 3 2 8 6 3 3 1 3 2 1 6 6 8 3 6 6 3 6 3 1 8 1 2 6 3 8 |     | 8 8 3 8 3 2 8 6 3 3 1 3 2 1 6 6 8 3 6 6 3 6 3 1 8 1 2 6 3 8 |
  | 8 6 6 3 2 6 6 8 6 1 3 2 8 3 1 2 8 3 6 2 8 8 3 2 2 6 1 8 6 3 |     | 8 6 6 3 2 6 6 8 6 1 3 2 8 3 1 2 8 3 6 2 8 8 3 2 2 6 1 8 6 3 |
  | 1 8 1 6 2 3 2 2 1 8 2 2 8 3 6 8 8 8 2 8 8 3 3 1 3 2 2 1 3 2 |     | 1 8 1 6 2 3 2 2 1 8 2 2 8 3 6 8 8 8 2 8 8 3 3 1 3 2 2 1 3 2 |
  | 8 1 3 6 8 8 6 6 3 3 2 2 3 8 8 8 8 8 8 8 1 3 3 8 2 3 6 2 8 2 |     | 8 1 3 6 8 8 6 6 3 3 2 2 3 4 4 4 4 4 4 4 1 3 3 8 2 3 6 2 8 2 |
  | 3 3 3 6 3 2 2 2 6 3 2 3 3 8 8 8 8 8 8 8 8 3 3 2 3 2 2 2 2 3 |     | 3 3 3 6 3 2 2 2 6 3 2 3 3 4 4 4 4 4 4 4 8 3 3 2 3 2 2 2 2 3 |
  | 3 2 1 2 2 8 6 3 8 8 8 3 1 8 8 8 8 8 8 8 6 1 8 3 8 3 6 8 1 8 |     | 3 2 1 2 2 8 6 3 8 8 8 3 1 4 4 4 4 4 4 4 6 1 8 3 8 3 6 8 1 8 |
  | 3 6 1 3 2 3 6 6 6 3 2 1 3 8 8 8 8 8 8 8 3 3 2 1 8 3 6 3 2 3 |     | 3 6 1 3 2 3 6 6 6 3 2 1 3 4 4 4 4 4 4 4 3 3 2 1 8 3 6 3 2 3 |
  | 8 1 3 8 6 2 3 3 3 3 2 8 6 8 8 8 8 8 8 8 2 8 8 3 8 2 3 1 3 2 |     | 8 1 3 8 6 2 3 3 3 3 2 8 6 4 4 4 4 4 4 4 2 8 8 3 8 2 3 1 3 2 |
  | 3 6 3 2 8 6 6 3 8 3 1 2 3 8 8 8 8 8 8 8 3 6 8 6 1 2 1 3 3 6 |     | 3 6 3 2 8 6 6 3 8 3 1 2 3 4 4 4 4 4 4 4 3 6 8 6 1 2 1 3 3 6 |
  | 3 8 8 2 3 8 3 6 8 8 3 1 3 3 8 8 2 2 2 2 3 8 1 1 3 3 2 3 1 3 |     | 3 8 8 2 3 8 3 6 8 8 3 1 3 3 8 8 2 2 2 2 3 8 1 1 3 3 2 3 1 3 |
  | 3 3 6 8 1 6 6 2 8 6 6 1 8 1 2 2 1 6 8 3 2 6 8 6 8 8 6 2 8 3 |     | 3 3 6 8 1 6 6 2 8 6 6 1 8 1 2 2 1 6 8 3 2 6 8 6 8 8 6 2 8 3 |
  | 8 3 3 1 8 3 2 3 3 3 8 3 3 3 3 2 3 8 3 1 3 6 6 6 6 6 3 6 2 3 |     | 8 3 3 1 8 3 2 3 3 3 8 3 3 3 3 2 3 8 3 1 3 6 6 6 6 6 3 6 2 3 |
  | 3 6 8 3 2 1 8 6 6 8 6 6 1 6 6 1 3 3 6 2 6 1 3 3 8 1 2 2 3 3 |     | 3 6 8 3 2 1 8 6 6 8 6 6 1 6 6 1 3 3 6 2 6 1 3 3 8 1 2 2 3 3 |
  | 1 8 3 6 3 2 6 8 8 1 6 6 8 6 6 6 2 6 8 3 8 1 3 8 2 6 3 2 6 6 |     | 1 8 3 6 3 2 6 8 8 1 6 6 8 6 6 6 2 6 8 3 8 1 3 8 2 6 3 2 6 6 |
  | 8 8 6 8 1 1 8 2 2 3 6 2 8 3 8 2 1 1 8 6 8 6 8 6 3 3 3 3 2 3 |     | 8 8 6 8 1 1 8 2 2 3 6 2 8 3 8 2 1 1 8 6 8 6 8 6 3 3 3 3 2 3 |
  | 1 3 8 1 3 1 6 3 6 8 2 3 3 8 2 2 2 1 3 2 8 8 3 8 6 6 3 8 3 8 |     | 1 3 8 1 3 1 6 3 6 8 2 3 3 8 2 2 2 1 3 2 8 8 3 8 6 6 3 8 3 8 |
  | 6 2 6 2 8 2 3 3 3 3 1 3 3 3 2 6 3 8 2 3 6 3 3 2 2 3 8 8 1 3 |     | 6 2 6 2 8 2 3 3 3 3 1 3 3 3 2 6 3 8 2 3 6 3 3 2 2 3 8 8 1 3 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(30,30))          'Output' v('25094a63') (grid(30,30))
").


%= fav(v('25094a63'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/25094a63.json']),filename(['/data/evaluation/25094a63.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('25094a63'),'muarc_cache/25094a63.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('25094a63')))
%~ warn_skip(clear_saveable_test_info(v('22a4bbc2')))
fav_testcase(v('22a4bbc2')>trn+1,"

   ___________       ___________
  |   8 8 8   |     |   2 2 2   |
  |   8 8 8   |     |   2 2 2   |
  | 1 1 1     |     | 1 1 1     |
  |   8 8 8   |     |   8 8 8   |
  |   8 8 8   |     |   8 8 8   |
  | 1 1 1 1 1 |     | 2 2 2 2 2 |
  |   8 8 8 8 |     |   8 8 8 8 |
  |   8 8 8 8 |     |   8 8 8 8 |
  | 1 1 1 1   |     | 1 1 1 1   |
  | 1 1 1 1   |     | 1 1 1 1   |
  |   8 8 8   |     |   2 2 2   |
  |   1 1 1 1 |     |   1 1 1 1 |
  |   1 1 1 1 |     |   1 1 1 1 |
  | 8 8 8     |     | 8 8 8     |
  |       1 1 |     |       2 2 |
  | 8 8 8     |     | 8 8 8     |
  | 8 8 8     |     | 8 8 8     |
  |     1 1   |     |     1 1   |
  |     1 1   |     |     1 1   |
   ¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(5,19))           'Output' v('22a4bbc2') (grid(5,19))
").


%= fav(v('22a4bbc2'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/22a4bbc2.json']),filename(['/data/evaluation/22a4bbc2.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('22a4bbc2'),'muarc_cache/22a4bbc2.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('22a4bbc2')))
%~ warn_skip(clear_saveable_test_info(v('21f83797')))
fav_testcase(v('21f83797')>trn+1,"

   ___________________________       ___________________________
  |                           |     |     2               2     |
  |                           |     |     2               2     |
  |                           |     |     2               2     |
  |                           |     |     2               2     |
  |     2                     |     | 2 2 2 2 2 2 2 2 2 2 2 2 2 |
  |                           |     |     2 1 1 1 1 1 1 1 2     |
  |                           |     |     2 1 1 1 1 1 1 1 2     |
  |                           |     |     2 1 1 1 1 1 1 1 2     |
  |                     2     |     | 2 2 2 2 2 2 2 2 2 2 2 2 2 |
  |                           |     |     2               2     |
  |                           |     |     2               2     |
  |                           |     |     2               2     |
  |                           |     |     2               2     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(13,13))          'Output' v('21f83797') (grid(13,13))
").


%= fav(v('21f83797'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/21f83797.json']),filename(['/data/evaluation/21f83797.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('21f83797'),'muarc_cache/21f83797.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('21f83797')))
%~ warn_skip(clear_saveable_test_info(v('212895b5')))
fav_testcase(v('212895b5')>trn+1,"

   ___________________________________       ___________________________________
  |                               5   |     |                             4 5 2 |
  |           5     5 5               |     |           5     5 5     4 4 4 2   |
  |         5                         |     |         5   2           4   2     |
  |                                   |     |               2     4 4 4 2       |
  |                                   |     |                 2   4   2         |
  |       5           8 8 8     5     |     |       5           8 8 8     5     |
  |               5   8 8 8   5       |     |               5 4 8 8 8 4 5       |
  |     5             8 8 8           |     |     5             8 8 8           |
  |         5                 5       |     |         5       2   4   2 5       |
  |                   5   5     5     |     |               2   5 4 5   2 5     |
  |       5                           |     |       5     2               2     |
  |                         5         |     |           2             5     2   |
  |                                   |     |         2                       2 |
  |     5               5             |     |     5 2             5             |
  |                             5     |     |     2                       5     |
  |     5                       5     |     |   2 5                       5     |
  |                   5         5     |     | 2                 5         5     |
  |   5                               |     |   5                               |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(17,18))          'Output' v('212895b5') (grid(17,18))
").


%= fav(v('212895b5'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/212895b5.json']),filename(['/data/evaluation/212895b5.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('212895b5'),'muarc_cache/212895b5.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('212895b5')))
%~ warn_skip(clear_saveable_test_info(v('20981f0e')))
fav_testcase(v('20981f0e')>trn+1,"

   _________________________________       _________________________________
  |                                 |     |                                 |
  |   2         2         2         |     |   2         2         2         |
  |     1 1                   1 1   |     |                                 |
  |     1 1                   1     |     |       1 1       1         1 1   |
  |               1                 |     |       1 1       1 1       1     |
  |               1 1               |     |                                 |
  |   2         2         2         |     |   2         2         2         |
  |                                 |     |                                 |
  |                 1 1       1     |     |         1       1 1       1     |
  |         1       1         1 1   |     |       1 1       1         1 1   |
  |       1 1                       |     |                                 |
  |   2         2         2         |     |   2         2         2         |
  |                                 |     |                                 |
  |                                 |     |                                 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(16,14))          'Output' v('20981f0e') (grid(16,14))
").


%= fav(v('20981f0e'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/20981f0e.json']),filename(['/data/evaluation/20981f0e.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('20981f0e'),'muarc_cache/20981f0e.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('20981f0e')))
%~ warn_skip(clear_saveable_test_info(v('20818e16')))
fav_testcase(v('20818e16')>trn+1,"

   _______________________________       ___________________
  | 8 8 8 8 8 2 2 2 2 2 2 2 2 2 8 |     | 5 5 3 3 3 3 2 2 2 |
  | 8 8 8 8 8 2 2 2 2 2 2 2 2 2 8 |     | 5 5 3 3 3 3 2 2 2 |
  | 8 8 8 8 8 2 2 2 2 2 2 2 2 2 8 |     | 3 3 3 3 3 3 2 2 2 |
  | 8 8 8 8 8 2 2 2 2 2 2 2 2 2 8 |     | 3 3 3 3 3 3 2 2 2 |
  | 8 8 8 8 8 2 2 2 2 2 2 2 2 2 8 |     | 3 3 3 3 3 3 2 2 2 |
  | 8 8 3 3 3 2 2 2 2 2 2 2 2 2 8 |     | 3 3 3 3 3 3 2 2 2 |
  | 8 8 3 3 3 3 3 3 8 8 8 8 8 8 8 |       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
  | 8 8 3 3 3 3 3 3 8 8 8 8 8 8 8 |
  | 8 8 3 3 3 3 3 3 8 8 8 8 8 8 8 |
  | 8 8 3 3 3 3 3 3 8 8 8 8 8 8 8 |
  | 8 8 3 3 3 3 3 3 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 5 5 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 5 5 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,16))          'Output' v('20818e16') (grid(9,6))
").


%= fav(v('20818e16'),[no_sol(i(complete),resize_grid(8,6,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([8,6]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/20818e16.json']),filename(['/data/evaluation/20818e16.json'])]).
%~ warn_skip(save_supertest(v('20818e16'),'muarc_cache/20818e16.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('20818e16')))
%~ warn_skip(clear_saveable_test_info(v('2072aba6')))
fav_testcase(v('2072aba6')>trn+1,"

   _______       _____________
  | 5     |     | 1 2         |
  |   5   |     | 2 1         |
  |     5 |     |     1 2     |
   ¯¯¯¯¯¯¯      |     2 1     |
                |         1 2 |
                |         2 1 |
                 ¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(3,3))            'Output' v('2072aba6') (grid(6,6))
").


%= fav(v('2072aba6'),[no_sol(i(complete),resize_grid(6,6,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([6,6]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2072aba6.json']),filename(['/data/evaluation/2072aba6.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('2072aba6'),'muarc_cache/2072aba6.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2072aba6')))
%~ warn_skip(clear_saveable_test_info(v('2037f2c7')))
fav_testcase(v('2037f2c7')>trn+1,"

   ___________________________________________________       _________________
  |                                                   |     |   8         8 8 |
  |                                                   |     | 8 8 8 8   8 8 8 |
  |           4 4 4 4                                 |     |     8         8 |
  |       1 1 4 6 6 4 1 1                             |     |               8 |
  |       3 3 3 3 3 3 3 3                             |       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
  |       1 3 4 6 6 4 3 1                             |
  |       1   4 6 6 4                                 |
  |               6                                   |
  |       1 3   6 6 4 3                               |
  |       1 1 4 6 6 4 1               4 4 4 4         |
  |       2 4 4 7 7 4 4 2         1 1 4 6 6 4 1 1     |
  |       2 4 4 7 7 4 4 2         3 3 3 3 3 3 3 3     |
  |       2     7 7     2         1 3 4 6 6 4 3 1     |
  |                               1 3 4 6 6 4 3 1     |
  |                               1 3 4 6 6 4 3 1     |
  |                               1 3 4 6 6 4 3 1     |
  |                               1 1 4 6 6 4 1 1     |
  |                               2 4 4 7 7 4 4 2     |
  |                               2 4 4 7 7 4 4 2     |
  |                               2     7 7     2     |
  |                                                   |
  |                                                   |
  |                                                   |
  |                                                   |
  |                                                   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(25,25))          'Output' v('2037f2c7') (grid(8,4))
").


%= fav(v('2037f2c7'),[no_sol(i(complete),resize_grid(7,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([7,3]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/2037f2c7.json']),filename(['/data/evaluation/2037f2c7.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('2037f2c7'),'muarc_cache/2037f2c7.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('2037f2c7')))
%~ warn_skip(clear_saveable_test_info(v('1e97544e')))
fav_testcase(v('1e97544e')>trn+1,"

   _______________________________________________       _______________________________________________
  | 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |     | 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |     | 4 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |     | 4 5 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 6 7 1 2 3 4 5 6 7 1 2       6 7 1 2 3 4 |     | 4 5 6 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 7 1 2 3 4 5 6 7 1 2       6 7 1 2 3 4 |     | 4 5 6 7 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 1 2 3 4 5 6 7 1 2       6 7 1 2 3 4 |     | 4 5 6 7 1 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 2 3 4 5 6 7 1 2       6 7 1 2 3 4 |     | 4 5 6 7 1 2 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2           7 1 2 3 4 5 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2           7 1 2       6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 5 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 6 7 1 2       6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 6 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 7 1 2       6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 7 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7           6 7 1 1 2     5 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 1 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7           6 7 1 2 2     5 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 2 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7           6 7 1 2 3 3 4 5 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 3 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 4 5 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 4 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 5 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 5 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 6 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 6 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 7 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 7 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 1 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 1 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 2 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 2 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 3 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 3 4 |
  | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 4 |     | 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 4 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(23,23))          'Output' v('1e97544e') (grid(23,23))
").


%= fav(v('1e97544e'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),indiv([i_repair_patterns_f]),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1e97544e.json']),filename(['/data/evaluation/1e97544e.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('1e97544e'),'muarc_cache/1e97544e.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1e97544e')))
%~ warn_skip(clear_saveable_test_info(v('1e81d6f9')))
fav_testcase(v('1e81d6f9')>trn+1,"

   _______________________________       _______________________________
  |       5                 1     |     |       5                 1     |
  |   4   5                       |     |   4   5                       |
  |       5         9 2 4         |     |       5         9 2           |
  | 5 5 5 5                       |     | 5 5 5 5                       |
  |         8                     |     |         8                     |
  |             1           4     |     |             1                 |
  |                   1           |     |                   1           |
  |     3     6                   |     |     3     6                   |
  |                             2 |     |                             2 |
  |       4         4   4         |     |                               |
  |   9                         2 |     |   9                         2 |
  |             2                 |     |             2                 |
  |       3 2         2     1     |     |       3 2         2     1     |
  |     3     2               2   |     |     3     2               2   |
  |           3   7 8             |     |           3   7 8             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,15))          'Output' v('1e81d6f9') (grid(15,15))
").


%= fav(v('1e81d6f9'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1e81d6f9.json']),filename(['/data/evaluation/1e81d6f9.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('1e81d6f9'),'muarc_cache/1e81d6f9.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1e81d6f9')))
%~ warn_skip(clear_saveable_test_info(v('1da012fc')))
fav_testcase(v('1da012fc')>trn+1,"

   _________________________________________       _________________________________________
  |                                         |     |                                         |
  |                   1 1 1 1               |     |                   2 2 2 2               |
  |                     1 1                 |     |                     2 2                 |
  |                                         |     |                                         |
  |                             1   1       |     |                             4   4       |
  |                           1 1 1 1 1     |     |                           4 4 4 4 4     |
  |   5 2 5 5 5 5               1   1       |     |   5 2 5 5 5 5               4   4       |
  |   5 5 5 5 5 5                           |     |   5 5 5 5 5 5                           |
  |   5 5 5 4 5 5                           |     |   5 5 5 4 5 5                           |
  |   5 5 5 5 5 5     1 1             1     |     |   5 5 5 5 5 5     6 6             3     |
  |   5 6 5 5 3 5   1 1 1 1         1 1 1   |     |   5 6 5 5 3 5   6 6 6 6         3 3 3   |
  |   5 5 5 5 5 5     1 1             1     |     |   5 5 5 5 5 5     6 6             3     |
  |                                         |     |                                         |
  |                                         |     |                                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(20,14))          'Output' v('1da012fc') (grid(20,14))
").


%= fav(v('1da012fc'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1da012fc.json']),filename(['/data/evaluation/1da012fc.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('1da012fc'),'muarc_cache/1da012fc.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1da012fc')))
%~ warn_skip(clear_saveable_test_info(v('1d398264')))
fav_testcase(v('1d398264')>trn+1,"

   _____________________       _____________________
  |                     |     |   3     1     2     |
  |                     |     |     3   1   2       |
  |       3 1 2         |     |       3 1 2         |
  |       2 6 2         |     | 2 2 2 2 6 2 2 2 2 2 |
  |       2 7 7         |     |       2 7 7         |
  |                     |     |     2   7   7       |
  |                     |     |   2     7     7     |
  |                     |     | 2       7       7   |
  |                     |     |         7         7 |
  |                     |     |         7           |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,10))          'Output' v('1d398264') (grid(10,10))
").


%= fav(v('1d398264'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),ohuman([(iz(_35124,keypad),iz(_35124,multicolor),centerof(_35124,_35144)-->sunburst(_35144))]),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1d398264.json']),filename(['/data/evaluation/1d398264.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 2) ']).
%~ warn_skip(save_supertest(v('1d398264'),'muarc_cache/1d398264.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1d398264')))
%~ warn_skip(clear_saveable_test_info(v('1d0a4b61')))
fav_testcase(v('1d0a4b61')>trn+1,"

   ___________________________________________________       ___________________________________________________
  | 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |     | 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |
  | 1 2 3 5 5 3 2 1 2 3 5         2 3 5 5 3 2 1 2 3 5 |     | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |
  | 1 5 2 3 3 2 5 1 5 2 3         5 2 3 3 2 5 1 5 2 3 |     | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |
  | 1 3 5 2 2 5 3 1 3 5 2         3 5 2 2 5 3 1 3 5 2 |     | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |
  | 1 3 5 2 2 5 3 1 3 5 2         3 5 2 2 5 3 1 3 5 2 |     | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |
  | 1 5 2 3 3 2 5 1 5 2 3         5 2 3 3 2 5 1 5 2 3 |     | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |
  | 1 2 3 5 5 3 2 1 2 3 5         2 3 5 5 3 2 1 2 3 5 |     | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |
  | 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |     | 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |
  | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |     | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |
  | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |     | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |
  | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |     | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |
  | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |     | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |
  | 1 5 2 3       1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |     | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |
  | 1 2 3 5       1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |     | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |
  | 1 1 1 1       1 1 1 1 1 1 1     1 1 1 1 1 1 1 1 1 |     | 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |
  | 1 2 3 5       1 2 3 5 5 3 2     3 5 5 3 2 1 2 3 5 |     | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |
  | 1 5 2         1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |     | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |
  | 1 3 5         1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |     | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |
  | 1 3 5         1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |     | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |
  | 1 5 2         1 5         5 1 5 2 3 3 2 5 1 5 2 3 |     | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |
  | 1 2 3         1 2         2 1 2 3 5 5 3 2 1 2 3 5 |     | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |
  | 1 1 1 1 1 1 1 1 1         1 1 1 1 1 1 1 1 1 1 1 1 |     | 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |
  | 1 2 3 5 5 3 2 1 2         2 1 2 3 5 5 3 2 1 2 3 5 |     | 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 5 3 2 1 2 3 5 |
  | 1 5 2 3 3 2 5 1 5         5 1 5 2 3 3 2 5 1 5 2 3 |     | 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 3 2 5 1 5 2 3 |
  | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |     | 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 2 5 3 1 3 5 2 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(25,25))          'Output' v('1d0a4b61') (grid(25,25))
").


%= fav(v('1d0a4b61'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1d0a4b61.json']),filename(['/data/evaluation/1d0a4b61.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('1d0a4b61'),'muarc_cache/1d0a4b61.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1d0a4b61')))
%~ warn_skip(clear_saveable_test_info(v('1c56ad9f')))
fav_testcase(v('1c56ad9f')>trn+1,"

   _____________________________       _____________________________
  |                             |     |                             |
  |                             |     |                             |
  |                             |     |                             |
  |       2 2 2 2 2 2           |     |       2 2 2 2 2 2           |
  |       2         2           |     |     2         2             |
  |       2         2           |     |       2         2           |
  |       2         2           |     |         2         2         |
  |       2         2           |     |       2         2           |
  |       2         2           |     |     2         2             |
  |       2         2           |     |       2         2           |
  |       2         2           |     |         2         2         |
  |       2         2           |     |       2         2           |
  |       2         2           |     |     2         2             |
  |       2 2 2 2 2 2           |     |       2 2 2 2 2 2           |
  |                             |     |                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(14,15))          'Output' v('1c56ad9f') (grid(14,15))
").


%= fav(v('1c56ad9f'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1c56ad9f.json']),filename(['/data/evaluation/1c56ad9f.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('1c56ad9f'),'muarc_cache/1c56ad9f.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1c56ad9f')))
%~ warn_skip(clear_saveable_test_info(v('1c0d0a4b')))
fav_testcase(v('1c0d0a4b')>trn+1,"

   ___________________________       ___________________________
  |                           |     |                           |
  |   8   8   8   8   8       |     |     2       2       2 2   |
  |     8     8 8 8   8 8     |     |   2   2               2   |
  |   8   8   8   8     8 8   |     |     2       2     2       |
  |                           |     |                           |
  |   8 8 8   8 8 8   8 8     |     |                       2   |
  |   8   8               8   |     |     2     2 2 2   2 2     |
  |     8     8 8 8   8 8     |     |   2   2               2   |
  |                           |     |                           |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(13,9))           'Output' v('1c0d0a4b') (grid(13,9))
").


%= fav(v('1c0d0a4b'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1c0d0a4b.json']),filename(['/data/evaluation/1c0d0a4b.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('1c0d0a4b'),'muarc_cache/1c0d0a4b.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1c0d0a4b')))
%~ warn_skip(clear_saveable_test_info(v('1c02dbbe')))
fav_testcase(v('1c02dbbe')>trn+1,"

   _______________________________       _______________________________
  |                               |     |                               |
  |                 3             |     |                               |
  |     3 5 5 5 5 5 5 5 5 5 5     |     |     3 3 3 3 3 3 3 5 5 5 5     |
  |     5 5 5 5 5 5 5 5 5 5 5     |     |     3 3 3 3 3 3 3 5 5 5 5     |
  |     5 5 5 5 5 5 5 5 5 5 5     |     |     3 3 3 3 3 3 3 5 5 5 5     |
  |     5 5 5 5 5 5 5 5 5 5 5     |     |     3 3 3 3 3 3 3 5 5 5 5     |
  |   3 5 5 5 5 5 5 5 5 5 5 5     |     |     3 3 3 3 3 3 3 5 5 5 5     |
  |     5 5 5 5 5 5 5 5 5 5 5 4   |     |     5 5 5 5 5 5 4 4 4 4 4     |
  |     5 5 5 5 5 5 5 5 5 5 5     |     |     5 5 5 5 5 5 4 4 4 4 4     |
  |     5 5 5 5 5 5 5 5 5 5 5     |     |     5 5 5 5 5 5 4 4 4 4 4     |
  |     5 5 5 5 5 5 5 5 5 5 5     |     |     5 5 5 5 5 5 4 4 4 4 4     |
  |     5 5 5 5 5 5 5 5 5 5 5     |     |     5 5 5 5 5 5 4 4 4 4 4     |
  |     5 5 5 5 5 5 5 5 5 5 4     |     |     5 5 5 5 5 5 4 4 4 4 4     |
  |                 4             |     |                               |
  |                               |     |                               |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,15))          'Output' v('1c02dbbe') (grid(15,15))
").


%= fav(v('1c02dbbe'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1c02dbbe.json']),filename(['/data/evaluation/1c02dbbe.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('1c02dbbe'),'muarc_cache/1c02dbbe.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1c02dbbe')))
%~ warn_skip(clear_saveable_test_info(v('1acc24af')))
fav_testcase(v('1acc24af')>trn+1,"

   _________________________       _________________________
  |                         |     |                         |
  |       1 1 1 1           |     |       1 1 1 1           |
  |       1     1           |     |       1     1           |
  | 1 1 1 1     1 1 1 1 1 1 |     | 1 1 1 1     1 1 1 1 1 1 |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
  |                 5       |     |                 5       |
  |   5 5 5   5 5   5 5     |     |   2 2 2   2 2   5 5     |
  |   5 5     5 5     5     |     |   2 2     2 2     5     |
  |                         |     |                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(12,12))          'Output' v('1acc24af') (grid(12,12))
").


%= fav(v('1acc24af'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1acc24af.json']),filename(['/data/evaluation/1acc24af.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('1acc24af'),'muarc_cache/1acc24af.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1acc24af')))
%~ warn_skip(clear_saveable_test_info(v('1a6449f1')))
fav_testcase(v('1a6449f1')>trn+1,"

   _______________________________________________       _____________________
  |   3   9       9 5   4         5       4   8   |     | 6       1 1     6   |
  |   3           9     9   3   1 1             2 |     |     6   6   4       |
  |     8   2 2 2 2 2 2 2 2 2           4         |     |     3     8   7   5 |
  |         2 8     7       2     1     4   7 9 8 |     | 4   3 4   3     8   |
  |     7   2   1 5 3   6 5 2 5     1 1 5         |     |     3 7 7 5     1   |
  | 9     8 2               2               8   7 |     | 3         9         |
  | 8   4   2   9 5   1     2                   9 |     |       5             |
  | 1       2 3       2     2     6 4 4 8         |     | 3             9     |
  | 8       2     1 4     8 2       4 2 7   9 1 6 |       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
  |   4 4 8 2 2 2 2 2 2 2 2 2     2   8   3   6   |
  |   7       8   3 7   9     3     3         1   |
  |           2 5         4 4 4 4 4 4 4 4 4 4 4 4 |
  |     3 3 3 3 3     4   4 6       1 1     6   4 |
  | 6   3       3         4     6   6   4       4 |
  |     3 7 9   3       6 4     3     8   7   5 4 |
  |   7 3       3   2 8   4 4   3 4   3     8   4 |
  |   8 3   9   3   3 3   4     3 7 7 5     1   4 |
  |     3 3 3 3 3 6 5     4 3         9         4 |
  |   9 2     2 3       9 4       5             4 |
  |   5 8       1   6   9 4 3             9     4 |
  | 9               3     4 4 4 4 4 4 4 4 4 4 4 4 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(23,21))          'Output' v('1a6449f1') (grid(10,8))
").


%= fav(v('1a6449f1'),[no_sol(i(complete),resize_grid(7,6,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([7,6]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1a6449f1.json']),filename(['/data/evaluation/1a6449f1.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('1a6449f1'),'muarc_cache/1a6449f1.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1a6449f1')))
%~ warn_skip(clear_saveable_test_info(v('1a2e2828')))
fav_testcase(v('1a2e2828')>trn+1,"

   _______________________       _
  |       4 4       8     |     | 8 |
  |       4 4       8     |      ¯¯¯
  | 3 3 3 4 4 3 3 3 8 3 3 |
  |       4 4       8     |
  |       4 4       8     |
  | 6 6 6 6 6 6 6 6 8 6 6 |
  | 6 6 6 6 6 6 6 6 8 6 6 |
  |       4 4       8     |
  |       4 4       8     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(11,9))           'Output' v('1a2e2828') (grid(1,1))
").


%= fav(v('1a2e2828'),[no_sol(i(complete),resize_grid(1,1,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([1,1]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1a2e2828.json']),filename(['/data/evaluation/1a2e2828.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(5, 1) ']).
%~ warn_skip(save_supertest(v('1a2e2828'),'muarc_cache/1a2e2828.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1a2e2828')))
%~ warn_skip(clear_saveable_test_info(v('19bb5feb')))
fav_testcase(v('19bb5feb')>trn+1,"

   _______________________________       _____
  |                               |     | 1 3 |
  |                               |     |   2 |
  |   8 8 8 8 8 8 8 8 8 8 8 8 8   |       ¯¯¯¯¯
  |   8 8 8 8 8 8 8 8 8 8 8 8 8   |
  |   8 8 1 1 8 8 8 3 3 8 8 8 8   |
  |   8 8 1 1 8 8 8 3 3 8 8 8 8   |
  |   8 8 8 8 8 8 8 8 8 8 8 8 8   |
  |   8 8 8 8 8 8 8 2 2 8 8 8 8   |
  |   8 8 8 8 8 8 8 2 2 8 8 8 8   |
  |   8 8 8 8 8 8 8 8 8 8 8 8 8   |
  |   8 8 8 8 8 8 8 8 8 8 8 8 8   |
  |   8 8 8 8 8 8 8 8 8 8 8 8 8   |
  |                               |
  |                               |
  |                               |
  |                               |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,16))          'Output' v('19bb5feb') (grid(2,2))
").


%= fav(v('19bb5feb'),[no_sol(i(complete),resize_grid(2,2,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([2,2]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/19bb5feb.json']),filename(['/data/evaluation/19bb5feb.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('19bb5feb'),'muarc_cache/19bb5feb.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('19bb5feb')))
%~ warn_skip(clear_saveable_test_info(v('1990f7a8')))
fav_testcase(v('1990f7a8')>trn+1,"

   _______________________________________       _______________
  |                                       |     | 2   2   2 2   |
  |                                       |     |   2 2   2   2 |
  |       2   2           2 2             |     |     2   2 2   |
  |         2 2           2   2           |     |               |
  |           2           2 2             |     | 2 2 2   2 2   |
  |                                       |     | 2   2     2   |
  |                                       |     | 2 2 2   2   2 |
  |                                       |       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
  |                           2 2         |
  |             2 2 2           2         |
  |             2   2         2   2       |
  |             2 2 2                     |
  |                                       |
  |                                       |
  |                                       |
  |                                       |
  |                                       |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(19,17))          'Output' v('1990f7a8') (grid(7,7))
").


%= fav(v('1990f7a8'),[no_sol(i(complete),resize_grid(7,7,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([7,7]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/1990f7a8.json']),filename(['/data/evaluation/1990f7a8.json']),-shape_match,-rotation_match,-mask_match,+color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('1990f7a8'),'muarc_cache/1990f7a8.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('1990f7a8')))
%~ warn_skip(clear_saveable_test_info(v('195ba7dc')))
fav_testcase(v('195ba7dc')>trn+1,"

   ___________________________       _____________
  |   7 7 7   7 2 7 7   7   7 |     | 1 1 1 1   1 |
  |       7   7 2   7 7 7   7 |     |   1 1 1   1 |
  | 7   7       2 7 7         |     | 1 1 1       |
  | 7 7 7       2 7 7     7 7 |     | 1 1 1   1 1 |
  |   7 7   7 7 2 7 7 7     7 |     | 1 1 1   1 1 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(13,5))           'Output' v('195ba7dc') (grid(6,5))
").


%= fav(v('195ba7dc'),[no_sol(i(complete),resize_grid(6,5,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([6,5]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/195ba7dc.json']),filename(['/data/evaluation/195ba7dc.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('195ba7dc'),'muarc_cache/195ba7dc.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('195ba7dc')))
%~ warn_skip(clear_saveable_test_info(v('184a9768')))
fav_testcase(v('184a9768')>trn+1,"

   _____________________________________________       _____________________________________________
  |                                       5     |     |                                             |
  |     1 1 1 1 1 1 1 1                         |     |     1 1 1 1 1 1 1 1                         |
  | 5   1       1 1 1 1     2 2 2 2 2           |     |     1 4 4 4 1 1 1 1                         |
  |     1 1 1 1 1 1 1 1                   3     |     |     1 1 1 1 1 1 1 1                         |
  |     1 1           1                   3     |     |     1 1 2 2 2 2 2 1                         |
  |     1 1           1     4 4 4   5     3     |     |     1 1 2 2 2 2 2 1                         |
  |     1 1 1 1 1 1 1 1                         |     |     1 1 1 1 1 1 1 1                         |
  |     1 1 1 1 1 1 1 1                         |     |     1 1 1 1 1 1 1 1                         |
  |                                             |     |                                             |
  |   5           4 4 4 4 4 4 4 4 4 4 4       5 |     |               4 4 4 4 4 4 4 4 4 4 4         |
  |         5     4     4 4 4 4 4 4 4 4         |     |               4 8 8 4 4 4 4 4 4 4 4         |
  |               4     4 4     4 4 4 4         |     |               4 8 8 4 4 3 3 4 4 4 4         |
  |   8 8         4 4 4 4 4     4 4 4 4         |     |               4 4 4 4 4 3 3 4 4 4 4         |
  |   8 8         4 4 4 4 4     4 4 4 4   5     |     |               4 4 4 4 4 3 3 4 4 4 4         |
  |               4 4 4 4 4 4 4 4 4 4 4         |     |               4 4 4 4 4 4 4 4 4 4 4         |
  |         5                                   |     |                                             |
  |                                   3         |     |                                             |
  |                                   3         |     |                                             |
  |       2 2 2 2 2         5         3         |     |                                             |
  |                                             |     |                                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(22,20))          'Output' v('184a9768') (grid(22,20))
",[indiv(colormass),human([delete_color(silver)])]).


%= fav(v('184a9768'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/184a9768.json']),filename(['/data/evaluation/184a9768.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('184a9768'),'muarc_cache/184a9768.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('184a9768')))
%~ warn_skip(clear_saveable_test_info(v('18419cfa')))
fav_testcase(v('18419cfa')>trn+1,"

   ___________________________________       ___________________________________
  |                                   |     |                                   |
  |                                   |     |                                   |
  |                                   |     |                                   |
  |     8 8 8 8 8 8 8 8               |     |     8 8 8 8 8 8 8 8               |
  |     8             8               |     |     8             8               |
  |     8             8               |     |     8             8               |
  |   8 8   2 2       8 8             |     |   8 8   2 2 2 2   8 8             |
  |   8     2           8             |     |   8     2     2     8             |
  |   8 8   2 2       8 8             |     |   8 8   2 2 2 2   8 8             |
  |     8             8               |     |     8             8               |
  |     8             8               |     |     8             8               |
  |     8 8 8 8 8 8 8 8               |     |     8 8 8 8 8 8 8 8               |
  |                                   |     |                                   |
  |                                   |     |                                   |
  |                                   |     |                                   |
  |                                   |     |                                   |
  |                                   |     |                                   |
  |                                   |     |                                   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(17,18))          'Output' v('18419cfa') (grid(17,18))
").


%= fav(v('18419cfa'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/18419cfa.json']),filename(['/data/evaluation/18419cfa.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('18419cfa'),'muarc_cache/18419cfa.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('18419cfa')))
%~ warn_skip(clear_saveable_test_info(v('17cae0c1')))
fav_testcase(v('17cae0c1')>trn+1,"

   ___________________       ___________________
  |     5             |     | 9 9 9 1 1 1 4 4 4 |
  |   5           5   |     | 9 9 9 1 1 1 4 4 4 |
  | 5     5 5 5       |     | 9 9 9 1 1 1 4 4 4 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(9,3))            'Output' v('17cae0c1') (grid(9,3))
").


%= fav(v('17cae0c1'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/17cae0c1.json']),filename(['/data/evaluation/17cae0c1.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('17cae0c1'),'muarc_cache/17cae0c1.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('17cae0c1')))
%~ warn_skip(clear_saveable_test_info(v('17b80ad2')))
fav_testcase(v('17b80ad2')>trn+1,"

   ___________________________       ___________________________
  | 8                         |     | 8       4       1         |
  |                 1       6 |     |         4       1       6 |
  |       8     8         2   |     |       8 4   8   6     2   |
  |     7                     |     |     7   4       6         |
  |                 6   4     |     |         4       6   4     |
  |                           |     |         4       6         |
  |         4     6           |     |         4     6 6         |
  |                   1       |     |         8       6 1       |
  |             3             |     |         8   3   6         |
  |   3                       |     |   3     8       6         |
  |         8       6       2 |     |         8       6       2 |
  |                           |     |         5       5         |
  |         5       5         |     |         5       5         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(13,13))          'Output' v('17b80ad2') (grid(13,13))
").


%= fav(v('17b80ad2'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/17b80ad2.json']),filename(['/data/evaluation/17b80ad2.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('17b80ad2'),'muarc_cache/17b80ad2.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('17b80ad2')))
%~ warn_skip(clear_saveable_test_info(v('16b78196')))
fav_testcase(v('16b78196')>trn+1,"

   _____________________________________________________________       _____________________________________________________________
  |                                                             |     |                                                             |
  |                                                             |     |                                                             |
  |               4     4                                       |     |                                                             |
  |               4 4 4 4                                       |     |                                                             |
  |               4 4 4                   3 3                   |     |                                                             |
  |               4 4                   3 3 3 3                 |     |                                                             |
  |               4                       3 3                   |     |                                                             |
  |                                                             |     |                                                             |
  |                                               6 6 6 6 6     |     |                                                             |
  |                                                 6 6 6       |     |         6 6 6 6 6                                           |
  |                                                             |     |         1 6 6 6 1                                           |
  |                                                             |     |         1 1 1 1 1                                           |
  |                                                             |     |         2 1 2 1 2                                           |
  |                                                             |     |         2 2 2 2 2                                           |
  | 8 8 8 8 8       8 8 8 8 8 8 8   8 8 8 8 8 8 8 8 8 8   8 8 8 |     | 8 8 8 8 8 2 2 2 8 8 8 8 8 8 8   8 8 8 8 8 8 8 8 8 8   8 8 8 |
  | 8 8 8 8 8 8   8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |     | 8 8 8 8 8 8 2 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |     | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8   8 8 8 8 8 |     | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8   8 8 8 8 8 |
  | 8 8 8 8 8 8 8 8   8 8 8 8 8 8 8 8     8 8 8 8 8     8 8 8 8 |     | 8 8 8 8 8 8 8 8   8 8 8 8 8 8 8 8 3 3 8 8 8 8 8     8 8 8 8 |
  |                                                             |     |                                 3 3 3 3                     |
  |                                                             |     |                                 4 3 3 4                     |
  |                                                             |     |                                 4 4 4 4                     |
  |                                                     3       |     |                                 4 4 4 3                     |
  |       2   2   2                 1       1         3 3       |     |                                 4 4 3 3                     |
  |       2 2 2 2 2                 1 1 1 1 1       3 3 3       |     |                                 4 3 3 3                     |
  |         2 2 2                     1   1       3 3 3 3       |     |                                 3 3 3 3                     |
  |           2                                                 |     |                                                             |
  |                                                             |     |                                                             |
  |                                                             |     |                                                             |
  |                                                             |     |                                                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(30,30))          'Output' v('16b78196') (grid(30,30))
").


%= fav(v('16b78196'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/16b78196.json']),filename(['/data/evaluation/16b78196.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('16b78196'),'muarc_cache/16b78196.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('16b78196')))
%~ warn_skip(clear_saveable_test_info(v('15696249')))
fav_testcase(v('15696249')>trn+1,"

   _______       ___________________
  | 2 4 3 |     | 2 4 3             |
  | 2 3 4 |     | 2 3 4             |
  | 2 3 4 |     | 2 3 4             |
   ¯¯¯¯¯¯¯      | 2 4 3             |
                | 2 3 4             |
                | 2 3 4             |
                | 2 4 3             |
                | 2 3 4             |
                | 2 3 4             |
                 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(3,3))            'Output' v('15696249') (grid(9,9))
").


%= fav(v('15696249'),[no_sol(i(complete),resize_grid(9,9,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([9,9]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/15696249.json']),filename(['/data/evaluation/15696249.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('15696249'),'muarc_cache/15696249.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('15696249')))
%~ warn_skip(clear_saveable_test_info(v('15663ba9')))
fav_testcase(v('15663ba9')>trn+1,"

   _______________________________       _______________________________
  |                               |     |                               |
  |   8 8 8 8 8 8 8               |     |   4 8 8 8 8 8 4               |
  |   8           8               |     |   8           8               |
  |   8 8         8               |     |   4 2         8               |
  |     8         8     8 8 8 8 8 |     |     8         8     4 8 8 8 4 |
  |     8   8 8 8 8     8       8 |     |     8   2 8 8 4     8       8 |
  |     8 8 8           8 8     8 |     |     4 8 4           4 2     8 |
  |                       8     8 |     |                       8     8 |
  |                   8 8 8   8 8 |     |                   4 8 2   2 4 |
  |     8 8 8         8       8   |     |     4 8 4         8       8   |
  |     8   8         8 8 8 8 8   |     |     8   8         4 8 8 8 4   |
  |     8 8 8                     |     |     4 8 4                     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,12))          'Output' v('15663ba9') (grid(15,12))
").


%= fav(v('15663ba9'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/15663ba9.json']),filename(['/data/evaluation/15663ba9.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('15663ba9'),'muarc_cache/15663ba9.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('15663ba9')))
%~ warn_skip(clear_saveable_test_info(v('15113be4')))
fav_testcase(v('15113be4')>trn+1,"

   _______________________________________________       _______________________________________________
  |     1 4 1     4       4     1 4     1 4   1   |     |     6 4 1     4       4     1 4     1 4   1   |
  | 1 1   4   1   4 1   1 4 1 1 1 4 1 1   4     1 |     | 6 6   4   1   4 1   1 4 1 1 1 4 1 1   4     1 |
  | 1 1 1 4     1 4 1   1 4     1 4     1 4 1     |     | 1 6 1 4     1 4 1   1 4     1 4     1 4 1     |
  | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |     | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |
  |   1   4 1     4 1     4 1 1   4     1 4 1     |     |   1   4 1     4 1     4 1 1   4     1 4 1     |
  |       4     1 4   1   4 1     4       4     1 |     |       4     1 4   1   4 1     4       4     1 |
  |   1   4     1 4 1   1 4   1   4 1     4   1   |     |   1   4     1 4 1   1 4   1   4 1     4   1   |
  | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |     | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |
  | 1 1   4   1   4 1 1 1 4       4   1   4   1 1 |     | 1 1   4   1   4 1 1 1 4       4   1   4   1 1 |
  |       4   1   4       4 1   1 4     1 4     1 |     |       4   1   4       4 1   1 4     1 4     1 |
  |   1 1 4   1   4 1     4       4       4       |     |   1 1 4   1   4 1     4       4       4       |
  | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |     | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |
  | 1 1 1 4 1 1   4 1     4     1 4     1 4     1 |     | 1 1 6 4 1 1   4 1     4     1 4     6 4     1 |
  | 1 1 1 4 1   1 4     1 4       4 1 1   4 1     |     | 6 6 1 4 1   1 4     1 4       4 6 6   4 1     |
  | 1 1   4 1 1   4 1 1   4     1 4   1 1 4 1     |     | 1 6   4 1 1   4 1 1   4     1 4   6 1 4 1     |
  | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |     | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 |
  |       4   1   4 1   1 4 1   1 4         6 6 4 |     |       4   1   4 1   1 4 1   1 4         6 6 4 |
  | 1 1   4   1 1 4       4 1     4         6 6 4 |     | 1 1   4   1 1 4       4 1     4         6 6 4 |
  |       4     1 4       4   1 1 4 6 6 6 6     4 |     |       4     1 4       4   1 1 4 6 6 6 6     4 |
  | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6     4 |     | 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6     4 |
  |     1 4 1     4   1   4 1 1   4     6 6     4 |     |     1 4 1     4   1   4 1 1   4     6 6     4 |
  | 1 1 1 4       4 1 1 1 4     1 4     6 6     4 |     | 1 1 1 4       4 1 1 1 4     1 4     6 6     4 |
  |       4 1   1 4 1 1 1 4       4 4 4 4 4 4 4 4 |     |       4 1   1 4 1 1 1 4       4 4 4 4 4 4 4 4 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(23,23))          'Output' v('15113be4') (grid(23,23))
").


%= fav(v('15113be4'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/15113be4.json']),filename(['/data/evaluation/15113be4.json']),-rotation_match,+shape_match,+mask_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('15113be4'),'muarc_cache/15113be4.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('15113be4')))
%~ warn_skip(clear_saveable_test_info(v('14754a24')))
fav_testcase(v('14754a24')>trn+1,"

   _________________________________       _________________________________
  |   5     5 5       5 5   5     5 |     |   5     5 5       5 5   5     5 |
  | 5 5         5   5     5 5 5     |     | 5 5         5   5     5 2 5     |
  |     4 5   5   5 5     4 5 5 5   |     |     4 5   5   5 5     4 2 2 5   |
  |   5 5 4     5     5     4     5 |     |   2 2 4     5     5     4     5 |
  |     4 5     5   5           5   |     |     4 5     5   5           5   |
  |       5   5 5 5 5   5   5 5   5 |     |       5   5 2 5 5   5   5 5   5 |
  | 5   5     4 5 4 5 5 5 5   5     |     | 5   5     4 2 4 5 5 5 5   5     |
  |         5 5 4 5 5             5 |     |         5 5 4 5 5             5 |
  |     5 5     5   5 5   5 5 4 5   |     |     5 5     5   5 5   5 5 4 5   |
  |     5     5       5 5   5 5 4 5 |     |     5     5       5 5   2 2 4 5 |
  | 5     4         5         5 5 5 |     | 5     4         5         2 5 5 |
  | 5 5 4 5 4 5   5 5 5         5 5 |     | 5 5 4 2 4 5   5 5 2         5 5 |
  | 5     5 5 5   5 4 5 4   5       |     | 5     2 5 5   5 4 2 4   5       |
  | 5     5 5     5   4       5 5   |     | 5     5 5     5   4       5 5   |
  | 5 5 5 5 5       5 5 5   5 5     |     | 5 5 5 5 5       5 5 5   5 5     |
  | 5 5 5 5         5   5 5 5 5   5 |     | 5 5 5 5         5   5 5 5 5   5 |
  | 5   5           5   5   5 5     |     | 5   5           5   5   5 5     |
  | 5         5     5 5 5 5       5 |     | 5         5     5 5 5 5       5 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(16,18))          'Output' v('14754a24') (grid(16,18))
").


%= fav(v('14754a24'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/14754a24.json']),filename(['/data/evaluation/14754a24.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('14754a24'),'muarc_cache/14754a24.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('14754a24')))
%~ warn_skip(clear_saveable_test_info(v('140c817e')))
fav_testcase(v('140c817e')>trn+1,"

   ___________________       ___________________
  | 9 9 9 9 9 9 9 9 9 |     | 9 9 9 1 9 9 1 9 9 |
  | 9 9 9 9 9 9 9 9 9 |     | 9 9 9 1 9 9 1 9 9 |
  | 9 9 9 9 9 9 9 9 9 |     | 9 9 3 1 3 9 1 9 9 |
  | 9 9 9 1 9 9 9 9 9 |     | 1 1 1 2 1 1 1 1 1 |
  | 9 9 9 9 9 9 9 9 9 |     | 9 9 3 1 3 9 1 9 9 |
  | 9 9 9 9 9 9 9 9 9 |     | 9 9 9 1 9 3 1 3 9 |
  | 9 9 9 9 9 9 1 9 9 |     | 1 1 1 1 1 1 2 1 1 |
  | 9 9 9 9 9 9 9 9 9 |     | 9 9 9 1 9 3 1 3 9 |
  | 9 9 9 9 9 9 9 9 9 |     | 9 9 9 1 9 9 1 9 9 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(9,9))            'Output' v('140c817e') (grid(9,9))
").


%= fav(v('140c817e'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/140c817e.json']),filename(['/data/evaluation/140c817e.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('140c817e'),'muarc_cache/140c817e.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('140c817e')))
%~ warn_skip(clear_saveable_test_info(v('137f0df0')))
fav_testcase(v('137f0df0')>trn+1,"

   _____________________       _____________________
  |     5 5     5 5     |     |     5 5 2 2 5 5     |
  |     5 5     5 5     |     |     5 5 2 2 5 5     |
  |                     |     | 1 1 2 2 2 2 2 2 1 1 |
  |     5 5     5 5     |     |     5 5 2 2 5 5     |
  |     5 5     5 5     |     |     5 5 2 2 5 5     |
  |                     |     | 1 1 2 2 2 2 2 2 1 1 |
  |     5 5     5 5     |     |     5 5 2 2 5 5     |
  |     5 5     5 5     |     |     5 5 2 2 5 5     |
  |                     |     |         1 1         |
  |                     |     |         1 1         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,10))          'Output' v('137f0df0') (grid(10,10))
").


%= fav(v('137f0df0'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/137f0df0.json']),filename(['/data/evaluation/137f0df0.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('137f0df0'),'muarc_cache/137f0df0.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('137f0df0')))
%~ warn_skip(clear_saveable_test_info(v('13713586')))
fav_testcase(v('13713586')>trn+1,"

   ___________________________________       ___________________________________
  |                                 5 |     |                                 5 |
  |                       2         5 |     |                       2 2 2 2 2 5 |
  |                       2         5 |     |                       2 2 2 2 2 5 |
  |                                 5 |     |                                 5 |
  |             3                   5 |     |             3 3 3 3 3 3 3 3 3 3 5 |
  |             3                   5 |     |             3 3 3 3 3 3 3 3 3 3 5 |
  |             3                   5 |     |             3 3 3 3 3 3 3 3 3 3 5 |
  |     7       3                   5 |     |     7 7 7 7 3 3 3 3 3 3 3 3 3 3 5 |
  |     7                           5 |     |     7 7 7 7 7 7 7 7 7 7 7 7 7 7 5 |
  |     7                           5 |     |     7 7 7 7 7 7 7 7 7 7 7 7 7 7 5 |
  |     7                           5 |     |     7 7 7 7 7 7 7 7 7 7 7 7 7 7 5 |
  |                                 5 |     |                                 5 |
  |                                 5 |     |                                 5 |
  |                                 5 |     |                                 5 |
  |                                 5 |     |                                 5 |
  |                                 5 |     |                                 5 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(17,16))          'Output' v('13713586') (grid(17,16))
").


%= fav(v('13713586'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/13713586.json']),filename(['/data/evaluation/13713586.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('13713586'),'muarc_cache/13713586.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('13713586')))
%~ warn_skip(clear_saveable_test_info(v('136b0064')))
fav_testcase(v('136b0064')>trn+1,"

   _______________________________       _______________
  | 2   2   6   6 4   5           |     |   5           |
  | 2   2     6   4               |     | 2 2           |
  | 2 2 2     6   4               |     | 1 1 1         |
  |               4               |     |     1 1 1     |
  | 1 1     3 3 3 4               |     |         6     |
  | 1   1     3   4               |     |         6     |
  |   1     3   3 4               |     |         6     |
  |               4               |     |         6     |
  | 1 1     6   6 4               |     |   3 3 3 3     |
  | 1   1     6   4               |     |   6           |
  |   1       6   4               |     |   6           |
  |               4               |     |   1 1 1       |
  | 6   6   1 1   4               |     |               |
  |   6     1   1 4               |     |               |
  |   6       1   4               |     |               |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,15))          'Output' v('136b0064') (grid(7,15))
").


%= fav(v('136b0064'),[no_sol(i(complete),resize_grid(7,7,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([7,7]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/136b0064.json']),filename(['/data/evaluation/136b0064.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('136b0064'),'muarc_cache/136b0064.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('136b0064')))
%~ warn_skip(clear_saveable_test_info(v('12eac192')))
fav_testcase(v('12eac192')>trn+1,"

   _________________       _________________
  |     1 8 1 1 1   |     |     3 3 1 1 1   |
  | 1 5 1 7 1 1     |     | 3 3 3 7 1 1     |
  |   8   7 7 7 8 8 |     |   8   7 7 7 8 8 |
  |   8 8       8   |     |   8 8       8   |
  |   7     8 5 5   |     |   3     3 3 3   |
  | 1             1 |     | 3             3 |
  | 1   8 7 7 8     |     | 3   8 7 7 3     |
  |     8 7 7   8 8 |     |     8 7 7   8 8 |
  |   8 8   8   8 8 |     |   8 8   3   8 8 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(8,9))            'Output' v('12eac192') (grid(8,9))
").


%= fav(v('12eac192'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/12eac192.json']),filename(['/data/evaluation/12eac192.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('12eac192'),'muarc_cache/12eac192.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('12eac192')))
%~ warn_skip(clear_saveable_test_info(v('12997ef3')))
fav_testcase(v('12997ef3')>trn+1,"

   ___________________       _______
  |                   |     |   3 3 |
  |           3       |     | 3 3 3 |
  |                   |     |   3   |
  |           6       |     |   6 6 |
  |   1 1             |     | 6 6 6 |
  | 1 1 1     8       |     |   6   |
  |   1               |     |   8 8 |
  |                   |     | 8 8 8 |
  |                   |     |   8   |
  |                   |       ¯¯¯¯¯¯¯
  |                   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(9,11))           'Output' v('12997ef3') (grid(3,9))
").


%= fav(v('12997ef3'),[no_sol(i(complete),resize_grid(9,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([9,3]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/12997ef3.json']),filename(['/data/evaluation/12997ef3.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 2) ']).
%~ warn_skip(save_supertest(v('12997ef3'),'muarc_cache/12997ef3.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('12997ef3')))
%~ warn_skip(clear_saveable_test_info(v('12422b43')))
fav_testcase(v('12422b43')>trn+1,"

   _______________       _______________
  | 5   8 8       |     | 5   8 8       |
  | 5     7       |     | 5     7       |
  | 5     4 4     |     | 5     4 4     |
  |     3 3       |     |     3 3       |
  |     1 1       |     |     1 1       |
  |               |     |     8 8       |
  |               |     |       7       |
  |               |     |       4 4     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(7,8))            'Output' v('12422b43') (grid(7,8))
").


%= fav(v('12422b43'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/12422b43.json']),filename(['/data/evaluation/12422b43.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(5, 1) ']).
%~ warn_skip(save_supertest(v('12422b43'),'muarc_cache/12422b43.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('12422b43')))
%~ warn_skip(clear_saveable_test_info(v('11e1fe23')))
fav_testcase(v('11e1fe23')>trn+1,"

   _______________________       _______________________
  |                       |     |                       |
  |                       |     |                       |
  |                       |     |                       |
  |                 2     |     |                 2     |
  |                       |     |                       |
  |                       |     |             2         |
  |                       |     |           5           |
  |                       |     |         8   6         |
  |                       |     |                       |
  |     8           6     |     |     8           6     |
  |                       |     |                       |
  |                       |     |                       |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(11,12))          'Output' v('11e1fe23') (grid(11,12))
").


%= fav(v('11e1fe23'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/11e1fe23.json']),filename(['/data/evaluation/11e1fe23.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('11e1fe23'),'muarc_cache/11e1fe23.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('11e1fe23')))
%~ warn_skip(clear_saveable_test_info(v('103eff5b')))
fav_testcase(v('103eff5b')>trn+1,"

   _________________________       _________________________
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
  |         3 1             |     |         3 1             |
  |       4 3               |     |       4 3               |
  |       2   4             |     |       2   4             |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
  |   8 8 8 8 8 8           |     |   2 2 2 4 4 4           |
  |   8 8 8 8 8 8           |     |   2 2 2 4 4 4           |
  |   8 8 8 8 8 8           |     |   2 2 2 4 4 4           |
  |         8 8 8 8 8 8     |     |         3 3 3 3 3 3     |
  |         8 8 8 8 8 8     |     |         3 3 3 3 3 3     |
  |         8 8 8 8 8 8     |     |         3 3 3 3 3 3     |
  |   8 8 8       8 8 8     |     |   4 4 4       1 1 1     |
  |   8 8 8       8 8 8     |     |   4 4 4       1 1 1     |
  |   8 8 8       8 8 8     |     |   4 4 4       1 1 1     |
  |                         |     |                         |
  |                         |     |                         |
  |                         |     |                         |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(12,22))          'Output' v('103eff5b') (grid(12,22))
").


%= fav(v('103eff5b'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/103eff5b.json']),filename(['/data/evaluation/103eff5b.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('103eff5b'),'muarc_cache/103eff5b.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('103eff5b')))
%~ warn_skip(clear_saveable_test_info(v('0f63c0b9')))
fav_testcase(v('0f63c0b9')>trn+1,"

   _______________________________       _______________________________
  |                               |     | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  |             8                 |     | 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 |
  |                               |     | 8                           8 |
  |                     1         |     | 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |
  |                               |     | 1                           1 |
  |                               |     | 1                           1 |
  |                               |     | 2                           2 |
  |       2                       |     | 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 |
  |                               |     | 2                           2 |
  |           3                   |     | 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 |
  |                               |     | 3                           3 |
  |                               |     | 3                           3 |
  |                               |     | 3                           3 |
  |                               |     | 3                           3 |
  |                               |     | 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,15))          'Output' v('0f63c0b9') (grid(15,15))
").


%= fav(v('0f63c0b9'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0f63c0b9.json']),filename(['/data/evaluation/0f63c0b9.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('0f63c0b9'),'muarc_cache/0f63c0b9.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0f63c0b9')))
%~ warn_skip(clear_saveable_test_info(v('0e671a1a')))
fav_testcase(v('0e671a1a')>trn+1,"

   ___________________________       ___________________________
  |                           |     |                           |
  |           3               |     |           3               |
  |                           |     |           5               |
  |                           |     |           5               |
  |                           |     |           5               |
  |                           |     |           5               |
  |                           |     |           5               |
  |                           |     |           5               |
  |                       2   |     |     5 5 5 5 5 5 5 5 5 2   |
  |                           |     |     5     5               |
  |     4                     |     |     4 5 5 5               |
  |                           |     |                           |
  |                           |     |                           |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(13,13))          'Output' v('0e671a1a') (grid(13,13))
").


%= fav(v('0e671a1a'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0e671a1a.json']),filename(['/data/evaluation/0e671a1a.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('0e671a1a'),'muarc_cache/0e671a1a.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0e671a1a')))
%~ warn_skip(clear_saveable_test_info(v('0d87d2a6')))
fav_testcase(v('0d87d2a6')>trn+1,"

   _________________________________________       _________________________________________
  |             1                           |     |             1                           |
  |                                         |     |             1                           |
  |       2 2 2 2 2             2 2 2 2     |     |       1 1 1 1 1             2 2 2 2     |
  |       2 2 2 2 2             2 2 2 2     |     |       1 1 1 1 1             2 2 2 2     |
  |       2 2 2 2 2                         |     |       1 1 1 1 1                         |
  |       2 2 2 2 2   2 2 2         2 2 2 2 |     |       1 1 1 1 1   2 2 2         2 2 2 2 |
  |                   2 2 2         2 2 2 2 |     |             1     2 2 2         2 2 2 2 |
  |     2 2           2 2 2         2 2 2 2 |     |     2 2     1     2 2 2         2 2 2 2 |
  |     2 2                                 |     |     2 2     1                           |
  |             1                           |     |             1                           |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(20,10))          'Output' v('0d87d2a6') (grid(20,10))
").


%= fav(v('0d87d2a6'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0d87d2a6.json']),filename(['/data/evaluation/0d87d2a6.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('0d87d2a6'),'muarc_cache/0d87d2a6.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0d87d2a6')))
%~ warn_skip(clear_saveable_test_info(v('0c9aba6e')))
fav_testcase(v('0c9aba6e')>trn+1,"

   _________       _________
  | 2 2   2 |     |         |
  | 2   2 2 |     |         |
  | 2 2     |     |     8 8 |
  |   2   2 |     | 8   8   |
  |   2 2   |     |       8 |
  | 2     2 |     |   8     |
  | 7 7 7 7 |       ¯¯¯¯¯¯¯¯¯
  | 6   6 6 |
  |   6     |
  |         |
  |       6 |
  | 6 6     |
  | 6   6   |
   ¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(4,13))           'Output' v('0c9aba6e') (grid(4,6))
").


%= fav(v('0c9aba6e'),[no_sol(i(complete),resize_grid(4,6,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([4,6]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0c9aba6e.json']),filename(['/data/evaluation/0c9aba6e.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('0c9aba6e'),'muarc_cache/0c9aba6e.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0c9aba6e')))
%~ warn_skip(clear_saveable_test_info(v('0c786b71')))
fav_testcase(v('0c786b71')>trn+1,"

   _________       _________________
  | 5 5 9 9 |     | 7 5 7 5 5 7 5 7 |
  | 9 5 5 5 |     | 5 5 5 9 9 5 5 5 |
  | 5 7 5 7 |     | 9 9 5 5 5 5 9 9 |
   ¯¯¯¯¯¯¯¯¯      | 9 9 5 5 5 5 9 9 |
                  | 5 5 5 9 9 5 5 5 |
                  | 7 5 7 5 5 7 5 7 |
                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(4,3))            'Output' v('0c786b71') (grid(8,6))
").


%= fav(v('0c786b71'),[no_sol(i(complete),resize_grid(8,6,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([8,6]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0c786b71.json']),filename(['/data/evaluation/0c786b71.json']),-shape_match,-rotation_match,-mask_match,+color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('0c786b71'),'muarc_cache/0c786b71.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0c786b71')))
%~ warn_skip(clear_saveable_test_info(v('0becf7df')))
fav_testcase(v('0becf7df')>trn+1,"

   _____________________       _____________________
  | 1 3                 |     | 1 3                 |
  | 2 8         1       |     | 2 8         3       |
  |         1 1 1       |     |         3 3 3       |
  |         1 1 1       |     |         3 3 3       |
  |     3 3 3 3 1 8     |     |     1 1 1 1 3 2     |
  |     3 3 2   8 8     |     |     1 1 8   2 2     |
  |         2   8 8     |     |         8   2 2     |
  |         2           |     |         8           |
  |         2           |     |         8           |
  |                     |     |                     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,10))          'Output' v('0becf7df') (grid(10,10))
").


%= fav(v('0becf7df'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0becf7df.json']),filename(['/data/evaluation/0becf7df.json']),-rotation_match,+shape_match,+mask_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('0becf7df'),'muarc_cache/0becf7df.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0becf7df')))
%~ warn_skip(clear_saveable_test_info(v('0bb8deee')))
fav_testcase(v('0bb8deee')>trn+1,"

   ___________________________       _____________
  |         1                 |     |   2 2 3     |
  |         1                 |     | 2 2 2   3 3 |
  |         1     3           |     |   2     3   |
  |         1       3 3       |     |     5 8   8 |
  |         1       3         |     | 5 5 5   8   |
  |   2 2   1                 |     |   5   8     |
  | 2 2 2   1                 |       ¯¯¯¯¯¯¯¯¯¯¯¯¯
  |   2     1                 |
  |         1                 |
  | 1 1 1 1 1 1 1 1 1 1 1 1 1 |
  |         1                 |
  |     5   1                 |
  | 5 5 5   1     8   8       |
  |   5     1       8         |
  |         1     8           |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(13,15))          'Output' v('0bb8deee') (grid(6,6))
").


%= fav(v('0bb8deee'),[no_sol(i(complete),resize_grid(6,6,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([6,6]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0bb8deee.json']),filename(['/data/evaluation/0bb8deee.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('0bb8deee'),'muarc_cache/0bb8deee.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0bb8deee')))
%~ warn_skip(clear_saveable_test_info(v('0b17323b')))
fav_testcase(v('0b17323b')>trn+1,"

   _______________________________       _______________________________
  | 1                             |     | 1                             |
  |                               |     |                               |
  |                               |     |                               |
  |                               |     |                               |
  |         1                     |     |         1                     |
  |                               |     |                               |
  |                               |     |                               |
  |                               |     |                               |
  |                 1             |     |                 1             |
  |                               |     |                               |
  |                               |     |                               |
  |                               |     |                               |
  |                               |     |                         2     |
  |                               |     |                               |
  |                               |     |                               |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(15,15))          'Output' v('0b17323b') (grid(15,15))
").


%= fav(v('0b17323b'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0b17323b.json']),filename(['/data/evaluation/0b17323b.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(2, 1) ']).
%~ warn_skip(save_supertest(v('0b17323b'),'muarc_cache/0b17323b.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0b17323b')))
%~ warn_skip(clear_saveable_test_info(v('0a2355a6')))
fav_testcase(v('0a2355a6')>trn+1,"

   _______________________       _______________________
  |                       |     |                       |
  |     8 8 8 8 8         |     |     2 2 2 2 2         |
  |     8   8   8         |     |     2   2   2         |
  |     8 8 8 8 8         |     |     2 2 2 2 2         |
  |     8   8       8 8 8 |     |     2   2       1 1 1 |
  |     8   8       8   8 |     |     2   2       1   1 |
  |     8 8 8       8 8 8 |     |     2 2 2       1 1 1 |
  |                       |     |                       |
  | 8 8 8                 |     | 3 3 3                 |
  | 8   8       8 8 8 8   |     | 3   3       1 1 1 1   |
  | 8 8 8 8 8   8     8   |     | 3 3 3 3 3   1     1   |
  | 8       8   8     8   |     | 3       3   1     1   |
  | 8 8 8 8 8   8 8 8 8   |     | 3 3 3 3 3   1 1 1 1   |
  |                       |     |                       |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(11,14))          'Output' v('0a2355a6') (grid(11,14))
").


%= fav(v('0a2355a6'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0a2355a6.json']),filename(['/data/evaluation/0a2355a6.json']),-rotation_match,-color_match,+shape_match,+mask_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('0a2355a6'),'muarc_cache/0a2355a6.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0a2355a6')))
%~ warn_skip(clear_saveable_test_info(v('0a1d4ef5')))
fav_testcase(v('0a1d4ef5')>trn+1,"

   _____________________________________________________________       _______
  |   2       2   8       2   2   2     2 8     2   8           |     | 3 1 9 |
  |       3 3 3 3 3 3       1 1 1 1 1 1 1 1 2 8 8 2             |     | 6 4 1 |
  | 8   2 3 3 3 3 3 3     2 1 1 1 1 1 1 1 1       9 9 9 9 9     |       ¯¯¯¯¯¯¯
  | 8   8 3 3 3 3 3 3 2 2 2 1 1 1 1 1 1 1 1 8   8 9 9 9 9 9 8 8 |
  | 2 8   3 3 3 3 3 3 8 8   1 1 1 1 1 1 1 1     2 9 9 9 9 9     |
  | 8     3 3 3 3 3 3     2 2 2 8 8 8 8   2 8 2   9 9 9 9 9     |
  |       8     8     2 8 2     2             8   9 9 9 9 9 8 8 |
  |   8 8 8     2   8       2 8 8       8   2   2   8     8 8   |
  |                   2 2 2     2 8 8 2     2     2     8 2 8   |
  | 8           8 2 8 2 8             2 8 2           8         |
  |     2 6 6 6 6   8     4 4 4 4 4 4 2       8     2       2   |
  | 8   8 6 6 6 6   8   8 4 4 4 4 4 4 2   2 2 2   1 1 1 1 1 8   |
  |   2   6 6 6 6 8   2 2 4 4 4 4 4 4 8   8       1 1 1 1 1   2 |
  |   2 8 6 6 6 6 8   8   4 4 4 4 4 4   8 2 2   2 1 1 1 1 1   8 |
  |     2 6 6 6 6       2 4 4 4 4 4 4     8   8 8 1 1 1 1 1 8   |
  |       6 6 6 6     2 8   8 8 2 8   8           1 1 1 1 1   2 |
  | 2 8   6 6 6 6   2         2 8       2 8     2               |
  |     8   2           8       2 8                 8 2       2 |
  |     2   8       2 8   8       8   8 8 8   8     8   2 2   2 |
  | 8           8 8 2 2 8   8 2 2 8         8   2   8       8 2 |
  | 2 2         2 8   8     2 2 8     2       2 2 2       2 2 8 |
  |   8 8     8 8   8   8                   2 2         8 2     |
  |     2 8 2   2     8       2   8       2 8 8   8   2       8 |
  | 2               8 8   2   8             2 2     2     8 8   |
  | 8 2       8   8   8 2       8     8   2     8   2 2 8       |
  |   8   2 2 8 2 8   2 2       2 2 2 2 2 2       8   8     8 2 |
  |     2 8 2 8                 8     2   2 2     8   2     8 8 |
  |         8       8   2 8                     2 8 2 8     8   |
  | 8 2   2 8 8       2       8 8   8       8 2 8 8   2 8 2 2 2 |
  | 2   8 8       8     8   8       8   2     8   8     2 8     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(30,30))          'Output' v('0a1d4ef5') (grid(3,2))
").


%= fav(v('0a1d4ef5'),[no_sol(i(complete),resize_grid(3,3,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0a1d4ef5.json']),filename(['/data/evaluation/0a1d4ef5.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('0a1d4ef5'),'muarc_cache/0a1d4ef5.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0a1d4ef5')))
%~ warn_skip(clear_saveable_test_info(v('09c534e7')))
fav_testcase(v('09c534e7')>trn+1,"

   _________________________________________       _________________________________________
  |                                         |     |                                         |
  |     1 1 1 1                 1 1 1 1     |     |     1 1 1 1                 1 1 1 1     |
  |     1 3 1 1                 1 1 4 1     |     |     1 3 3 1                 1 4 4 1     |
  |     1 1 1 1                 1 1 1 1     |     |     1 1 1 1                 1 4 4 1     |
  |         1                   1 1 1 1     |     |         1                   1 1 1 1     |
  |     1 1 1 1 1 1                 1       |     |     1 1 1 1 1 1                 1       |
  |     1 1 1 1 1 1   1 1 1       1 1 1     |     |     1 3 3 3 3 1   1 1 1       1 1 1     |
  |     1 1 1 1 1 1 1 1 1 1       1 1 1     |     |     1 3 3 3 3 1 1 1 3 1       1 4 1     |
  |     1 1 1 1 1 1   1 1 1       1 1 1     |     |     1 1 1 1 1 1   1 1 1       1 1 1     |
  |                                         |     |                                         |
  |                                         |     |                                         |
  |                                         |     |                                         |
  |           1 1 1 1     1 1 1             |     |           1 1 1 1     1 1 1             |
  |           1 1 2 1 1 1 1 1 1             |     |           1 2 2 1 1 1 1 2 1             |
  | 1 1 1     1 1 1 1     1 1 1             |     | 1 1 1     1 2 2 1     1 1 1             |
  | 1 1 1 1 1 1 1 1 1                       |     | 1 2 1 1 1 1 1 1 1                       |
  | 1 1 1         1                         |     | 1 2 1         1                         |
  | 1 1 1       1 1 1                       |     | 1 1 1       1 1 1                       |
  |             1 1 1                       |     |             1 2 1                       |
  |             1 1 1                       |     |             1 1 1                       |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(20,20))          'Output' v('09c534e7') (grid(20,20))
").


%= fav(v('09c534e7'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/09c534e7.json']),filename(['/data/evaluation/09c534e7.json']),-rotation_match,+shape_match,+mask_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('09c534e7'),'muarc_cache/09c534e7.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('09c534e7')))
%~ warn_skip(clear_saveable_test_info(v('0934a4d8')))
fav_testcase(v('0934a4d8')>trn+1,"

   _____________________________________________________________       ___________
  | 9 9 2 3 4 4 7 5 3 3 6 6 3 5 6 4 4 6 5 3 6 6 3 3 5 7 4 4 3 2 |     | 3 1 4 4 4 |
  | 7 9 3 5 4 4 5 7 3 3 6 6 6 3 4 6 6 4 3 6 6 6 3 3 7 5 4 4 5 3 |     | 3 4 1 4 4 |
  | 3 2 9 9 7 5 4 4 4 1 3 3 6 4 4 7 7 4 4 6 3 8 8 8 8 8 5 7 9 9 |     | 6 6 3 3 5 |
  | 2 3 7 9 5 7 4 4 1 4 3 3 4 6 7 4 4 7 6 4 3 8 8 8 8 8 7 5 9 7 |     | 4 3 5 2 3 |
  | 7 7 9 3 9 9 5 3 3 6 6 4 6 7 9 9 9 9 7 6 4 8 8 8 8 8 9 9 3 9 |       ¯¯¯¯¯¯¯¯¯¯¯
  | 7 7 3 9 7 9 3 2 5 3 4 6 2 6 9 9 9 9 6 2 6 8 8 8 8 8 9 7 9 3 |
  | 9 3 7 7 3 2 9 9 6 4 4 7 9 2 6 7 7 6 2 9 7 4 4 6 9 9 2 3 7 7 |
  | 3 9 7 7 2 3 7 9 4 6 7 4 2 9 2 6 6 2 9 2 4 7 6 4 9 7 3 2 7 7 |
  | 3 3 4 1 3 5 6 4 2 4 7 7 1 6 7 2 2 7 6 1 7 7 4 2 4 6 5 3 1 4 |
  | 3 3 1 4 6 3 4 6 2 2 7 1 6 1 2 7 7 2 1 6 1 7 2 2 6 4 3 6 4 1 |
  | 6 6 3 3 6 4 4 7 1 1 2 4 7 2 1 6 6 1 2 7 4 2 1 1 7 4 4 6 3 3 |
  | 6 6 3 3 4 6 7 4 1 3 2 2 2 7 6 1 1 6 7 2 2 2 3 1 4 7 6 4 3 3 |
  | 3 6 6 4 6 2 9 2 9 9 9 7 2 4 1 7 7 1 4 2 7 9 9 9 2 9 2 6 4 6 |
  | 5 3 4 6 7 6 2 9 9 9 7 9 2 2 7 7 7 7 2 2 9 7 9 9 9 2 6 7 6 4 |
  | 6 4 4 7 9 9 6 2 9 7 9 9 3 1 2 4 4 2 1 3 9 9 7 9 2 6 9 9 7 4 |
  | 4 6 7 4 9 9 7 6 7 9 9 9 1 1 2 2 2 2 1 1 9 9 9 7 6 7 9 9 4 7 |
  | 4 6 7 4 9 9 7 6 7 9 9 9 1 1 2 2 2 2 1 1 9 9 9 7 6 7 9 9 4 7 |
  | 6 4 4 7 9 9 6 2 9 7 9 9 3 1 2 4 4 2 1 3 9 9 7 9 2 6 9 9 7 4 |
  | 5 3 4 6 7 6 2 9 9 9 7 9 2 2 7 7 7 7 2 2 9 7 9 9 9 2 6 7 6 4 |
  | 3 6 6 4 6 2 9 2 9 9 9 7 2 4 1 7 7 1 4 2 7 9 9 9 2 9 2 6 4 6 |
  | 6 6 3 3 4 6 7 4 1 3 2 2 2 7 6 1 1 6 7 2 2 2 3 1 4 7 6 4 3 3 |
  | 6 6 3 3 6 4 4 7 1 1 2 4 7 2 1 6 6 1 2 7 4 2 1 1 7 4 4 6 3 3 |
  | 3 3 1 4 6 3 4 6 2 2 7 1 6 1 2 7 7 2 1 6 1 7 2 2 6 4 3 6 4 1 |
  | 3 3 4 1 3 5 6 4 2 4 7 7 1 6 7 2 2 7 6 1 7 7 4 2 4 6 5 3 1 4 |
  | 3 9 7 7 2 3 7 9 4 6 7 4 2 9 2 6 6 2 9 2 4 7 6 4 9 7 3 2 7 7 |
  | 9 3 7 7 3 2 9 9 6 4 4 7 9 2 6 7 7 6 2 9 7 4 4 6 9 9 2 3 7 7 |
  | 7 7 3 9 7 9 3 2 5 3 4 6 2 6 9 9 9 9 6 2 6 4 3 5 2 3 9 7 9 3 |
  | 7 7 9 3 9 9 5 3 3 6 6 4 6 7 9 9 9 9 7 6 4 6 6 3 3 5 9 9 3 9 |
  | 2 3 7 9 5 7 4 4 1 4 3 3 4 6 7 4 4 7 6 4 3 3 4 1 4 4 7 5 9 7 |
  | 3 2 9 9 7 5 4 4 4 1 3 3 6 4 4 7 7 4 4 6 3 3 1 4 4 4 5 7 9 9 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(30,30))          'Output' v('0934a4d8') (grid(5,4))
").


%= fav(v('0934a4d8'),[no_sol(i(complete),resize_grid(4,4,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([4,4]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0934a4d8.json']),filename(['/data/evaluation/0934a4d8.json']),-shape_match,-rotation_match,-mask_match,-color_match,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('0934a4d8'),'muarc_cache/0934a4d8.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0934a4d8')))
%~ warn_skip(clear_saveable_test_info(v('08573cc6')))
fav_testcase(v('08573cc6')>trn+1,"

   _______________________       _______________________
  | 3 2                   |     |                       |
  |                       |     |                       |
  |                       |     |                       |
  |                       |     |                       |
  |                       |     |     3 3 3 3 3 3 2     |
  |                       |     |     2           2     |
  |             1         |     |     2   3 3 1   2     |
  |                       |     |     2   2       2     |
  |                       |     |     2   2       2     |
  |                       |     |     2   2 3 3 3 3     |
  |                       |     |     2                 |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(11,11))          'Output' v('08573cc6') (grid(11,11))
").


%= fav(v('08573cc6'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/08573cc6.json']),filename(['/data/evaluation/08573cc6.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('08573cc6'),'muarc_cache/08573cc6.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('08573cc6')))
%~ warn_skip(clear_saveable_test_info(v('070dd51e')))
fav_testcase(v('070dd51e')>trn+1,"

   _____________________       _____________________
  |                     |     |                     |
  |                     |     |                     |
  |       4             |     |       4             |
  |                     |     |       4             |
  |     3         3     |     |     3 4 3 3 3 3     |
  |                     |     |       4             |
  |                     |     |       4             |
  |                     |     |       4             |
  |     7     7         |     |     7 4 7 7         |
  |                     |     |       4             |
  |       4             |     |       4             |
  |                     |     |                     |
  |           9         |     |           9         |
  |                     |     |           9         |
  |   8         8       |     |   8 8 8 8 9 8       |
  |                     |     |           9         |
  |                     |     |           9         |
  |                     |     |           9         |
  |           9         |     |           9         |
  |                     |     |                     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,20))          'Output' v('070dd51e') (grid(10,20))
").


%= fav(v('070dd51e'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/070dd51e.json']),filename(['/data/evaluation/070dd51e.json']),-rotation_match,-mask_match,+shape_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',grid_size_same,alphabetical_v,'(2, 1) ','(2, 1)']).
%~ warn_skip(save_supertest(v('070dd51e'),'muarc_cache/070dd51e.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('070dd51e')))
%~ warn_skip(clear_saveable_test_info(v('0692e18c')))
fav_testcase(v('0692e18c')>trn+1,"

   _______       ___________________
  |     6 |     |             6 6   |
  |   6   |     |             6   6 |
  | 6     |     |               6 6 |
   ¯¯¯¯¯¯¯      |       6 6         |
                |       6   6       |
                |         6 6       |
                | 6 6               |
                | 6   6             |
                |   6 6             |
                 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(3,3))            'Output' v('0692e18c') (grid(9,9))
").


%= fav(v('0692e18c'),[no_sol(i(complete),resize_grid(9,9,Color),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),out_grid([9,9]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0692e18c.json']),filename(['/data/evaluation/0692e18c.json']),-shape_match,-rotation_match,-mask_match,+color_match,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('0692e18c'),'muarc_cache/0692e18c.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0692e18c')))
%~ warn_skip(clear_saveable_test_info(v('0607ce86')))
fav_testcase(v('0607ce86')>trn+1,"

   _____________________________________________       _____________________________________________
  |   3       3 3         3                     |     |                                             |
  |   1 1 2 3 3   1 1 2 3 3 1 1 1 2 3 3   3     |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   1 1 3 3 3   1 1 2 3 3   1 1 2 3 3 3   3   |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   1 1 2 3 3   1 1 2 3 3   1 1 1 3 3       3 |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   1 3 3 3 1   1 1 2 3 3   1 1 2 3 3         |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   8 8 8 8 8   8 8 8 8 8   8 8 8 8 8         |     |   8 8 8 8 8   8 8 8 8 8   8 8 8 8 8         |
  |                                             |     |                                             |
  |   3 1 3 3 3   3 1 2 3 3   1 1 2 3 3     3   |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   1 1 2 3 3   1 3 2 1 3   1 1 2 3 3         |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  | 1 1 1 2 3 3   1 1 2 3 3 3 1 3 2 3 3         |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   8 1 8 8 3   8 8 8 8 8   1 8 8 8 8         |     |   8 8 8 8 8   8 8 8 8 8   8 8 8 8 8         |
  |       1                               3 3   |     |                                             |
  |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3     3   |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   1 1 3 3 3   1 1 2 3 3   1 1 2 3 3         |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  |   1 1 2 3 3   1 1 1 3 3   1 1 2 3 1         |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  | 1 1 1 2 3 3   1 1 2 3 1   1 1 2 3 3         |     |   1 1 2 3 3   1 1 2 3 3   1 1 2 3 3         |
  | 3 8 8 8 3 3 1 8 8 8 8 8   8 8 8 8 8     1   |     |   8 8 8 8 8   8 8 8 8 8   8 8 8 8 8         |
  |   1                                         |     |                                             |
  |   3 3   3   3   1 1   3                   1 |     |                                             |
  |     3     1           3                     |     |                                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(22,21))          'Output' v('0607ce86') (grid(22,21))
").


%= fav(v('0607ce86'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/0607ce86.json']),filename(['/data/evaluation/0607ce86.json']),-rotation_match,-mask_match,+shape_match,+color_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('0607ce86'),'muarc_cache/0607ce86.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('0607ce86')))
%~ warn_skip(clear_saveable_test_info(v('05a7bcf2')))
fav_testcase(v('05a7bcf2')>trn+1,"

   _____________________________________________________________       _____________________________________________________________
  |                   8                     2                   |     |                   8                     2                   |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |                   8                 2 2 2                   |     |                   8                 2 2 2                   |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |     4 4           8                     2                   |     |     3 3 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 |
  |     4 4           8                     2                   |     |     3 3 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 |
  |                   8                 2 2 2                   |     |                   8                 2 2 2                   |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |                   8               2 2 2 2                   |     |                   8               2 2 2 2                   |
  |                   8                 2 2 2                   |     |                   8                 2 2 2                   |
  |                   8                     2                   |     |                   8                     2                   |
  |     4             8                   2 2                   |     |     3 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 |
  |     4             8                 2 2 2                   |     |     3 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 2 |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |                   8                     2                   |     |                   8                     2                   |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |                   8                     2                   |     |                   8                     2                   |
  |                   8                     2                   |     |                   8                     2                   |
  |         4 4       8                   2 2                   |     |         3 3 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 |
  |         4 4       8                 2 2 2                   |     |         3 3 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 2 |
  |           4       8                 2 2 2                   |     |           3 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 2 |
  |                   8                     2                   |     |                   8                     2                   |
  |                   8                     2                   |     |                   8                     2                   |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |                   8                     2                   |     |                   8                     2                   |
  |       4 4         8                   2 2                   |     |       3 3 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 |
  |                   8                     2                   |     |                   8                     2                   |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |                   8                   2 2                   |     |                   8                   2 2                   |
  |                   8                     2                   |     |                   8                     2                   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(30,30))          'Output' v('05a7bcf2') (grid(30,30))
").


%= fav(v('05a7bcf2'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/05a7bcf2.json']),filename(['/data/evaluation/05a7bcf2.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(3, 1) ']).
%~ warn_skip(save_supertest(v('05a7bcf2'),'muarc_cache/05a7bcf2.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('05a7bcf2')))
%~ warn_skip(clear_saveable_test_info(v('03560426')))
fav_testcase(v('03560426')>trn+1,"

   _____________________       _____________________
  |                     |     | 8 8 8               |
  |                     |     | 8 8 8               |
  |                     |     | 8 8 8               |
  |                     |     | 8 8 7 7             |
  |                     |     |     7 2 2 2         |
  |                     |     |       2 2 2         |
  | 8 8 8               |     |                     |
  | 8 8 8               |     |                     |
  | 8 8 8   7 7   2 2 2 |     |                     |
  | 8 8 8   7 7   2 2 2 |     |                     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(10,10))          'Output' v('03560426') (grid(10,10))
").


%= fav(v('03560426'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/03560426.json']),filename(['/data/evaluation/03560426.json']),grid_size_same]).
%~ warn_skip(save_supertest(v('03560426'),'muarc_cache/03560426.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('03560426')))
%~ warn_skip(clear_saveable_test_info(v('00dbd492')))
fav_testcase(v('00dbd492')>trn+1,"

   ___________________       ___________________
  | 2 2 2 2 2 2 2     |     | 2 2 2 2 2 2 2     |
  | 2           2     |     | 2 4 4 4 4 4 2     |
  | 2           2     |     | 2 4 4 4 4 4 2     |
  | 2     2     2     |     | 2 4 4 2 4 4 2     |
  | 2           2     |     | 2 4 4 4 4 4 2     |
  | 2           2     |     | 2 4 4 4 4 4 2     |
  | 2 2 2 2 2 2 2     |     | 2 2 2 2 2 2 2     |
  |                   |     |                   |
  |                   |     |                   |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(9,9))            'Output' v('00dbd492') (grid(9,9))
").


%= fav(v('00dbd492'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/00dbd492.json']),filename(['/data/evaluation/00dbd492.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(4, 1) ']).
%~ warn_skip(save_supertest(v('00dbd492'),'muarc_cache/00dbd492.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('00dbd492')))
%~ warn_skip(clear_saveable_test_info(v('009d5c81')))
fav_testcase(v('009d5c81')>trn+1,"

   _____________________________       _____________________________
  |                             |     |                             |
  |                             |     |                             |
  |         8 8   8   8 8       |     |         3 3   3   3 3       |
  |         8   8   8   8       |     |         3   3   3   3       |
  |         8 8   8   8 8       |     |         3 3   3   3 3       |
  |                             |     |                             |
  |                             |     |                             |
  |                             |     |                             |
  |                             |     |                             |
  |                             |     |                             |
  |       1   1                 |     |                             |
  |         1                   |     |                             |
  |       1 1 1                 |     |                             |
  |                             |     |                             |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(14,14))          'Output' v('009d5c81') (grid(14,14))
").


%= fav(v('009d5c81'),[no_sol(i(complete),copy_grid(in),incomplete),test_suite([less_fav]),test_suite([evaluation]),test_suite([eval400]),test_suite([devaluation]),loadmask(['/data/evaluation/*.json']),loadmask(['./data/devaluation/*.json']),filename(['/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/data/devaluation/009d5c81.json']),filename(['/data/evaluation/009d5c81.json']),-rotation_match,-mask_match,-color_match,+shape_match,grid_size_same,alphabetical_v,'(5, 1) ']).
%~ warn_skip(save_supertest(v('009d5c81'),'muarc_cache/009d5c81.ansi.pl'))
%~ warn_skip(clear_saveable_test_info(v('009d5c81')))
%~ warn_skip(clear_saveable_test_info(v('00576224')))
fav_testcase(v('00576224')>trn+1,"

   _____       _____________
  | 7 9 |     | 7 9 7 9 7 9 |
  | 4 3 |     | 4 3 4 3 4 3 |
   ¯¯¯¯¯      | 9 7 9 7 9 7 |
              | 3 4 3 4 3 4 |
              | 7 9 7 9 7 9 |
              | 4 3 4 3 4 3 |
               ¯¯¯¯¯¯¯¯¯¯¯¯¯

        'Training Pair #2 Input' (grid(2,2))            'Output' v('00576224') (grid(6,6))
").

%fav(t('05269061'),[indiv(complete)]).
fav(t('007bbfb7'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,fractal_repetition,'(5, 1)']).
fav(t('00d62c1b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),loop_filling,'(5, 1)']).

fav(v(e99362f0),[combine_are_cells]).
fav(v('27a77e38'),[second_most_used_color]).
fav(v('7c9b52a0'),[giz([never_repair])]).
fav(v('7ee1c6ea'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).

%fav(v('7c9b52a0'),[indiv([force_by_color,pbox_vm])]).
fav(v('7c9b52a0'),[indiv([colormass,bg_shapes(colormass)])]).
fav(t('484b58aa'),[indiv(i_repair_patterns_f)]).
fav(t('0dfd9992'),[indiv(i_repair_patterns_f)]).
fav(v('af22c60d'),[indiv(i_repair_patterns_f)]).
fav(v('903d1b4a'),[indiv(i_repair_patterns_f)]).
fav(v('e66aafb8'),[indiv(i_repair_patterns_f)]).
fav(v('4aab4007'),[indiv(i_repair_patterns_f)]).
fav(v('1e97544e'),[indiv(i_repair_patterns_f)]).
fav(t('b8825c91'),[indiv(i_repair_patterns_f)]).
fav(v('47996f11'),[indiv(i_repair_patterns_f)]).
fav(v('981571dc'),[indiv(i_repair_patterns_f)]).
fav(v('929ab4e9'),[indiv(i_repair_patterns_f)]).
fav(t('ff805c23'),[indiv(i_repair_patterns_f),human(repair_in_vm(repair_fourway),get(changed),trim_to_rect)]). fav(t('ff805c23'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_expansion,pattern_completion,crop,'(3, 1)']). %fav(t('ff805c23'),[ohuman(repair_in_vm(repair_repeats(blue)),get(changed),trim_to_rect)]).
%fav(TestID,[indiv(i_repair_patterns_f)]):- is_symgrid(TestID).
fav(t('3631a71a'),[indiv(i_repair_patterns_f)]).   fav(t('3631a71a'),[ohuman(repair_in_vm(repair_fourway),get(repaired))]). fav(t('3631a71a'),[-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_rotation,pattern_expansion,image_filling,'(4, 1)']).
fav(t('29ec7d0e'),[indiv(i_repair_patterns_f),human(repair_in_vm(repair_repeats(Black)),get(repaired))]):- get_black(Black). fav(t('29ec7d0e'),[-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,pattern_expansion,image_filling,detect_grid,'(4, 1)']).
fav(v('de493100'),[indiv(i_repair_patterns_f)]). fav(v(de493100),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
fav(t('9ecd008a'),[indiv(i_repair_patterns_f), human([indiv_is_one_hole,fix_image,selected_indiv,trim_to_rect])]).
fav(t('73251a56'),[indiv(i_repair_patterns_f),learn([learn_mapping_stateful]),ohuman([apply_mapping_stateful])]).


fav(t('9aec4887'),[indiv_option(color_blind),todo_sol([find_individuals([hollow,inside([rectangle])],I),rest_indivdual(Is),put_inside(Is,I),
  if_edge_strong([color=C]),touch(Is,Point),set_color(C,Point)])]).

fav(t(a740d043),[ohuman(remove_color(blue))]).

fav(t('47c1f68c'),[hedra,
   human(compute_max_color(C1),compute_next_color(C2),remove_color(C1),subst_color(C2,C1),blur(flipH),blur(flipV))]).
fav(t('47c1f68c'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),recoloring,image_repetition,image_reflection,find_the_intruder,detect_grid,crop,color_guessing,'(3, 1)']).
fav(v('cad67732'),[ohuman(i(whole),first_object_term(rotated),learn_rule),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
fav(t('27a28665'),[ohuman(i(whole),learn_rule)]).
%fav(t('27a28665'),[ohuman(i([glyphic]),one_obj,into_monochrome,db(largest:colorlesspoints,out:grid:p(1,1):color),resize_grid(1,1))]).
%fav(t('27a28665'),[ohuman(i(whole),one_obj,into_monochrome,db(largest:colorlesspoints,out:grid:p(1,1):color),resize_grid(1,1))]).
fav(t('27a28665'),[learn([shape_to_color]),no_sol([make_box(1),shape_to_color(C),cls_with(C)])]).
fav(t('27a28665'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_negative,associate_images_to_patterns,associate_colors_to_patterns,'(7, 3)']).
%fav(t('44f52bb0'),[ohuman(i(whole),print_grid,get_vm(objs,[Obj|_]),print_grid,(call(iz(Obj,symmetric_type(flipH)))->(print_grid,set_out([[blue]]));(print_grid,set_out([[orange]]))))]).
fav(t('d8c310e9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_repetition,pattern_expansion,pattern_completion,'(3, 1)']).
fav(t('44f52bb0'),[ohuman(i(whole),first_object_bool(symmetric_type(flipH)),learn_rule)]).

fav(t('40853293'),[indiv(by_color(2))]).
%fav(t(d631b094),ohuman(globalpoints,grid_target=[get(points)],maplist(arg(1)))).
fav(t('d631b094'),[indiv(lo_dots),ohuman(i(lo_dots),get(objs),learn_rule)]).
fav(t('d631b094'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,dominant_color,count_tiles,'(4, 1)']).

%fav(t('0d3d703e'),[ohuman(i([columns,done]),db(objs:color,color),get(objs),learn_rule),-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),associate_colors_to_colors,'(4, 1)']).
fav(t('0d3d703e'),[indiv(i_columns),ohuman(i([columns,done]),get(objs),learn_rule),-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),associate_colors_to_colors,'(4, 1)']).
%fav(t('a85d4709'),[ohuman(i([rows,done]),o([rows,done]),   db(objs:colorlesspoints,color=Color),set(objs:rectangle,objs:monochrome=true,objs:color=Color)),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),summarize,separate_images,associate_colors_to_images,'(4, 1)']).
fav(t('a85d4709'),[indiv(i_rows),learn(i([rows,done]),o([rows,done]),learn_rule),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),summarize,separate_images,associate_colors_to_images,'(4, 1)']).

fav(t('60b61512'),[indiv(pbox_vm_special_sizes(3,3))]).

fav(t('d0f5fe59'),[%indiv(colormass),
  ohuman(color(largest,Color),ray(Color-point_01_01,count),trim_to_rect),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),separate_shapes,pairwise_analogy,count_shapes,associate_images_to_numbers,'(3, 1)']).

/*
  Grid = [[1,1,1,1]]
  The Drawer does 
  Draw Grid = loc2D(1,1),line_hv(h), pen(1), mass(4)
  
  % Grid<->Drawer   Drawer<->Grid
  
  DSL for Grid 
  DSL for Drawer -> 
  DSL for Solver
  
  Slover = [- pen(1), + pen(2)]
  
  DrawGrid = loc2D(1,1),line_hv(h), pen(2), mass(5)
  Grid = [[2,2,2,2,2]]
*/
/*
fav(t('44f52bb0'),[ohuman(i(whole),grid_call((
  get_vm(objs,[Obj|_]),(iz(Obj,symmetric_type(flipH))->set_vm(grid,[[blue]]);set_vm(grid,[[orange]])),set_vm(grid,[[orange]]))))]).
fav(t('44f52bb0'),[lmDSL(into_monochrome,grid_to_individual(whole),one_obj,resize_grid(1,1,Color),db(largest:symmetric_type(flipH),Color)), -shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),detect_symmetry,associate_images_to_bools,'(6, 2)']).
*/

fav(t('f76d97a5'),[indiv([include_black,force_by_color]),was__lmDSL([compute_max_color(C1),compute_next_color(C2),remove_color(C1),subst_color(C2,C1)])]).

fav(t('25d487eb'),[indiv(colormass),ohuman([rocketship])]).
fav(t('25d487eb'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),draw_line_from_point,direction_guessing,color_guessing,'(3, 1)']).
fav(v('762cd429'),[grid_size_same,ohuman(i([shape_lib(filled_squares),delete_rest,shrink_all_to_size(1),tighten_grid_arround_objects])),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).

%fav(t('6e82a1ae'),[ohuman([rocketship])]).
fav(v('e41c6fd3'),[indiv(colormass), ohuman([forall(((iz(X,outl),color(X,cyan),vert_pos(X,Vert))),
                                                                               (iz(Y,outl),vert_pos(Y,Vert)))])]).
fav(t(c444b776),[detect_grid]).
fav(v('94133066'),[ohuman([largest_indiv,trim_to_rect,rot90,flipV])]).
fav(v('762cd429'),[indiv(shape_lib(filled_squares)),ohuman(delete_rest,shrink_all_to_size(1),tighten_grid_arround_objects)]).
fav(v(f9d67f8b),[grid_size_same,ohuman([overlay_each_pattern]),-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).

fav(t('e9bb6954'),[debug_indiv]).
fav(t('5582e5ca'),[grid_size_same,ohuman([compute_max_color(_134548),cls_with(_134548)]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),dominant_color,count_tiles,'(3, 1)']).
fav(t('6f8cd79b'),[grid_size_same,ohuman([add_borders(cyan)]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),ex_nihilo,contouring,'(4, 1)']).
fav(t('dae9d2b5'),[ohuman([cut_in_half,overlay_all,set_all_fg(magenta)])]).
fav(t('d6ad076f'),[ohuman([find_smaller,shoot_at_other,wide_beam])]).
fav(t('d511f180'),[indiv(i_by_color),ohuman([swap_colors(cyan,silver)])]).
fav(t('a79310a0'),[ohuman([gravity(1,s),swap_colors(cyan,red)])]).
fav(t('a48eeaf7'),[ohuman([largest_indiv(I),tiny_individuals(Is),gravity_to(Is,I)])]).
fav(t('97999447'),[ohuman([find_ones,until_edges([copy_right(silver),copy_right(sameR)])])]).
fav(t('8be77c9e'),[ohuman([grow([[sameR],[flipV]])])]).
fav(t('7f4411dc'),[ohuman([shave_away_1s])]).
fav(t('7b6016b9'),[ohuman([fillFromBorder(green),subst_color(black,red)])]).
fav(t('6f8cd79b'),[ohuman([add_borders(cyan)])]).
fav(t('6d58a25d'),[debug_indiv,print_grid,"the blue object is a downward beam maker, each beam must connect to one of its colors_cc "]).
fav(t('6cf79266'),[learn([find(nines),remove_them]),ohuman(reverse_learned)]).
fav(v(f9d67f8b),[ohuman([overlay_each_pattern])]).



fav(t('9d9215db'),[ohuman([overlay_each_pattern])]).
fav(t('810b9b61'),[ohuman([(iz(X,rectangle),iz(X,hollow),iz(X,thick1),iz(X,noexit))-->color(X,green)])]).
fav(v('1d398264'),[ohuman([((iz(X,keypad),iz(X,multicolor),centerof(X,C))-->sunburst(C))])]).
fav(v('e9bb6954'),[ohuman([(iz(X,keypad), iz(X,monocolor),centerof(X,C)-->starburst(C))]),e('box of nine draw outward, if you hit a drawn line blacken it')]).
fav(t('5c2c9af4'),[ohuman([two_closest_dots_to_edge,make_a_box,grow_box_that_much_bigger,grow_box_that_much_bigger,grow_box_that_much_bigger])]).
fav(t('5582e5ca'),[ohuman([compute_max_color(C1),cls_with(C1)])]).
fav(t('5521c0d9'),[ohuman([with_each_indiv,move_above_itself])]).
fav(t('5521c0d9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,measure_length,'(3, 1)']).
fav(t('5117e062'),[ohuman([find_two_color_indivs,selected_indiv,trim_to_rect,main_color,paint_landscape])]).
fav(t('44d8ac46'),[ohuman([find_individuals([hollow,boxes,inside([rectangle])],I),indiv_fill_color(I,red)])]).
fav(t('447fd412'),[ohuman([find_two_color_indivs,find_lesser_block,select_scaled_versions,builds,create_greater_blocks])]).
fav(t('447fd412'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_resizing,pattern_repetition,draw_pattern_from_point,'(3, 1)']).
fav(t('3c9b0459'),[ohuman([rot180])]).

%fav(t('d511f180'),[ohuman([compute_max_color(C1),compute_next_color(C2),swap_colors(C2,C1)])]).
fav(t(dae9d2b5),[ohuman([cut_in_half,overlay_all,set_all_fg(magenta)]),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,recoloring,pattern_juxtaposition,'(5, 2)']).


fav(v('4b6b68e5'),
 [ 

ohuman([
  doall((iz(Obj,outline),internal_region(Obj,Region),individuate_by_color(Region),
         largestIn(Region,Largest),color(Largest,Color),fill(Color,Region)))]),

   nthDSL(2,[gather_object(O1,X,(iz(X,dot),inside(X,P),iz(P,polygon),wall_thickness(P,1),noexit(P))),
          colors_cc(O1,CC),first(C,CC),part_of(O1,E),color(E,C),fillAt(E,C),
                forall(X,(iz(X,dot), \+ (inside(X,P),iz(P,polygon))),delete(X))])

  ]).




is_fti_step(print_info).


fav(v(be03b35f),[ohuman(
  (get_bgc(BG)),remove_color(BG),
   
   remove_color(red),   
   show_make_symmetrical
  %compute_max_color(C1),compute_next_color(C2), remove_color(C1),subst_color(C2,C1),
  %blur(flipH),blur(flipV)
  )]).
fav(v(be03b35f),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).

fav(t('4347f46a'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),loop_filling,color_guessing,'(3, 1)']).

fav(v('73ccf9c2'),[ohuman(colormass,most_unique(symmetry_type),get(solution),trim_to_rect),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).

fav(v('a8610ef7'),[grid_size_same,find_symmetry,-rotation_match,-color_match,+shape_match,+mask_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',test_suite([eval400]),'(4, 1)']).

fav(v('6ea4a07e'),[clue(mass(in)+mass(out)=:=9),ohuman(corispond_colors,invert_existence),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(6, 2) ']).

fav(t('23b5c85d'),[ohuman([smallest_indiv,trim_to_rect])]).
fav(t('1cf80156'),[ohuman([trim_to_rect])]).
fav(v('929ab4e9'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
fav(v('94133066'),[ohuman([largest_indiv,trim_to_rect,rot90,flipV]),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
fav(t('23b5c85d'),[ohuman([smallest_indiv,trim_to_rect]),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_minimum,measure_area,crop,'(5, 1)']).
fav(t('8d5021e8'),[human_skip([grow([[rot180, flipV],[flipH, sameR],[rot180, flipV]])])]).

fav(t('6150a2bd'),[clue(mass(in)=:=mass(out)),ohuman(rot180),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),image_rotation,'(2, 1)']).
fav(t('ed36ccf7'),[clue(mass(in)=:=mass(out)),ohuman(rot270),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),image_rotation,'(4, 1)']).



:- style_check(-singleton).

fav(t('83302e8f'),
 [ohuman(i(complete),
   with_objects(iz(type(square)),subst_color(black,green)),
   with_objects(iz(media(shaped)),subst_color(black,yellow)),   
   objects_into_grid)]).

fav(t(ff28f65a),[ohuman(count_shapes,associate_images_to_numbers),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),count_shapes,associate_images_to_numbers,'(8, 3)']).
fav(t('1b60fb0c'),[
 %indiv([i_repair_patterns_f]),
 %ohuman([new_things_are_a_color,fix_image]),
 %ohuman([unbind_color(black),now_fill_in_blanks(blur(flipD)),subst_color(fg,red)]),
 %ohuman([blur_least(_,fg),subst_color(blue,red),overlay_original]),
 %ohuman([blur_least(_,fg),remember_repaired,with_objects(changedUntrimmed,[subst_color(blue,red),add_object(changedUntrimmed)])]),
 ohuman([blur_least(_,_),remember_repaired,with_objects(changedUntrimmed,[subst_color(_,red),add_object(changedUntrimmed)])]),
 skip_human(
   in_out(In,Out),
   subtractGrid(Out,In,Alien),
   rot_by_90([Alien,A,B,C]),
   find_by_shape(In,Alien,[A,B,C]),
   find_by_shape(Out,Alien,[A,B,C,Alien]))]).
fav(t('1b60fb0c'),[grid_size_same, -rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),pattern_rotation,pattern_expansion,pattern_deconstruction,'https://github.com/fchollet/ARC/pull/33','(3, 1)']).

%fav(t('23b5c85d'),[b7249182
%fav(t('db3e9e38'),[ohuman([flipV,C1=orange,C2=blue,[],flipV]).
%fav(t(_),[ohuman([fillFromBorder(none,yellow)])]).

fav(t('8d510a79'),[indivs(find_grids,colormass),grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),draw_line_from_point,direction_guessing,detect_wall,associate_colors_to_bools,'(2, 1)']).

fav(v('d304284e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).


fav(t('7b6016b9'),[grid_size_same,ohuman([fillFromBorder(green),subst_color(Black,red)]),
  -rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),loop_filling,color_guessing,background_filling,'(3, 1)']):- get_black(Black).
fav(t('1cf80156'),[out_grid(4,4),ohuman([trim_to_rect]),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),crop,'(3, 1)']).
fav(t('8d5021e8'),[out_grid(4,9),human_skip([grow([[rot180,flipV],[flipH,sameR],[rot180,flipV]])]),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(3, 1)']).
fav(t('8be77c9e'),[out_grid(3,6),human_skip([grow([[sameR],[flipV]])]),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(3, 1)']).
fav(t(c9e6f938),[out_grid(6,3),human_skip([grow([[sameR,flipH]])]),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(3, 1)']).
fav(t('5c2c9af4'),[grid_size_same,ohuman([two_closest_dots_to_edge,make_a_box,grow_box_that_much_bigger,grow_box_that_much_bigger,grow_box_that_much_bigger]),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),rectangle_guessing,pattern_expansion,'(3, 1)']).
fav(t('7f4411dc'),[grid_size_same,ohuman([shave_away_1s]),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),remove_noise,rectangle_guessing,'(3, 1)']).
fav(t('3c9b0459'),[grid_size_same,ohuman([rot180]),-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),image_rotation,'(4, 1)']).
fav(t('44d8ac46'),[grid_size_same,ohuman([find_individuals([hollow,boxes,inside([rectangle])],_138006),indiv_fill_color(_138006,red)]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,loop_filling,'(4, 1)']).
fav(t('ae4f1146'),[indiv(i_mono_nsew),learn([call(set_bgc(cyan))]),ohuman([largest_indiv,trim_to_rect,set_bg(cyan)])]).
fav(t('ae4f1146'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,crop,count_tiles,'(4, 1)']).
fav(t('97999447'),[grid_size_same,ohuman([find_ones,until_edges([copy_right(silver),copy_right(sameR)])]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,draw_line_from_point,'(3, 1)']).
fav(t('6cf79266'),[grid_size_same,learn([find(nines),remove_them]),ohuman(reverse_learned),-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),rectangle_guessing,recoloring,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','(3, 1)']).
fav(t('810b9b61'),[grid_size_same,ohuman([(iz(_140032,rectangle),iz(_140032,hollow),iz(_140032,thick1),iz(_140032,noexit)-->color(_140032,green))]),-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,detect_closed_curves,'(3, 1)']).

fav(t(a48eeaf7),[grid_size_same,ohuman([largest_indiv(_136900),tiny_individuals(_136910),gravity_to(_136910,_136900)]),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,gravity,direction_guessing,bring_patterns_close,'(2, 1)']).
fav(v('4b6b68e5'),[grid_size_same,nthDSL(2,[gather_object(_145350,_145352,(iz(_145352,dot),inside(_145352,_145378),iz(_145378,polygon),wall_thickness(_145378,1),noexit(_145378))),colors_cc(_145350,_145418),first(_145428,_145418),part_of(_145350,_145442),color(_145442,_145428),fillAt(_145442,_145428),forall(_145352,(iz(_145352,dot),\+ (inside(_145352,_145378),iz(_145378,polygon))),delete(_145352))]),ohuman([doall((iz(_145548,outline),internal_region(_145548,_145562),individuate_by_color(_145562),largestIn(_145562,_145584),color(_145584,_145596),fill(_145596,_145562)))]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).

fav(t(d6ad076f),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),draw_line_from_point,connect_the_dots,bridges,'(3, 1)']).
fav(t('9d9215db'),[grid_size_same,ohuman([overlay_each_pattern]),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
fav(v(e41c6fd3),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
fav(v('1d398264'),[grid_size_same,ohuman([(iz(_140032,keypad),iz(_140032,multicolor),centerof(_140032,_140052)-->sunburst(_140052))]),-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 2) ']).
fav(v(e9bb6954),[grid_size_same,e('box of nine draw outward, if you hit a drawn line blacken it'),ohuman([(iz(_142198,keypad),iz(_142198,monocolor),centerof(_142198,_142218)-->starburst(_142218))]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
fav(t(d511f180),[grid_size_same,ohuman([swap_colors(cyan,silver)]),-rotation_match,-color_match,+shape_match,+mask_match,+'Errors',test_suite([train400]),'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021#760920',associate_colors_to_colors,'(3, 1)']).
fav(t('73251a56'),[grid_size_same,learn([learn_mapping_stateful]),ohuman([apply_mapping_stateful]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),image_filling,diagonal_symmetry,'(3, 1)']).
fav(t(f76d97a5),[grid_size_same,was__lmDSL([compute_max_color(_134548),compute_next_color(_134558),remove_color(_134548),subst_color(_134558,_134548)]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),take_negative,recoloring,associate_colors_to_colors,'(3, 1)']).
fav(t(ce22a75a),[grid_size_same,hint(grow_blue),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),replace_pattern,'(2, 1)']).
fav(t(a79310a0),[grid_size_same,ohuman([gravity(1,s),swap_colors(cyan,red)]),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),recoloring,pattern_moving,pairwise_analogy,'(3, 1)']).

fav(t(b230c067),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),separate_shapes,recoloring,find_the_intruder,associate_colors_to_bools,'(2, 1)']).
fav(t(d2abd087),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),separate_shapes,recoloring,count_tiles,associate_colors_to_numbers,'(3, 1)']).
fav(v('0a2355a6'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
fav(v('37d3e8b2'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
fav(t('6e82a1ae'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,count_tiles,associate_colors_to_numbers,'(3, 1)']).
fav(t(b6afb2da),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),replace_pattern,rectangle_guessing,recoloring,'(2, 1)']).
fav(t(e509e548),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,homeomorphism,associate_colors_to_shapes,'(3, 1)']).
fav(t(ea32f347),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),separate_shapes,recoloring,count_tiles,associate_colors_to_ranks,'(4, 1)']).
fav(t('08ed6ac7'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,order_numbers,measure_length,associate_colors_to_ranks,'(2, 1)']).
fav(v('626c0bcc'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
fav(v('639f5a19'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).

fav(v('6ea4a07e'),[clue(mass(in)+mass(out)=9),ohuman(use_clues),clue(corispond_colors,invert_existence),-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(6, 2) ']).

fav(TestID,Stuff):- fav_testcase((TestID>_+_),_,Stuff).

fav_less(V,[test_suite([less_fav])|T]):- less_fav(V,T).


less_fav(TestID,Stuff):- fav_testcase((TestID>_+_),_,Stuff).
less_fav(t(ba97ae07),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),rectangle_guessing,recoloring,pattern_modification,pairwise_analogy,'(4, 1)']).
less_fav(t(cbded52d),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,+'Errors',test_suite([train400]),separate_images,pattern_repetition,pattern_modification,pattern_juxtaposition,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',detect_grid,connect_the_dots,'(3, 1)']).
less_fav(t(e8dc4411),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),pattern_expansion,direction_guessing,'(3, 1)']).
less_fav(v('45737921'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('4e45f183'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('4f537728'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(dc2aa30b),  [grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('776ffc46'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),recoloring,find_the_intruder,detect_enclosure,associate_colors_to_patterns,'(4, 1)']).
less_fav(t('150deff5'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),pattern_deconstruction,pattern_coloring,associate_colors_to_patterns,'(3, 1)']).
less_fav(t('6e02f1e3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),count_different_colors,associate_images_to_numbers,'(5, 1)']).
less_fav(t('995c5fa3'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_complement,summarize,separate_images,detect_wall,associate_colors_to_images,'(4, 1)']).
less_fav(t('25d8a9c8'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_noise,recoloring,detect_hor_lines,'(4, 1)']).
less_fav(v('17cae0c1'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(d4b1c2b1),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(7, 1) ']).
less_fav(v('7039b2d7'),[out_grid(4,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('1190e5a7'),[out_grid(4,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),detect_grid,detect_background_color,create_image_from_info,count_ver_lines,count_hor_lines,color_guessing,'(3, 1)']).
less_fav(v(e872b94a),[out_grid(1,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(d9fac9be),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,x_marks_the_spot,test_suite([train400]),summarize,find_the_intruder,'(4, 1)']).
less_fav(t('445eab21'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,measure_area,'(3, 1)']).
less_fav(t('239be575'),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),detect_connectedness,associate_images_to_bools,'(6, 2)']).
less_fav(t(f9012d9b),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_expansion,pattern_completion,crop,'(3, 1)']).
less_fav(v('1a2e2828'),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(t(de1cd16c),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,summarize,separate_images,count_tiles,'(4, 1)']).
less_fav(v(cd3c21df),[out_grid(1,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(a3325580),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,summarize,separate_shapes,remove_intruders,count_tiles,'(6, 1)']).
less_fav(t('72ca375d'),[out_grid(4,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),find_the_intruder,detect_symmetry,crop,'(3, 1)']).
less_fav(v('8597cfd7'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(be94b721),[out_grid(3,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,separate_shapes,crop,count_tiles,'(4, 1)']).
less_fav(v('642d658d'),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(b9b7f026),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,find_the_intruder,'(3, 1)']).
less_fav(v('3194b014'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('695367ec'),[out_grid(15,15),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('332efdb3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('28e73c20'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),mimic_pattern,ex_nihilo,'(5, 1)']).
less_fav(v(ed74f2f2),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(t(d4469b4b),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),dominant_color,associate_images_to_colors,'(7, 2)']).
less_fav(t('017c7c7b'),[out_grid(3,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),recoloring,pattern_repetition,pattern_expansion,image_expansion,'(3, 1)']).
less_fav(t(e179c5f4),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,bouncing,'(3, 1)']).
less_fav(v('32e9702f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(f8c80d96),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,background_filling,'(3, 1)']).
less_fav(v(a3f84088),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(d5d6de2d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),replace_pattern,remove_intruders,loop_filling,'(3, 2)']).
less_fav(v('1c0d0a4b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('9565186b'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),take_maximum,separate_shapes,recoloring,count_tiles,associate_color_to_bools,'(4, 1)']).
less_fav(t(b1948b0a),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,associate_colors_to_colors,'(3, 1)']).
less_fav(t('794b24be'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),count_tiles,associate_images_to_numbers,'(10, 2)']).
less_fav(t(a9f96cdd),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),replace_pattern,out_of_boundary,'(4, 1)']).
less_fav(t('6773b310'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,detect_grid,count_tiles,associate_colors_to_numbers,'(4, 1)']).
less_fav(v('3b4c2228'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 2) ']).
less_fav(t('3428a4f5'),[out_grid(5,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,pattern_differences,detect_wall,'(4, 2)']).
less_fav(v('195ba7dc'),[out_grid(6,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('5d2a5c43'),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 2) ']).
less_fav(v('66f2d22f'),[out_grid(7,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(d19f7514),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('1b2d62fb'),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,pattern_intersection,detect_wall,'(5, 1)']).
less_fav(t('94f9d214'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_complement,separate_images,pattern_intersection,'(4, 1)']).
less_fav(v(e345f17b),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 2) ']).
less_fav(v('31d5ba1a'),[out_grid(5,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 2) ']).
less_fav(t('0520fde7'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,pattern_intersection,detect_wall,'(3, 1)']).
less_fav(t(fafffa47),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_complement,separate_images,pattern_intersection,'(5, 1)']).
less_fav(t('2bcee788'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_reflection,image_filling,direction_guessing,background_filling,'(4, 1)']).
less_fav(v('604001fa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('009d5c81'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('0c9aba6e'),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('506d28a5'),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('34b99a2b'),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('6430c8c4'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_complement,separate_images,pattern_intersection,detect_wall,'(4, 1)']).
less_fav(t('99b1bc43'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_complement,separate_images,pattern_intersection,detect_wall,'(4, 1)']).
less_fav(t(ce4f8723),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_intersection,take_complement,separate_images,detect_wall,'(4, 1)']).
less_fav(t(f2829549),[out_grid(3,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_complement,separate_images,pattern_intersection,detect_wall,'(5, 1)']).
less_fav(v(e133d23d),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('2037f2c7'),[out_grid(7,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('9110e3c5'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(7, 2) ']).
less_fav(v(d5c634a2),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(7, 2) ']).
less_fav(v('2072aba6'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).


less_fav(t('83302e8f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,loop_filling,detect_grid,detect_closed_curves,associate_colors_to_bools,'(3, 1)']).
less_fav(v('8a371977'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(a61f2674),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),take_minimum,take_maximum,separate_shapes,remove_intruders,recoloring,count_tiles,associate_colors_to_ranks,'(2, 1)']).
less_fav(t('54d9e175'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),separate_images,detect_grid,associate_images_to_images,'(4, 1)']).
less_fav(t(e8593010),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),loop_filling,holes,count_tiles,associate_colors_to_numbers,'(3, 1)']).
less_fav(v('575b1a71'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(ccd554ac),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(t(eb5a1d5d),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),summarize,'(3, 1)']).
less_fav(v('1990f7a8'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('91413438'),[out_grid(12,12),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,count_tiles,algebra,'(4, 1)']).
less_fav(v('358ba94e'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('80af3007'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_resizing,image_resizing,fractal_repetition,crop,'(3, 1)']).
less_fav(v('5b6cbef5'),[out_grid(16,16),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(e57337a4),[-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('8719f442'),[out_grid(15,15),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('2697da3f'),[out_grid(15,15),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(eb281b96),[out_grid(17,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(2, 1)']).


less_fav(v('8e2edd66'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('8b28cd80'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(5, 2) ']).
less_fav(v('0692e18c'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('007bbfb7'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,fractal_repetition,'(5, 1)']).
less_fav(v(bc4146bd),[out_grid(20,4),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(bbc9ae5d),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_expansion,image_expansion,'(5, 1)']).
less_fav(t('53b68214'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_expansion,image_expansion,'(3, 2)']).
less_fav(v('48131b3c'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('28bf18c6'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_repetition,crop,'(3, 1)']).
less_fav(t(f25fbde4),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_resizing,crop,'(3, 1)']).
less_fav(t('3af2c5a8'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_rotation,image_repetition,image_reflection,'(3, 1)']).
less_fav(t('46442a0e'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(3, 1)']).
less_fav(v('59341089'),[out_grid(12,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(a59b95c0),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(ed98d772),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(t(bc1d5164),[-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_moving,pattern_juxtaposition,pairwise_analogy,crop,'(5, 1)']).
less_fav(t('88a62173'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),separate_images,find_the_intruder,detect_grid,crop,'(3, 1)']).
less_fav(t(feca6190),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_expansion,image_expansion,draw_line_from_point,diagonals,'(5, 1)']).
less_fav(t('4c4377d9'),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(4, 1)']).
less_fav(v(b1fc8b8e),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(5, 2) ']).
less_fav(t('6d0aefbc'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(4, 1)']).
less_fav(t('746b3537'),[out_grid(1,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),direction_guessing,crop,'(5, 1)']).
less_fav(v(fc754716),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(e5c44e8f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(da515329),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).


less_fav(t('8eb1be9a'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_repetition,image_filling,'(2, 1)']).
less_fav(t(a3df8b1e),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,draw_line_from_point,diagonals,bouncing,'(3, 1)']).
less_fav(t(ea786f4a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_modification,draw_line_from_point,diagonals,'(3, 1)']).
less_fav(v('9ddd00f0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('56ff96f3'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),rectangle_guessing,pattern_completion,'(4, 1)']).
less_fav(t('8f2ea7aa'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),fractal_repetition,crop,'(3, 1)']).
less_fav(t('99fa7670'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,draw_line_from_point,'(4, 1)']).
less_fav(t('623ea044'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),draw_line_from_point,diagonals,'(3, 1)']).
less_fav(t('3ac3eb23'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_repetition,draw_pattern_from_point,'(2, 1)']).
less_fav(v(e619ca6e),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('69889d6e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(ded97339),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),connect_the_dots,'(3, 1)']).
less_fav(v(bf32578f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('22168020'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,'(3, 1)']).


less_fav(v(ba9d41b8),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('6e19193c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),draw_line_from_point,direction_guessing,diagonals,'(2, 1)']).
less_fav(t('7ddcd7ec'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),draw_line_from_point,direction_guessing,diagonals,'(3, 1)']).
less_fav(v('705a3229'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('42a50994'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),remove_noise,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/pull/43',count_tiles,'(4, 1)']).
less_fav(v('423a55dc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(b9630600),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(e9afcf9a),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),pattern_modification,'(2, 1)']).
less_fav(v('55783887'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(t('025d127b'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),pattern_modification,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','(2, 1)']).
less_fav(t('253bf280'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),direction_guessing,connect_the_dots,'(8, 1)']).
less_fav(t('25ff71a9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,'(4, 2)']).




less_fav(v('1c56ad9f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('42a15761'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('64a7c07e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('85b81ff1'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(d931c21c),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(f3e62deb),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(6, 2) ']).
less_fav(t('1f85a75f'),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),find_the_intruder,crop,'(2, 1)']).
less_fav(t('5ad4f10b'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),remove_noise,recoloring,image_resizing,crop,color_guessing,'(3, 1)']).
less_fav(v('2c0b0aff'),[out_grid(8,7),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('8efcae92'),[out_grid(6,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,separate_images,rectangle_guessing,crop,count_tiles,'(3, 1)']).
less_fav(t('9f236235'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,image_reflection,detect_grid,'(3, 1)']).
less_fav(t('0b148d64'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,find_the_intruder,detect_grid,crop,'(3, 1)']).
less_fav(v(aee291af),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('3de23699'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_negative,rectangle_guessing,crop,'(4, 1)']).
less_fav(t('7468f01a'),[out_grid(8,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),image_reflection,crop,'(3, 1)']).
less_fav(t(fcb5c309),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,separate_images,rectangle_guessing,recoloring,crop,count_tiles,'(3, 1)']).
less_fav(v('5289ad53'),[out_grid(3,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('39a8645d'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,crop,count_patterns,'(3, 1)']).
less_fav(v('351d6448'),[out_grid(13,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t(b94a9452),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_negative,crop,'(3, 1)']).
less_fav(t('2dc579da'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),find_the_intruder,detect_grid,crop,'(3, 1)']).
less_fav(t('97a05b5b'),[out_grid(9,17),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),shape_guessing,pattern_moving,pattern_juxtaposition,crop,'(3, 1)']).
less_fav(t('681b3aeb'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_moving,jigsaw,crop,bring_patterns_close,'(3, 1)']).
less_fav(t('48d8fb45'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),find_the_intruder,crop,'(3, 1)']).
less_fav(t('1fad071e'),[out_grid(5,1),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),count_patterns,associate_images_to_numbers,'(3, 1)']).
less_fav(t('539a4f51'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_expansion,image_expansion,'(3, 1)']).



less_fav(v('4852f2fa'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 2) ']).
less_fav(t(b0c4d837),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),measure_length,associate_images_to_numbers,'(6, 1)']).
less_fav(v(c8b7cc0f),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('4522001f'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pairwise_analogy,image_rotation,'(2, 1)']).
less_fav(t(a740d043),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),recoloring,detect_background_color,crop,'(3, 1)']).
less_fav(t('1f0c79e5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,direction_guessing,diagonals,'(4, 1)']).
less_fav(v(c62e2108),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(ca8f78db),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('9bebae7a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(5, 1) ']).
less_fav(t(a78176bb),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_intruders,draw_parallel_line,direction_guessing,'(3, 1)']).
less_fav(t('3345333e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_noise,pattern_reflection,pattern_completion,'(2, 1)']).
less_fav(t('7e0986d6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_noise,color_guessing,'(2, 1)']).
less_fav(t(e48d4e1a),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),pattern_moving,out_of_boundary,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/13049','https://github.com/fchollet/ARC/pull/37',detect_grid,count_tiles,'(4, 1)']).
less_fav(t(aabf363d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_intruders,recoloring,color_guessing,'(2, 1)']).
less_fav(t('1a07d186'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),find_the_intruder,bring_patterns_close,'(3, 1)']).
less_fav(t(caa06a1f),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),pattern_expansion,image_filling,'(3, 1)']).
less_fav(v('50a16a69'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(f823c43c),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('4093f84a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),recoloring,projection_unto_rectangle,gravity,'(3, 1)']).
less_fav(v('2a5f8217'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(ce602527),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),size_guessing,shape_guessing,remove_intruders,find_the_intruder,crop,'(4, 1)']).
less_fav(v('7bb29440'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('5833af48'),[out_grid(16,8),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('505fff84'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(bbb1b8b6),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(7, 2) ']).
less_fav(t(a87f7484),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,find_the_intruder,crop,'(4, 1)']).
less_fav(t('6cdd2623'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_noise,find_the_intruder,connect_the_dots,'(3, 1)']).
less_fav(v(f5c89df1),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('6df30ad6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(d56f2372),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).



less_fav(v('7d1f7ee8'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(bf699163),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('662c240a'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,find_the_intruder,detect_symmetry,crop,'(4, 1)']).
less_fav(v(c3202e5a),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('8731374e'),[out_grid(6,7),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),rectangle_guessing,draw_line_from_point,crop,'(3, 1)']).
less_fav(t('91714a58'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_noise,find_the_intruder,'(3, 1)']).
less_fav(v(c1990cce),[out_grid(13,13),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('310f3251'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(fb791726),[out_grid(12,12),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(f5b8619d),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_expansion,image_repetition,draw_line_from_point,'(3, 1)']).
less_fav(v(f0afb749),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('10fcaaa3'),[out_grid(8,4),-shape_match,-rotation_match,-mask_match,-color_match,+'Errors',test_suite([train400]),pattern_expansion,image_repetition,'https://github.com/fchollet/ARC/pull/31','(4, 1)']).
less_fav(v('48f8583b'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(v('6f473927'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(e6de6e8f),[out_grid(7,8),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('8403a5d5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,draw_line_from_point,direction_guessing,'(3, 1)']).
less_fav(t(aba27056),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,draw_line_from_point,diagonals,'(3, 1)']).
less_fav(t('4258a5f9'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,contouring,'(2, 1)']).
less_fav(v('21f83797'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('759f3fd3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t(db3e9e38),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,out_of_boundary,'(2, 1)']).
less_fav(t(dc1df850),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,out_of_boundary,contouring,'(3, 1)']).
less_fav(v(aa18de87),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('834ec97d'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),spacing,pattern_repetition,measure_distance_from_side,draw_line_from_border,'(3, 1)']).
less_fav(t(a64e4611),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,background_filling,'(3, 1)']).
less_fav(t(b60334d2),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),replace_pattern,'(2, 1)']).
less_fav(v('31adaf00'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('00dbd492'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('8fbca751'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(c97c0139),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('551d5bf1'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('3eda0437'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),take_maximum,rectangle_guessing,recoloring,measure_area,'(4, 1)']).
less_fav(t('2281f1f4'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_intersection,draw_line_from_point,direction_guessing,'(3, 1)']).
less_fav(t('7447852a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,pairwise_analogy,'(3, 1)']).
less_fav(v('4e469f39'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('913fb3ed'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),contouring,associate_colors_to_colors,'(4, 1)']).
less_fav(v(e7639916),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(dbc1a6ce),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),connect_the_dots,'(4, 1)']).
less_fav(v(e0fb7511),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(d4f3cd78),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,recoloring,draw_line_from_point,'(2, 1)']).
less_fav(t(c1d99e64),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),draw_line_from_border,detect_grid,'(3, 1)']).
less_fav(t('6d75e8bb'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,pattern_completion,'(3, 1)']).
less_fav(t(b27ca6d3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),find_the_intruder,count_tiles,contouring,'(2, 1)']).
less_fav(t(a5313dff),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),loop_filling,'(3, 1)']).
less_fav(t(af902bf9),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,x_marks_the_spot,test_suite([train400]),ex_nihilo,'(3, 1)']).
less_fav(v(fd4b2b02),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('5b526a93'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('7e02026e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('90f3ed37'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),recoloring,pattern_repetition,'(3, 1)']).
less_fav(t(d06dbe63),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,pairwise_analogy,'(2, 1)']).
less_fav(t(ef135b50),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/issues/28',draw_line_from_point,connect_the_dots,bridges,'(3, 1)']).
less_fav(v(cb227835),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('60b61512'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_completion,'(2, 1)']).
less_fav(t('4612dd53'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,pattern_completion,'(3, 1)']).
less_fav(v(da2b0fe3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 2) ']).
less_fav(v(bf89d739),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('9772c176'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t(e9614598),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,measure_length,direction_guessing,'(2, 2)']).
less_fav(t('3aa6fb7a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_rotation,pattern_completion,'(2, 1)']).
less_fav(t('22233c11'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),size_guessing,pattern_expansion,'(3, 1)']).
less_fav(t(a699fb00),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,connect_the_dots,'(3, 1)']).
less_fav(v(aa300dc3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(e73095fd),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,loop_filling,'(3, 1)']).
less_fav(t(a8d7556c),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),rectangle_guessing,recoloring,'(3, 1)']).
less_fav(v(ac605cbb),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(6, 1) ']).


less_fav(v('60a26a3e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('6c434453'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),replace_pattern,'(2, 1)']).
less_fav(v('84f2aca1'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('55059096'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('54d82841'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,gravity,'(3, 1)']).
less_fav(v(e88171ec),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('0b17323b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('41e4d17e'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),pattern_repetition,draw_line_from_point,'(2, 1)']).
less_fav(t(b2862040),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,detect_closed_curves,associate_colors_to_bools,'(4, 1)']).
less_fav(v('292dd178'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('67385a82'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,measure_area,associate_colors_to_bools,'(4, 1)']).


less_fav(t(a5f85a15),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,pattern_modification,pairwise_analogy,'(3, 1)']).
less_fav(t(aedd82e4),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),take_minimum,separate_shapes,recoloring,count_tiles,associate_colors_to_bools,'(4, 1)']).
less_fav(t(ba26e723),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,pattern_modification,pairwise_analogy,'(5, 1)']).
less_fav(t(bb43febb),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),rectangle_guessing,loop_filling,'(2, 1)']).
less_fav(t(ce9e57f2),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),take_half,recoloring,count_tiles,'(3, 1)']).
less_fav(t(d406998b),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,one_yes_one_no,cylindrical,'(4, 1)']).
less_fav(v('817e6c09'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(ae58858e),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(bd14c3bf),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',test_suite([eval400]),'(3, 1)']).
less_fav(v(ce039d91),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(e7dd8335),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('456873bc'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(c8f0f002),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,associate_colors_to_colors,'(3, 1)']).
less_fav(t('6a1e5592'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),recoloring,pattern_moving,jigsaw,'https://github.com/fchollet/ARC/pull/16','(2, 1)']).
less_fav(t(d90796e8),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),replace_pattern,'(3, 1)']).
less_fav(v('12eac192'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(c92b942c),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(db93a21d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),measure_length,measure_area,draw_line_from_point,contouring,algebra,'(4, 1)']).
less_fav(t('6455b5f5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),take_minimum,take_maximum,measure_area,loop_filling,associate_colors_to_ranks,'(4, 1)']).
less_fav(v('137f0df0'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('5207a7b5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('3bd67248'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,draw_line_from_border,diagonals,'(3, 1)']).
less_fav(t('868de0fa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),measure_area,loop_filling,'https://github.com/fchollet/ARC/pull/45',even_or_odd,color_guessing,associate_colors_to_bools,'(5, 1)']).
less_fav(t(a65b410d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,count_tiles,associate_colors_to_ranks,'(3, 1)']).
less_fav(v('62ab2642'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(b7fb29bc),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(c0f76784),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),measure_area,loop_filling,associate_colors_to_numbers,'(3, 1)']).
less_fav(v(dc2e9a9d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('543a7ed5'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),loop_filling,contouring,'(2, 1)']).
less_fav(v('140c817e'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('694f12f3'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),rectangle_guessing,measure_area,loop_filling,associate_colors_to_ranks,'(2, 1)']).
less_fav(v('15663ba9'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('516b51b7'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(fea12743),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',test_suite([eval400]),'(3, 1)']).
less_fav(v('84db8fc4'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('3f23242b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(fe9372f3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('941d9a10'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pairwise_analogy,loop_filling,detect_grid,'(3, 1)']).
less_fav(v(aa4ec2a5),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(d364b489),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,'(2, 1)']).
less_fav(t('95990924'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,'(3, 1)']).
less_fav(t('272f95fa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),mimic_pattern,grid_coloring,detect_grid,'(2, 1)']).
less_fav(v(e9c9d9a1),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('6b9890af'),[out_grid(8,8),-shape_match,-rotation_match,-mask_match,+color_match,x_marks_the_spot,test_suite([train400]),pattern_resizing,pattern_moving,crop,'(3, 1)']).
less_fav(t('4be741c5'),[out_grid(3,1),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),summarize,'(3, 1)']).
less_fav(t(c8cbb738),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_moving,jigsaw,crop,'(3, 1)']).
less_fav(t('3f7978a0'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),rectangle_guessing,find_the_intruder,crop,'(3, 1)']).
less_fav(v('67636eac'),[out_grid(9,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('3979b1a8'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('60c09cac'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t(cce03e0d),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pairwise_analogy,image_repetition,image_expansion,'(3, 1)']).
less_fav(v(c48954c1),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('9172f3a0'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_resizing,'(2, 1)']).
less_fav(t(d10ecb37),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),crop,'(3, 1)']).
less_fav(t('963e52fc'),[out_grid(12,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_expansion,image_expansion,'(3, 1)']).
less_fav(v('0c786b71'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
%less_fav(v('00576224'),[out_grid(6,6)]).
less_fav(v('8ba14f53'),[-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(t('67e8384a'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_rotation,image_repetition,image_reflection,'(4, 1)']).
less_fav(t('7fe24cdd'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_rotation,image_repetition,'(3, 1)']).
less_fav(t(ac0a08a4),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),size_guessing,image_resizing,count_tiles,'(3, 1)']).
less_fav(t(b91ae062),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),size_guessing,image_resizing,count_different_colors,'(5, 1)']).
less_fav(t('2dee498d'),[-shape_match,-rotation_match,-mask_match,+color_match,+'Errors',test_suite([train400]),'https://github.com/fchollet/ARC/issues/30',divide_by_n,detect_repetition,crop,'(3, 1)']).
less_fav(t('7b7f7511'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),separate_images,detect_repetition,crop,'(3, 1)']).
less_fav(t(c59eb873),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_resizing,'(3, 1)']).
less_fav(v(d017b73f),[out_grid(7,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('6fa7a44f'),[out_grid(3,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,image_reflection,'(4, 1)']).
less_fav(t(a416b8f3),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_repetition,'(3, 1)']).
less_fav(t('1bfc4729'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,'(2, 1)']).
less_fav(t(d22278a0),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),pattern_expansion,pairwise_analogy,'https://github.com/fchollet/ARC/pull/4','(4, 1)']).
less_fav(t(bda2d7a6),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),recoloring,pattern_modification,pairwise_analogy,color_permutation,'(3, 2)']).
less_fav(v('3a301edc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(t(b527c5c6),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),size_guessing,pattern_expansion,draw_line_from_point,direction_guessing,contouring,'(4, 1)']).
less_fav(t('0a938d79'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,draw_line_from_point,direction_guessing,'(4, 1)']).
less_fav(t(d037b0a7),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,draw_line_from_point,'(3, 1)']).
less_fav(t('3befdf3e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),take_negative,pattern_expansion,'(3, 1)']).
less_fav(v(c87289bb),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(f15e1fac),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),obstacles,gravity,draw_line_from_point,direction_guessing,'(3, 1)']).
less_fav(v('79fb03f4'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(v(a934301b),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('496994bd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_reflection,'(2, 1)']).
less_fav(v('9def23fe'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(d9f24cd1),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),obstacles,gravity,draw_line_from_point,'(2, 1)']).
less_fav(v(f9a67cb5),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(b942fd60),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(v(d37a1ef5),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('8cb8642d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(b8cdaf2b),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,pairwise_analogy,draw_line_from_point,diagonals,'(4, 1)']).
less_fav(v('712bf12e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('1e32b0e9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),separate_images,pattern_completion,image_repetition,detect_grid,'(3, 1)']).
less_fav(t('928ad970'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),rectangle_guessing,draw_rectangle,color_guessing,'(3, 1)']).
less_fav(v(a57f2f04),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('4938f0c2'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
less_fav(v(d492a647),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('0962bcdd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,'(2, 1)']).
less_fav(t(e5062a87),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),pattern_repetition,pattern_juxtaposition,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','(3, 1)']).
less_fav(v('54db823b'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',test_suite([eval400]),'(4, 1)']).
less_fav(v('58e15b12'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(b7249182),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,'(3, 1)']).
less_fav(t('4c5c2cf0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
less_fav(v('97239e3d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(e40b9e2f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
less_fav(v('782b5218'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('444801d8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),rectangle_guessing,pattern_repetition,pattern_expansion,'(3, 1)']).
less_fav(v(e5790162),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(baf41dbf),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('95a58926'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('760b3cac'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_reflection,direction_guessing,'(3, 1)']).
less_fav(t('5c0a986e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),draw_line_from_point,direction_guessing,diagonals,'(3, 1)']).
less_fav(t(ecdecbb3),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_modification,draw_line_from_point,'(3, 1)']).
less_fav(v('73c3b0d8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(f8be4b64),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',test_suite([eval400]),'(4, 1)']).
less_fav(v('17b80ad2'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('6d58a25d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),print_grid,draw_line_from_point,debug_indiv,'(3, 1)',"the blue object is a downward beam maker, each beam must connect to one of its colors_cc "]).
less_fav(t('7df24a62'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_rotation,pattern_repetition,pattern_juxtaposition,out_of_boundary,'(4, 1)']).
less_fav(t('3bdb4ada'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),recoloring,pattern_repetition,holes,'(2, 1)']).
less_fav(t('3618c87e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),gravity,'(3, 1)']).
less_fav(v('917bccba'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(f1cefba8),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_modification,draw_line_from_point,'(3, 1)']).
less_fav(v('92e50de0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('72207abc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(ac0c5833),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('0d87d2a6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('855e0971'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),separate_images,holes,draw_line_from_point,direction_guessing,'(4, 1)']).
less_fav(v(f83cb3f6),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('3e980e27'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_repetition,pattern_reflection,pattern_juxtaposition,direction_guessing,'(4, 1)']).
less_fav(t('29623171'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),take_maximum,separate_images,grid_coloring,detect_grid,count_tiles,'(3, 1)']).
less_fav(v('72a961c9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('963f59bc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(ec883f72),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,draw_line_from_point,diagonals,'(4, 1)']).
less_fav(t(d43fd935),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),projection_unto_rectangle,draw_line_from_point,direction_guessing,'(3, 1)']).
less_fav(t('72322fa7'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_repetition,pattern_juxtaposition,'(3, 1)']).
less_fav(v('18419cfa'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('5b692c0f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('98cf29f8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,bring_patterns_close,'(3, 1)']).
less_fav(v('2546ccf6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('93c31fbe'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('3391f8c0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('1c02dbbe'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('2c608aff'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),projection_unto_rectangle,draw_line_from_point,'(4, 1)']).
less_fav(t('74dd1130'),[grid_size_same,+shape_match,+mask_match,+color_match,test_suite([train400]),image_reflection,diagonal_symmetry,'(4, 1)',3]).
less_fav(t('85c4e7cd'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),recoloring,color_permutation,color_guessing,'(4, 1)']).
less_fav(v('4364c1c4'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('9b4c17c4'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(4, 2) ']).
less_fav(t('05f2a901'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,direction_guessing,bring_patterns_close,'(3, 1)']).
less_fav(t('3906de3d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),gravity,'(3, 1)']).
less_fav(t('5168d44c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),recoloring,pattern_moving,direction_guessing,contouring,'(3, 1)']).
less_fav(t('6855a6e4'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,x_marks_the_spot,test_suite([train400]),pattern_moving,direction_guessing,'(3, 1)']).
less_fav(t('9dfd6313'),[grid_size_same,-mask_match,+shape_match,+color_match,test_suite([train400]),image_reflection,diagonal_symmetry,'(3, 1)',3]).
less_fav(t(a1570a43),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,x_marks_the_spot,test_suite([train400]),rectangle_guessing,pattern_moving,'(4, 1)']).


less_fav(t(dc433765),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),pattern_moving,only_one,'https://github.com/fchollet/ARC/issues/29',direction_guessing,'(7, 2)']).
less_fav(t(f8a8fe49),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_reflection,pattern_moving,'(3, 1)']).
less_fav(v('20981f0e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('4acc7107'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('67c52801'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('8ee62060'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('9c56f360'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e1d2900e),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('7837ac64'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),grid_coloring,extrapolate_image_from_grid,detect_grid,crop,color_guessing,'(4, 1)']).
less_fav(v('3ee1011a'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('2f0c5170'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('780d0b14'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,detect_grid,'(3, 1)']).
less_fav(v(d4c90558),[out_grid(8,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('81c0276b'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('7c9b52a0'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(b190f7f5),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,replace_pattern,image_resizing,image_expansion,color_palette,'(3, 1)']).
less_fav(v('19bb5feb'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('20818e16'),[out_grid(8,6)]).
less_fav(t(e6721834),[out_grid(17,15),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),
  pattern_moving,pattern_juxtaposition,crop,'(3, 1)']).
less_fav(t(f8ff0b80),[out_grid(1,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,separate_shapes,order_numbers,count_tiles,'(3, 1)']).
less_fav(v('50aad11f'),[out_grid(4,8),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(b7cb93ac),[out_grid(4,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('12997ef3'),[out_grid(9,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 2) ']).
less_fav(t(e50d258f),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,separate_images,detect_background_color,crop,count_tiles,'(3, 1)']).
less_fav(t('5614dbcf'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),remove_noise,image_resizing,'(2, 1)']).
less_fav(t(e98196ab),[out_grid(11,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,image_juxtaposition,detect_wall,'(3, 1)']).
less_fav(t(e3497940),[out_grid(4,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,image_reflection,image_juxtaposition,detect_wall,'(3, 1)']).
less_fav(t('234bbc79'),[out_grid(7,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),recoloring,crop,bring_patterns_close,'(4, 1)']).
less_fav(v('62b74c02'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(bd4472b8),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,ex_nihilo,detect_wall,color_palette,color_guessing,'(3, 1)']).
less_fav(v('7c8af763'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(b548a754),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,x_marks_the_spot,test_suite([train400]),pattern_modification,pattern_expansion,'(3, 1)']).
less_fav(v('9f27f097'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e95e3d8e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(c663677b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('1d0a4b61'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('321b1fc6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,pattern_juxtaposition,'(2, 1)']).
less_fav(t('2204b7a8'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,proximity_guessing,'(3, 1)']).
less_fav(t(c9f8e694),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,pattern_repetition,color_palette,'(2, 1)']).
less_fav(t(e76a88a6),[indiv(i_mono),grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),pattern_repetition,pattern_juxtaposition,'(2, 1)']).
less_fav(v(c7d4e6ad),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(fafd9572),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(a680ac02),[out_grid(8,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(b4a43f3b),[out_grid(18,18),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(f8b3ba0a),[out_grid(1,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,order_numbers,find_the_intruder,dominant_color,detect_grid,count_tiles,'(4, 1)']).
less_fav(t('8e1813be'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),recoloring,image_within_image,direction_guessing,crop,color_guessing,'(3, 1)']).
less_fav(v('4c177718'),[out_grid(15,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 2) ']).
less_fav(v('3d31c5b3'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(t('75b8110e'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,image_juxtaposition,'(5, 1)']).
less_fav(t(cf98881b),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,pattern_juxtaposition,detect_wall,'(5, 1)']).
less_fav(v('477d2879'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).


less_fav(v(d2acf2cb),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e9b4f6fc),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(a68b268e),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,pattern_juxtaposition,detect_grid,'(6, 1)']).
less_fav(t(d23f8c26),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),image_expansion,crop,'(3, 1)']).
less_fav(v(f4081712),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(t('31aa019c'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),remove_noise,find_the_intruder,contouring,'(3, 1)']).
less_fav(t('9ecd008a'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_rotation,pattern_reflection,pattern_expansion,image_filling,crop,'(3, 1)']).
less_fav(v('67b4a34d'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(f5aa3634),[out_grid(4,3),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('9a4bb226'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(e26a3af2),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),separate_images,remove_noise,'(3, 1)']).
less_fav(t('5bd6f4ac'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),rectangle_guessing,crop,'(4, 1)']).
less_fav(v(ad7e01d0),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('469497ad'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),image_resizing,draw_line_from_point,diagonals,'(3, 1)']).
less_fav(v('15696249'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('27f8ce4f'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(c3e719e8),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),take_maximum,image_repetition,image_expansion,count_different_colors,'(3, 1)']).
less_fav(t('9af7a82c'),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,separate_images,order_numbers,count_tiles,'(4, 1)']).
less_fav(v('692cd3b6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(b15fca0b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(ff72ca3e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('2bee17df'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),take_maximum,draw_line_from_border,count_tiles,'(3, 1)']).
less_fav(t('23581191'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_intersection,draw_line_from_point,'(2, 1)']).
less_fav(v('45bbe264'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('67a423a3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_intersection,contouring,'(3, 1)']).
less_fav(v('770cc55f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(f35d900a),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,'(4, 1)']).


less_fav(t('673ef223'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),recoloring,portals,draw_line_from_point,'(3, 1)']).
less_fav(t(bdad9b1f),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),take_intersection,recoloring,draw_line_from_point,direction_guessing,'(2, 1)']).
less_fav(v(ac3e2b04),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('29c11459'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),draw_line_from_point,count_tiles,'(2, 1)']).
less_fav(v('3490cc26'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t(d4a91cb9),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),direction_guessing,connect_the_dots,'(3, 1)']).
less_fav(v(c074846d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(5, 2) ']).
less_fav(t(a2fd1cf0),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),connect_the_dots,'(3, 1)']).
less_fav(v(f3b10344),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('508bd3b6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_reflection,draw_line_from_point,direction_guessing,'(3, 1)']).
less_fav(t('56dc2b01'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,gravity,direction_guessing,'(3, 1)']).
less_fav(v('992798f6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('896d5239'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(a04b2602),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(bcb3040b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('32597951'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,find_the_intruder,'(3, 1)']).
less_fav(t('36fdfd69'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),rectangle_guessing,recoloring,'(3, 1)']).
less_fav(t('50846271'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,pattern_completion,'(4, 1)']).
less_fav(t('50cb2852'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),rectangle_guessing,holes,'(3, 1)']).
less_fav(v('14754a24'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('1acc24af'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('22a4bbc2'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('7d419a02'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(d94c3b52),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('212895b5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('0ca9ddb6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,associate_patterns_to_colors,'(3, 1)']).
less_fav(v('9caba7c3'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(d47aa2ff),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('891232d6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('2753e76c'),[-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('833dafe3'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(e1baa8a4),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('4290ef0e'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_moving,crop,concentric,'(3, 1)']).
less_fav(t('846bdb03'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,x_marks_the_spot,test_suite([train400]),pattern_reflection,pattern_moving,crop,color_matching,'(4, 1)']).
less_fav(v(b7999b51),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('73182012'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e78887d1),[out_grid(11,3),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(ce8d95cc),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('2013d3e2'),[-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_deconstruction,crop,'(2, 1)']).
less_fav(v('6a11f6da'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('7953d61e'),[out_grid(8,8),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('68b67ca3'),[-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(d13f3404),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_expansion,draw_line_from_point,diagonals,'(3, 1)']).
less_fav(t('62c24649'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),image_rotation,image_repetition,image_reflection,'(3, 1)']).
less_fav(v('08573cc6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('178fcbfb'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),draw_line_from_point,direction_guessing,'(3, 1)']).
less_fav(v('0f63c0b9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('52fd389e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(f25ffba3),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_repetition,pattern_reflection,'(2, 1)']).
less_fav(v('29700607'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e69241bd),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e760a62e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(b782dc8a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,maze,'(2, 1)']).
less_fav(v(e7b06bea),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('9c1e755f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('57aa92db'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_resizing,pattern_repetition,draw_pattern_from_point,'(4, 1)']).
less_fav(v('94414823'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('99306f82'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('12422b43'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('96a8c0cd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('39e1d7f9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_repetition,grid_coloring,detect_grid,'(3, 1)']).
less_fav(t(e21d9049),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,draw_line_from_point,color_palette,'(2, 1)']).
less_fav(v('27a77e38'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(cf133acc),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('045e512c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,direction_guessing,'(3, 1)']).
less_fav(v(c35c1b4c),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('2dd70a9a'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),maze,draw_line_from_point,direction_guessing,'(3, 1)']).
less_fav(t('1f876c06'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),diagonals,connect_the_dots,'(3, 1)']).
less_fav(t('06df4c85'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),grid_coloring,detect_grid,connect_the_dots,'(3, 1)']).
less_fav(v('9b2a60aa'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('2b01abd0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('642248e4'),[grid_size_same]).
less_fav(v('0607ce86'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('94be5b80'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(b7f8a4d8),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('11852cab'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),pattern_expansion,'https://github.com/fchollet/ARC/pull/33','(3, 1)']).
less_fav(v(ecaa0ec1),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('2c737e39'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(c6e1b8da),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(bb52a14b),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('03560426'),[grid_size_same]).
less_fav(t('890034e9'),[grid_size_same]).
less_fav(t('264363fd'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),pattern_repetition,pattern_juxtaposition,draw_line_from_point,'(3, 1)']).
less_fav(t('67a3c6ac'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),image_reflection,'(3, 1)']).
less_fav(t('6aa20dc0'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),pattern_resizing,pattern_repetition,pattern_juxtaposition,'(3, 1)']).
less_fav(t(d07ae81c),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),draw_line_from_point,diagonals,color_guessing,'(3, 1)']).
less_fav(t('1caeab9d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,pattern_alignment,'(3, 1)']).
less_fav(t(beb8660c),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,order_numbers,count_tiles,'(3, 1)']).
less_fav(v('09c534e7'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('15113be4'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('40f6cd08'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('4ff4c9da'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('7ee1c6ea'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(ac2e8ecf),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(dd2401ed),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(e21a174a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 2) ']).
less_fav(v(e74e1818),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('90c28cc7'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,rectangle_guessing,crop,'(3, 1)']).
less_fav(t('1c786137'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),detect_enclosure,crop,'(3, 1)']).
less_fav(v(c658a4bd),[out_grid(8,8),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('93b4f4b3'),[out_grid(6,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(ea9794b1),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(t('8e5a5113'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),separate_images,image_rotation,image_repetition,'https://github.com/fchollet/ARC/pull/8',detect_wall,'(3, 1)']).
less_fav(v('4cd1b7b2'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('9b365c51'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('88a10436'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,pattern_juxtaposition,'(3, 1)']).
less_fav(v(f21745ec),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(ddf7fa4f),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,color_palette,'(3, 1)']).
less_fav(v('103eff5b'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('33b52de3'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('845d6e51'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(b457fec5),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(c909285e),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),rectangle_guessing,find_the_intruder,crop,'(3, 1)']).
less_fav(v('414297c0'),[out_grid(11,12),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(c64f1187),[out_grid(11,8),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(b0f4d537),[out_grid(7,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(e99362f0),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(v('281123b4'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(t('7c008303'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),separate_images,recoloring,detect_grid,crop,color_palette,color_guessing,'(3, 1)']).
less_fav(t(dc0a314f),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_completion,crop,'(3, 1)']).
less_fav(t('77fdfe62'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),recoloring,detect_grid,crop,color_guessing,'(3, 1)']).
less_fav(t('49d1d64f'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_expansion,image_expansion,'(3, 1)']).
less_fav(v('05a7bcf2'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('0e671a1a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('11e1fe23'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('5daaa586'),[out_grid(12,15),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),draw_line_from_point,direction_guessing,detect_grid,crop,'(3, 1)']).
less_fav(t('46f33fce'),[out_grid(20,20),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_resizing,image_resizing,'(3, 1)']).
less_fav(t('8a004b2b'),[out_grid(14,9),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),rectangle_guessing,pattern_resizing,pattern_repetition,pattern_juxtaposition,crop,'(3, 1)']).
less_fav(t(a61ba2ce),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([train400]),pattern_moving,jigsaw,crop,bring_patterns_close,'(2, 1)']).
less_fav(t('137eaa0f'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_juxtaposition,'(3, 1)']).
less_fav(v(e633a9e5),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('93b581b8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_expansion,out_of_boundary,color_guessing,'(3, 1)']).
less_fav(v('13713586'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(a406ac07),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('2685904e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(6, 1) ']).
less_fav(t('82819916'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),pattern_repetition,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/pull/32',draw_line_from_point,color_guessing,associate_colors_to_colors,'(4, 1)']).
less_fav(v('88207623'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('22eb0ac0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),connect_the_dots,color_matching,'(3, 1)']).
less_fav(t(c444b776),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),separate_images,image_repetition,find_the_intruder,detect_grid,'(2, 1)']).
less_fav(t(b775ac94),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),recoloring,pattern_rotation,pattern_repetition,pattern_reflection,pattern_juxtaposition,pattern_expansion,direction_guessing,'(3, 1)']).
less_fav(t('36d67576'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_rotation,pattern_repetition,pattern_reflection,pattern_juxtaposition,'(3, 1)']).
less_fav(v('42918530'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('79369cc6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('0e206a2e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_rotation,pattern_repetition,pattern_reflection,pattern_juxtaposition,associate_patterns_to_patterns,'(3, 1)']).
less_fav(v('696d4842'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('1e0a9b12'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),pattern_moving,gravity,'(3, 1)']).
less_fav(t('1f642eb9'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([train400]),projection_unto_rectangle,image_within_image,'(3, 1)']).
less_fav(t('228f6490'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,x_marks_the_spot,test_suite([train400]),shape_guessing,pattern_moving,loop_filling,'(3, 1)']).
less_fav(t(ae3edfdc),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),gravity,bring_patterns_close,'(3, 1)']).
less_fav(v('0becf7df'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('16b78196'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('5ffb2104'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('66e6c45b'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('8dae5dfc'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('90347967'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(a096bf4d),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(df8cc377),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e681b708),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(f3cdc58f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(f45f5ca7),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('7d18a6fb'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e7a25a18),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('0bb8deee'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('136b0064'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(ca8de6ea),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('363442ee'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,pattern_juxtaposition,detect_wall,'(3, 1)']).
less_fav(v('5a5a2103'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t(c3f564a4),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,image_filling,'(3, 1)']).
less_fav(t('0dfd9992'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_expansion,image_filling,'(3, 1)']).
less_fav(t('484b58aa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),pattern_repetition,pattern_expansion,image_filling,'(3, 1)']).
less_fav(v(af22c60d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(e9ac8c9e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(d89b689b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),summarize,pattern_juxtaposition,direction_guessing,'(3, 1)']).
less_fav(t(d687bc17),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',test_suite([train400]),remove_intruders,'https://github.com/fchollet/ARC/pull/39',gravity,find_the_intruder,direction_guessing,bring_patterns_close,'(3, 1)']).
less_fav(v('903d1b4a'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('63613498'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),recoloring,detect_wall,compare_image,'(3, 1)']).
less_fav(v('1da012fc'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(ef26cbf6),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('0a1d4ef5'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('6d0160f0'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),separate_image,pattern_moving,find_the_intruder,detect_grid,'(4, 1)']).
less_fav(v('0934a4d8'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(t('6ecd11f4'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),recoloring,pattern_resizing,crop,color_palette,'(3, 1)']).
less_fav(v(b0722778),[out_grid(2,11),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v(e66aafb8),[out_grid(5,8),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v('1a6449f1'),[out_grid(7,6),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('25094a63'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('9aec4887'),[out_grid(6,6),todo_sol([find_individuals([hollow,inside([rectangle])],_137826),rest_indivdual(_137858),put_inside(_137858,_137826),if_edge_strong([color=_137892]),touch(_137858,_137904),set_color(_137892,_137904)]),indiv_option(color_blind),-shape_match,-rotation_match,-mask_match,+color_match,x_marks_the_spot,test_suite([train400]),recoloring,pattern_moving,crop,color_guessing,'(3, 1)']).
less_fav(v('9356391f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(v('85fa5666'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v(f0df5ff0),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('40853293'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test_suite([train400]),'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',connect_the_dots,'(2, 1)']).
less_fav(v('070dd51e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',test_suite([eval400]),'(2, 1) ','(2, 1)']).
less_fav(v('3ed85e70'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('68b16354'),[grid_size_same,+shape_match,+mask_match,+color_match,test_suite([train400]),image_reflection,'(3, 1)',2]).
less_fav(v(ea959feb),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(fd096ab6),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t('952a094c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),rectangle_guessing,inside_out,'(3, 1)']).
less_fav(v('50f325b5'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('58743b76'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test_suite([eval400]),'(2, 1) ']).
less_fav(t(cdecee7f),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),summarize,pairwise_analogy,'(3, 1)']).
less_fav(v('1e97544e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('4aab4007'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('09629e4f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),take_minimum,separate_images,enlarge_image,detect_grid,create_grid,count_tiles,adapt_image_to_grid,'(4, 1)']).
less_fav(v('184a9768'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(af24b4cc),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(b20f7c8b),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(aab50785),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(5, 1) ']).
less_fav(v(e4075551),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(fcc82909),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([train400]),separate_images,pattern_expansion,count_different_colors,'(3, 1)']).
less_fav(v('5af49b42'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('6ad5bdfd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(b8825c91),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([train400]),pattern_rotation,pattern_reflection,pattern_completion,'(4, 1)']).
less_fav(v(cfb2ce5a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(d282b262),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t(a8c38be5),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test_suite([train400]),pattern_moving,jigsaw,crop,'(2, 1)']).
less_fav(v('981571dc'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('47996f11'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test_suite([eval400]),'(4, 1) ']).
less_fav(v('256b0a75'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('1e81d6f9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v('5783df64'),[-shape_match,-rotation_match,-mask_match,-color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(t('9edfc990'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([train400]),holes,background_filling,'(3, 1)']).
less_fav(v('319f2597'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).
less_fav(v(e2092e0c),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test_suite([eval400]),'(3, 1) ']).


:- style_check(-singleton).




less_fav(v('09c534e7'),[]).
less_fav(v('25094a63'),[]).
less_fav(v('16b78196'),[]).
less_fav(v('070dd51e'),[]).
less_fav(v('8a371977'),[]).
less_fav(v('696d4842'),[]).
less_fav(v('40f6cd08'),[]).
less_fav(v('05a7bcf2'),[]).
less_fav(t('264363fd'),[]).
less_fav(t('7837ac64'),[]).

less_fav(X,[]):- clause(less_fav(X),true).


/*
first i look to see if the grid sizes are purporional, if not i look to see if the output grid can be recognised on the input
if not i look to see if the input grid can be recognised on the output

f35d900a

the input has a small number of localcolorlesspointlist .. the equal number on each media(image)
two localcolorlesspointlist are the equal color, oh there are two pairs of localcolorlesspointlist
there is a silver dashed box made up from the localcolorlesspointlist
no two silver dots can touch though
the silver dots seem to originate from the four localcolorlesspointlist
teh silver has to come out at least once.. after than if they are going to become two they skip
arround each color point there is a box of the oposting color



 cbded52d

we have a blue background and black TTT board .. to blue openings are made of 2x2s
in each rectangle there can be a color.. each color has a default pos on the 2x2
if like in the game of TTT you can win, but not diagonlly.. place the color on that area


 150deff5.json

  there is a hairything and it is soem color we will paint the entire thing another color
  now we will find the jaggedity bits and paint magenta in dashes of three lonbg until all that is left is 2x2 s



1) identify background and individuation





*/

