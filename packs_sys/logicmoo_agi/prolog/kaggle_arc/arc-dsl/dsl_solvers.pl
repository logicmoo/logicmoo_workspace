
:- ensure_loaded('solvers.pl').

michod_solved_ordered(ID):-
  E= (Len-ID),
  findall(TestID,(michod_solved_len(ID,_Len),fix_test_name(ID,TestID)),List), list_to_set(List,Set),
  findall(E,(member(ID,Set),michod_solved_len(ID,Len)),List2),
  sort(List2,Set2),member(E,Set2). 

 
michod_solved_len(t(ID),Len):- clause(l_solve(ID,_IN,_OUT),Program),length(Program,Len).
michod_solved_len(v(ID),Len):- member((ID,Len),[('3c9b0459', 1), ('60c09cac', 1), ('6150a2bd', 1), ('67a3c6ac', 1), ('68b16354', 1), ('68b67ca3', 1), ('74dd1130', 1), ('9172f3a0', 1), ('9dfd6313', 1), ('a416b8f3', 1), ('b1948b0a', 1), ('c59eb873', 1), ('c8f0f002', 1), ('d10ecb37', 1), ('d511f180', 1), ('ed36ccf7', 1), ('2dee498d', 2), ('4c4377d9', 2), ('5582e5ca', 2), ('5614dbcf', 2), ('5bd6f4ac', 2), ('6d0aefbc', 2), ('6fa7a44f', 2), ('8be77c9e', 2), ('c9e6f938', 2), ('d4b1c2b1', 2), ('0b148d64', 3), ('19bb5feb', 3), ('1cf80156', 3), ('1f85a75f', 3), ('23b5c85d', 3), ('25ff71a9', 3), ('32597951', 3), ('3aa6fb7a', 3), ('4258a5f9', 3), ('59341089', 3), ('7b7f7511', 3), ('9a4bb226', 3), ('9ecd008a', 3), ('ac0a08a4', 3), ('b91ae062', 3), ('be94b721', 3), ('c1d99e64', 3), ('c909285e', 3), ('e7639916', 3), ('f25ffba3', 3), ('0c786b71', 4), ('0d3d703e', 4), ('1a2e2828', 4), ('1c786137', 4), ('1e0a9b12', 4), ('1e81d6f9', 4), ('28bf18c6', 4), ('2dc579da', 4), ('3194b014', 4), ('3618c87e', 4), ('3af2c5a8', 4), ('42a50994', 4), ('4347f46a', 4), ('44d8ac46', 4), ('44f52bb0', 4), ('46f33fce', 4), ('50cb2852', 4), ('56ff96f3', 4), ('62c24649', 4), ('662c240a', 4), ('67e8384a', 4), ('7468f01a', 4), ('833dafe3', 4), ('a740d043', 4), ('a79310a0', 4), ('aabf363d', 4), ('ae4f1146', 4), ('b27ca6d3', 4), ('bf699163', 4), ('ce22a75a', 4), ('d56f2372', 4), ('dc1df850', 4), ('f0df5ff0', 4), ('f25fbde4', 4), ('2013d3e2', 5), ('22eb0ac0', 5), ('41e4d17e', 5), ('445eab21', 5), ('5783df64', 5), ('642d658d', 5), ('6f8cd79b', 5), ('73182012', 5), ('73ccf9c2', 5), ('8efcae92', 5), ('9565186b', 5), ('9f236235', 5), ('a699fb00', 5), ('ae58858e', 5), ('aedd82e4', 5), ('bb43febb', 5), ('bc4146bd', 5), ('cd3c21df', 5), ('ce9e57f2', 5), ('e0fb7511', 5), ('e74e1818', 5), ('e872b94a', 5), ('e98196ab', 5), ('f76d97a5', 5), ('fc754716', 5), ('00d62c1b', 6), ('0520fde7', 6), ('0ca9ddb6', 6), ('10fcaaa3', 6), ('1c0d0a4b', 6), ('2546ccf6', 6), ('319f2597', 6), ('32e9702f', 6), ('332efdb3', 6), ('3906de3d', 6), ('46442a0e', 6), ('48131b3c', 6), ('48d8fb45', 6), ('5117e062', 6), ('543a7ed5', 6), ('62ab2642', 6), ('67385a82', 6), ('7953d61e', 6), ('7b6016b9', 6), ('7bb29440', 6), ('7fe24cdd', 6), ('84db8fc4', 6), ('8d5021e8', 6), ('928ad970', 6), ('a5313dff', 6), ('a934301b', 6), ('b60334d2', 6), ('b94a9452', 6), ('ccd554ac', 6), ('d037b0a7', 6), ('d0f5fe59', 6), ('d406998b', 6), ('d631b094', 6), ('dae9d2b5', 6), ('e21a174a', 6), ('e3497940', 6), ('e9afcf9a', 6), ('ea32f347', 6), ('ed98d772', 6), ('00576224', 7), ('007bbfb7', 7), ('017c7c7b', 7), ('05f2a901', 7), ('08ed6ac7', 7), ('140c817e', 7), ('1a6449f1', 7), ('1b2d62fb', 7), ('1f876c06', 7), ('2072aba6', 7), ('363442ee', 7), ('39a8645d', 7), ('40853293', 7), ('496994bd', 7), ('4f537728', 7), ('5168d44c', 7), ('55059096', 7), ('5521c0d9', 7), ('64a7c07e', 7), ('66e6c45b', 7), ('84f2aca1', 7), ('85c4e7cd', 7), ('8ee62060', 7), ('90c28cc7', 7), ('9ddd00f0', 7), ('a59b95c0', 7), ('b6afb2da', 7), ('b9b7f026', 7), ('ba97ae07', 7), ('bbb1b8b6', 7), ('bcb3040b', 7), ('c48954c1', 7), ('c9f8e694', 7), ('d23f8c26', 7), ('d2abd087', 7), ('d5d6de2d', 7), ('d9fac9be', 7), ('dbc1a6ce', 7), ('ded97339', 7), ('e9614598', 7), ('ea786f4a', 7), ('f5aa3634', 7), ('f823c43c', 7), ('f8ff0b80', 7), ('0c9aba6e', 8), ('1190e5a7', 8), 
 ('195ba7dc', 8), ('239be575', 8), ('25d8a9c8', 8), ('310f3251', 8), ('3f7978a0', 8), ('506d28a5', 8), ('54d82841', 8), ('5c0a986e', 8), ('5d2a5c43', 8), ('60a26a3e', 8), ('60b61512', 8), ('6430c8c4', 8), ('66f2d22f', 8), ('67a423a3', 8), ('6d75e8bb', 8), ('6e02f1e3', 8), ('6ea4a07e', 8), ('7039b2d7', 8), ('7c008303', 8), ('7e02026e', 8), ('7f4411dc', 8), ('810b9b61', 8), ('929ab4e9', 8), ('94f9d214', 8), ('9f27f097', 8), ('a1570a43', 8), ('a61f2674', 8), ('b230c067', 8), ('ce4f8723', 8), ('d13f3404', 8), ('d19f7514', 8), ('dc433765', 8), ('e133d23d', 8), ('e2092e0c', 8), ('e345f17b', 8), ('e50d258f', 8), ('e76a88a6', 8), ('e7dd8335', 8), ('e8593010', 8), ('f2829549', 8), ('fafffa47', 8), ('fcb5c309', 8), ('ff805c23', 8), ('025d127b', 9), ('0692e18c', 9), ('0b17323b', 9), ('0becf7df', 9), ('137eaa0f', 9), ('1f642eb9', 9), ('22168020', 9), ('2281f1f4', 9), ('253bf280', 9), ('25d487eb', 9), ('27a28665', 9), ('27a77e38', 9), ('2c608aff', 9), ('31aa019c', 9), ('358ba94e', 9), ('3ac3eb23', 9), ('3bd67248', 9), ('444801d8', 9), ('4852f2fa', 9), ('4be741c5', 9), ('5b6cbef5', 9), ('6455b5f5', 9), ('67636eac', 9), ('681b3aeb', 9), ('694f12f3', 9), ('6a11f6da', 9), ('6e82a1ae', 9), ('72ca375d', 9), ('73251a56', 9), ('7d1f7ee8', 9), ('7ddcd7ec', 9), ('868de0fa', 9), ('8e5a5113', 9), ('8f2ea7aa', 9), ('903d1b4a', 9), ('917bccba', 9), ('a5f85a15', 9), ('aee291af', 9), ('b2862040', 9), ('b8825c91', 9), ('b8cdaf2b', 9), ('bbc9ae5d', 9), ('bd4472b8', 9), ('bda2d7a6', 9), ('c3202e5a', 9), ('cce03e0d', 9), ('cf98881b', 9), ('d364b489', 9), ('d4f3cd78', 9), ('d90796e8', 9), ('da2b0fe3', 9), ('e66aafb8', 9), ('e7a25a18', 9), ('f0afb749', 9), ('fcc82909', 9), ('09c534e7', 10), ('11852cab', 10), ('12eac192', 10), ('15696249', 10), ('178fcbfb', 10), ('1b60fb0c', 10), ('1caeab9d', 10), ('1fad071e', 10), ('2037f2c7', 10), ('21f83797', 10), ('23581191', 10), ('27f8ce4f', 10), ('31d5ba1a', 10), ('321b1fc6', 10), ('3428a4f5', 10), ('34b99a2b', 10), ('37d3e8b2', 10), ('3b4c2228', 10), ('3de23699', 10), ('3eda0437', 10), ('47c1f68c', 10), ('48f8583b', 10), ('54d9e175', 10), ('5ad4f10b', 10), ('5b526a93', 10), ('623ea044', 10), ('67b4a34d', 10), ('6b9890af', 10), ('6c434453', 10), ('77fdfe62', 10), ('782b5218', 10), ('794b24be', 10), ('7c8af763', 10), ('7c9b52a0', 10), ('80af3007', 10), ('83302e8f', 10), ('88a10436', 10), ('88a62173', 10), ('890034e9', 10), ('8e2edd66', 10), ('8eb1be9a', 10), ('99b1bc43', 10), ('a3325580', 10), ('a9f96cdd', 10), ('aa18de87', 10), ('ad7e01d0', 10), ('af902bf9', 10), ('b548a754', 10), ('bb52a14b', 10), ('bdad9b1f', 10), ('c0f76784', 10), ('c3e719e8', 10), ('c8cbb738', 10), ('d37a1ef5', 10), ('d4469b4b', 10), ('d47aa2ff', 10), ('d492a647', 10), ('d8c310e9', 10), ('dc0a314f', 10), ('ddf7fa4f', 10), ('de1cd16c', 10), ('ef26cbf6', 10), ('f4081712', 10), ('f8b3ba0a', 10), ('009d5c81', 11), ('09629e4f', 11), ('2a5f8217', 11), ('2c737e39', 11), ('45737921', 11), ('575b1a71', 11), ('6ecd11f4', 11), ('6f473927', 11), ('760b3cac', 11), ('7e0986d6', 11), ('817e6c09', 11), ('8597cfd7', 11), ('8fbca751', 11), ('981571dc', 11), ('a04b2602', 11), ('a85d4709', 11), ('c074846d', 11), ('c444b776', 11), ('c7d4e6ad', 11), ('d4a91cb9', 11), ('e69241bd', 11), ('eb281b96', 11), ('ed74f2f2', 11), ('feca6190', 11), ('ff28f65a', 11), ('070dd51e', 12), ('0962bcdd', 12), ('25094a63', 12), ('281123b4', 12), ('31adaf00', 12), ('33b52de3', 12), ('3a301edc', 12), ('52fd389e', 12), ('7ee1c6ea', 12), ('85b81ff1', 12), ('90347967', 12), ('913fb3ed', 12), ('a68b268e', 12), ('aa300dc3', 12), ('b4a43f3b', 12), ('be03b35f', 12), ('beb8660c', 12), ('bf32578f', 12), ('c35c1b4c', 12), ('c8b7cc0f', 12), ('c92b942c', 12), ('ce039d91', 12), ('e41c6fd3', 12), ('e99362f0', 12), ('ea9794b1', 12), ('f5c89df1', 12), ('05269061', 13), ('0d87d2a6', 13), ('11e1fe23', 13), ('3631a71a', 13), ('62b74c02', 13), ('92e50de0', 13), ('95990924', 13), ('9c56f360', 13), ('af24b4cc', 13), ('d43fd935', 13), ('d94c3b52', 13), ('db3e9e38', 13), ('e509e548', 13), ('e73095fd', 13), ('e9b4f6fc', 13), ('00dbd492', 14), ('18419cfa', 14), ('1bfc4729', 14), ('292dd178', 14), ('2c0b0aff', 14), ('6df30ad6', 14), ('7447852a', 14), ('759f3fd3', 14), ('8e1813be', 14), ('91714a58', 14), ('93b4f4b3', 14), ('93b581b8', 14), ('97999447', 14), ('9edfc990', 14), ('a61ba2ce', 14), ('a65b410d', 14), ('bc1d5164', 14), ('ce602527', 14), ('1a07d186', 15), ('22a4bbc2', 15), ('4093f84a', 15), ('4364c1c4', 15), ('50a16a69', 15), ('55783887', 15), ('5c2c9af4', 15), ('695367ec', 15), ('6cf79266', 15), ('6e19193c', 15), ('72207abc', 15), ('72a961c9', 15), ('75b8110e', 15), ('762cd429', 15), ('8a004b2b', 15), ('941d9a10', 15), ('9af7a82c', 15), ('9b4c17c4', 15), ('a8610ef7', 15), ('a87f7484', 15), ('af22c60d', 15), ('ba9d41b8', 15), ('c3f564a4', 15), ('cbded52d', 15), ('d687bc17', 15), ('e26a3af2', 15), ('e619ca6e', 15), ('ef135b50', 15), ('f21745ec', 15), ('fafd9572', 15), ('12422b43', 16), ('1f0c79e5', 16), ('29c11459', 16), ('414297c0', 16), ('4612dd53', 16), ('516b51b7', 16), ('5289ad53', 16), ('5af49b42', 16), ('705a3229', 16), ('88207623', 16), ('8a371977', 16), ('963e52fc', 16), ('a680ac02', 16), ('aa4ec2a5', 16), ('ae3edfdc', 16), ('b7fb29bc', 16), ('ba26e723', 16), ('c658a4bd', 16), ('e1baa8a4', 16), ('2697da3f', 17), ('3391f8c0', 17), ('42a15761', 17), ('56dc2b01', 17), ('6773b310', 17), ('73c3b0d8', 17), ('770cc55f', 17), ('a406ac07', 17), ('aab50785', 17), ('ce8d95cc', 17), ('e1d2900e', 17), ('e48d4e1a', 17), ('e57337a4', 17), ('e633a9e5', 17), ('e7b06bea', 17), ('e9bb6954', 17), ('e9c9d9a1', 17), ('f45f5ca7', 17), ('20981f0e', 18), ('2204b7a8', 18), ('3d31c5b3', 18), ('3f23242b', 18), ('45bbe264', 18), ('4938f0c2', 18), ('5207a7b5', 18), ('58743b76', 18), ('673ef223', 18), ('780d0b14', 18), ('834ec97d', 18), ('846bdb03', 18), ('8719f442', 18), ('90f3ed37', 18), ('9110e3c5', 18), ('a8d7556c', 18), ('b782dc8a', 18), ('b7999b51', 18), ('b7f8a4d8', 18), ('c1990cce', 18), ('ca8de6ea', 18), ('d9f24cd1', 18), ('e5062a87', 18), ('e681b708', 18), ('ecdecbb3', 18), ('f5b8619d', 18), ('f83cb3f6', 18), ('f8c80d96', 18), ('0e671a1a', 19), ('137f0df0', 19), ('29700607', 19), ('3bdb4ada', 19), ('539a4f51', 19), ('551d5bf1', 19), ('5daaa586', 19), ('8403a5d5', 19), ('8dae5dfc', 19), ('91413438', 19), ('94133066', 19), ('9bebae7a', 19), ('bf89d739', 19), ('d931c21c', 19), ('ec883f72', 19), ('f3cdc58f', 19), ('1c02dbbe', 20), ('272f95fa', 20), ('29623171', 20), ('2bee17df', 20), ('2f0c5170', 20), ('53b68214', 20), ('54db823b', 20), ('7d18a6fb', 20), ('8731374e', 20), ('a2fd1cf0', 20), ('b0c4d837', 20), ('d6ad076f', 20), ('db93a21d', 20), ('df8cc377', 20), ('e40b9e2f', 20), ('e8dc4411', 20), ('fb791726', 20), ('3345333e', 21), ('50aad11f', 21), ('642248e4', 21), ('6cdd2623', 21), ('8d510a79', 21), ('9772c176', 21), ('a3df8b1e', 21), ('b190f7f5', 21), ('bd14c3bf', 21), ('cdecee7f', 21), ('d304284e', 21), ('06df4c85', 22), ('0a2355a6', 22), ('12997ef3', 22), ('2b01abd0', 22), ('4522001f', 22), ('63613498', 22), ('639f5a19', 22), ('746b3537', 22), ('95a58926', 22), ('a48eeaf7', 22), ('a57f2f04', 22), ('ac0c5833', 22), ('caa06a1f', 22), ('cad67732', 22), ('d2acf2cb', 22), ('d5c634a2', 22), ('d89b689b', 22), ('e21d9049', 22), ('f9012d9b', 22), ('fe9372f3', 22), ('ff72ca3e', 22), ('03560426', 23), ('0a938d79', 23), ('1d0a4b61', 23), ('228f6490', 23), ('36fdfd69', 23), ('4c177718', 23), ('50f325b5', 23), ('604001fa', 23), ('992798f6', 23), ('995c5fa3', 23), ('9def23fe', 23), ('b1fc8b8e', 23), ('c663677b', 23), ('ca8f78db', 23), ('d06dbe63', 23), ('de493100', 23), ('e179c5f4', 23), ('e78887d1', 23), ('e95e3d8e', 23), ('eb5a1d5d', 23), ('045e512c', 24), ('13713586', 24), ('1990f7a8', 24), ('4e469f39', 24), ('72322fa7', 24), ('79369cc6', 24), ('82819916', 24), ('855e0971', 24), ('99fa7670', 24), ('a78176bb', 24), ('ac3e2b04', 24), ('447fd412', 25), ('6aa20dc0', 25), ('6d58a25d', 25), ('952a094c', 25), ('9caba7c3', 25), ('baf41dbf', 25), ('c97c0139', 25), ('d4c90558', 25), ('e6721834', 25), ('e88171ec', 25), ('0dfd9992', 26), ('17cae0c1', 26), ('1da012fc', 26), ('29ec7d0e', 26), ('2bcee788', 26), ('36d67576', 26), ('776ffc46', 26), ('81c0276b', 26), ('98cf29f8', 26), ('9c1e755f', 26), ('cb227835', 26), ('d282b262', 26), ('e9ac8c9e', 26), ('f35d900a', 26), ('103eff5b', 27), ('2753e76c', 27), ('39e1d7f9', 27), ('469497ad', 27), ('484b58aa', 27), ('5a5a2103', 27), ('712bf12e', 27), ('99306f82', 27), ('c62e2108', 27), ('e4075551', 27), ('3befdf3e', 28), ('9aec4887', 28), ('15663ba9', 29), ('17b80ad2', 29), ('49d1d64f', 29), ('57aa92db', 29), ('5833af48', 29), ('963f59bc', 29), ('dc2e9a9d', 29), ('f3e62deb', 29), ('845d6e51', 30), ('aba27056', 30), ('f1cefba8', 30), ('1e32b0e9', 31), ('28e73c20', 31), ('4aab4007', 31), ('4b6b68e5', 32), ('4c5c2cf0', 32), ('508bd3b6', 32), ('6d0160f0', 32), ('a096bf4d', 32), ('d07ae81c', 32), ('e760a62e', 32), ('f8a8fe49', 32), ('20818e16', 33), ('6a1e5592', 33), ('ea959feb', 33), ('1acc24af', 34), ('fd096ab6', 34), ('0bb8deee', 35), ('0e206a2e', 35), ('c87289bb', 35), ('d22278a0', 35), ('f9a67cb5', 35), ('4290ef0e', 37), ('dc2aa30b', 38), ('50846271', 39), ('150deff5', 40), ('b20f7c8b', 40), ('b527c5c6', 40), ('b7249182', 40), ('6855a6e4', 41), ('9d9215db', 41), ('96a8c0cd', 42), ('cfb2ce5a', 42), ('e5790162', 42), ('1d398264', 43), ('264363fd', 44), ('234bbc79', 45), ('58e15b12', 45), ('7df24a62', 45), ('9356391f', 45), ('f15e1fac', 45), ('22233c11', 46), ('5ffb2104', 46), ('8ba14f53', 46), ('2dd70a9a', 47), ('a3f84088', 48), ('a64e4611', 48), ('ac605cbb', 48), ('7837ac64', 50), 
 ('a8c38be5', 50), ('b775ac94', 57), ('891232d6', 58), ('69889d6e', 59), ('97a05b5b', 60), ('3e980e27', 61), ('6ad5bdfd', 67)]).


read_dsl_test:-  make,     
      setup_call_cleanup(open('arc-dsl/solvers.py',read,In2,[]),read_python(In2),close(In2)),
      uast,!,
      setup_call_cleanup(open('arc-dsl/constants.py',read,In1,[]),read_python(In1),close(In1)),
      !.
read_michod_test:- read_sols,read_dsl.

uast:- uast_test.
uast_test:-setup_call_cleanup(open('arc-dsl/dsl.py.uast',read,In3,[]),read_uast_python(In3),close(In3)).
uast_test2:-setup_call_cleanup(open('arc-dsl/dsl.json',read,In3,[]),read_as_term(In3),close(In3)).

read_as_term(In):- locally(set_prolog_flag(allow_variable_name_as_functor,true),read_term(In,Term,[])),pp(Term).
read_uast_python(In):- %json_read(In,AST),
 must_det_ll((
  notrace(((read_stream_to_codes(In,Codes),
  text_to_string(Codes,String),
  replace_in_string(['{'='[','}'=']','"'='\'',
    "'@type':" = "",
        '\n'=' ','\r'=' ','\t'=' ',
        '        '=' ','   '=' ','  '=' ', '  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ',
        ', }'=' }',', ]'=' ]'],String,SAST),!,
  %catch((atom_to_term(SAST,AST,Vs),maplist(call,Vs)),_,fail),
  atom_to_term(SAST,AST,Vs),maplist(call,Vs)))),
  do_uast_python(AST))).

remove_i('<>').
%remove_i(AB):- eq_swap(AB,BA),remove_ii(BA),!.
remove_i(AB):- remove_ii(AB),!.


eq_swap(AB,AB).
eq_swap(A:B,B:A):-!.

remove_ii('@pos':_).
remove_ii('@role':_).
remove_ii('else_stmts': []).
remove_ii(_ : []).
remove_ii(keywords:[]).

remove_ii(_ :['python:If.orelse']).
remove_ii(_ :['python:For.orelse']).

remove_ii('Format': '').

remove_ii('MapVariadic': false).
remove_ii('Receiver': false).
remove_ii('Variadic': false).
remove_ii(async:false).

remove_ii(is_async:0).

remove_ii('Name': '~').
remove_ii('Type': '~').
remove_ii(_: '~').

remove_ii(ctx:LoadSave):-load_store(LoadSave).

remove_ii('if' : '@token').
remove_ii('for' : '@token').
%remove_ii('uast:Block' : '@type').
remove_ii('@token':LCase):-  lowercase_word(LCase).
%remove_ii('@token',LCase):-  freeze(LCase,lowercase_word(LCase)).

lowercase_word(LCase):- atomic(LCase),downcase_atom(LCase,LCase), \+ upcase_atom(LCase,LCase).

%s_ast([List],[O]):- !, s_ast0(List,O),!, O\=@=List.
%s_ast(List,O):- !, s_ast0(List,O),!, O\=@=List.

%s_ast0([List],[O]):- s_ast0(List,O),!.
%s_ast(ListIn,O):- is_list(ListIn),!, ss_ast([S|List],O),is_list(List),permutation(List,Prem),maplist(eq_swap,Prem,I),ListIn=I,!.
%s_ast0(ListIn,O):- is_list(ListIn),!, ss_ast([S|List],O) ,permutation(List,Prem),eq_swap(S:'@type',W),ListIn=[W|Prem],!.  permutation(List,Prem),
s_ast(identifier_name(A),S):- atom_string(A,S),!.
s_ast(S,O):- ss_ast(S,O).
s_ast(('Name': 'None'),[]).
s_ast(comparators([V]),V).
s_ast(ops([V]),V).

s_ast(['uast:Identifier',[]],[]).

s_ast(S,O):- basic_wrapper(F),M=..[F,O],S=M, \+ atom(O),!.
%s_ast0(Swap,O):- eq_swap(Swap,Term),ss_ast(Term,O),!.

basic_wrapper(boxed_name_value).
basic_wrapper(boxed_str_value).
basic_wrapper(index_value).
basic_wrapper(num_token).

%swap_colons(A:B,Swap):- freeze(Swap,eq_swap(A:B,Swap)),!.

tokenX(X:'@token',X):-X\=='@token'.
tokenX('@token':X,X):-X\=='@token'.

load_store('Load').
load_store('Store').

ss_ast('python:BoxedName'(boxed_value:'uast:Identifier'('Name':ToIndices),ctx:'Load'),ToIndices).
ss_ast('python:Assign'(targets:To,value:From),setvars(To,From)).
ss_ast('python:Call'(args:Args,func:Call),eval(Call,Args)).

ss_ast('uast:Identifier'('Name':X),X):-!.
ss_ast('python:Num'(TokenX),X):- tokenX(TokenX,X).
ss_ast('python:Add'(TokenX),X):- tokenX(TokenX,X).

ss_ast('python:Tuple'(ctx:LoadSave,elts:X),X):-load_store(LoadSave).
ss_ast('python:BoxedName'(boxed_value:X,ctx:LoadSave),X):-load_store(LoadSave).
%ss_ast(body_stmts:X,X).
%ss_ast('Statements':X,X).

%basic_wrapper(tuple_elts).


not_excluded(X):- \+ remove_i(X),!.

simplify_ast(I,O):- var(I),!,O=I.
simplify_ast(I,O):- \+ compound(I),!,O=I.
simplify_ast(I,O):- s_ast(I,O),!.
%simplify_ast(M:I,M:O):- s_ast(I,O),!.
simplify_ast(M:I,M:O):- !, simplify_ast(I,O).
simplify_ast([I|J],[II|JJ]):- once(include(not_excluded,[I|J],[II|JJ])), [I|J]\=@=[II|JJ],!.
  %simplify_ast([II|JJ],IJO).
simplify_ast([I|J],[II|JJ]):- once(maplist(d_simplify_ast,[I|J],[II|JJ])), [I|J]\=@=[II|JJ],!.

simplify_ast([I,A1|J],IJ):- atom(I), reassemble_arumwents(I,[A1|J],IJ).

%simplify_ast([I|J],[II|JJ]):- once(maplist(d_simplify_ast,[I|J],[II|JJ])), [I|J]\=@=[II|JJ],!.
%simplify_ast(I,'<>'):- I\=='<>',remove_i(I),!.
/*simplify_ast([A,B,I|J],[II|JJ]):- s_ast([A,B,I],II),!,simplify_ast(J,JJ).
simplify_ast([A,I|J],[II|JJ]):- s_ast([A,I],II),!,simplify_ast(J,JJ).
simplify_ast([Rm|I],O):- remove_i(Rm),!,simplify_ast(I,O).
*/
simplify_ast([I|J],[I|J]):- !.
simplify_ast(I,O):- compound_name_arguments(I,F,II),maplist(d_simplify_ast,II,OO),!,
  reassemble_arumwents(F,OO,O).

reassemble_arumwents(F,OO,O):- %%compound_name_arguments(O,F,OO).
  make_kw(F,FKW),
  grab_kwords(OO,FOO),
  flatten(FOO,FOOF),
  predsort(sort_on(nth_kw_order(F)),FOOF,SortedFOO),
  maplist(arg(1),SortedFOO,SortedKW),
  maplist(arg(2),SortedFOO,SortedVals),
  flatten([FKW|SortedKW],KFOOF),
  list_to_set(KFOOF,SetKW),
  atomic_list_concat(SetKW,'_',SFOO),
  make_kw(SFOO,SFOOS),
  O=..[SFOOS|SortedVals].


make_kw(I,O):- is_list(I),!,maplist(make_kw,I,M),flatten(M,F),list_to_set(F,Set),atomic_list_concat(Set,'_',O).
make_kw(I,O):-atomic_list_concat([_,B],  ':',I),!,make_kw(B,O).
make_kw(I,O):-atomic_list_concat(['',B], '@',I),!,make_kw(B,O).
make_kw(I,O):-atomic_list_concat([_,B],  '.',I),!,make_kw(B,O).
make_kw(I,O):-atomic_list_concat([A,B|C],'_',I),!,list_to_set([A,B|C],Set),atomic_list_concat(Set,'_',O).
make_kw(I,O):-string_to_functor(I,O),!.

% grab_kwords(F,OO,FOO,Values):-

grab_kwords([(K:V)|FOO],[KW=V|OUT]):-!,
  make_kw(K,KW),
  grab_kwords(FOO,OUT),!.
grab_kwords([],[]).

nth_kw_order(F,Kw:_,Nth4):- !, nth_kw_order(F,Kw,Nth4).
nth_kw_order(F,Kw=_,Nth4):- !, nth_kw_order(F,Kw,Nth4).
nth_kw_order(F,Kw,Nth4):- kw_order(F,List),nth1(Nth1,List,E),E==Kw,!,Nth4 is 4 * Nth1.
nth_kw_order(_,_,10).

kw_order('python:Subscript',[value,slice]).
kw_order(_,[op,left,right]).
kw_order(_,[is,op,values,value]).
kw_order(_,[ops,left,comparators]).
kw_order(_,[func,args,ctx]).
kw_order(_,[while,for,if,test,target,iter,ifs,else,token,stmts,body,orelse]).
kw_order(_,[targets,value]).
kw_order(_,[return,value]).

d_simplify_ast(AST,ASTO):- once(simplify_ast(AST,AST1)),%writeln(AST1),
  AST1\=@=AST,!,d_simplify_ast(AST1,ASTO).
d_simplify_ast(AST,AST).

do_uast_python(AST):- 
 notrace((d_simplify_ast(AST,AST11),d_simplify_ast(AST11,AST1), nop((writeq(AST1),nl,nl,print(AST1))),nl,nl)),!,
  must_det_ll((compile_uast_python(AST1,_OUT))).

:- if(false).
see_training_pair(IN,OUT):-
%<backtracks to here>
    iprops = propFromGrid(IN); 
    oprops = propFromGrid(OUT); 
% <or maybe only to here>
     important, unimportant = search_for_the_important_and_unimportant_props(ioprop,oprops);
     matchesOut = make_output_grid(iprops,important, unimportant);
   % if (matchesOut==OUT) return(important, unimportant);
   fail.
pass_test(TEST):-
  iprops = propFromGrid(TEST); 
  make_output_grid(iprops,important, unimportant).
:- endif.

pp_w_dot(PP):-print_tree_with_final(PP,'.\n').
%pp_w_dot(PP):-pp(PP),writeln('.').

compile_uast_python(AST,[]):- \+ compound(AST),!,nop(dmsg(compile_uast_python(AST,_))).
compile_uast_python(AST,OUT):- is_list(AST),!,maplist(compile_uast_python,AST,OUTE),into_codeL(OUTE,OUT),!.
compile_uast_python(alias_name_node(Name,AST),OUT):- !, compile_pyfunction_to_kl_one(Name,AST,OUTE),into_codeL(OUTE,OUT),!,
  format('~N% Compiled KL-1 for ~w~n',[Name]),
  maplist(pp_w_dot,OUT),!.
compile_uast_python(AST,OUT):- compound_name_arguments(AST,_,ARGS),!,maplist(compile_uast_python,ARGS,OUTE),
   into_codeL(OUTE,OUT).

compile_pyfunction_to_kl_one(Name,function_type_body(function_type_arguments_returns(IN,OUT),ASTNode),[Head:-Body]):- 
  flag(py_gensym,_,1),
  in_cmt((writeln("% Universal AST Pass #0"),pp(red,def(Name,function_type_body(function_type_arguments_returns(IN,OUT),ASTNode))))),
  must_det_ll((
   compute_head(Name,HEADL,IN,OUT,ASTNode,NewAST,RET,Code12),
   into_callL(HEADL,Head),
   compile_block_return_arg_until_done(RET,NewAST,Code3),
   code_to_conjuncts([Code12,Code3],Body))).

code_to_conjuncts(BodyE,Body):- into_codeL(BodyE,BodyL), list_to_conjuncts(BodyL,Body),!.
into_codeL(OUTE,OUTO):- flatten([OUTE],OUTL),maplist(conjuncts_to_list,OUTL,OUTL1),flatten(OUTL1,OUTO).

into_callL([A,O|UTE],OUTO):- cmpd(A),append_term(A,O,AO),!,into_callL([AO|UTE],OUTO).
into_callL([A|OUTE],OUTO):- atom(A), \+ (upcase_atom(A,UC),UC=A),!,OUTO=..[A|OUTE].
into_callL([A|OUTE],OUTO):- A\==op_call, A\=op_call(_), !, into_callL([op_call(A)|OUTE],OUTO).
into_callL(OUTE,OUTO):- OUTO=..[call,OUTE],!.

%string_to_var(Str,_):- any_to_atom(Str,Atom),downcase_atom(Atom,DAtom),Atom\==DAtom,!,fail.
string_to_var(Str,VAR):- is_list(Str),maplist(string_to_var,Str,VAR).
string_to_var(Str,'$VAR'(UAtom)):- any_to_atom(Str,Atom),upcase_atom(Atom,UAtom),!.
string_to_gensym_var(Str,VAR):- is_list(Str),maplist(string_to_gensym_var,Str,VAR).
string_to_gensym_var(Str,'$VAR'(Sym)):- any_to_atom(Str,Atom),upcase_atom(Atom,UAtom),py_gensym(UAtom,Sym).

compute_head(Name,HEADL,IN,OUT,ASTNode,NewAST,RET,Code12):-
     must_det_ll((atom_string(AName,Name),
     maplist(compile_parameter(head),HEADARGS,Replace,IN,Code1),
     subst_2L(Replace,HEADARGS,ASTNode+OUT,NewAST+NewOUT),
     maplist(compile_parameter(head,RET,_ReplaceOut),NewOUT,Code2),
     append([AName|HEADARGS],[RET],HEADL),
     into_codeL([Code1,Code2],Code12))).

compile_block_return_arg_until_done(RET,NewAST,Code):-
  compile_block_return_arg_until_done1(ra(0),RET,NewAST,Code1),
  compile_block_return_arg_until_done1(ra(1),RET,Code1,Code2),
  compile_block_return_arg_until_done1(ra(2),RET,Code2,Code3),
  compile_block_return_arg_until_done1(ra(0),RET,Code3,Code4),
  compile_block_return_arg_until_done1(ra(1),RET,Code4,Code5),
  compile_block_return_arg_until_done1(ra(2),RET,Code5,Code),!.

compile_block_return_arg_until_done1(PASS,RET,NewAST,Code):- 
  compile_block([block],PASS,RET,NewAST,Code3),!,
  (NewAST\=@=Code3->compile_block_return_arg_until_done1(PASS,RET,Code3,Code)
    ;(nop(in_cmt((writeln("% Body Pass #1"),pp(yellow,NewAST)))),
      do_gather_replace_vars(NewAST,Code))),!.




str_var(Str):- is_ftVar(Str),!.
str_var(Str):- string(Str),!.
str_var(Str):- number(Str),!.
str_var(Str):- atom(Str),!.

itr_var(V):- original_var(V,S),atom(S),atom_contains(S,'_').

original_var(V):- original_var(V,_),!.
original_var('$VAR'(V),S):-!, original_var(V,S).
original_var(V,S):- string(V), !, atom_string(A,V),!,original_var(A,S).
original_var(V,V):- number(V),!.
original_var(V,_):- \+ atom(V),!,fail.
original_var(V,V):- downcase_atom(V,DC),!, \+ arg_or_cc(DC).
arg_or_cc(DC):- atom_concat('arg_',_,DC).
arg_or_cc(DC):- atom_concat('cc_',_,DC).

/*
do_gather_replace_vars(NewAST,Code):-
  sub_term(E,NewAST),compound(E),E=call(Str=WHAT),string(Str), str_var(WHAT) , !,
  subst_2L([E,Str],[true,WHAT],NewAST,CodeM),!,
  do_gather_replace_vars(CodeM,Code).
*/
do_gather_replace_vars(NewAST,Code):- 
  do_gather_replace_vars([],NewAST,Code),!.

do_gather_replace_vars(NotIn,NewAST,Code):- fail,
  sub_term(E1,NewAST),compound(E1),arg(A1,E1,S1),string(S1),
  sub_term(E2,NewAST),compound(E2),arg(A2,E2,S2),string(S2),S1==S2,(E1\=@=E2;A1\==A2),(A1\==1;A2\==1),
  Str=S1,
  string_to_var(Str,VAR),
  subst001(NewAST,Str,VAR,CodeM),!,
  do_gather_replace_vars([Str|NotIn],CodeM,Code).

do_gather_replace_vars(NotIn,NewAST,Code):-
  sub_term(E,NewAST),compound(E),replace_str_var(E,Str,VAR),  \+ (original_var(Str), \+ original_var(VAR)),
  \+ itr_var(Str), \+ member(Str,NotIn),
  subst001(NewAST,Str,VAR,CodeM),!,
  do_gather_replace_vars([Str|NotIn],CodeM,Code).


do_gather_replace_vars(_,Code,Code).


replace_str_var(call(Str=VAR),call(Str=VAR),true):- Str=@=VAR,!.
replace_str_var(call(Str=VAR),Str,VAR):- is_ftVar(Str), is_ftVar(VAR).
replace_str_var(call(Str=VAR),Str,VAR):- string(Str), is_ftVar(VAR).
replace_str_var(call_func_args(_,List),Str,VAR):- !, member(Str,List),should_been_var(Str),string_to_var(Str,VAR).
replace_str_var(tuple_elts(List),Str,VAR):- !, member(Str,List),should_been_var(Str),string_to_var(Str,VAR).
replace_str_var(E,Str,VAR):- E=into_tuple(_,_,_),!,arg(_,E,Str),should_been_var(Str), string_to_var(Str,VAR).
replace_str_var(E,Str,VAR):- E=from_tuple(_,_,_),!,arg(_,E,Str),should_been_var(Str), string_to_var(Str,VAR).
replace_str_var(VAR,Str,VAR):- is_ftVar(VAR),VAR='$VAR'(Atom),downcase_atom(Atom,DAtom),atom_string(DAtom,Str),!.
/*
do_gather_replace_vars(NewAST,Code):-
  sub_term(E,NewAST),fail,string(E),!,
  string_t o_var(E,VAR), subst_2L([E],[VAR],NewAST,CodeM),!,
  do_gather_replace_vars(CodeM,Code).*/

should_been_var(Str):- string(Str), lowercase_word(Str).
tok_atom(ADDTOK,Atom):- atom(ADDTOK),!,Atom=ADDTOK.
tok_atom(ADDTOK,Atom):- string(ADDTOK),!,atom_string(Atom,ADDTOK).
tok_atom(ADDTOK,Atom):- \+ compound(ADDTOK),!,Atom=ADDTOK.
%1````````````````````````````````````````tok_atom(ADDTOK,Atom):- functor(ADDTOK,Atom,1),!.
tok_atom(ADDTOK,Atom):- arg(1,ADDTOK,Atom). %,atom_string(Atom,Str).
tok_atom(ADDTOK,ADDTOK).

:- discontiguous compile_block/5.

compile_block(PF,_PASS,_RET,A,A):- is_ftVar(A),!.
compile_block(PF,_PASS,_RET,A,A):- \+ compound(A),!.
compile_block(PF,PASS,RET,(AST,ASTL),Code):- !,
  compile_block(PF,PASS,RET,AST,Code1),
  compile_block(PF,PASS,RET,ASTL,Code2),
  into_codeL([Code1,Code2],Code).
compile_block(PF,PASS,RET,[AST|ASTL],Code):- !,
  compile_block(PF,PASS,RET,AST,Code1),
  compile_block(PF,PASS,RET,ASTL,Code2),
  into_codeL([Code1,Code2],Code).  
compile_block(PF,PASS,RET,orelse_else_stmts(AST),BODY):- !,
  compile_block(PF,PASS,RET,AST,CODE),
  code_to_conjuncts(CODE,BODY).
compile_block(PF,PASS,RET,body_stmts(AST),BODY):- !,
  compile_block(PF,PASS,RET,AST,CODE),
  code_to_conjuncts(CODE,BODY).
compile_block(PF,PASS,RET,block_statements(AST),BODY):-!,
  compile_block(PF,PASS,RET,AST,CODE),
  code_to_conjuncts(CODE,BODY).
compile_block(PF,ra(0),RET,if_test_body(Test,TrueBody),CODE):- !,
  compile_block(PF,ra(0),RET,(testif(Test)->TrueBody;true),CODE).
compile_block(PF,ra(0),RET,if_test_body_orelse(Test,TrueBody,ElseBody),CODE):-!,
  compile_block(PF,ra(0),RET,(testif(Test)->TrueBody;ElseBody),CODE).
compile_block(PF,ra(0),RET,for_target_iter_body(Var,Range,Body),CODE):- !,
  string_to_gensym_var(Var,VAR),
  subst(Body,Var,VAR,NBody),
  compile_block(PF,ra(0),RET,
    for_each(assign_targets_value([VAR],Range),NBody),CODE).




compile_block(_PF,ra(0),_RET,call_func_args("isinstance",[ELEMENT_01,STuple]),[
 call_func_args(isinstance,[ELEMENT_01,Tuple])]):- 
   string(STuple),atom_string(Tuple,STuple),!.

compile_block(PF,ra(0),_RET,expr_value(string_value(Str)),comment(Str)).

compile_block(PF,ra(0),RET,return_value(Stuff),Code):-
  compile_block(PF,ra(0),RET,[assign_targets_value([RET],Stuff),exit_proc(RET)],Code).



compile_block(PF,ra(0),_RET,bin_op_left_right(ADDTOK,L,R),call_func_args(ADDTOK,[L,R])).
compile_block(PF,ra(0),_RET,bin_op_left_right(ADDTOK,L,R,O),call_func_args(ADDTOK,[L,R],O)).
%compile_block(PF,ra(0),_RET,bin_op_left_right(ADDTOK,L,R),call_func_args(Str,[L,R])):- 
%  tok_atom(ADDTOK,Str),!.
compile_block(PF,ra(0),_RET,unary_op_operand(ADDTOK,L),call_func_args(Str,[L])):- 
  tok_atom(ADDTOK,Str),!.

compile_block(PF,ra(0),_RET,compare_ops_left_comparators(ops([]),_L,comparators([])),[]):-!.
compile_block(PF,ra(0),RET,compare_ops_left_comparators(ops([E1|List1]),L,comparators([E2|List2])),Code):- 
  length(List1,Len),length(List2,Len),
  compile_block(PF,ra(0),RET,compare_ops_left_comparators(E1,L,E2),Code1),
  compile_block(PF,ra(0),RET,compare_ops_left_comparators(ops(List1),L,comparators(List2)),Code2),
  into_codeL([Code1,Code2],Code).
compile_block(PF,ra(0),_RET,compare_ops_left_comparators(ADDTOK,L,R),call_func_args(Str,[L,R])):- %\+ is_list(ADDTOK),
  tok_atom(ADDTOK,Str),!.
%compare_ops_left_comparators(['python:NotIn'],J,"ci")


compile_block(PF,_PASS,_RET,set_comp_elt_generators(A,
                               [ comprehension_target_iter_ifs(B,C,D)]), set_comp_elt_generators_ifs(A,B,C,D)).
compile_block(PF,ra(0),_RET,
    set_comp_elt_generators(A,comprehension_target_iter(B,C,D)),
    set_comp_elt_generators_comprehension_target_iter(A,B,C,D)).
compile_block(PF,PASS,RET, 
   generator_exp_elt_generators(tuple_elts([E1|List1]), [E2|List2]),Code):-
   length(List1,Len),length(List2,Len),
   compile_block(PF,PASS,RET,generator_exp_elt_generator_1(E1,E2),Code1),
   compile_block(PF,PASS,RET,generator_exp_elt_generators(tuple_elts(List1),List2),Code2),
   into_codeL([Code1,Code2],Code).

compile_block(PF,_PASS,_RET, set_comp_elt_generators(B,C),elt_generator(v2,VARS,C)):- is_vars(B,VARS).
compile_block(PF,_PASS,_RET, generator_exp_elt_generators(B,C),elt_generator(v1,VARS,C)):- is_vars(B,VARS).
compile_block(PF,_PASS,_RET, comprehension_target_iter(B,C),assign_targets_value1(VARS,C)):- is_vars(B,VARS).
compile_block(PF,_PASS,_RET, generator_exp_elt_generators(B),assign_targets_value(["T"],B)).
compile_block(PF,_PASS,_RET, set_comp_elt_generators(B),assign_targets_value(["T"],B)).

is_vars(List,List):- is_list(List),!,maplist(is_vars,List).
is_vars(B,B):- is_vars(B),!.

is_vars(B):- str_var(B),!.
is_vars(tuple_elts(_)).







/*
def vperiod(
    obj: Object
) -> Integer:
    """ vertical periodicity """
    normalized = normalize(obj)
    h = height(normalized)
    for p in range(1, h):
        offsetted = shift(normalized, (-p, 0))
        pruned = frozenset({(c, (i, j)) for c, (i, j) in offsetted if i >= 0})
        if pruned.issubset(normalized):
            return p
    return h
*/
%~ def( "vperiod",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' vertical periodicity ')),
%~                          assign_targets_value(["normalized"],call_func_args("normalize",["obj"])),
%~                          assign_targets_value(["h"],call_func_args("height",["normalized"])),
%~                          for_target_iter_body( "p",
%~                            call_func_args("range",[1,"h"]),
%~                            body_stmts( [ assign_targets_value( ["offsetted"],
%~                                            call_func_args( "shift", [
%~                                              "normalized",
%~                                              tuple_elts([unary_op_operand(us_ub_token(-),"p"),0])])),
%~                                          assign_targets_value( ["pruned"],
%~                                            call_func_args( "frozenset", [
%~                                              set_comp_elt_generators(
%~                                                 tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                                 [ comprehension_target_iter_ifs(
%~                                                      tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                                      "offsetted",
%~                                                      [ compare_ops_left_comparators(gt_e_token(>=),"i",0)])])])),
%~                                          if_test_body(
%~                                             call_func_args(
%~                                                qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                                                ["normalized"]),
%~                                             body_stmts([return_value("p")]))])),
%~                          return_value("h")])))



compile_block(PF,ra(0),_RET, 
  qualified_identifier_identifiers([Prune,boxed_attribute_value(Sub)]),
  [Sub,Prune]).

compile_block(PF,ra(0),_RET,  call_func_args([A,B],Stuff), call_func_args(A,[B|Stuff])).


compile_block(PF,Pass,RET,assign_targets_value(Str,Compound),Code):- \+ var(Str), Str=[Str1],!,
  compile_block(PF,Pass,RET,assign_targets_value(Str1,Compound),Code),!.

compile_block(PF,ra(0),_RET,assign_targets_value(tuple_elts([]),tuple_elts([])),[]):-!.
compile_block(PF,ra(0),RET,assign_targets_value(tuple_elts([E1|List1]),tuple_elts([E2|List2])),Code):-
  length(List1,Len),length(List2,Len),
  compile_block(PF,ra(0),RET,assign_targets_value(E1,E2),Code1),
  compile_block(PF,ra(0),RET,assign_targets_value(tuple_elts(List1),tuple_elts(List2)),Code2),
  into_codeL([Code1,Code2],Code).

compile_block(PF,ra(0),_RET,assign_targets_value(tuple_elts([Str1,Str2]),StrT),Code):-
  str_var(Str1),str_var(Str2),str_var(StrT),into_codeL(from_tuple(StrT,Str1,Str2),Code),!.

compile_block(PF,ra(0),_RET,assign_targets_value(StrT,tuple_elts([Str1,Str2])),Code):-
  str_var(Str1),str_var(Str2),str_var(StrT),into_codeL(into_tuple(Str1,Str2,StrT),Code).

compile_block(PF,ra(0),_RET,assign_targets_value(Str1,StrT),Code):-
  str_var(Str1),str_var(StrT),!,must_det_ll((into_callL('='(Str1,StrT),Code))).

compile_block(PF,ra(0),_RET,assign_targets_value(T1,Cmpd),Code):- compile_expression(T1,Cmpd,Code12), into_codeL(Code12,Code),!.

compile_block(PF,PASS,RET,Cmpd,OUT):- cmpd(Cmpd), 
  once(( PF\=assign_targets_value(_,_), 
   PF\=op_call(_),
  within_arg(Cmpd,Arg),needs_eval(Arg),Arg\=op_call(_),Arg\=assign_targets_value(_,_),
  py_gensym('ARG_',VarName),string_to_var(VarName,VAR),
  subst001C(Cmpd,Arg,VAR,NewCmpd))),
  in_cmt(pp(yellow,replacing_with_var(Arg))),
  NewCmpd\==Cmpd,!,
  compile_block(PF,PASS,RET,[assign_targets_value(VAR,Arg),NewCmpd],OUT).

compile_block(PF,ra(2),_RET, assign_targets_value(Var,Compound),Code):- !, append_last_term(Var,Compound,Code).


compile_block_simple(call_func_args(Isinstance, [A_01, Int]),integer(A_01)):- Int==int,Isinstance=isinstance.

compile_block(PF,ra(0),RET,call(from_tuple(LOC_02,I,J)),CODE):-
  compile_block(PF,ra(0),RET,from_tuple(LOC_02,I,J),CODE).
compile_block(PF,ra(0),RET,call(to_tuple(LOC_02,I,J)),CODE):-
  compile_block(PF,ra(0),RET,to_tuple(LOC_02,I,J),CODE).


within_arg(Cmpd,E):- arg(_,Cmpd,Arg),within_arg_e(Arg,E).
within_arg_e(Arg,E):- \+ is_list(Arg),!,E=Arg.
within_arg_e(Arg,E):- member(E,Arg).

compile_block(PF,_,_LASTHEADARG,NewAST,NewAST):- \+ compound(NewAST),!.  
compile_block(PF,PASS,RET,AST,Code):- 
  compound_name_arguments(AST,F,ASTARGS),
  maplist(compile_block([AST|PF],PASS,RET),ASTARGS,ECode),!,
  compound_name_arguments(Code,F,ECode).

needs_eval(Str):- cmpd(Str), \+ is_block_stmnt(Str), \+ str_var(Str).
cmpd(Cmpd):- compound(Cmpd), \+ is_ftVar(Cmpd), \+ is_list(Cmpd).

is_block_stmnt(Str):- \+ compound(Str),!,fail.
is_block_stmnt(List):- is_list(List),!,fail.
is_block_stmnt(exit_proc(_)).
is_block_stmnt(comment(_)).
is_block_stmnt(throw(_)).
is_block_stmnt(assign_targets_value(_,_)).
is_block_stmnt(Cmpd):- functor(Cmpd,F,A),is_block_stmnt_fa(F,A),!.
is_block_stmnt(Cmpd):- functor(Cmpd,_,A),arg(A,Cmpd,LastArg),\+ is_list(LastArg),is_ftVar(LastArg),!.
is_block_stmnt_fa(op_call,2).
is_block_stmnt_fa(add_token,3).
is_block_stmnt_fa(_,4).
is_block_stmnt_fa(subscript_value_slice,3).
is_block_stmnt_fa(F,_):- is_block_stmnt_f(F),!.
is_block_stmnt_fa(isinstance,2).
is_block_stmnt_f(call).
is_block_stmnt_f(E):- atom_contains(E,token).
is_block_stmnt_f(=).
is_block_stmnt_f(;).
is_block_stmnt_f(testif).
is_block_stmnt_f(->).
is_block_stmnt_f(*->).
is_block_stmnt_f(bool_op_values).
is_block_stmnt_f(append_last_term).


into_funct(Str,Atom):- string(Str),must_det_ll(atom_string(Atom,Str)),!.
into_funct(Var,Atom):- str_var(Var),!,must_det_ll( Atom=Var),!.
into_funct(call_func_args(Var),Atom):- into_funct(Var,Atom).
into_funct(Var,Var).


compile_parameter(Head,Args,Replaces,List,Code):- is_list(List),!,maplist(compile_parameter(Head),List,Args,Replaces,CodeE),into_codeL(CodeE,Code),!.

compile_parameter(_Head,VAR,Replaces,SName,[true]):- string(SName), string_to_var(SName,VAR),
  Replaces = SName,!.

compile_parameter(_Head,VAR,_Replaces,VAR,[true]):- str_var(VAR), \+ string(VAR),!.

compile_parameter(_Head,'$VAR'(Name),SName,argument_name(SName),[true]):- atom_string(DName,SName),
   py_gensym(DName,GenSym),
   upcase_atom(GenSym,Name).
compile_parameter(_Head,'$VAR'(Name),OSName,argument_type(SName),[willBeType('$VAR'(Name),DName)]):-
   (var(OSName)->SName= OSName;true),
   atom_string(DName,SName), atom_string(ODName,OSName), 
   py_gensym(ODName,GenSym), upcase_atom(GenSym,Name).

% needs_eval
compile_parameter(_Head,'$VAR'(Name),SName,Invoke,[assign_targets_value(['$VAR'(Name)],Invoke)]):- 
  py_gensym('ARG_',Name),atom_string(Name,SName),!.

py_gensym(Atom,Sym):- atom_concat(_,'_',Atom), flag(py_gensym,X,X+1), atomic_list_concat([Atom,X],'0',Sym).
py_gensym(Atom,Sym):- flag(py_gensym,X,X+1), atomic_list_concat([Atom,X],'_0',Sym).

should_append_var(subscript_value_slice).
should_append_var(starred_value).
should_append_var(tuple_elts).
should_append_var(call_func_args).

compile_expression(VAR,Value,call(VAR=Value)):- is_ftVar(Value),!.
compile_expression(VAR,expr_value(Value),C):- compile_expression(VAR,Value,C).
compile_expression(VAR,boxed_bool_literal_value(bool_value(True),_),VAR=True).

compile_expression(VAR,call_func(Str),make_new(Str,VAR)).

compile_expression(VAR,bin_op_left_right(Funct,OBJ,ARGS),Code12):-
   compute_call((Funct),[OBJ,ARGS],[VAR],Code12),!.
compile_expression(VAR,call_func_args(Funct,OBJARGS),Code12):-
   compute_call(call_func_args(Funct),OBJARGS,[VAR],Code12),!.
compile_expression(VAR,subscript_value_slice(Funct,OBJARGS),Code12):-
   compute_call(call_func_args(subscript_value_slice),[Funct,OBJARGS],[VAR],Code12),!.
compile_expression(VAR,tuple_elts(OBJARGS),Code12):-
   compute_call(call_func_args(tuple_elts),OBJARGS,[VAR],Code12),!.
compile_expression(VAR,starred_value(OBJARGS),Code12):-
   compute_call(call_func_args(starred_value),[OBJARGS],[VAR],Code12),!.

compile_expression(_V1,assign_targets_value(VAR,OBJARGS),Code12):- compile_expression(VAR,OBJARGS,Code12).
compile_expression(VAR, if_exp_test_body_orelse(Test,TrueBody,ElseBody), 
  (testif(Test)->assign_targets_value([VAR],TrueBody);assign_targets_value([VAR],ElseBody))).
compile_expression(VAR,CALL,Code12):- cmpd(CALL), compound_name_arguments(CALL,F,ARGS),
   should_append_var(F), compute_call(F,ARGS,[VAR],Code12),!.
compile_expression(VAR,CALL,Code12):- append_last_term(VAR,CALL,Code12),!.

%append_last_term(VAR,Value,OUT):- is_ftVar(Value),!,OUT = assign_targets_value(VAR,Value)
append_last_term(Str,G,Code):- should_been_var(Str),string_to_var(Str,VAR),!,append_last_term(VAR,G,Code).
append_last_term(VAR,Value,OUT):- is_ftVar(Value),!,OUT = (append_last_term(VAR,Value,Code),call(Code)).
append_last_term(VAR,(_,G),Code):- !,append_last_term(VAR,(_,G),Code).
append_last_term(VAR,G,Code):- cmpd(G),functor(G,F,_),should_append_var(F), append_term(G,VAR,Code).
append_last_term(VAR,G,Code):- is_list(G),last(G,Last),!,append_last_term(VAR,Last,Code).
append_last_term(VAR,G,Code):- cmpd(G),append_term(G,VAR,Code).


compute_call(FuncNameStr,IN,OUT,Code123):- 
  append(Left,[NeedsEval|Right],IN),
  needs_eval(NeedsEval),!,
  py_gensym('ARG_',VarName),string_to_var(VarName,VAR),
  append(Left,[VAR|Right],NewIN),
  compute_call(FuncNameStr,NewIN,OUT,Code12),
  into_codeL([assign_targets_value([VAR],NeedsEval),Code12],Code123).

compute_call(FuncNameStr,IN,OUT,Code12):-
   must_det_ll((into_funct(FuncNameStr,AName),
     maplist(compile_parameter(body),HEADARGS,Replace,IN,Code1),
     subst_2L(Replace,HEADARGS,OUT,NewOUT),
     maplist(compile_parameter(body,RET,_ReplaceOut),NewOUT,Code2),
     append([AName|HEADARGS],[RET],HEADL),
     into_callL(HEADL,HEAD),
     into_codeL([Code1,Code2,HEAD],Code12))).

/*
compute_call(NeedsEval,IN,OUT,ASTNode,NewAST,RET,Code123):-  needs_eval(NeedsEval),!,
  py_gensym('cc_',VarName),string_to_var(VarName,VAR),
  compute_call(VAR,IN,OUT,ASTNode,NewAST,RET,Code12),
  into_codeL([assign_targets_value([VAR],NeedsEval),Code12],Code123).

compute_call(FuncNameStr,IN,OUT,ASTNode,NewAST,RET,Code12):-
   must_det_ll((into_funct(FuncNameStr,AName),
     maplist(compile_parameter(body),HEADARGS,Replace,IN,Code1),
     subst_2L(Replace,HEADARGS,ASTNode+OUT,NewAST+NewOUT),
     maplist(compile_parameter(body,RET,_ReplaceOut),NewOUT,Code2),
     append([AName|HEADARGS],[RET],HEADL),
     into_callL(HEADL,HEAD),
     into_codeL([Code1,Code2,HEAD],Code12))).
*/



/*
while_test_body
subscript_value_slice
set_comp_elt_generators
qualified_identifier_identifiers

if_test_body
if_exp_test_body_orelse
if_exp_test_body
generator_exp_elt_generators
for_target_iter_body
expr_value
comprehension_target_iter
comprehension_ifs_target_iter
boxed_attribute_value
bool_op_values
aug_assign_op_target_value
set_elts
tuple_elts
lambda_args_body
generator_exp_elt_generators
comprehension_ifs_target_iter
call_func_args_keywords
%keyword_value(qualified_identifier_identifiers([CONTAINER_01,boxed_attribute_value("count")]))
*/

  /*

bool01_t(1,true).
bool01_t(0,false).

zeven_t(Z,Truth) :- Z mod 2 #= 0 #<==> B, bool01_t(B,Truth).

%zodd_t(Z,Truth) :- Z mod 2 #= 1 #<==> B, bool01_t(B,Truth).
zodd_t(Z,Truth)  :- Z mod 2 #=         B, bool01_t(B,Truth). % tweaked
?- tpartition(zeven_t,[1,2,3,4,5,6,7],Es,Os).
Es = [2,4,6], Os = [1,3,5,7].
?- tpartition(zodd_t ,[1,2,3,4,5,6,7],Os,Es). % argument order differs
Es = [2,4,6], Os = [1,3,5,7].


  vsplit(Grid,N,Tuple):-
    len(Grid,GridLen),
    H #= GridLen // N,
    subscript_value_slice(Grid,0,GridSlice0),
    len(GridSlice0,GridSlice0Len),
    W #= GridSlice0Len,
    Step1 #= GridLen % N,
    into_integer(Step1 != 0, Offset),    
    for_range(I,N,      
       HS #= H * I + I * Offset,
       crop(Grid, [HS, 0], [H, W], G),
       create_tupple(T,Tuple)). 

    

    alias_name_node( "vsplit",
      function_type_body(
         function_type_arguments_returns(
            [ argument_name("grid"),
              argument_name("n")],
            [argument_type("Tuple")]),
         block_statements(/*a1*/ [ expr_value(string_value(' split grid vertically ')),
                                   assign_targets_value( [ [ "h","w"]], [
                                     bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),"n"),
                                     call_func_args("len",[subscript_value_slice("grid",0)])]),
                                   assign_targets_value( ["offset"],
                                     compare_ops_left_comparators( ops([not_eq_token('!=')]),
                                       bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),"n"),
                                       comparators([0]))),
                                   return_value(/*a1*/ call_func_args( "tuple", [
                                                         generator_exp_elt_generators(
                                                            call_func_args( "crop", [
                                                              "grid",
                                                              [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"h","i"),bin_op_left_right(mult_token(*),"i","offset")),
                                                                0],
                                                              ["h","w"]]),
                                                            [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))])))]),

*/
:- dynamic(michod_solved/2).
:- dynamic(py_consts/2).

:- dynamic(michod_dsl/4).
print_sols:- make, T = michod_solved(TestID,Prog),
   %forall(T,(print(fun(TestID)),maplist(writeln,Prog),nl)).
   %forall(T,(michod_solved_cor(TestID,Prog,Clause),portray_clause(user_output,Clause))).
   forall(T,(michod_solved_cor(TestID,Prog,Clause),print(Clause))).
print_dsl:- T = michod_dsl(Fun,Params,_Cmt,Src),
   forall(T,(print(fun(Fun,Params)),maplist(writeln,Src),nl)).
read_python(In):- 
 repeat, 
  read_line_to_string(In,S), 
   (S==end_of_file -> ! ; once(process_line(In,S)),fail).

%py_substs(N,V):- between(0,60,X),atom_concat(x,X,N),V='$VAR'(X).
py_substs(N,V):- between(0,60,X),atom_concat(x,X,N),atom_concat('X',X,XX),V='$VAR'(XX).
py_substs('T',true).
py_substs('F',false).
py_substs(N,V):- py_consts(N,V).
py_substs(C,(X,Y)):- fff(C,X,Y).
py_substs('IntegerTuple','Point').
fff('DOWN',1, 0).
fff('RIGHT',0, 1).
fff('UP',-1, 0).
fff('LEFT',0, -1).
fff('ORIGIN',0, 0).
fff('UNITY',1, 1).
fff('NEG_UNITY',-1, -1).
fff('UP_RIGHT',-1, 1).
fff('DOWN_LEFT',1, -1).
fff('ZERO_BY_TWO',0, 2).
fff('TWO_BY_ZERO',2, 0).
fff('TWO_BY_TWO',2, 2).
fff('THREE_BY_THREE',3, 3).

%michod_solved_cor(TestID,Prog):- michod_solved(TestID,Prog).
michod_solved_cor(TestID,Prog,Clause):- 
  michod_solved(TestID,Prog),
  Head=..[solve,TestID,'I','O'],
  
  maplist(predifiy,Prog,Preds),
  findall(N-V,py_substs(N,V),Substs),
  subst_1L(['O'-'$VAR'('OUT'),'I'-'$VAR'('IN')|Substs],(Head:-Preds),Clause).

predifiy(X=Y,Preds):- Y=..[F|A],append(A,[X],O),
   get_typearray(F,AT),add_argType(O,AT,OO),Preds=..[f,F|OO].

get_typearray(F,[]):- var(F),!. 
get_typearray(F,AT):- michod_dsl(F,AT,_,_),!.
get_typearray(_F,[]).

add_argType([O|OO],[AT|ATT],[ATO|ATOO]):-
  add_1argType(O,AT,ATO),!,
  add_argType(OO,ATT,ATOO).
add_argType(A,_,A).

add_1argType(O,(A:'Boolean'),A=O):-!.
add_1argType(O,('returns':T),O:out(T)):-!.
add_1argType(O,(A:'Boolean,'),A=O):-!.
add_1argType(O,'Callable',O):- downcase_atom(O,O).
add_1argType(O,function:'Callable',O):- downcase_atom(O,O).
add_1argType(O,A:T,OT):- atom(A),atom(T),upcase_atom(A,UA),upcase_atom(T,UA),!,add_1argType(O,T,OT).
add_1argType(O,AT,O:AT):-!.
add_1argType(A,_,A).

read_solve(In,Prog):- read_line_to_string(In,Str),read_solve(In,Str,Prog).
read_solve(In,Str,[Var=Fun|Prog]):- str_between([s(Var),' = ',s(Fun)],Str),read_solve(In,Prog).
read_solve(_,Str,[]):- str_between(['return O',_],Str).

process_line(In,end_of_file):- close(In).
process_line(_, Str):- str_between(['import',_],Str),!.
process_line(_, Str):- str_between(['from',_],Str),!.
%process_line(_, Str):- str_between([s(N),'=',s(V)],Str),asserta_new(py_const(N,V)),print(N=V).
process_line(_, Str):- str_between([''],Str),!.
process_line(In,Str):- str_between(['def solve_',s(TestID),'(I)',_],Str),!,
  read_solve(In,Prog),
  assertz_new(michod_solved(TestID,Prog)).

process_line(In,Str):- str_between(['def ',s(Fun),'(',_],Str),
 read_params(In,Params),!,
 %read_cmt(In,Cmt),!, 
  read_until_blank(In,Src),
  writeln(Fun+Params=Src),
  assertz_new(michod_dsl(Fun,Params,_Cmt,Src)).

ltrim(Str,Rest):- string_concat('\t',Rest,Str). ltrim(Str,Rest):- string_concat(' ',Rest,Str). ltrim(Str,Str).
rtrim(Str,Rest):- string_concat(Rest,'\t',Str). rtrim(Str,Rest):- string_concat(Rest,' ',Str). rtrim(Str,Str).

trim(Str,StrO):- Str==end_of_file,!,StrO="".
trim(Str,StrO):- ltrim(Str,StrM),rtrim(StrM,StrO).

str_between([Const|Template],Str):- ground(Const),string_concat(Const,Rest,Str),!,str_between(Template,Rest).
str_between([Const|Template],Str):- ground(Const),trim(Str,Rest),Rest\==Str,!,str_between([Const|Template],Rest).
str_between([s(Var)],Str):- var(Var),!,read_funct(Str,Var),!.
str_between([Var],Str):- var(Var),Str=Var,!.

str_between([s(Var),Const|Template],Str):- var(Var),ground(Const),sub_string(Str,Left,_,_,Const),
  sub_string(Str,0,Left,_,SVar),read_funct(SVar,Var),!,string_concat(SVar,Rest,Str),str_between([Const|Template],Rest).
str_between([SVar,Const|Template],Str):- var(SVar),ground(Const),sub_string(Str,Left,_,_,Const),
  sub_string(Str,0,Left,_,SVar),string_concat(SVar,Rest,Str),str_between([Const|Template],Rest).
%str_between(Template,Str):- trim(Str,S),S\==Str,!,str_between(Template,Str).

%writeln(params:Str),
read_params(In,Prog):- read_line_to_string(In,Str), trim(Str,StrS),read_params(In,StrS,Prog).

read_params(_,Str,['returns':Type]):- str_between([') ->',s(Type),':',_],Str),!.
read_params(In,Str,[Name:Type|Prog]):- str_between([s(Name),':',s(Type),','],Str),!,read_params(In,Prog).
read_params(In,Str,[Name:Type|Prog]):- str_between([s(Name),':',s(Type)],Str),!,read_params(In,Prog).
read_cmt(In,Cmt):- read_line_to_string(In,Str), str_between(['"""',SType,'"""'],Str),trim(SType,Cmt).

read_until_blank(In,Src):- read_line_to_string(In,Str),trim(Str,T),
 (T=="" -> Src=[] ; (read_until_blank(In,More),Src=[Str|More])).

read_funct(SFun,Fun):- trim(SFun,SS),
 trim_comma(SS,S),catch((atom_to_term(S,Fun,Vs),maplist(call,Vs)),_,atom_string(Fun,S)).

trim_comma(SS,S):- atom_concat(S,',',SS),!.
trim_comma(S,S).




end_of_file.



