
:- use_module(library(lists)).

make_ignore_feats :-
	make_ignore_feats('ignore_feats.pl').

make_ignore_feats(File) :-
	open(File, write, S),
	all_feats(Feats),
	write_ignore_feats(Feats, S),
	close(S).

all_feats(Feats) :-
	findall(Feat, feat(Feat), Feats).

feat(Feat) :-
	feature_instantiation_schedule(Schedule),
	member(FeatGroup, Schedule),
	member(Feat, FeatGroup).

write_ignore_feats([], _S).
write_ignore_feats([F | R], S) :-
	format(S, '~N~q~n', [ignore_feature(F)]),
	write_ignore_feats(R, S).

feature_instantiation_schedule([[toptype,stype,politeness_pos,interjection_type],[wh,vform,inv,whmoved,operator_wrapped,gap,subcat,vp_vform,vp_passivised,passivised,passive_subj],[postposition,conj,prenumber,adjpos,advpos],[case],[takes_post_mods,post_mod_type,can_be_premod,takes_advp],[gapsin,gapsout],[nform,subj_nform],[pronoun],[agr,relagr],[num_type,time_type,can_be_np],[sem_pp_type,pp_sem_pp_type,sem_p_type,takes_gap_mod],[takes_loc_pp],[takes_from_pp],[takes_to_pp],[takes_with_pp],[takes_duration_pp],[takes_date_pp],[takes_time_pp],[takes_attrib_pp],[takes_about_pp],[takes_cost_pp],[takes_passive_by_pp],[def,subj_def,obj_def,indobj_def],[adv_type],[takes_adv_type],[det_type],[takes_det_type],[sem_n_type],[subj_sem_n_type],[obj_sem_n_type],[indobj_sem_n_type],[rel_sem_n_type],[n_pre_mod_type],[n_post_mod_type],[n_of_mod_type],[syn_type],[subj_syn_type],[obj_syn_type],[indobj_syn_type],[sem_n_type_1,def_1,syn_type_1]]).

