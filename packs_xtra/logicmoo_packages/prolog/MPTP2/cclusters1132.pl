v3_lattice3(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v1_abcmiz_0(A) :- l1_orders_2(A),v13_struct_0(A, 1).
v1_lattice3(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v2_abcmiz_0(A).
v2_yellow_0(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v2_abcmiz_0(A).
v1_abcmiz_0(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v2_abcmiz_0(A).
v2_abcmiz_0(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v1_abcmiz_0(A).
 ~ (v1_facirc_1(A))  :-  ~ (v11_struct_0(B)) ,v1_instalg1(B),l1_msualg_1(B),v1_relat_1(C), ~ (v3_relat_1(C)) ,v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, C)))),true.
v6_trees_3(A) :-  ~ (v11_struct_0(B)) ,v1_instalg1(B),l1_msualg_1(B),v1_relat_1(C), ~ (v3_relat_1(C)) ,v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_finseq_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, C)))),true.
v2_funct_1(A) :- m1_subset_1(A, k4_abcmiz_1),true.
 ~ (v2_struct_0(A))  :- l1_msualg_1(A),v1_instalg1(A),v1_abcmiz_1(A).
 ~ (v11_struct_0(A))  :- l1_msualg_1(A),v1_instalg1(A),v1_abcmiz_1(A).
 ~ (v7_abcmiz_1(A, B))  :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_abcmiz_1(A, B, k13_abcmiz_1(B)),v6_abcmiz_1(A, B).
v8_abcmiz_1(A, B) :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_abcmiz_1(A, B, k13_abcmiz_1(B)),v6_abcmiz_1(A, B).
 ~ (v6_abcmiz_1(A, B))  :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_abcmiz_1(A, B, k13_abcmiz_1(B)),v7_abcmiz_1(A, B).
v8_abcmiz_1(A, B) :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_abcmiz_1(A, B, k13_abcmiz_1(B)),v7_abcmiz_1(A, B).
v7_abcmiz_1(A, B) :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_abcmiz_1(A, B, k13_abcmiz_1(B)), ~ (v6_abcmiz_1(A, B)) ,v8_abcmiz_1(A, B).
v6_abcmiz_1(A, B) :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_abcmiz_1(A, B, k13_abcmiz_1(B)), ~ (v7_abcmiz_1(A, B)) ,v8_abcmiz_1(A, B).
v1_facirc_1(A) :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m3_abcmiz_1(A, B),true.
v13_abcmiz_1(A, B) :- l1_msualg_1(B),v1_relat_1(A),v2_relat_1(A),v4_relat_1(A, u1_struct_0(B)),v1_funct_1(A),v1_partfun1(A, u1_struct_0(B)).
v13_abcmiz_1(A, B) :- v12_abcmiz_1(B),l1_msualg_1(B),v1_relat_1(A),v4_relat_1(A, u1_struct_0(B)),v1_funct_1(A),v1_partfun1(A, u1_struct_0(B)).
v15_abcmiz_1(A, B) :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_abcmiz_1, k34_abcmiz_1(B)))),v1_xboole_0(A),v1_funct_1(A).
v1_abcmiz_a(A) :- v1_instalg1(B),v1_abcmiz_1(B),v1_abcmiz_a(B),l1_msualg_1(B),m1_instalg1(A, B),v1_abcmiz_1(A).
v1_facirc_1(A) :- m1_subset_1(A, k2_abcmiz_1),true.
v5_abcmiz_1(A, B, k28_abcmiz_1(B)) :- v1_instalg1(B),v1_abcmiz_1(B),v3_abcmiz_1(B),l1_msualg_1(B),m1_subset_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, k28_abcmiz_1(B))))),v2_abcmiz_a(A, B).
v2_abcmiz_1(A, B) :- v1_instalg1(B),v1_abcmiz_1(B),l1_msualg_1(B),m1_subset_1(A, u4_struct_0(B)),v4_abcmiz_a(A, B).
v12_abcmiz_1(A) :- l1_msualg_1(A), ~ (v11_struct_0(A)) ,v1_instalg1(A),v3_abcmiz_a(A).
v3_abcmiz_1(A) :- l1_msualg_1(A),v1_instalg1(A),v1_abcmiz_1(A),v3_abcmiz_a(A).
v1_zfmisc_1(A) :- true,v2_setfam_1(A).
 ~ (v1_xboole_0(A))  :- true,v1_int_1(A), ~ (v1_abian(A)) .
v5_ordinal1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A).
v3_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k1_numbers),v5_ordinal1(A),v1_funct_1(A),v1_finset_1(A).
v6_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k5_numbers),v5_ordinal1(A),v1_funct_1(A),v1_finset_1(A).
v1_afinsq_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A).
v1_afinsq_1(A) :- true,v1_relat_1(A),v5_ordinal1(A),v1_funct_1(A),v1_finset_1(A).
v4_relat_1(A, k5_numbers) :- true,v1_relat_1(A),v5_ordinal1(A),v1_funct_1(A),v1_finset_1(A).
v5_ordinal1(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A).
v5_ordinal1(A) :- m1_subset_1(A, k8_afinsq_1(B)),true.
v1_finset_1(A) :- m1_subset_1(A, k8_afinsq_1(B)),true.
v6_membered(A) :- true,v7_ordinal1(A).
v4_partfun3(A) :- true,v1_relat_1(A),v6_valued_0(A).
v1_afvect0(A) :- l1_analoaf(A), ~ (v2_struct_0(A)) ,v2_tdgroup(A).
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,v1_instalg1(B),l1_msualg_1(B),m2_algspec1(A, B),true.
 ~ (v11_struct_0(A))  :-  ~ (v11_struct_0(B)) ,v1_instalg1(B),l1_msualg_1(B),m2_algspec1(A, B),true.
v4_algstr_0(A, B) :- l1_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v2_algstr_0(A, B),v3_algstr_0(A, B).
v2_algstr_0(A, B) :- l1_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v4_algstr_0(A, B).
v3_algstr_0(A, B) :- l1_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v4_algstr_0(A, B).
v7_algstr_0(A) :- l1_algstr_0(A),v5_algstr_0(A),v6_algstr_0(A).
v5_algstr_0(A) :- l1_algstr_0(A),v7_algstr_0(A).
v6_algstr_0(A) :- l1_algstr_0(A),v7_algstr_0(A).
v2_algstr_0(A, B) :- v5_algstr_0(B),l1_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v3_algstr_0(A, B) :- v6_algstr_0(B),l1_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v11_algstr_0(A, B) :- l2_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v9_algstr_0(A, B),v10_algstr_0(A, B).
v9_algstr_0(A, B) :- l2_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v11_algstr_0(A, B).
v10_algstr_0(A, B) :- l2_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v11_algstr_0(A, B).
v14_algstr_0(A) :- l2_algstr_0(A),v12_algstr_0(A),v13_algstr_0(A).
v12_algstr_0(A) :- l2_algstr_0(A),v14_algstr_0(A).
v13_algstr_0(A) :- l2_algstr_0(A),v14_algstr_0(A).
v9_algstr_0(A, B) :- v12_algstr_0(B),l2_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v10_algstr_0(A, B) :- v13_algstr_0(B),l2_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v18_algstr_0(A, B) :- l3_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v16_algstr_0(A, B),v17_algstr_0(A, B).
v16_algstr_0(A, B) :- l3_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v18_algstr_0(A, B).
v17_algstr_0(A, B) :- l3_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v18_algstr_0(A, B).
v21_algstr_0(A) :- l3_algstr_0(A),v19_algstr_0(A),v20_algstr_0(A).
v19_algstr_0(A) :- l3_algstr_0(A),v21_algstr_0(A).
v20_algstr_0(A) :- l3_algstr_0(A),v21_algstr_0(A).
v16_algstr_0(A, B) :- v19_algstr_0(B),l3_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v17_algstr_0(A, B) :- v20_algstr_0(B),l3_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v25_algstr_0(A, B) :- l4_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v23_algstr_0(A, B),v24_algstr_0(A, B).
v23_algstr_0(A, B) :- l4_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v25_algstr_0(A, B).
v24_algstr_0(A, B) :- l4_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),v25_algstr_0(A, B).
v28_algstr_0(A) :- l4_algstr_0(A),v26_algstr_0(A),v27_algstr_0(A).
v26_algstr_0(A) :- l4_algstr_0(A),v28_algstr_0(A).
v27_algstr_0(A) :- l4_algstr_0(A),v28_algstr_0(A).
v23_algstr_0(A, B) :- v26_algstr_0(B),l4_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v24_algstr_0(A, B) :- v27_algstr_0(B),l4_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v32_algstr_0(A) :- l5_algstr_0(A),v30_algstr_0(A),v31_algstr_0(A).
v30_algstr_0(A) :- l5_algstr_0(A),v32_algstr_0(A).
v31_algstr_0(A) :- l5_algstr_0(A),v32_algstr_0(A).
v35_algstr_0(A) :- l5_algstr_0(A),v33_algstr_0(A),v34_algstr_0(A).
v33_algstr_0(A) :- l5_algstr_0(A),v35_algstr_0(A).
v34_algstr_0(A) :- l5_algstr_0(A),v35_algstr_0(A).
v5_algstr_0(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v4_algstr_1(A).
v6_algstr_0(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v4_algstr_1(A).
v2_algstr_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v4_algstr_1(A).
v3_algstr_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v4_algstr_1(A).
v4_algstr_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v5_algstr_0(A),v6_algstr_0(A),v2_algstr_1(A),v3_algstr_1(A).
v2_algstr_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v4_algstr_1(A).
v1_algstr_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v3_rlvect_1(A),v4_rlvect_1(A).
v4_algstr_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v3_rlvect_1(A),v4_rlvect_1(A).
v23_algstr_0(A, B) :-  ~ (v2_struct_0(B)) ,v21_algstr_0(B),v5_algstr_1(B),l4_algstr_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v32_algstr_0(A) :- l5_algstr_0(A), ~ (v2_struct_0(A)) ,v7_algstr_1(A).
v6_algstr_1(A) :- l5_algstr_0(A), ~ (v2_struct_0(A)) ,v7_algstr_1(A).
v13_algstr_0(A) :- l2_algstr_0(A),v13_struct_0(A, 1).
v2_rlvect_1(A) :- l2_algstr_0(A),v13_struct_0(A, 1).
v3_rlvect_1(A) :- l2_algstr_0(A),v13_struct_0(A, 1).
v4_rlvect_1(A) :- l2_algstr_0(A),v13_struct_0(A, 1).
v1_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v7_struct_0(A).
v4_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v7_struct_0(A).
v2_group_1(A) :- l3_algstr_0(A),v13_struct_0(A, 1).
v3_group_1(A) :- l3_algstr_0(A),v13_struct_0(A, 1).
v5_group_1(A) :- l3_algstr_0(A),v13_struct_0(A, 1).
v10_altcat_1(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v8_altcat_1(A),v9_altcat_1(A).
v8_altcat_1(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v10_altcat_1(A),v12_altcat_1(A).
v9_altcat_1(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v10_altcat_1(A),v12_altcat_1(A).
v13_altcat_1(A) :- l2_altcat_1(A),v13_struct_0(A, 1).
v2_altcat_1(A) :- l2_altcat_1(A),v13_altcat_1(A).
v1_altcat_2(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v12_altcat_1(A).
v2_altcat_1(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_2(A, B).
v11_altcat_1(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A).
v12_altcat_1(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v3_altcat_2(A, B).
v1_ami_wstd(A, B) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),l1_extpro_1(A, B), ~ (v2_struct_0(A)) ,v2_memstr_0(A, B),v2_ami_wstd(A, B).
 ~ (v2_extpro_1(A, B, C))  :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v2_ami_wstd(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),v3_ami_wstd(A, B, C).
 ~ (v3_ami_wstd(A, B, C))  :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v2_ami_wstd(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),v2_extpro_1(A, B, C).
v5_ami_wstd(A, C, B) :-  ~ (v1_xboole_0(C)) ,v1_setfam_1(C), ~ (v2_struct_0(B)) ,v2_memstr_0(B, C),l1_extpro_1(B, C),v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(B)),v1_xboole_0(A),v1_funct_1(A),v1_finset_1(A).
v4_ami_wstd(A, C, B) :-  ~ (v1_xboole_0(C)) ,v1_setfam_1(C), ~ (v2_struct_0(B)) ,v2_memstr_0(B, C),v2_ami_wstd(B, C),l1_extpro_1(B, C),v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(B)), ~ (v1_xboole_0(A)) ,v1_funct_1(A),v1_finset_1(A),v5_amistd_1(A, C, B),v5_ami_wstd(A, C, B).
v9_compos_1(A, k4_amistd_1(B)) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),m1_subset_1(A, u1_compos_1(k4_amistd_1(B))),true.
 ~ (v2_extpro_1(A, B, C))  :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),v4_amistd_1(A, B, C).
 ~ (v4_amistd_1(A, B, C))  :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),v2_extpro_1(A, B, C).
v6_amistd_1(A, C, B) :-  ~ (v1_xboole_0(C)) ,v1_setfam_1(C), ~ (v2_struct_0(B)) ,v2_memstr_0(B, C),v3_amistd_1(B, C),l1_extpro_1(B, C),v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(B)), ~ (v1_xboole_0(A)) ,v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v5_amistd_1(A, C, B).
v1_amistd_2(A, B, C) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),v3_compos_1(C), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v2_amistd_2(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),true.
v9_compos_1(A, B) :-  ~ (v1_xboole_0(C)) ,v1_setfam_1(C),v3_compos_1(B), ~ (v2_struct_0(B)) ,v2_memstr_0(B, C),v2_amistd_2(B, C),l1_extpro_1(B, C),m1_subset_1(A, u1_compos_1(B)),v2_extpro_1(A, C, B).
v9_compos_1(A, B) :-  ~ (v1_xboole_0(C)) ,v1_setfam_1(C),v3_compos_1(B), ~ (v2_struct_0(B)) ,v2_memstr_0(B, C),v2_amistd_2(B, C),l1_extpro_1(B, C),m1_subset_1(A, u1_compos_1(B)),v4_amistd_1(A, C, B).
v3_amistd_2(A, B, C) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),v3_compos_1(C),v6_compos_1(C),v7_compos_1(C),v8_compos_1(C), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v3_extpro_1(C, B),v2_amistd_2(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),v4_amistd_1(A, B, C).
v3_amistd_2(A, B, C) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),v3_compos_1(C),v6_compos_1(C),v7_compos_1(C),v8_compos_1(C), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v3_extpro_1(C, B),v2_amistd_2(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),v2_extpro_1(A, B, C).
v3_amistd_2(A, B, C) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),v3_compos_1(C),v6_compos_1(C),v7_compos_1(C),v8_compos_1(C), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v3_extpro_1(C, B),v4_amistd_2(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),true.
v3_amistd_2(A, B, C) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),v3_compos_1(C),v6_compos_1(C),v7_compos_1(C),v8_compos_1(C), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v3_extpro_1(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),v1_amistd_5(A, B, C).
v1_amistd_5(A, B, C) :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),v3_compos_1(C),v6_compos_1(C),v7_compos_1(C),v8_compos_1(C), ~ (v2_struct_0(C)) ,v2_memstr_0(C, B),v3_extpro_1(C, B),v2_amistd_5(C, B),l1_extpro_1(C, B),m1_subset_1(A, u1_compos_1(C)),true.
v2_analmetr(A) :- l1_analmetr(A), ~ (v2_struct_0(A)) ,v3_analmetr(A).
 ~ (v7_struct_0(A))  :- l1_rlvect_1(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v1_anproj_2(A).
v1_relat_1(A) :-  ~ (v1_xboole_0(B)) ,m1_finseq_1(B, k5_numbers), ~ (v1_xboole_0(C)) ,v1_freealg(C),m1_subset_1(A, u1_struct_0(k7_freealg(B, C))),true.
v1_funct_1(A) :-  ~ (v1_xboole_0(B)) ,m1_finseq_1(B, k5_numbers), ~ (v1_xboole_0(C)) ,v1_freealg(C),m1_subset_1(A, u1_struct_0(k7_freealg(B, C))),true.
v3_trees_2(A) :-  ~ (v1_xboole_0(B)) ,m1_finseq_1(B, k5_numbers), ~ (v1_xboole_0(C)) ,v1_freealg(C),m1_subset_1(A, u1_struct_0(k7_freealg(B, C))),true.
v6_trees_3(A) :-  ~ (v1_xboole_0(B)) ,m1_finseq_1(B, k5_numbers), ~ (v1_xboole_0(C)) ,v1_freealg(C),m1_finseq_1(A, u1_struct_0(k7_freealg(B, C))),true.
 ~ (v10_aofa_000(A))  :- l1_unialg_1(A), ~ (v2_struct_0(A)) ,v2_unialg_1(A),v3_unialg_1(A),v4_unialg_1(A),v3_freealg(A),v3_aofa_000(A),v4_aofa_000(A),v5_aofa_000(A),v6_aofa_000(A).
v11_aofa_000(A) :- l1_unialg_1(A), ~ (v2_struct_0(A)) ,v2_unialg_1(A),v3_unialg_1(A),v4_unialg_1(A),v3_freealg(A),v3_aofa_000(A),v4_aofa_000(A),v5_aofa_000(A),v6_aofa_000(A).
v13_aofa_000(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_unialg_1(B),v3_unialg_1(B),v4_unialg_1(B),v3_aofa_000(B),v4_aofa_000(B),v5_aofa_000(B),v6_aofa_000(B),l1_unialg_1(B), ~ (v1_xboole_0(C)) ,m1_subset_1(D, k1_zfmisc_1(C)),m1_aofa_000(A, B, C, D),true.
v14_aofa_000(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_unialg_1(B),v3_unialg_1(B),v4_unialg_1(B),v3_aofa_000(B),v4_aofa_000(B),v5_aofa_000(B),v6_aofa_000(B),l1_unialg_1(B), ~ (v1_xboole_0(C)) ,m1_subset_1(D, k1_zfmisc_1(C)),m1_aofa_000(A, B, C, D),true.
v1_aofa_i00(A, k7_freealg(k19_aofa_000, k26_aofa_i00), k5_numbers, k1_aofa_i00(k9_funct_2(k5_numbers, k4_numbers), k6_numbers, k6_numbers)) :- m4_aofa_i00(A),true.
v1_aofa_i00(A, k7_freealg(k19_aofa_000, k26_aofa_i00), B, C) :-  ~ (v1_xboole_0(B)) ,v4_card_3(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(B, k4_numbers))),v1_funct_1(D),v2_funct_1(D),v1_funct_2(D, B, k1_card_1(B)),v2_funct_2(D, k1_card_1(B)),m1_subset_1(D, k1_zfmisc_1(k2_zfmisc_1(B, k1_card_1(B)))),m6_aofa_i00(A, B, C, D),true.
v1_pre_poly(A) :-  ~ (v1_xboole_0(B)) , ~ (v1_xboole_0(D)) ,m1_subset_1(C, k5_numbers),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k4_finseq_2(C, D)))),v1_funct_1(A),v1_funct_2(A, B, k4_finseq_2(C, D)).
v1_finset_1(A) :- v1_finset_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),true.
v8_relat_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v6_armstrng(A, B).
v3_armstrng(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v6_armstrng(A, B).
v4_armstrng(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v6_armstrng(A, B).
v5_armstrng(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v6_armstrng(A, B).
v6_armstrng(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v8_relat_2(A),v3_armstrng(A, B),v4_armstrng(A, B),v5_armstrng(A, B).
v7_armstrng(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v3_armstrng(A, B),v4_armstrng(A, B).
v3_armstrng(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v8_relat_2(A),v7_armstrng(A, B).
v4_armstrng(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v8_relat_2(A),v7_armstrng(A, B).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k9_setfam_1(B), k9_setfam_1(B)))),v3_armstrng(A, B).
v1_relat_1(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k2_arrow(B)),true.
v1_relat_1(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k3_arrow(B)),true.
v7_ordinal1(A) :- m1_subset_1(A, k5_arytm_3),v3_ordinal1(A).
v4_asympt_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v3_asympt_0(A).
v2_asympt_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v4_asympt_0(A).
v5_asympt_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v4_asympt_0(A).
v4_asympt_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v2_asympt_0(A),v5_asympt_0(A).
v13_bcialg_1(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v20_bcialg_1(A).
v20_bcialg_1(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v13_bcialg_1(A).
v17_bcialg_1(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v20_bcialg_1(A).
v16_bcialg_1(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v13_bcialg_1(A).
v16_bcialg_1(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v19_bcialg_1(A).
v16_bcialg_1(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v15_bcialg_1(A).
v16_bcialg_1(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v20_bcialg_1(A).
v1_partfun1(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,v3_bcialg_1(B),v4_bcialg_1(B),v5_bcialg_1(B),v7_bcialg_1(B),l2_bcialg_1(B),m2_bcialg_1(C, B),m4_bcialg_2(A, B, C),true.
v3_relat_2(A) :-  ~ (v2_struct_0(B)) ,v3_bcialg_1(B),v4_bcialg_1(B),v5_bcialg_1(B),v7_bcialg_1(B),l2_bcialg_1(B),m2_bcialg_1(C, B),m4_bcialg_2(A, B, C),true.
v8_relat_2(A) :-  ~ (v2_struct_0(B)) ,v3_bcialg_1(B),v4_bcialg_1(B),v5_bcialg_1(B),v7_bcialg_1(B),l2_bcialg_1(B),m2_bcialg_1(C, B),m4_bcialg_2(A, B, C),true.
v1_bcialg_3(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v8_bcialg_1(A),v11_bcialg_3(A).
v10_bcialg_3(A) :- l2_bcialg_1(A), ~ (v2_struct_0(A)) ,v3_bcialg_1(A),v4_bcialg_1(A),v5_bcialg_1(A),v7_bcialg_1(A),v8_bcialg_1(A),v11_bcialg_3(A).
v5_algstr_0(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v6_algstr_0(A),v2_rlvect_1(A).
v6_algstr_0(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v5_algstr_0(A),v2_rlvect_1(A).
v6_algstr_0(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v3_rlvect_1(A),v4_rlvect_1(A).
v1_xboole_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(k1_xboole_0, k1_xboole_0), k1_xboole_0))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(k1_xboole_0, k1_xboole_0), k1_xboole_0).
v1_binop_1(A, k1_xboole_0) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(k1_xboole_0, k1_xboole_0), k1_xboole_0))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(k1_xboole_0, k1_xboole_0), k1_xboole_0).
v2_binop_1(A, k1_xboole_0) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(k1_xboole_0, k1_xboole_0), k1_xboole_0))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(k1_xboole_0, k1_xboole_0), k1_xboole_0).
v1_rat_1(A) :- m1_subset_1(A, k3_numbers),true.
v1_trees_2(A) :- true, ~ (v1_xboole_0(A)) ,v1_trees_1(A),v1_bintree1(A).
v2_bintree1(A) :-  ~ (v2_struct_0(B)) ,v1_dtconstr(B),v2_dtconstr(B),v3_bintree1(B),l1_lang1(B),m1_subset_1(A, k4_dtconstr(B)),true.
v1_bintree1(A) :- true, ~ (v1_xboole_0(A)) ,v1_trees_1(A),v1_bintree2(A).
v5_pre_topc(A, k5_topmetr, B) :- v1_borsuk_2(B),l1_pre_topc(B),m1_subset_1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_borsuk_2(A, B, C, D),true.
v1_connsp_1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_borsuk_2(A).
v6_pre_topc(A) :- l1_pre_topc(A),v2_struct_0(A).
v1_compts_1(A) :- l1_pre_topc(A),v2_struct_0(A),v2_pre_topc(A).
 ~ (v1_zfmisc_1(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal2(A).
v1_xreal_0(A) :- m1_subset_1(A, k3_numbers),true.
 ~ (v1_xboole_0(A))  :- true,v1_xreal_0(A), ~ (v1_rat_1(A)) .
v1_xreal_0(A) :- v3_topmetr(B),l1_struct_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v3_topmetr(A) :- v3_topmetr(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v1_ideal_1(A, B) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_c0sp1(A, B).
v1_c0sp1(A, B) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_c0sp1(A, B).
v2_c0sp1(A, B) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_ideal_1(A, B),v1_c0sp1(A, B).
v2_c0sp1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v2_funcsdom(B),v3_group_1(B),v5_group_1(B),v1_vectsp_1(B),v3_vectsp_1(B),l1_funcsdom(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_c0sp1(A, B).
v2_cantor_1(A, B) :- l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_tops_2(A, B),v1_cantor_1(A, B).
v3_ordinal1(A) :- true,v1_card_1(A).
v1_card_1(A) :- true,v1_xboole_0(A).
v1_card_1(A) :- true,v7_ordinal1(A).
v1_finset_1(A) :- m1_subset_1(A, k4_ordinal1),true.
v1_finset_1(A) :- true,v7_ordinal1(A).
v7_ordinal1(A) :- true,v3_ordinal1(A),v1_finset_1(A).
v1_xboole_0(A) :- true,v3_card_1(A, k1_xboole_0).
v3_card_1(A, k1_xboole_0) :- true,v1_xboole_0(A).
 ~ (v1_xboole_0(A))  :- true,v3_card_1(A, 1).
v1_zfmisc_1(A) :- true,v3_card_1(A, 1).
v3_card_1(A, 1) :- true, ~ (v1_xboole_0(A)) ,v1_zfmisc_1(A).
v4_ordinal1(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A).
v1_card_3(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v2_relat_1(A) :- v1_setfam_1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C).
v4_funct_1(A) :- true,v3_card_3(A).
v2_card_3(A) :- true,v3_card_3(A).
 ~ (v1_finset_1(A))  :- true,v5_card_3(A).
v4_card_3(A) :- true,v5_card_3(A).
v5_card_3(A) :- true, ~ (v1_finset_1(A)) ,v4_card_3(A).
v4_card_3(A) :- true,v1_finset_1(A).
v4_card_3(A) :- v4_card_3(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v2_card_3(A) :- v2_card_3(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v5_funct_1(A, B) :- v1_relat_1(B),v2_relat_1(B),v1_funct_1(B),m1_subset_1(A, k4_card_3(B)),true.
v4_relat_1(A, B) :- v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),m1_subset_1(A, k4_card_3(C)),true.
v5_funct_1(A, B) :- v1_relat_1(B),v1_funct_1(B),m1_subset_1(A, k8_card_3(B)),true.
v4_relat_1(A, B) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),m1_subset_1(A, k8_card_3(C)),true.
v1_partfun1(A, B) :- v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m1_subset_1(A, k4_card_3(C)),true.
v1_partfun1(A, B) :- v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m1_subset_1(A, k4_card_3(C)),true.
 ~ (v1_xboole_0(A))  :- true, ~ (v1_finset_1(A)) .
 ~ (v2_card_fil(A, B))  :-  ~ (v1_finset_1(B)) ,m1_card_fil(A, B),v1_card_fil(A, B),v3_card_fil(A, B).
v2_card_1(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A),v4_card_fil(A).
v1_card_5(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A),v4_card_fil(A).
v2_card_1(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A),v5_card_fil(A).
v1_card_5(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A),v6_card_fil(A).
v5_card_fil(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A),v6_card_fil(A).
v4_card_fil(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A),v6_card_fil(A).
v1_card_5(A) :- true, ~ (v1_finset_1(A)) ,v1_card_1(A), ~ (v2_card_1(A)) .
v2_finset_1(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v4_ordinal1(A) :- true,v3_ordinal1(A), ~ (v1_finset_1(A)) ,v1_card_1(A).
 ~ (v1_finset_1(A))  :- true,v3_ordinal1(A),v4_ordinal1(A), ~ (v1_xboole_0(A)) .
 ~ (v4_card_3(A))  :- true, ~ (v1_finset_1(A)) ,v1_card_1(A), ~ (v2_card_1(A)) .
v1_cat_5(A) :- l1_cat_1(A), ~ (v2_struct_0(A)) , ~ (v11_struct_0(A)) ,v2_cat_1(A),v3_cat_5(A).
v3_cat_5(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_cat_1(B),v3_cat_5(B),l1_cat_1(B),m3_cat_2(A, B),true.
v5_ordinal1(A) :- m1_catalan2(C, B),m1_subset_1(A, C),true.
v5_relat_1(A, B) :- m1_catalan2(C, B),m1_subset_1(A, C),true.
v1_finset_1(A) :- m1_catalan2(C, B),m1_subset_1(A, C),true.
 ~ (v1_catalg_1(A, B))  :-  ~ (v2_struct_0(B)) ,l1_msualg_1(B),l3_msualg_1(A, B),v4_msualg_1(A, B).
 ~ (v11_struct_0(A))  :- l1_msualg_1(A), ~ (v2_struct_0(A)) ,v1_instalg1(A),v2_catalg_1(A).
v2_catalg_1(A) :- m1_catalg_1(A, B),true.
 ~ (v2_struct_0(A))  :-  ~ (v1_xboole_0(B)) ,m1_catalg_1(A, B),true.
v4_funct_1(A) :- true,v3_matrix_2(A).
v2_relat_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k2_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k2_numbers),v1_cfdiff_1(A).
v1_comseq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k2_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k2_numbers),v1_cfdiff_1(A).
v2_cgames_1(A) :- v3_ordinal1(B),m1_subset_1(A, k2_cgames_1(B)),true.
v1_cgames_1(A) :- l1_cgames_1(A),v2_cgames_1(A).
v7_ordinal1(A) :- v1_relat_1(B),v1_funct_1(B), ~ (v1_xboole_0(B)) ,v1_finseq_1(B),m1_subset_1(A, k4_finseq_1(B)),true.
 ~ (v1_xboole_0(A))  :- v1_relat_1(B),v1_funct_1(B), ~ (v1_xboole_0(B)) ,v1_finseq_1(B),m1_subset_1(A, k4_finseq_1(B)),true.
v2_cgames_1(A) :- v2_cgames_1(B),m1_subset_1(A, k10_cgames_1(B)),true.
v5_cgames_1(A) :- true,v2_cgames_1(A),v7_cgames_1(A).
v6_cgames_1(A) :- true,v2_cgames_1(A),v7_cgames_1(A).
v7_cgames_1(A) :- true,v2_cgames_1(A),v5_cgames_1(A),v6_cgames_1(A).
v6_cgames_1(A) :- true,v2_cgames_1(A),v10_cgames_1(A).
 ~ (v7_cgames_1(A))  :- true,v2_cgames_1(A),v10_cgames_1(A).
v10_cgames_1(A) :- true,v2_cgames_1(A),v6_cgames_1(A), ~ (v7_cgames_1(A)) .
v5_cgames_1(A) :- true,v2_cgames_1(A),v9_cgames_1(A).
 ~ (v7_cgames_1(A))  :- true,v2_cgames_1(A),v9_cgames_1(A).
v9_cgames_1(A) :- true,v2_cgames_1(A),v5_cgames_1(A), ~ (v7_cgames_1(A)) .
 ~ (v5_cgames_1(A))  :- true,v2_cgames_1(A),v8_cgames_1(A).
 ~ (v6_cgames_1(A))  :- true,v2_cgames_1(A),v8_cgames_1(A).
v8_cgames_1(A) :- true,v2_cgames_1(A), ~ (v5_cgames_1(A)) , ~ (v6_cgames_1(A)) .
v2_chord(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v4_glib_000(A).
v6_chord(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v2_chord(A).
v6_chord(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v6_chord(B),m2_glib_000(A, B, C, k21_glib_000(B, C)),true.
 ~ (v1_xboole_0(A))  :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_000(B),m3_chord(A, B),true.
v2_circcmb3(A, B) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_msafree2(B),l1_msualg_1(B),l3_msualg_1(A, B),v4_msualg_1(A, B),v4_msafree2(A, B),v3_circcmb3(A, B).
 ~ (v2_struct_0(A))  :- l1_msualg_1(A),v4_circcmb3(A).
v8_struct_0(A) :- l1_msualg_1(A),v4_circcmb3(A).
 ~ (v11_struct_0(A))  :- l1_msualg_1(A),v4_circcmb3(A).
v1_msualg_1(A) :- l1_msualg_1(A),v4_circcmb3(A).
v1_circcomb(A) :- l1_msualg_1(A),v4_circcmb3(A).
v2_circcomb(A) :- l1_msualg_1(A),v4_circcmb3(A).
v5_circcomb(A) :- l1_msualg_1(A), ~ (v2_struct_0(A)) ,v4_circcmb3(A).
v3_msualg_1(A, B) :- v4_circcmb3(B),l1_msualg_1(B),l3_msualg_1(A, B),v4_msafree2(A, B),v5_circcmb3(A, B).
v4_msualg_1(A, B) :- v4_circcmb3(B),l1_msualg_1(B),l3_msualg_1(A, B),v4_msafree2(A, B),v5_circcmb3(A, B).
v3_circcmb3(A, B) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_msafree2(B),l1_msualg_1(B),l3_msualg_1(A, B),v4_msualg_1(A, B),v4_msafree2(A, B),v5_circcmb3(A, B).
v4_msualg_1(A, B) :-  ~ (v1_xboole_0(C)) ,v1_finset_1(C),m1_circcmb3(B, C),m2_circcmb3(A, C, B),true.
v4_circcomb(A, B) :-  ~ (v1_xboole_0(C)) ,v1_finset_1(C),m1_circcmb3(B, C),m2_circcmb3(A, C, B),true.
v3_circcmb3(A, B) :-  ~ (v1_xboole_0(C)) ,v1_finset_1(C),v8_struct_0(B),m1_circcmb3(B, C),m2_circcmb3(A, C, B),true.
 ~ (v2_facirc_1(A))  :-  ~ (v2_facirc_1(B)) ,m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_facirc_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v6_valued_0(A).
v5_circcomb(A) :- l1_msualg_1(A), ~ (v2_struct_0(A)) ,v3_circcomb(A).
v2_msafree2(A) :- l1_msualg_1(A), ~ (v2_struct_0(A)) ,v1_circcomb(A).
v4_msualg_1(A, B) :-  ~ (v2_struct_0(B)) ,l1_msualg_1(B),l3_msualg_1(A, B),v6_circcomb(A, B).
v4_msafree2(A, B) :-  ~ (v2_struct_0(B)) ,l1_msualg_1(B),l3_msualg_1(A, B),v6_circcomb(A, B).
v1_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)), ~ (v1_xboole_0(D)) ,m1_subset_1(D, k1_zfmisc_1(k1_msaterm(B, C))),m1_subset_1(A, u1_struct_0(k1_circtrm1(B, C, D))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)), ~ (v1_xboole_0(D)) ,m1_subset_1(D, k1_zfmisc_1(k1_msaterm(B, C))),m1_subset_1(A, u1_struct_0(k1_circtrm1(B, C, D))),true.
v1_finset_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)), ~ (v1_xboole_0(D)) ,m1_subset_1(D, k1_zfmisc_1(k1_msaterm(B, C))),m1_subset_1(A, u1_struct_0(k1_circtrm1(B, C, D))),true.
v3_trees_2(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)), ~ (v1_xboole_0(D)) ,m1_subset_1(D, k1_zfmisc_1(k1_msaterm(B, C))),m1_subset_1(A, u1_struct_0(k1_circtrm1(B, C, D))),true.
v1_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m3_msaterm(D, B, C),m1_subset_1(A, u4_struct_0(k1_circtrm1(B, C, D))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m3_msaterm(D, B, C),m1_subset_1(A, u4_struct_0(k1_circtrm1(B, C, D))),true.
v1_finset_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m3_msaterm(D, B, C),m1_subset_1(A, u4_struct_0(k1_circtrm1(B, C, D))),true.
v3_trees_2(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m3_msaterm(D, B, C),m1_subset_1(A, u4_struct_0(k1_circtrm1(B, C, D))),true.
v1_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_msafree2(B),v5_msafree2(B),l1_msualg_1(B),v4_msualg_1(C, B),v4_msafree2(C, B),l3_msualg_1(C, B),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k5_msafree2(B, C)), D)),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_msafree2(B),v5_msafree2(B),l1_msualg_1(B),v4_msualg_1(C, B),v4_msafree2(C, B),l3_msualg_1(C, B),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k5_msafree2(B, C)), D)),true.
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_msafree2(B),v5_msafree2(B),l1_msualg_1(B),v4_msualg_1(C, B),v4_msafree2(C, B),l3_msualg_1(C, B),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k5_msafree2(B, C)), D)),true.
v1_finset_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_msafree2(B),v5_msafree2(B),l1_msualg_1(B),v4_msualg_1(C, B),v4_msafree2(C, B),l3_msualg_1(C, B),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k5_msafree2(B, C)), D)),true.
v3_trees_2(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v2_msafree2(B),v5_msafree2(B),l1_msualg_1(B),v4_msualg_1(C, B),v4_msafree2(C, B),l3_msualg_1(C, B),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k5_msafree2(B, C)), D)),true.
v1_classes1(A) :- true,v2_classes1(A).
v1_ordinal1(A) :- true,v1_classes2(A).
v2_classes1(A) :- true,v1_classes2(A).
v1_classes2(A) :- true,v1_ordinal1(A),v2_classes1(A).
v1_relat_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_clvect_1(B),v3_clvect_1(B),v4_clvect_1(B),v5_clvect_1(B),v8_clvect_1(B),l2_clvect_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_clvect_1(C),v3_clvect_1(C),v4_clvect_1(C),v5_clvect_1(C),v8_clvect_1(C),l2_clvect_1(C),m1_subset_1(A, u1_struct_0(k9_clopban1(B, C))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_clvect_1(B),v3_clvect_1(B),v4_clvect_1(B),v5_clvect_1(B),v8_clvect_1(B),l2_clvect_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_clvect_1(C),v3_clvect_1(C),v4_clvect_1(C),v5_clvect_1(C),v8_clvect_1(C),l2_clvect_1(C),m1_subset_1(A, u1_struct_0(k9_clopban1(B, C))),true.
v1_relat_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_clvect_1(B),v3_clvect_1(B),v4_clvect_1(B),v5_clvect_1(B),v8_clvect_1(B),l2_clvect_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_clvect_1(C),v3_clvect_1(C),v4_clvect_1(C),v5_clvect_1(C),v8_clvect_1(C),l2_clvect_1(C),m1_subset_1(A, u1_struct_0(k14_clopban1(B, C))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_clvect_1(B),v3_clvect_1(B),v4_clvect_1(B),v5_clvect_1(B),v8_clvect_1(B),l2_clvect_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_clvect_1(C),v3_clvect_1(C),v4_clvect_1(C),v5_clvect_1(C),v8_clvect_1(C),l2_clvect_1(C),m1_subset_1(A, u1_struct_0(k14_clopban1(B, C))),true.
v3_clopban1(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v2_cfuncdom(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_clopban2(A).
v2_vectsp_1(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v2_cfuncdom(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_clopban2(A).
v6_vectsp_1(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v2_cfuncdom(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_clopban2(A).
v2_clopban2(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v2_cfuncdom(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_clopban2(A).
v3_clopban2(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v2_cfuncdom(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_clopban2(A).
v4_clopban2(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v2_cfuncdom(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_clopban2(A).
v5_clopban2(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v3_clopban1(A),v2_cfuncdom(A),v3_group_1(A),v1_vectsp_1(A),v2_vectsp_1(A),v3_vectsp_1(A),v6_vectsp_1(A),v2_clopban2(A),v3_clopban2(A),v4_clopban2(A).
v9_clvect_1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_clvect_1(B),v3_clvect_1(B),v4_clvect_1(B),v5_clvect_1(B),v8_clvect_1(B),l2_clvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, k5_numbers, u1_struct_0(B)),v1_clopban3(A, B).
v1_clopban3(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_clvect_1(B),v3_clvect_1(B),v4_clvect_1(B),v5_clvect_1(B),v8_clvect_1(B),v3_clopban1(B),l2_clvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, k5_numbers, u1_struct_0(B)),v2_clopban3(A, B).
v4_vectsp_1(A) :- l1_clopban2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v2_clvect_1(A),v3_clvect_1(A),v4_clvect_1(A),v5_clvect_1(A),v8_clvect_1(A),v2_cfuncdom(A),v5_clopban2(A).
v2_closure1(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m2_pboole(A, B, k5_mssubfam(B, C), k5_mssubfam(B, C)),v4_closure1(A, B, C).
v6_closure1(A, B) :- l1_struct_0(B),l1_closure1(A, B),v7_closure1(A, B).
v8_closure1(A, B) :- l1_struct_0(B),l1_closure1(A, B),v9_closure1(A, B).
v10_closure1(A, B) :- l1_struct_0(B),l1_closure1(A, B),v9_closure1(A, B).
v11_closure1(A, B) :- l1_struct_0(B),l1_closure1(A, B),v7_closure1(A, B).
v1_closure2(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m1_subset_1(A, k1_zfmisc_1(k1_closure2(B, C))),v2_closure2(A, B, C).
v3_closure2(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m1_subset_1(A, k1_zfmisc_1(k1_closure2(B, C))),v4_closure2(A, B, C).
v5_closure2(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m1_subset_1(A, k1_zfmisc_1(k1_closure2(B, C))),v4_closure2(A, B, C).
 ~ (v1_xboole_0(A))  :- v1_relat_1(B),v4_relat_1(B, C),v1_funct_1(B),v1_partfun1(B, C),m1_subset_1(A, k1_zfmisc_1(k1_closure2(C, B))),v5_closure2(A, C, B).
v6_closure2(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m1_subset_1(A, k1_zfmisc_1(k1_closure2(B, C))),v2_closure2(A, B, C).
 ~ (v1_xboole_0(A))  :- v1_relat_1(B),v4_relat_1(B, C),v1_funct_1(B),v1_partfun1(B, C),m1_subset_1(A, k1_zfmisc_1(k1_closure2(C, B))),v6_closure2(A, C, B).
v8_closure2(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k6_closure2(B, C), k6_closure2(B, C)))),v1_funct_1(A),v1_funct_2(A, k6_closure2(B, C), k6_closure2(B, C)),v10_closure2(A, B, C).
v12_closure2(A, B) :- l1_struct_0(B),l1_closure2(A, B),v13_closure2(A, B).
v14_closure2(A, B) :- l1_struct_0(B),l1_closure2(A, B),v15_closure2(A, B).
v16_closure2(A, B) :- l1_struct_0(B),l1_closure2(A, B),v15_closure2(A, B).
v17_closure2(A, B) :- l1_struct_0(B),l1_closure2(A, B),v13_closure2(A, B).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k3_coh_sp(B)),true.
v1_classes1(A) :- m1_subset_1(A, k3_coh_sp(B)),true.
v1_coh_sp(A) :- m1_subset_1(A, k3_coh_sp(B)),true.
 ~ (v1_xboole_0(A))  :- true,v1_cohsp_1(A).
 ~ (v1_xboole_0(A))  :- true,v2_cohsp_1(A).
v2_finsub_1(A) :- true,v1_classes1(A).
v3_cohsp_1(A) :- true, ~ (v1_xboole_0(A)) ,v1_classes1(A),v1_coh_sp(A).
v6_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_cohsp_1(A).
v5_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v4_cohsp_1(A).
v5_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v8_cohsp_1(A).
v7_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v9_cohsp_1(A).
v8_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v9_cohsp_1(A).
v4_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v10_cohsp_1(A).
v9_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v10_cohsp_1(A).
v8_cohsp_1(A) :- v3_cohsp_1(B),v1_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B),v5_cohsp_1(A).
v9_cohsp_1(A) :- v2_finsub_1(B),v1_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B),v7_cohsp_1(A),v8_cohsp_1(A).
v10_cohsp_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v4_cohsp_1(A),v9_cohsp_1(A).
v2_compact1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v5_compact1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v6_waybel_3(A).
v6_waybel_3(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v9_pre_topc(A),v5_compact1(A).
v4_compact1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_compact1(A).
v3_compact1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v4_compact1(A).
v5_compact1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_compact1(A).
v3_compact1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v8_pre_topc(A),v5_compact1(A).
v5_compact1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_compts_1(A).
v5_compact1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A).
v6_compact1(A, B, C) :- v2_pre_topc(B),l1_pre_topc(B),v2_pre_topc(C),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v7_compact1(A, B, C).
v2_compl_sp(A, B) :- l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_compl_sp(A, B) :- l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v1_xcmplx_0(A) :- m1_subset_1(A, u1_struct_0(k1_complfld)),true.
v5_compos_1(A, B) :- l1_compos_1(B), ~ (v1_xboole_0(A)) ,v1_zfmisc_1(A),v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(B)),v1_funct_1(A),v1_finset_1(A).
v2_compos_1(A, B) :- l1_compos_1(B),v1_xboole_0(A),v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(B)),v1_funct_1(A),v1_finset_1(A).
v5_compos_1(A, B) :- l1_compos_1(B), ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(B)),v1_funct_1(A),v1_finset_1(A),v2_compos_1(A, B).
v2_compts_1(A, B) :- l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v4_pre_topc(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v8_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_compts_1(A, B).
v1_compts_1(A) :- l1_pre_topc(A),v8_struct_0(A),v2_pre_topc(A).
v2_compts_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_finset_1(A).
v8_pre_topc(A) :- l1_pre_topc(A),v2_struct_0(A).
v8_pre_topc(A) :- v8_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v4_funct_1(A) :- m1_rfunct_3(A, B, C), ~ (v1_xboole_0(A)) .
v2_margrel1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v4_relat_1(A, k3_finseq_2(k5_numbers)) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k3_finseq_2(k5_numbers), k5_numbers))),v1_funct_1(A).
v6_valued_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k3_finseq_2(k5_numbers), k5_numbers))),v1_funct_1(A).
v2_comput_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k3_finseq_2(k5_numbers), k5_numbers))),v1_funct_1(A),v3_margrel1(A, k5_numbers).
v3_comput_1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v2_margrel1(A) :- m1_subset_1(A, k2_comput_1(B)),true.
v2_margrel1(A) :-  ~ (v1_xboole_0(B)) , ~ (v1_xboole_0(C)) ,m1_subset_1(C, k1_zfmisc_1(k2_comput_1(B))),m1_subset_1(A, C),true.
v4_relat_1(A, k3_finseq_2(k5_numbers)) :- m1_subset_1(A, k2_comput_1(k5_numbers)),true.
v6_valued_0(A) :- m1_subset_1(A, k2_comput_1(k5_numbers)),true.
v2_margrel1(A) :- m1_subset_1(A, k2_comput_1(k5_numbers)),true.
v2_margrel1(A) :- m1_subset_1(A, k9_comput_1),true.
v1_relat_1(A) :- true,v7_comput_1(A).
v1_funct_1(A) :- true,v7_comput_1(A).
v4_relat_1(A, k3_finseq_2(k5_numbers)) :- true,v1_relat_1(A),v7_comput_1(A).
v6_valued_0(A) :- true,v1_relat_1(A),v7_comput_1(A).
v2_margrel1(A) :- true,v1_relat_1(A),v7_comput_1(A).
v7_comput_1(A) :- m1_subset_1(A, k9_comput_1),true.
v2_margrel1(A) :- m1_subset_1(A, k9_comput_1),true.
v3_margrel1(A, k5_numbers) :- m1_subset_1(A, k9_comput_1),true.
v3_margrel1(A, k5_numbers) :- m1_subset_1(A, k2_comput_1(k5_numbers)),v7_comput_1(A).
v2_comput_1(A) :- true,v1_relat_1(A),v4_relat_1(A, k3_finseq_2(k5_numbers)),v1_funct_1(A),v7_comput_1(A).
 ~ (v1_xboole_0(A))  :- true,v1_relat_1(A),v1_funct_1(A),v2_margrel1(A),v8_comput_1(A, 1).
 ~ (v1_xboole_0(A))  :- true,v1_relat_1(A),v1_funct_1(A),v2_margrel1(A),v8_comput_1(A, 2).
 ~ (v1_xboole_0(A))  :- true,v1_relat_1(A),v1_funct_1(A),v2_margrel1(A),v8_comput_1(A, 3).
v3_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k2_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k2_numbers),v1_comseq_2(A).
 ~ (v1_comseq_2(A))  :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k2_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k2_numbers), ~ (v3_seq_2(A)) .
v4_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v1_series_1(A).
v1_series_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v2_series_1(A).
v1_comseq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k2_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k2_numbers),v1_comseq_3(A).
v1_comseq_3(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k2_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k2_numbers),v2_comseq_3(A).
v5_conlat_1(A, B) :- v2_struct_0(B),v11_struct_0(B),l5_struct_0(B),l2_conlat_1(A, B),true.
v6_conlat_1(A, B) :- v1_conlat_1(B),l5_struct_0(B),l2_conlat_1(A, B),true.
v2_connsp_1(A, B) :- l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_connsp_1(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_connsp_1(A, B).
v4_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_connsp_1(A, B).
v2_connsp_1(A, B) :- l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v1_convex4(A, B) :-  ~ (v2_struct_0(B)) ,l1_clvect_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v6_ordinal1(A) :- true,v1_xboole_0(A).
v6_ordinal1(A) :- v6_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v1_dilworth(A, B) :- l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_zfmisc_1(A).
v6_orders_2(A, B) :- v3_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_dilworth(A, B).
v2_dilworth(A, B) :- l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_zfmisc_1(A).
v3_dilworth(A) :- l1_orders_2(A),v8_struct_0(A).
v1_finset_1(A) :- v3_dilworth(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_dilworth(A, B).
v4_dilworth(A) :- l1_orders_2(A),v8_struct_0(A).
v1_finset_1(A) :- v4_dilworth(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_dilworth(A, B).
v8_struct_0(A) :- l1_orders_2(A),v3_dilworth(A),v4_dilworth(A).
v5_dilworth(A, B) :- v2_struct_0(B),l1_orders_2(B),m1_eqrel_1(A, u1_struct_0(B)),v1_xboole_0(A).
v6_dilworth(A, B) :- v2_struct_0(B),l1_orders_2(B),m1_eqrel_1(A, u1_struct_0(B)),true.
v1_dilworth(A, B) :-  ~ (v2_struct_0(B)) ,v4_dilworth(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v7_dilworth(A, B).
v7_dilworth(A, B) :-  ~ (v2_struct_0(B)) ,v4_orders_2(B),v5_orders_2(B),v4_dilworth(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_card_1(A, 1).
v6_trees_3(A) :-  ~ (v1_xboole_0(B)) ,m1_trees_3(C, B),m1_finseq_1(A, C),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,m1_dynkin(A, B),true.
 ~ (v1_xboole_0(A))  :- m1_finseq_1(A, k1_numbers),v1_matrprob(A).
v4_partfun3(A) :- m1_finseq_1(A, k1_numbers),v1_matrprob(A).
 ~ (v3_relat_1(A))  :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v3_matrprob(A).
 ~ (v3_relat_1(A))  :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v4_matrprob(A).
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,m1_eqrel_1(A, B),true.
v1_setfam_1(A) :- m1_eqrel_1(A, B),true.
v3_card_1(A, B) :- v7_ordinal1(B),m1_subset_1(A, k1_euclid(B)),true.
v1_finseq_1(A) :- v7_ordinal1(B),m1_subset_1(A, u1_struct_0(k15_euclid(B))),true.
v3_valued_0(A) :- v7_ordinal1(B),m1_subset_1(A, u1_struct_0(k15_euclid(B))),true.
v3_card_1(A, B) :- v7_ordinal1(B),m1_subset_1(A, u1_struct_0(k15_euclid(B))),true.
v1_euclid_7(A) :- true,v1_xboole_0(A).
v2_euclid_7(A) :- true,v1_xboole_0(A).
v1_euclid_7(A) :- true,v3_euclid_7(A).
v2_euclid_7(A) :- true,v3_euclid_7(A).
v3_euclid_7(A) :- true,v1_euclid_7(A),v2_euclid_7(A).
v3_euclid_7(A) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k1_euclid(B))),v5_euclid_7(A, B).
v4_euclid_7(A, B) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k1_euclid(B))),v5_euclid_7(A, B).
v5_euclid_7(A, B) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k1_euclid(B))),v3_euclid_7(A),v4_euclid_7(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k1_euclid(B))),v5_euclid_7(A, B).
v3_valued_0(A) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, u1_struct_0(k7_real_ns1(B))),true.
v3_valued_0(A) :- m1_subset_1(A, u1_struct_0(k10_funcsdom(B))),true.
v1_rlvect_3(A, k10_funcsdom(k2_finseq_1(B))) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k10_funcsdom(k2_finseq_1(B))))),v3_euclid_7(A).
v1_rlvect_3(A, k7_real_ns1(B)) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k7_real_ns1(B)))),v3_euclid_7(A).
v5_relat_1(A, k1_numbers) :- v7_ordinal1(B),m1_subset_1(A, u1_struct_0(k14_euclid(B))),true.
v3_card_1(A, B) :- v7_ordinal1(B),m1_subset_1(A, u1_struct_0(k14_euclid(B))),true.
v1_exchsort(A) :- true,v5_ordinal1(A),v1_relat_1(A),v1_funct_1(A).
v1_exchsort(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A).
v2_exchsort(A, k1_xboole_0) :- true,v5_ordinal1(A),v1_relat_1(A),v1_funct_1(A).
v2_exchsort(A, 1) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A).
v5_ordinal1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_exchsort(A),v2_exchsort(A, k1_xboole_0).
v5_exchsort(A, B) :- l1_orders_2(B),v1_xboole_0(A),v1_relat_1(A),v5_relat_1(A, u1_struct_0(B)),v1_funct_1(A),v1_exchsort(A).
v5_relat_1(A, B) :- m1_subset_1(A, k8_afinsq_1(B)),true.
v3_valued_0(A) :- v1_relat_1(B),v1_funct_1(B),v3_valued_0(B),v1_exchsort(B),m1_exchsort(A, B),true.
v5_ordinal1(A) :- v3_ordinal1(B), ~ (v1_xboole_0(C)) ,m1_subset_1(A, k9_funct_2(B, C)),true.
v3_valued_0(A) :- v3_membered(B), ~ (v1_xboole_0(B)) ,m1_subset_1(A, k9_funct_2(C, B)),true.
v5_relat_1(A, B) :- v1_relat_1(C),v5_relat_1(C, B),v1_funct_1(C),v1_exchsort(C),m1_exchsort(A, C),true.
v5_relat_1(A, B) :- m1_subset_1(C, k1_zfmisc_1(B)),m1_subset_1(A, k1_funct_2(D, C)),true.
 ~ (v1_xboole_0(A))  :- true,v1_facirc_1(A).
 ~ (v1_facirc_1(A))  :- true,v7_ordinal1(A).
 ~ (v2_facirc_1(A))  :- true,v1_xboole_0(A).
v1_xboole_0(A) :- true,v1_relat_1(A), ~ (v2_facirc_1(A)) .
v1_circcomb(A) :- l1_msualg_1(A),v11_struct_0(A).
v2_circcomb(A) :- l1_msualg_1(A),v11_struct_0(A).
v3_circcomb(A) :- l1_msualg_1(A),v11_struct_0(A).
 ~ (v1_facirc_1(A))  :- true,v1_xboole_0(A).
v3_facirc_1(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v1_fcont_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A).
v1_fcont_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v1_xboole_0(A).
v2_fcont_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v1_xboole_0(A).
v2_fcont_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A).
v1_fcont_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v2_fcont_1(A).
v2_rcomp_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v1_xboole_0(A).
v3_rcomp_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v1_xboole_0(A).
v1_setfam_1(A) :- v1_setfam_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v14_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v3_filter_0(A).
v11_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v3_filter_0(A).
v3_filter_0(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v11_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v3_filter_0(A).
v1_finseq_1(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v1_finset_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A).
v5_relat_1(A, B) :- m1_finseq_1(A, B),true.
v1_finseq_1(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v4_relat_1(A, k5_numbers) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A).
v2_finseq_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A).
v4_relat_1(A, k5_numbers) :- true,v1_relat_1(A),v1_funct_1(A),v2_finseq_1(A).
v3_finseq_1(A) :- true,v1_xboole_0(A).
v4_funct_1(A) :- true,v3_finseq_1(A).
v1_finseq_1(A) :- v3_finseq_1(B),m1_subset_1(A, B),true.
v3_finseq_1(A) :- v3_finseq_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v2_funct_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v3_valued_0(A),v7_valued_0(A),v1_finseq_1(A).
v3_card_1(A, B) :-  ~ (v1_xboole_0(C)) ,v7_ordinal1(B),m1_subset_1(A, k4_finseq_2(B, C)),true.
v4_funct_1(A) :- m1_finseq_2(A, B),true.
v1_finset_1(A) :- v1_finset_1(B),m1_eqrel_1(A, B),true.
v1_finset_1(A) :- true,v1_xboole_0(A).
v1_finset_1(A) :- v1_finset_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v1_finset_1(A) :- v1_finset_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C).
v1_finset_1(A) :- true,v1_zfmisc_1(A).
 ~ (v1_zfmisc_1(A))  :- true, ~ (v1_finset_1(A)) .
v1_finsub_1(A) :- true,v4_finsub_1(A).
v3_finsub_1(A) :- true,v4_finsub_1(A).
v4_finsub_1(A) :- true,v1_finsub_1(A),v3_finsub_1(A).
v1_finset_1(A) :- m1_subset_1(A, k5_finsub_1(B)),true.
v4_fin_topo(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_orders_2(A) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),l1_orders_2(B),m1_fintopo6(A, B), ~ (v2_struct_0(A)) .
v3_fintopo6(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v2_setfam_1(A) :- true,v1_xboole_0(A).
v2_setfam_1(A) :- v2_setfam_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_fomodel0(A, B) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(k4_finseq_2(1, B))),true.
v5_relat_1(A, B) :- m1_subset_1(C, k1_zfmisc_1(B)),v1_relat_1(A),v5_relat_1(A, C).
v1_partfun1(A, B) :-  ~ (v1_xboole_0(C)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_2(A, B, C).
v5_relat_1(A, B) :- m1_subset_1(A, k3_finseq_2(B)),true.
v3_relat_1(A) :- true,v1_relat_1(A),v5_relat_1(A, k1_tarski(k1_xboole_0)).
v5_relat_1(A, k1_tarski(k1_xboole_0)) :- true,v1_relat_1(A),v3_relat_1(A).
v7_ordinal1(A) :- true,v1_int_1(A), ~ (v3_xxreal_0(A)) .
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v7_ordinal1(B),v1_relat_1(A),v1_funct_1(A),v3_card_1(A, k2_xcmplx_0(B, k1_xboole_0)),v1_finseq_1(A).
v1_pre_poly(A) :-  ~ (v1_xboole_0(B)) ,v3_finseq_1(B),v1_relat_1(A),v5_relat_1(A, B),v1_funct_1(A).
v1_pre_poly(A) :- m1_subset_1(A, k9_funct_2(B, k3_finseq_2(C))),true.
 ~ (v1_xboole_0(A))  :- v7_ordinal1(B),v1_relat_1(A),v1_funct_1(A),v3_card_1(A, k2_xcmplx_0(B, 1)),v1_finseq_1(A).
v1_lexbfs(A) :- true,v3_finseq_1(A).
 ~ (v3_relat_1(A))  :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k2_fomodel0(k3_finseq_2(B), k6_subset_1(k3_finseq_2(B), k1_tarski(k1_xboole_0)))), ~ (v1_xboole_0(A)) .
v1_xboole_0(A) :- v1_xboole_0(B),m1_subset_1(A, k3_finseq_2(B)),true.
 ~ (v3_relat_1(A))  :- v1_setfam_1(B),v1_relat_1(A),v5_relat_1(A, B), ~ (v1_xboole_0(A)) .
v1_setfam_1(A) :- v1_setfam_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v7_ordinal1(A) :- true,v1_int_1(A),v2_xxreal_0(A).
v10_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v9_fomodel1(A, B).
v5_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v7_fomodel1(A, B).
v8_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v6_fomodel1(A, B).
v8_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v4_fomodel1(A, B).
v9_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v8_fomodel1(A, B).
v5_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v6_fomodel1(A, B).
v10_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v5_fomodel1(A, B).
 ~ (v7_fomodel1(A, B))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v8_fomodel1(A, B).
 ~ (v7_fomodel1(A, B))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v4_fomodel1(A, B).
 ~ (v6_fomodel1(A, B))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v4_fomodel1(A, B).
v6_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v5_fomodel1(A, B),v8_fomodel1(A, B).
v3_fomodel0(A, k15_fomodel1(B)) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v12_fomodel1(A, B).
v12_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v3_fomodel0(A, k15_fomodel1(B)).
 ~ (v1_xboole_0(A))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),true.
 ~ (v8_struct_0(A))  :- l1_fomodel1(A), ~ (v6_struct_0(A)) ,v11_fomodel1(A).
v13_fomodel1(A, B) :- v7_ordinal1(C), ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v14_fomodel1(A, C, B).
 ~ (v1_xboole_0(A))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v7_ordinal1(C),m1_subset_1(A, k1_funct_1(k28_fomodel1(B), C)),true.
 ~ (v1_xboole_0(A))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k29_fomodel1(B)),true.
v3_card_1(A, 1) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v14_fomodel1(A, k6_numbers, B).
v15_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k32_fomodel1(B)),true.
v13_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k35_fomodel1(B)),true.
v8_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)), ~ (v7_fomodel1(A, B)) ,v10_fomodel1(A, B).
v6_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)), ~ (v4_fomodel1(A, B)) ,v8_fomodel1(A, B).
v4_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k16_fomodel1(B)),true.
v14_fomodel1(A, k2_xcmplx_0(B, C), D) :-  ~ (v6_struct_0(D)) ,v11_fomodel1(D),l1_fomodel1(D),v7_ordinal1(B),v7_ordinal1(C),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(D)), k1_tarski(k1_xboole_0))),v14_fomodel1(A, k2_xcmplx_0(B, k3_xcmplx_0(k6_numbers, C)), D).
v4_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)), ~ (v5_fomodel1(A, B)) ,v9_fomodel1(A, B).
v5_relat_1(A, k5_fomodel1(B)) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v13_fomodel1(A, B).
v5_relat_1(A, k17_fomodel1(B)) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v15_fomodel1(A, B).
 ~ (v6_struct_0(A))  :-  ~ (v6_struct_0(B)) ,l1_fomodel1(B),l1_fomodel1(A),v16_fomodel1(A, B).
v11_fomodel1(A) :- v11_fomodel1(B),l1_fomodel1(B),l1_fomodel1(A),v16_fomodel1(A, B).
v5_relat_1(A, B) :-  ~ (v6_struct_0(C)) ,v11_fomodel1(C),l1_fomodel1(C), ~ (v1_xboole_0(B)) ,v8_fomodel1(D, C),m1_subset_1(D, k1_fomodel1(C)),m1_fomodel2(A, C, B, D),true.
v9_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel1(B)),v4_fomodel1(A, B).
v1_funcop_1(A) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B), ~ (v1_xboole_0(C)) ,v1_relat_1(A),v1_funct_1(A),v1_fomodel2(A, B, C).
 ~ (v1_xboole_0(A))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B), ~ (v1_xboole_0(C)) ,v9_fomodel1(D, B),m1_subset_1(D, k1_fomodel1(B)),m1_fomodel2(A, B, C, D),true.
v1_funcop_1(A) :-  ~ (v1_xboole_0(B)) , ~ (v1_xboole_0(D)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k9_funct_2(C, D)))),v1_funct_1(A),v1_funct_2(A, B, k9_funct_2(C, D)).
v1_funcop_1(A) :- v3_finseq_1(B),v1_relat_1(A),v5_relat_1(A, B),v1_funct_1(A).
v1_funct_1(A) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B), ~ (v1_xboole_0(C)) ,v1_relat_1(D),v1_funct_1(D),v1_fomodel2(D, B, C),v2_fomodel2(A, B, C, D).
v1_fomodel2(A, B, C) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B), ~ (v1_xboole_0(C)) ,m1_subset_1(A, k16_fomodel2(B, C)),true.
v15_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v3_fomodel2(A, B, k6_numbers).
v3_fomodel2(A, B, k6_numbers) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v15_fomodel1(A, B).
v4_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v7_ordinal1(C),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v3_fomodel2(A, B, C).
v3_fomodel2(A, B, k2_xcmplx_0(C, D)) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v7_ordinal1(C),v7_ordinal1(D),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v3_fomodel2(A, B, k1_nat_1(C, k4_nat_1(k6_numbers, D))).
 ~ (v15_fomodel1(A, B))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k6_subset_1(k3_finseq_2(k15_fomodel1(B)), k1_tarski(k1_xboole_0))),v5_fomodel2(A, B).
v4_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k33_fomodel2(B)),true.
 ~ (v5_fomodel2(A, B))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v7_ordinal1(C),m1_subset_1(A, k35_fomodel2(B, C)),true.
v5_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v7_ordinal1(C),m1_subset_1(A, k36_fomodel2(B, C)),true.
v4_relat_1(A, k37_fomodel1(B)) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B), ~ (v1_xboole_0(C)) ,m1_subset_1(A, k16_fomodel2(B, C)),true.
v1_partfun1(A, k37_fomodel1(B)) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B), ~ (v1_xboole_0(C)) ,m1_subset_1(A, k16_fomodel2(B, C)),v4_relat_1(A, k37_fomodel1(B)).
v7_fomodel2(A, B, C) :-  ~ (v6_struct_0(C)) ,v11_fomodel1(C),l1_fomodel1(C),m1_subset_1(D, k1_zfmisc_1(B)),m1_subset_1(A, k1_fomodel1(C)),v7_fomodel2(A, D, C).
 ~ (v7_fomodel2(A, k3_xboole_0(B, C), D))  :-  ~ (v6_struct_0(D)) ,v11_fomodel1(D),l1_fomodel1(D),m1_subset_1(A, k1_fomodel1(D)), ~ (v7_fomodel2(A, k13_fomodel0(C, B), D)) .
 ~ (v7_fomodel2(A, k2_xboole_0(D, B), C))  :- v1_finset_1(B), ~ (v6_struct_0(C)) ,v11_fomodel1(C),l1_fomodel1(C),m1_subset_1(A, k1_fomodel1(C)), ~ (v7_fomodel2(A, B, C)) , ~ (v7_fomodel2(A, D, C)) .
v9_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v6_fomodel2(A, B).
v4_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k43_fomodel2(B)),true.
v5_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k43_fomodel2(B)),true.
v4_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k43_fomodel2(B)),true.
v5_fomodel2(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k43_fomodel2(B)),true.
v1_fomodel2(A, B, C) :-  ~ (v1_xboole_0(C)) , ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B), ~ (v6_struct_0(D)) ,v11_fomodel1(D),v16_fomodel1(D, B),l1_fomodel1(D),v1_relat_1(A),v1_funct_1(A),v1_fomodel2(A, D, C).
v10_fomodel2(A, B, C, D) :-  ~ (v1_xboole_0(B)) , ~ (v6_struct_0(C)) ,v11_fomodel1(C),l1_fomodel1(C),m1_subset_1(D, k16_fomodel2(C, B)),v10_fomodel2(E, B, C, D),m1_subset_1(A, k1_zfmisc_1(E)),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_partfun1(C, B),v3_relat_2(C),v8_relat_2(C),m1_subset_1(C, k1_zfmisc_1(k2_zfmisc_1(B, B))),m1_subset_1(A, k8_eqrel_1(B, C)),true.
v9_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k37_fomodel1(B)),true.
v10_fomodel1(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k37_fomodel1(B)),true.
v1_fomodel3(A, k9_fomodel3(B, C, k1_int_2(k19_fomodel1(D, E))), C) :-  ~ (v6_struct_0(D)) ,v11_fomodel1(D),l1_fomodel1(D), ~ (v1_xboole_0(B)) , ~ (v7_fomodel1(E, D)) ,v10_fomodel1(E, D),m1_subset_1(E, k1_fomodel1(D)),m1_subset_1(C, k1_zfmisc_1(k2_zfmisc_1(B, B))),m1_fomodel2(A, D, B, E),v2_fomodel3(A, D, B, E, C).
v1_fomodel3(A, k9_fomodel3(B, C, k1_int_2(k19_fomodel1(D, E))), k3_fomodel2(k6_margrel1)) :-  ~ (v6_struct_0(D)) ,v11_fomodel1(D),l1_fomodel1(D), ~ (v1_xboole_0(B)) ,v7_fomodel1(E, D),m1_subset_1(E, k1_fomodel1(D)),m1_subset_1(C, k1_zfmisc_1(k2_zfmisc_1(B, B))),m1_fomodel2(A, D, B, E),v2_fomodel3(A, D, B, E, C).
v1_pre_poly(A) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k9_funct_2(k33_fomodel2(B), k33_fomodel2(B))),true.
v2_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_zfmisc_1(k1_fomodel4(B))),true.
v1_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel4(B)),true.
v3_fomodel4(A, B, C, k1_xboole_0) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v7_fomodel4(A, B, C).
v7_fomodel4(A, B, C) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v3_fomodel4(A, B, C, k1_xboole_0).
v7_fomodel4(A, B, C) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v1_xboole_0(D),v3_fomodel4(A, B, C, D).
v11_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v1_fomodel4(A, B).
v11_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_fomodel4(B)),true.
v1_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v7_ordinal1(D),v4_fomodel4(A, D, B, C, E).
v8_fomodel4(A, B, C, D) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v8_fomodel4(A, B, C, k6_subset_1(D, E)).
v8_fomodel4(A, B, C, k2_xboole_0(D, E)) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v8_fomodel4(A, B, C, k6_subset_1(D, E)).
v8_fomodel4(A, B, C, D) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v8_fomodel4(A, B, C, k3_xboole_0(D, E)).
v8_fomodel4(A, B, C, D) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),m1_subset_1(E, k1_zfmisc_1(D)),v8_fomodel4(A, B, C, E).
v12_fomodel4(A, k6_numbers, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v12_fomodel4(A, 1, B).
v12_fomodel4(A, 1, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v12_fomodel4(A, 2, B).
v2_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(C, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v3_fomodel4(A, B, C, D).
 ~ (v1_xboole_0(A))  :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),m1_subset_1(A, k1_zfmisc_1(k9_funct_2(k9_setfam_1(k1_fomodel4(B)), k9_setfam_1(k1_fomodel4(B))))),v12_fomodel4(A, k6_numbers, B).
v1_finset_1(A) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v13_fomodel4(A, B).
v13_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v13_fomodel4(C, B),m1_subset_1(A, k1_zfmisc_1(C)),true.
v11_fomodel4(A, B) :-  ~ (v6_struct_0(B)) ,v11_fomodel1(B),l1_fomodel1(B),v1_fomodel4(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(C, u1_struct_0(B)),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_tops_2(A, B),v1_yellow_8(A, B, C).
v3_frechet(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_frechet(A).
v4_frechet(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_frechet(A).
v1_funcop_1(A) :- v4_funct_1(B),v1_relat_1(A),v5_relat_1(A, B),v1_funct_1(A).
v2_funcop_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_funcop_1(A).
v1_funcop_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A).
v1_funct_1(A) :- true,v1_xboole_0(A).
v2_funct_1(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v1_funct_1(A) :- v1_relat_1(B),v1_funct_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_funct_1(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
 ~ (v1_zfmisc_1(A))  :- true,v1_relat_1(A),v1_funct_1(A), ~ (v3_funct_1(A)) .
v3_funct_1(A) :- true,v1_zfmisc_1(A),v1_relat_1(A),v1_funct_1(A).
v4_funct_1(A) :- true,v1_xboole_0(A).
v1_relat_1(A) :- v4_funct_1(B),m1_subset_1(A, B),true.
v1_funct_1(A) :- v4_funct_1(B),m1_subset_1(A, B),true.
v4_funct_1(A) :- v4_funct_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v4_relat_1(A, B) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_relat_1(A),v1_funct_1(A),v5_funct_1(A, C).
v1_funct_2(A, B, C) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_partfun1(A, B).
v2_funct_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A),v3_funct_2(A, C, B).
v2_funct_2(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A),v3_funct_2(A, C, B).
v3_funct_2(A, B, C) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v2_funct_1(A),v2_funct_2(A, C).
v3_funct_2(A, B, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, B))),v1_relat_2(A),v1_funct_1(A),v1_partfun1(A, B),v1_funct_2(A, B, B).
v1_partfun1(A, B) :-  ~ (v1_xboole_0(C)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C).
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) , ~ (v1_xboole_0(C)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C).
v4_funct_1(A) :- m1_funct_2(A, B, C),true.
v1_funcop_1(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v1_funcop_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A),v1_funct_7(A).
v1_funct_7(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A).
v2_relat_1(A) :-  ~ (v1_xboole_0(B)) ,v1_relat_1(B),v2_relat_1(B),v1_funct_1(B),v1_finseq_1(B),m2_funct_7(A, B),true.
v1_funct_7(A) :-  ~ (v1_xboole_0(B)) ,v1_relat_1(B),v2_relat_1(B),v1_funct_1(B),v1_finseq_1(B),m2_funct_7(A, B),true.
v2_funct_8(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v4_funct_8(A, B, C) :- v1_membered(B),v1_membered(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v2_funct_8(A),v3_funct_8(A, B, C).
v2_funct_8(A) :- v1_membered(B),v1_membered(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v4_funct_8(A, B, C).
v3_funct_8(A, B, C) :- v1_membered(B),v1_membered(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v4_funct_8(A, B, C).
v6_funct_8(A, B, C) :- v1_membered(B),v1_membered(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v2_funct_8(A),v5_funct_8(A, B, C).
v2_funct_8(A) :- v1_membered(B),v1_membered(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v6_funct_8(A, B, C).
v5_funct_8(A, B, C) :- v1_membered(B),v1_membered(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v6_funct_8(A, B, C).
v1_funcop_1(A) :- v1_funct_1(B),v1_funct_2(B, C, D),m1_subset_1(B, k1_zfmisc_1(k2_zfmisc_1(C, D))),v1_relat_1(E),v4_relat_1(E, C),v1_funct_1(E),v1_partfun1(E, C),v1_relat_1(F),v4_relat_1(F, D),v1_funct_1(F),v1_partfun1(F, D),m1_functor0(A, C, D, B, E, F),true.
v1_altcat_2(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v12_altcat_1(A).
v6_functor0(A, B, C) :- l1_struct_0(B), ~ (v2_struct_0(C)) ,l1_struct_0(C),l1_functor0(A, B, C),v10_functor0(A, B, C).
v6_functor0(A, B, C) :- l1_struct_0(B), ~ (v2_struct_0(C)) ,l1_struct_0(C),l1_functor0(A, B, C),v11_functor0(A, B, C).
v6_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v15_functor0(A, B, C).
v6_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v16_functor0(A, B, C).
v8_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),true.
v12_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),true.
v10_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v15_functor0(A, B, C).
v13_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v15_functor0(A, B, C).
v15_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v10_functor0(A, B, C),v13_functor0(A, B, C).
v11_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v16_functor0(A, B, C).
v14_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v16_functor0(A, B, C).
v16_functor0(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B), ~ (v2_struct_0(C)) ,v12_altcat_1(C),l2_altcat_1(C),m2_functor0(A, B, C),v11_functor0(A, B, C),v14_functor0(A, B, C).
v3_valued_0(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k1_numbers))),v5_relat_1(A, k1_rcomp_1(k6_numbers, 1)),v1_funct_1(A),v1_funct_2(A, B, k1_numbers).
v1_xxreal_0(A) :- true,v1_fvaluat1(A).
v1_fvaluat1(A) :- true,v1_int_1(A).
v1_int_1(A) :- true,v1_xreal_0(A),v1_fvaluat1(A).
v1_algstr_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v2_rlvect_1(A),v4_rlvect_1(A).
v6_vectsp_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v5_group_1(A),v3_vectsp_1(A).
v5_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v5_group_1(A),v1_vectsp_1(A).
v5_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v5_group_1(A),v2_vectsp_1(A).
v3_gcd_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v33_algstr_0(A),v5_group_1(A),v1_vectsp_1(A),v2_vectsp_1(A),v4_vectsp_1(A),v6_vectsp_1(A),v3_rlvect_1(A),v4_rlvect_1(A).
v6_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v5_glib_000(A).
v3_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v7_glib_000(A).
v5_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v7_glib_000(A).
v7_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v3_glib_000(A),v5_glib_000(A).
v8_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v3_glib_000(A),v6_glib_000(A).
v3_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v8_glib_000(A).
v6_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v8_glib_000(A).
v2_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v3_glib_000(A),v4_glib_000(A).
v2_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v4_glib_000(A),v6_glib_000(A).
v2_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_000(B),m1_glib_000(A, B),true.
v3_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v3_glib_000(B),m1_glib_000(A, B),true.
v4_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v4_glib_000(B),m1_glib_000(A, B),true.
v5_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v5_glib_000(B),m1_glib_000(A, B),true.
v2_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B), ~ (v1_xboole_0(C)) ,v1_finset_1(C),m1_subset_1(C, k1_zfmisc_1(k6_glib_000(B))),v1_finset_1(D),m1_subset_1(D, k1_zfmisc_1(k21_glib_000(B, C))),m2_glib_000(A, B, C, D),true.
v4_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_subset_1(C, k6_glib_000(B)),m1_subset_1(D, k1_zfmisc_1(k21_glib_000(B, k6_domain_1(k6_glib_000(B), C)))),m2_glib_000(A, B, k6_domain_1(k6_glib_000(B), C), D),true.
v2_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_subset_1(C, k6_glib_000(B)),m2_glib_000(A, B, k6_domain_1(k6_glib_000(B), C), k1_xboole_0),true.
v4_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_subset_1(C, k6_glib_000(B)),m2_glib_000(A, B, k6_domain_1(k6_glib_000(B), C), k1_xboole_0),true.
v7_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B), ~ (v1_xboole_0(C)) ,m1_subset_1(C, k1_zfmisc_1(k6_glib_000(B))),m2_glib_000(A, B, C, k1_xboole_0),true.
v9_glib_000(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_subset_1(C, k1_zfmisc_1(k7_glib_000(B))),m2_glib_000(A, B, k6_glib_000(B), C),true.
v9_glib_000(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m2_glib_000(A, B, k6_glib_000(B), k1_xboole_0),true.
v9_glib_000(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m2_glib_000(A, B, k6_glib_000(B), k6_subset_1(k7_glib_000(B), k1_tarski(C))),true.
v9_glib_000(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m2_glib_000(A, B, k6_glib_000(B), k6_subset_1(k7_glib_000(B), C)),true.
v19_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v18_glib_000(A).
v15_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v20_glib_000(A).
v18_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v20_glib_000(A).
v20_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v15_glib_000(A),v18_glib_000(A).
v21_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v15_glib_000(A),v19_glib_000(A).
v15_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v21_glib_000(A).
v19_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v21_glib_000(A).
v14_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v15_glib_000(A),v16_glib_000(A).
v14_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v16_glib_000(A),v19_glib_000(A).
v4_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v5_glib_001(A, B).
v5_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v3_glib_001(A, B).
v6_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v3_glib_001(A, B).
v5_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v6_glib_001(A, B).
v1_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v7_glib_001(A, B).
 ~ (v3_glib_001(A, B))  :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v7_glib_001(A, B).
v4_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v7_glib_001(A, B).
v1_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v8_glib_001(A, B).
 ~ (v3_glib_001(A, B))  :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v8_glib_001(A, B).
v5_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),v8_glib_001(A, B).
v1_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v4_glib_000(A).
v3_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v3_glib_000(A),v4_glib_000(A).
v7_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v2_glib_002(A).
v1_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v3_glib_002(A).
v2_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v3_glib_002(A).
v3_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v1_glib_002(A),v2_glib_002(A).
v3_glib_002(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_subset_1(C, k6_glib_000(B)),m2_glib_000(A, B, k6_domain_1(k6_glib_000(B), C), k1_xboole_0),true.
v2_glib_002(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_002(B),m1_glib_000(A, B),true.
v1_glib_002(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_glib_000(A, B),v4_glib_002(A, B).
v4_glib_002(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_subset_1(C, k6_glib_000(B)),m2_glib_000(A, B, k1_glib_002(B, C), k21_glib_000(B, k1_glib_002(B, C))),true.
v4_glib_002(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m1_subset_1(C, k3_glib_002(B)),m2_glib_000(A, B, C, k21_glib_000(B, C)),true.
v3_glib_002(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_000(B), ~ (v4_glib_000(B)) ,v3_glib_002(B),v11_glib_000(C, B),m1_subset_1(C, k6_glib_000(B)),m2_glib_000(A, B, k6_subset_1(k6_glib_000(B), k1_tarski(C)), k21_glib_000(B, k6_subset_1(k6_glib_000(B), k1_tarski(C)))),true.
v6_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v16_glib_000(A).
v8_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v15_glib_000(A),v16_glib_000(A).
v20_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v7_glib_002(A).
v6_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v8_glib_002(A).
v7_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v8_glib_002(A).
v8_glib_002(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v6_glib_002(A),v7_glib_002(A).
v7_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v1_glib_003(A),v8_glib_003(A).
v7_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v1_glib_003(A),v2_glib_003(A),v3_glib_003(A),v11_glib_003(A).
v9_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v1_glib_003(A),v2_glib_003(A),v3_glib_003(A),v11_glib_003(A).
v10_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v1_glib_003(A),v2_glib_003(A),v3_glib_003(A),v11_glib_003(A).
v11_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v1_glib_003(A),v2_glib_003(A),v3_glib_003(A),v7_glib_003(A),v9_glib_003(A),v10_glib_003(A).
v7_glib_003(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v1_glib_003(B),v7_glib_003(B),m1_glib_000(A, B),v1_glib_003(A),v4_glib_003(A, B).
v8_glib_003(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v1_glib_003(B),v8_glib_003(B),m1_glib_000(A, B),v1_glib_003(A),v4_glib_003(A, B).
v9_glib_003(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_003(B),v9_glib_003(B),m1_glib_000(A, B),v2_glib_003(A),v5_glib_003(A, B).
v10_glib_003(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v3_glib_003(B),v10_glib_003(B),m1_glib_000(A, B),v3_glib_003(A),v6_glib_003(A, B).
v15_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v12_glib_003(A),v13_glib_003(A),v14_glib_003(A),v19_glib_003(A).
v17_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v12_glib_003(A),v13_glib_003(A),v14_glib_003(A),v19_glib_003(A).
v18_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v12_glib_003(A),v13_glib_003(A),v14_glib_003(A),v19_glib_003(A).
v19_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v12_glib_000(A),v12_glib_003(A),v13_glib_003(A),v14_glib_003(A),v15_glib_003(A),v17_glib_003(A),v18_glib_003(A).
v1_glib_002(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_000(B),v1_glib_003(B),v7_glib_003(B),v7_ordinal1(C),m2_glib_000(A, B, k1_mcart_1(k15_glib_004(B, k16_glib_004(B), C)), k21_glib_000(B, k1_mcart_1(k15_glib_004(B, k16_glib_004(B), C)))),true.
v1_glib_002(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_000(B),v1_glib_003(B),v7_glib_003(B),v7_ordinal1(C),m2_glib_000(A, B, k1_mcart_1(k15_glib_004(B, k16_glib_004(B), C)), k2_mcart_1(k15_glib_004(B, k16_glib_004(B), C))),true.
v8_glib_003(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_finset_1(A),v1_glib_000(A),v1_glib_003(A),v1_glib_005(A).
v5_group_1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A),v1_gr_cy_1(A).
v7_ordinal1(A) :-  ~ (v1_xboole_0(B)) ,v7_ordinal1(B),m1_subset_1(A, u1_struct_0(k4_gr_cy_1(B))),true.
v7_graph_1(A, B) :-  ~ (v2_struct_0(B)) ,l1_graph_1(B),m1_graph_1(A, B),v1_xboole_0(A).
v9_graph_1(A, B) :-  ~ (v2_struct_0(B)) ,l1_graph_1(B),m1_graph_1(A, B),v2_funct_1(A),v1_xboole_0(A).
v2_finseq_1(A) :- m1_finseq_1(B, C),m1_subset_1(A, k1_zfmisc_1(B)),true.
v2_funct_1(A) :-  ~ (v2_struct_0(B)) ,l1_graph_1(B),m1_graph_1(A, B),v1_xboole_0(A).
v7_graph_1(A, B) :-  ~ (v2_struct_0(B)) ,l1_graph_1(B),m1_graph_1(A, B),v1_xboole_0(A).
v5_funct_1(A, B) :- v1_relat_1(B),v1_funct_1(B),m1_subset_1(C, k1_zfmisc_1(B)),v1_relat_1(A),v1_funct_1(A),v5_funct_1(A, C).
v5_funct_1(A, B) :- v1_relat_1(B),v1_funct_1(B),v1_relat_1(C),v1_funct_1(C),v5_funct_1(C, B),m1_subset_1(A, k1_zfmisc_1(C)),true.
v1_grnilp_1(A) :-  ~ (v2_struct_0(B)) ,v2_group_1(B),v3_group_1(B),v1_grnilp_1(B),l3_algstr_0(B),m1_group_2(A, B),true.
v1_grnilp_1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A),v5_group_1(A).
v1_grnilp_1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A),v1_gr_cy_1(A).
v1_grsolv_1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A),v1_grnilp_1(A).
v1_group_1(A) :- l3_algstr_0(A),v2_group_1(A).
v3_group_1(A) :-  ~ (v2_struct_0(B)) ,v2_group_1(B),v3_group_1(B),l3_algstr_0(B),m1_group_2(A, B),true.
v5_group_1(A) :-  ~ (v2_struct_0(B)) ,v2_group_1(B),v3_group_1(B),v5_group_1(B),l3_algstr_0(B),m1_group_2(A, B),true.
v8_struct_0(A) :-  ~ (v2_struct_0(B)) ,v8_struct_0(B),v2_group_1(B),v3_group_1(B),l3_algstr_0(B),m1_group_2(A, B),true.
v6_group_1(A, B, C) :-  ~ (v2_struct_0(B)) ,v2_group_1(B),v3_group_1(B),l3_algstr_0(B), ~ (v2_struct_0(C)) ,v2_group_1(C),v3_group_1(C),l3_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_group_6(A, B, C).
v2_pralg_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_group_7(A).
v1_finseq_1(A) :-  ~ (v2_struct_0(B)) ,l3_algstr_0(B),m1_subset_1(A, k4_card_3(k12_pralg_1(k1_tarski(1), k9_finseq_1(B)))),true.
v1_finseq_1(A) :-  ~ (v2_struct_0(B)) ,l3_algstr_0(B),m1_subset_1(A, u1_struct_0(k2_group_7(k1_tarski(1), k9_finseq_1(B)))),true.
v1_finseq_1(A) :-  ~ (v2_struct_0(B)) ,l3_algstr_0(B), ~ (v2_struct_0(C)) ,l3_algstr_0(C),m1_subset_1(A, k4_card_3(k12_pralg_1(k2_tarski(1, 2), k10_finseq_1(B, C)))),true.
v1_finseq_1(A) :-  ~ (v2_struct_0(B)) ,l3_algstr_0(B), ~ (v2_struct_0(C)) ,l3_algstr_0(C),m1_subset_1(A, u1_struct_0(k2_group_7(k2_tarski(1, 2), k10_finseq_1(B, C)))),true.
v1_finseq_1(A) :-  ~ (v2_struct_0(B)) ,l3_algstr_0(B), ~ (v2_struct_0(C)) ,l3_algstr_0(C), ~ (v2_struct_0(D)) ,l3_algstr_0(D),m1_subset_1(A, k4_card_3(k12_pralg_1(k1_enumset1(1, 2, 3), k11_finseq_1(B, C, D)))),true.
v1_finseq_1(A) :-  ~ (v2_struct_0(B)) ,l3_algstr_0(B), ~ (v2_struct_0(C)) ,l3_algstr_0(C), ~ (v2_struct_0(D)) ,l3_algstr_0(D),m1_subset_1(A, u1_struct_0(k2_group_7(k1_enumset1(1, 2, 3), k11_finseq_1(B, C, D)))),true.
v1_groupp_1(A, B, C) :- v7_ordinal1(B),v1_int_2(B), ~ (v2_struct_0(C)) ,v8_struct_0(C),v2_group_1(C),v3_group_1(C),v2_group_10(C, B),l3_algstr_0(C),m1_subset_1(A, u1_struct_0(C)),true.
v2_group_10(A, B) :- v7_ordinal1(B),v1_int_2(B), ~ (v2_struct_0(C)) ,v8_struct_0(C),v2_group_1(C),v3_group_1(C),v2_group_10(C, B),l3_algstr_0(C),m1_group_2(A, C),true.
v2_group_10(A, B) :- v7_ordinal1(B),l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A),v3_groupp_1(A, B).
v2_groupp_1(A, B) :- v7_ordinal1(B),l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A),v3_groupp_1(A, B).
v3_groupp_1(A, B) :- v7_ordinal1(B),l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A),v2_group_10(A, B),v2_groupp_1(A, B).
v2_groupp_1(A, B) :- v7_ordinal1(B),v1_int_2(B), ~ (v2_struct_0(C)) ,v8_struct_0(C),v2_group_1(C),v3_group_1(C),v2_groupp_1(C, B),l3_algstr_0(C),m1_group_2(A, C),true.
v3_groupp_1(A, B) :- v7_ordinal1(B),v1_int_2(B),l3_algstr_0(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v2_group_1(A),v3_group_1(A),v5_group_1(A),v2_group_10(A, B).
v1_hahnban(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v2_hahnban(A, B).
v4_hahnban(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v3_hahnban(A, B).
v4_hahnban(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v5_hahnban(A, B).
v7_hahnban(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v5_hahnban(A, B).
v5_hahnban(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v6_hahnban(A, B).
v5_hahnban(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v4_hahnban(A, B),v7_hahnban(A, B).
v2_hahnban1(A, C, B) :-  ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v3_group_1(C),v4_vectsp_1(C),v5_vectsp_1(C),l6_algstr_0(C), ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v8_vectsp_1(B, C),v9_vectsp_1(B, C),v10_vectsp_1(B, C),v11_vectsp_1(B, C),l1_vectsp_1(B, C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_hahnban1(A, C, B).
v3_hahnban1(A, C, B) :- l1_struct_0(C), ~ (v2_struct_0(B)) ,l1_vectsp_1(B, C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v4_hahnban1(A, C, B).
v7_hahnban1(A, k1_complfld, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v8_vectsp_1(B, k1_complfld),v9_vectsp_1(B, k1_complfld),v10_vectsp_1(B, k1_complfld),v11_vectsp_1(B, k1_complfld),l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v5_hahnban1(A, B).
v2_relat_1(A) :- v1_finset_1(B),m1_finseq_1(A, k1_zfmisc_1(B)), ~ (v1_xboole_0(A)) ,v1_hallmar1(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_finset_1(B), ~ (v1_xboole_0(C)) ,m1_finseq_1(C, k1_zfmisc_1(B)),m1_subset_1(D, k5_numbers),m3_hallmar1(A, B, C, D),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_finset_1(B), ~ (v1_xboole_0(C)) ,m1_finseq_1(C, k1_zfmisc_1(B)),m4_hallmar1(A, B, C),true.
 ~ (v1_xboole_0(A))  :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),m3_glib_001(A, B),true.
v5_glib_001(A, B) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v3_glib_002(B),m3_glib_001(A, B),v4_glib_001(A, B).
 ~ (v1_glib_001(A, B))  :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v3_glib_002(B),m3_glib_001(A, B), ~ (v3_glib_001(A, B)) ,v5_glib_001(A, B).
v2_hahnban1(A, k1_complfld, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v3_rlvect_1(B),v4_rlvect_1(B),v8_vectsp_1(B, k1_complfld),v9_vectsp_1(B, k1_complfld),v10_vectsp_1(B, k1_complfld),v11_vectsp_1(B, k1_complfld),l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(k1_complfld)),v1_hermitan(A, B).
v4_hermitan(A, B) :-  ~ (v2_struct_0(B)) ,l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)),v3_hermitan(A, B).
v2_bilinear(A, k1_complfld, B, B) :-  ~ (v2_struct_0(B)) ,l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)),v1_bilinear(A, k1_complfld, B, B),v3_hermitan(A, B).
v1_bilinear(A, k1_complfld, B, B) :-  ~ (v2_struct_0(B)) ,l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)),v2_bilinear(A, k1_complfld, B, B),v3_hermitan(A, B).
v2_hermitan(A, B, B) :-  ~ (v2_struct_0(B)) ,l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)),v4_bilinear(A, k1_complfld, B, B),v3_hermitan(A, B).
v4_bilinear(A, k1_complfld, B, B) :-  ~ (v2_struct_0(B)) ,l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)),v2_hermitan(A, B, B),v3_hermitan(A, B).
v5_hermitan(A, B) :-  ~ (v2_struct_0(B)) ,v4_rlvect_1(B),l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)),v2_bilinear(A, k1_complfld, B, B),v6_hermitan(A, B).
v5_hermitan(A, B) :-  ~ (v2_struct_0(B)) ,v4_rlvect_1(B),l1_vectsp_1(B, k1_complfld),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)))),v1_funct_1(A),v1_funct_2(A, k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)), u1_struct_0(k1_complfld)),v1_bilinear(A, k1_complfld, B, B),v6_hermitan(A, B).
v5_anproj_2(A) :- l1_collsp(A), ~ (v2_struct_0(A)) ,v2_collsp(A),v3_collsp(A),v4_collsp(A),v2_anproj_2(A),v3_anproj_2(A),v6_anproj_2(A).
 ~ (v1_xboole_0(A))  :- true,v5_hilbert1(A).
v1_hilbert1(A) :- true,v5_hilbert1(A).
v2_hilbert1(A) :- true,v5_hilbert1(A).
v3_hilbert1(A) :- true,v5_hilbert1(A).
v4_hilbert1(A) :- true,v5_hilbert1(A).
v5_hilbert1(A) :- m1_subset_1(A, k1_zfmisc_1(k13_finseq_1(k5_numbers))),v1_hilbert1(A),v2_hilbert1(A),v3_hilbert1(A),v4_hilbert1(A).
v1_finseq_1(A) :- m1_subset_1(A, k1_hilbert1),true.
v1_funcop_1(A) :- v1_relat_1(B),v2_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_partfun1(B, k5_numbers),m1_subset_1(C, k1_hilbert1),m1_subset_1(D, k1_hilbert1),m1_subset_1(E, k1_hilbert1),m1_subset_1(A, k3_hilbert3(B, k3_hilbert1(C, k3_hilbert1(D, E)))),true.
v2_hilbert3(A) :- m1_subset_1(A, k1_hilbert1),v1_hilbert3(A).
v1_finseq_1(A) :- m1_subset_1(A, k14_idea_1),true.
v3_ideal_1(A, B) :-  ~ (v2_struct_0(B)) ,v5_group_1(B),l3_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v2_ideal_1(A, B).
v2_ideal_1(A, B) :-  ~ (v2_struct_0(B)) ,v5_group_1(B),l3_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v3_ideal_1(A, B).
v5_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v6_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v7_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v8_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v9_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v10_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v11_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v12_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v13_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
v14_incsp_1(A) :- l2_incsp_1(A),v15_incsp_1(A).
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
v6_trees_3(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(D, u4_struct_0(B)),m1_subset_1(A, k3_msualg_1(B, D, k11_msafree(B, C))),true.
v1_instalg1(A) :- l1_msualg_1(A), ~ (v2_struct_0(A)) .
v1_instalg1(A) :- l1_msualg_1(A),v11_struct_0(A).
v11_struct_0(A) :- l1_msualg_1(A),v2_struct_0(A),v1_instalg1(A).
 ~ (v2_struct_0(A))  :- l1_msualg_1(A), ~ (v11_struct_0(A)) ,v1_instalg1(A).
v1_instalg1(A) :- v1_instalg1(B),l1_msualg_1(B),m1_instalg1(A, B),true.
v1_int_1(A) :- m1_subset_1(A, k4_numbers),true.
v1_int_1(A) :- true,v7_ordinal1(A).
v1_xreal_0(A) :- true,v1_int_1(A).
v3_gcd_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) , ~ (v6_struct_0(A)) ,v13_algstr_0(A),v3_group_1(A),v5_group_1(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v1_vectsp_1(A),v4_vectsp_1(A),v1_vectsp_2(A),v1_int_3(A).
v1_int_3(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v33_algstr_0(A),v3_group_1(A),v5_group_1(A),v4_rlvect_1(A),v4_vectsp_1(A).
 ~ (v1_xboole_0(A))  :- true,v7_ordinal1(A),v1_int_2(A).
v2_relat_1(A) :- true,v1_relat_1(A),v1_funct_1(A), ~ (v1_xboole_0(A)) ,v1_finseq_1(A),v5_valued_0(A),v1_partfun3(A),v1_int_6(A).
 ~ (v1_xboole_0(A))  :- true,v1_relat_1(A), ~ (v2_relat_1(A)) ,v1_funct_1(A),v1_finseq_1(A),v5_valued_0(A).
v5_valued_0(A) :- v1_relat_1(B),v1_funct_1(B), ~ (v1_xboole_0(B)) ,v1_finseq_1(B),v5_valued_0(B),v1_partfun3(B),v1_int_6(B),m1_int_6(A, B),true.
v1_rcomp_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v2_measure5(A).
v5_xxreal_2(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)), ~ (v1_xboole_0(A)) ,v2_measure5(A).
v1_relat_1(A) :-  ~ (v1_xboole_0(B)) ,v1_rcomp_1(B),m1_subset_1(B, k1_zfmisc_1(k1_numbers)),m1_subset_1(A, k1_integra1(B)),true.
v1_funct_1(A) :-  ~ (v1_xboole_0(B)) ,v1_rcomp_1(B),m1_subset_1(B, k1_zfmisc_1(k1_numbers)),m1_subset_1(A, k1_integra1(B)),true.
v1_finseq_1(A) :-  ~ (v1_xboole_0(B)) ,v1_rcomp_1(B),m1_subset_1(B, k1_zfmisc_1(k1_numbers)),m1_subset_1(A, k1_integra1(B)),true.
v3_valued_0(A) :-  ~ (v1_xboole_0(B)) ,v1_rcomp_1(B),m1_subset_1(B, k1_zfmisc_1(k1_numbers)),m1_subset_1(A, k1_integra1(B)),true.
 ~ (v1_xboole_0(A))  :- true,v7_intpro_1(A).
v1_intpro_1(A) :- true,v7_intpro_1(A).
v2_intpro_1(A) :- true,v7_intpro_1(A).
v3_intpro_1(A) :- true,v7_intpro_1(A).
v4_intpro_1(A) :- true,v7_intpro_1(A).
v5_intpro_1(A) :- true,v7_intpro_1(A).
v6_intpro_1(A) :- true,v7_intpro_1(A).
v7_intpro_1(A) :- m1_subset_1(A, k1_zfmisc_1(k13_finseq_1(k5_numbers))),v1_intpro_1(A),v2_intpro_1(A),v3_intpro_1(A),v4_intpro_1(A),v5_intpro_1(A),v6_intpro_1(A).
v1_finseq_1(A) :- m1_subset_1(A, k1_intpro_1),true.
v1_isomichi(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_pre_topc(A, B).
v1_isomichi(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_tops_1(A, B).
v2_isomichi(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_tops_1(A, B).
v4_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_isomichi(A, B),v2_isomichi(A, B).
v1_decomp_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_isomichi(A, B).
v2_isomichi(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_decomp_1(A, B).
v5_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v6_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v6_tops_1(A, B).
v4_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_tops_1(A, B).
v3_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v6_tops_1(A, B).
v4_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v6_tops_1(A, B).
v6_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_pre_topc(A, B),v4_tops_1(A, B).
v4_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_tops_1(A, B).
v4_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_tops_1(A, B).
v5_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_pre_topc(A, B),v4_tops_1(A, B).
 ~ (v4_isomichi(A, B))  :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_isomichi(A, B).
 ~ (v5_isomichi(A, B))  :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_isomichi(A, B).
 ~ (v3_isomichi(A, B))  :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_isomichi(A, B).
 ~ (v5_isomichi(A, B))  :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_isomichi(A, B).
 ~ (v3_isomichi(A, B))  :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_isomichi(A, B).
 ~ (v4_isomichi(A, B))  :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_isomichi(A, B).
v3_isomichi(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_isomichi(A, B).
v3_isomichi(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_isomichi(A, B).
v4_isomichi(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v1_subset_1(A, u1_struct_0(B)).
v1_jordan21(A) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))), ~ (v1_xboole_0(A)) ,v2_compts_1(A, k15_euclid(2)),v2_sppol_1(A).
v1_jordan2c(A, B) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))),v1_xboole_0(A).
 ~ (v1_xboole_0(A))  :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))), ~ (v1_jordan2c(A, B)) .
v1_subset_1(A, u1_struct_0(k15_euclid(B))) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))),v1_jordan2c(A, B).
v8_pre_topc(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v8_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
v2_connsp_1(A, k15_euclid(B)) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))),v1_convex1(A, k15_euclid(B)).
v1_goboard5(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v3_topreal1(A).
 ~ (v1_xboole_0(A))  :- m1_finseq_1(A, k3_finseq_2(u1_struct_0(k15_euclid(2)))), ~ (v3_relat_1(A)) ,v1_matrix_1(A),v2_goboard1(A),v3_goboard1(A),v4_goboard1(A),v5_goboard1(A).
v2_relat_1(A) :- m1_finseq_1(A, k3_finseq_2(u1_struct_0(k15_euclid(2)))), ~ (v3_relat_1(A)) ,v1_matrix_1(A),v2_goboard1(A),v3_goboard1(A),v4_goboard1(A),v5_goboard1(A).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_jordan21(A).
v1_jordan21(A) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal2(A).
v1_jordan23(A) :- true,v1_relat_1(A),v1_funct_1(A),v2_funct_1(A),v1_finseq_1(A).
v3_jordan23(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A),v1_jordan23(A).
v2_jordan23(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A),v1_finseq_1(A).
v2_jordan23(A) :- true,v1_relat_1(A),v1_funct_1(A),v2_funct_1(A),v1_finseq_1(A).
v5_pre_topc(A, k3_pcomps_1(B), k3_pcomps_1(B)) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),v7_metric_1(B),v8_metric_1(B),v9_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k3_pcomps_1(B)), u1_struct_0(k3_pcomps_1(B))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k3_pcomps_1(B)), u1_struct_0(k3_pcomps_1(B))),v1_jordan24(A, B).
v3_tops_2(A, k3_pcomps_1(B), k3_pcomps_1(B)) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),v7_metric_1(B),v8_metric_1(B),v9_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k3_pcomps_1(B)), u1_struct_0(k3_pcomps_1(B))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k3_pcomps_1(B)), u1_struct_0(k3_pcomps_1(B))),v2_funct_2(A, u1_struct_0(k3_pcomps_1(B))),v1_jordan24(A, B).
v5_relat_1(A, k1_numbers) :- v7_ordinal1(B),m1_subset_1(A, u1_struct_0(k15_euclid(B))),true.
v1_jordan2c(A, B) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))),v2_compts_1(A, k15_euclid(B)).
 ~ (v1_zfmisc_1(A))  :- v1_topreal2(B),m1_subset_1(B, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),m1_jordan_a(A, B),true.
v15_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v4_lattice3(A).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k9_setfam_1(B)))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k9_setfam_1(B)).
v2_prob_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k9_setfam_1(B)))),v1_funct_1(A),v3_funct_1(A),v1_funct_2(A, k5_numbers, k9_setfam_1(B)).
v3_prob_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k9_setfam_1(B)))),v1_funct_1(A),v3_funct_1(A),v1_funct_2(A, k5_numbers, k9_setfam_1(B)).
v3_kurato_0(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k9_setfam_1(B)))),v1_funct_1(A),v3_funct_1(A),v1_funct_2(A, k5_numbers, k9_setfam_1(B)).
v13_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v1_lattice2(A).
v3_filter_0(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v1_lattice2(A).
v1_lattice2(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v13_lattices(A),v3_filter_0(A).
v13_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v10_lattices(A).
v14_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v10_lattices(A).
v15_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v10_lattices(A).
v1_lattice2(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v10_lattices(A),v11_lattices(A).
 ~ (v2_struct_0(A))  :- l1_orders_2(A),v1_lattice3(A).
 ~ (v2_struct_0(A))  :- l1_orders_2(A),v2_lattice3(A).
v4_lattice3(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v10_lattices(A).
v1_lattice6(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v10_lattices(A).
v2_lattice6(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v10_lattices(A).
v4_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v5_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v6_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v7_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v8_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v9_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v10_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v6_lattices(A),v7_lattices(A),v8_lattices(A),v9_lattices(A).
v15_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v13_lattices(A),v14_lattices(A).
v13_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v15_lattices(A).
v14_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v15_lattices(A).
v11_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v17_lattices(A).
v15_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v17_lattices(A).
v16_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v17_lattices(A).
v17_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v11_lattices(A),v15_lattices(A),v16_lattices(A).
v12_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v11_lattices(A).
v18_lattices(A, B) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),l3_lattices(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v19_lattices(A, B) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),l3_lattices(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v20_lattices(A, B) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),l3_lattices(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v18_lattices(A, B).
v21_lattices(A, B) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),l3_lattices(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v19_lattices(A, B).
v1_finset_1(A) :- v1_lexbfs(B),m1_subset_1(A, B),true.
v4_lexbfs(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v13_glib_000(A),v3_lexbfs(A).
v13_glib_000(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v1_funct_1(A),v1_partfun1(A, k5_numbers),v4_lexbfs(A).
v13_glib_000(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_000(B),m1_lexbfs(A, B),v5_lexbfs(A, B).
v3_lexbfs(A) :- v1_relat_1(B),v4_relat_1(B, k5_numbers),v1_funct_1(B),v1_finset_1(B),v1_glib_000(B),v2_glib_000(B),m1_lexbfs(A, B),v5_lexbfs(A, B).
 ~ (v2_struct_0(A))  :- l1_orders_2(A),v2_lfuzzy_0(A).
v1_lfuzzy_0(A) :- l1_orders_2(A),v2_lfuzzy_0(A).
v1_lfuzzy_0(A) :- l1_orders_2(A),v2_struct_0(A).
v1_xreal_0(A) :-  ~ (v2_struct_0(B)) ,v1_lfuzzy_0(B),l1_orders_2(B),m1_subset_1(A, u1_struct_0(B)),true.
v3_orders_2(A) :- l1_orders_2(A),v1_lfuzzy_0(A).
v4_orders_2(A) :- l1_orders_2(A),v1_lfuzzy_0(A).
v5_orders_2(A) :- l1_orders_2(A),v1_lfuzzy_0(A).
v16_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v1_lfuzzy_0(A).
v1_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v1_lfuzzy_0(A).
v2_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v1_lfuzzy_0(A).
v3_yellow_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v2_lfuzzy_0(A).
v3_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v2_lfuzzy_0(A).
v2_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v16_waybel_0(A).
v9_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v2_lfuzzy_0(A).
v3_valued_0(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k1_numbers))),v5_relat_1(A, k1_rcomp_1(k6_numbers, 1)),v1_funct_1(A),v1_funct_2(A, B, k1_numbers).
v1_relat_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v5_rlvect_1(C),v6_rlvect_1(C),v7_rlvect_1(C),v8_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_normsp_1(C),l1_normsp_1(C),m1_subset_1(A, u1_struct_0(k11_lopban_1(B, C))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v5_rlvect_1(C),v6_rlvect_1(C),v7_rlvect_1(C),v8_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_normsp_1(C),l1_normsp_1(C),m1_subset_1(A, u1_struct_0(k11_lopban_1(B, C))),true.
v1_relat_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v5_rlvect_1(C),v6_rlvect_1(C),v7_rlvect_1(C),v8_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_normsp_1(C),l1_normsp_1(C),m1_subset_1(A, u1_struct_0(k16_lopban_1(B, C))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v5_rlvect_1(C),v6_rlvect_1(C),v7_rlvect_1(C),v8_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_normsp_1(C),l1_normsp_1(C),m1_subset_1(A, u1_struct_0(k16_lopban_1(B, C))),true.
v3_lopban_1(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v2_funcsdom(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_lopban_2(A).
v2_vectsp_1(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v2_funcsdom(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_lopban_2(A).
v6_vectsp_1(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v2_funcsdom(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_lopban_2(A).
v2_lopban_2(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v2_funcsdom(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_lopban_2(A).
v3_lopban_2(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v2_funcsdom(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_lopban_2(A).
v4_lopban_2(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v2_funcsdom(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_lopban_2(A).
v5_lopban_2(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v2_funcsdom(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v3_lopban_1(A),v3_group_1(A),v1_vectsp_1(A),v2_vectsp_1(A),v3_vectsp_1(A),v6_vectsp_1(A),v2_lopban_2(A),v3_lopban_2(A),v4_lopban_2(A).
v3_normsp_1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, k5_numbers, u1_struct_0(B)),v1_lopban_3(A, B).
v1_lopban_3(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),v3_lopban_1(B),l1_normsp_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, k5_numbers, u1_struct_0(B)),v2_lopban_3(A, B).
v4_vectsp_1(A) :- l1_lopban_2(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v3_normsp_0(A),v4_normsp_0(A),v2_normsp_1(A),v2_funcsdom(A),v3_group_1(A),v1_vectsp_1(A),v3_vectsp_1(A),v5_lopban_2(A).
v2_xxreal_0(A) :- true,v1_xreal_0(A),v1_lpspace2(A).
v2_card_3(A) :- true,v1_xboole_0(A).
v1_xboolean(A) :- m1_subset_1(A, k6_margrel1),true.
v1_margrel1(A) :- m1_subset_1(A, k9_funct_2(B, k6_margrel1)),true.
v1_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v33_algstr_0(B),v3_group_1(B),v5_group_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v4_vectsp_1(B),v5_vectsp_1(B),l6_algstr_0(B),v7_ordinal1(C),m1_subset_1(A, u1_struct_0(k7_prvect_1(B, C))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v33_algstr_0(B),v3_group_1(B),v5_group_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v4_vectsp_1(B),v5_vectsp_1(B),l6_algstr_0(B),v7_ordinal1(C),m1_subset_1(A, u1_struct_0(k7_prvect_1(B, C))),true.
v1_matrix_2(A, C, B) :- v7_ordinal1(C), ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v33_algstr_0(B),v3_group_1(B),v5_group_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v4_vectsp_1(B),v5_vectsp_1(B),l6_algstr_0(B),m1_matrix_1(A, u1_struct_0(B), C, C),v2_matrix_1(A, B).
v2_matrix_2(A, C, B) :- v7_ordinal1(C), ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v33_algstr_0(B),v3_group_1(B),v5_group_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v4_vectsp_1(B),v5_vectsp_1(B),l6_algstr_0(B),m1_matrix_1(A, u1_struct_0(B), C, C),v2_matrix_1(A, B).
v1_matrixj1(A, B) :-  ~ (v1_xboole_0(B)) ,m1_finseq_1(A, k3_finseq_2(k3_finseq_2(B))),v2_matrixj1(A, B).
v2_matrixj1(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v33_algstr_0(B),v3_group_1(B),v5_group_1(B),v4_vectsp_1(B),v5_vectsp_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),l6_algstr_0(B),m1_finseq_1(A, k3_finseq_2(k3_finseq_2(u1_struct_0(B)))),v1_matrixj2(A, B).
v1_pre_poly(A) :-  ~ (v1_xboole_0(B)) ,m1_finseq_1(A, k3_finseq_2(B)),v1_matrix_1(A).
v3_pre_topc(A, B) :-  ~ (v3_pencil_1(B)) ,v1_matroid0(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_matroid0(A) :- l1_pre_topc(A),v4_matroid0(A).
v4_matroid0(A) :- l1_pre_topc(A),v8_struct_0(A).
 ~ (v3_pencil_1(A))  :- l1_pre_topc(A),v1_tdlat_3(A).
v1_matroid0(A) :- l1_pre_topc(A),v1_tdlat_3(A).
v2_matroid0(A) :- l1_pre_topc(A),v1_tdlat_3(A).
v4_taxonom2(A) :- m1_eqrel_1(A, B),true.
v1_finset_1(A) :-  ~ (v3_pencil_1(B)) ,v3_matroid0(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_pre_topc(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) , ~ (v3_pencil_1(B)) ,v1_matroid0(B),v2_matroid0(B),v4_matroid0(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_matroid0(A, B).
v1_finset_1(A) :-  ~ (v2_struct_0(B)) , ~ (v3_pencil_1(B)) ,v1_matroid0(B),v2_matroid0(B),v4_matroid0(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_matroid0(A, B).
v2_matrprob(A) :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v4_matrprob(A).
v3_matrprob(A) :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v4_matrprob(A).
v4_matrprob(A) :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v2_matrprob(A),v3_matrprob(A).
v2_matrprob(A) :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v6_matrprob(A).
v5_matrprob(A) :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v6_matrprob(A).
v6_matrprob(A) :- m1_finseq_1(A, k3_finseq_2(k1_numbers)),v1_matrix_1(A),v2_matrprob(A),v5_matrprob(A).
v1_matrtop3(A, B) :- m1_subset_1(C, k1_zfmisc_1(B)),v1_relat_1(A),v1_funct_1(A),v1_funcop_1(A),v1_matrtop3(A, C).
v1_matrtop3(A, k3_xboole_0(B, C)) :- true,v1_relat_1(A),v1_funct_1(A),v1_funcop_1(A),v1_matrtop3(A, B),v1_matrtop3(A, C).
v1_pre_poly(A) :- v7_ordinal1(B),v7_ordinal1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(C))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(C))).
v1_topreal9(A, B) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))),v3_matrtop3(A, B).
v3_tops_2(A, k15_euclid(B), k15_euclid(B)) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))),v3_matrtop3(A, B).
v1_grcat_1(A, k15_euclid(B), k15_euclid(B)) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))),v3_matrtop3(A, B).
v2_matrtop3(A, B) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))),v3_matrtop3(A, B).
v3_tops_2(A, k15_euclid(B), k15_euclid(B)) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k15_euclid(B)), u1_struct_0(k15_euclid(B))),v1_topreal9(A, B),v1_grcat_1(A, k15_euclid(B), k15_euclid(B)),v2_matrtop3(A, B).
v3_mazurulm(A, B, C) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v5_rlvect_1(C),v6_rlvect_1(C),v7_rlvect_1(C),v8_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_normsp_1(C),l1_normsp_1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v3_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_mazurulm(A, B, C).
v2_mazurulm(A, B, C) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v5_rlvect_1(C),v6_rlvect_1(C),v7_rlvect_1(C),v8_rlvect_1(C),v3_normsp_0(C),v4_normsp_0(C),v2_normsp_1(C),l1_normsp_1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_mazurulm(A, B, C),v3_mazurulm(A, B, C).
v2_finsub_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v1_finsub_1(A),v1_prob_1(A, B).
v1_finsub_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v2_finsub_1(A),v1_prob_1(A, B).
v3_finsub_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v2_finsub_1(A),v1_prob_1(A, B).
v3_measure1(A, B) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v1_prob_1(A, B),v4_prob_1(A, B).
v4_prob_1(A, B) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v1_prob_1(A, B),v3_measure1(A, B).
v1_finsub_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))), ~ (v1_xboole_0(A)) ,v1_prob_1(A, B),v3_measure1(A, B).
v6_xxreal_2(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v1_measure5(A).
v6_xxreal_2(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v2_measure5(A).
v6_xxreal_2(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v3_measure5(A).
v6_xxreal_2(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v4_measure5(A).
v1_measure5(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v1_xboole_0(A).
v3_finset_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v6_ordinal1(A), ~ (v1_xboole_0(A)) ,v1_setfam_1(A).
v5_membered(A) :- true,v6_membered(A).
v4_membered(A) :- true,v5_membered(A).
v3_membered(A) :- true,v4_membered(A).
v2_membered(A) :- true,v3_membered(A).
v1_membered(A) :- true,v3_membered(A).
v1_membered(A) :- m1_subset_1(A, k1_zfmisc_1(k2_numbers)),true.
v2_membered(A) :- m1_subset_1(A, k1_zfmisc_1(k7_numbers)),true.
v3_membered(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),true.
v4_membered(A) :- m1_subset_1(A, k1_zfmisc_1(k3_numbers)),true.
v5_membered(A) :- m1_subset_1(A, k1_zfmisc_1(k4_numbers)),true.
v6_membered(A) :- m1_subset_1(A, k1_zfmisc_1(k5_numbers)),true.
v1_xcmplx_0(A) :- v1_membered(B),m1_subset_1(A, B),true.
v1_xxreal_0(A) :- v2_membered(B),m1_subset_1(A, B),true.
v1_xreal_0(A) :- v3_membered(B),m1_subset_1(A, B),true.
v1_rat_1(A) :- v4_membered(B),m1_subset_1(A, B),true.
v1_int_1(A) :- v5_membered(B),m1_subset_1(A, B),true.
v7_ordinal1(A) :- v6_membered(B),m1_subset_1(A, B),true.
v6_membered(A) :- true,v1_xboole_0(A).
v1_membered(A) :- v1_membered(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v2_membered(A) :- v2_membered(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_membered(A) :- v3_membered(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v4_membered(A) :- v4_membered(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v5_membered(A) :- v5_membered(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v6_membered(A) :- v6_membered(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v6_membered(A) :- m1_subset_1(A, k5_numbers),true.
v7_membered(A) :- true,v1_xboole_0(A).
v3_memstr_0(A, C, B) :- l1_memstr_0(B, C),v1_xboole_0(A),v1_relat_1(A),v4_relat_1(A, u1_struct_0(B)),v1_funct_1(A),v5_funct_1(A, u1_memstr_0(C, B)).
v3_mesfunc5(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k7_numbers))),v1_funct_1(A),v6_supinf_2(A).
v4_mesfunc5(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k7_numbers))),v1_funct_1(A),v2_mesfunc5(A).
v11_metric_1(A) :- l1_metric_1(A), ~ (v2_struct_0(A)) ,v6_metric_1(A),v7_metric_1(A),v8_metric_1(A),v9_metric_1(A).
v7_metric_1(A) :- l1_metric_1(A), ~ (v2_struct_0(A)) ,v6_metric_1(A),v8_metric_1(A),v11_metric_1(A),v12_metric_1(A).
v9_metric_1(A) :- l1_metric_1(A), ~ (v2_struct_0(A)) ,v6_metric_1(A),v8_metric_1(A),v11_metric_1(A),v12_metric_1(A).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),v8_metric_1(B),v9_metric_1(B),l1_metric_1(B),m1_metric_2(A, B),true.
v2_tbsp_1(A, B) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),v7_metric_1(B),v8_metric_1(B),v9_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, u1_struct_0(B)))),v1_funct_1(A),v3_funct_1(A),v1_funct_2(A, k5_numbers, u1_struct_0(B)).
v1_metric_6(A, B) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),v7_metric_1(B),v8_metric_1(B),v9_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, k5_numbers, u1_struct_0(B)),v3_tbsp_1(A, B).
v3_pcomps_1(A) :- l1_pre_topc(A),v2_struct_0(A),v2_pre_topc(A).
v12_pre_topc(A) :- l1_pre_topc(A),v2_pre_topc(A),v3_pcomps_1(A).
v5_topgen_4(A, B) :- v2_pre_topc(B),v3_pcomps_1(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_pre_topc(A, B).
v6_topgen_4(A, B) :- v2_pre_topc(B),v3_pcomps_1(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_pre_topc(A, B).
v5_waybel23(A) :- l1_pre_topc(A),v2_pre_topc(A),v3_pcomps_1(A),v1_metrizts(A).
v7_topgen_1(A) :- l1_pre_topc(A),v2_pre_topc(A),v3_pcomps_1(A),v1_metrizts(A).
v1_metrizts(A) :- l1_pre_topc(A),v2_pre_topc(A),v7_topgen_1(A),v3_pcomps_1(A).
v1_metrizts(A) :- l1_pre_topc(A),v2_pre_topc(A),v5_waybel23(A).
v10_pre_topc(A) :- l1_pre_topc(A),v2_pre_topc(A),v9_pre_topc(A),v1_metrizts(A).
v1_metrizts(A) :- l1_pre_topc(A),v2_pre_topc(A),v1_orders_4(A).
v3_pre_topc(A, k15_euclid(B)) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))),v1_mfold_1(A, B).
v1_frechet(A) :- v7_ordinal1(B),l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v2_mfold_1(A, B).
v1_tdlat_3(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v2_mfold_1(A, k1_xboole_0).
v2_mfold_1(A, k1_xboole_0) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A).
v8_pre_topc(A) :- v7_ordinal1(B),l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_mfold_1(A, B).
v5_waybel23(A) :- v7_ordinal1(B),l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_mfold_1(A, B).
v2_mfold_1(A, B) :- v7_ordinal1(B),l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_mfold_1(A, B).
v3_mfold_1(A, B) :- v7_ordinal1(B),l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v8_pre_topc(A),v5_waybel23(A),v2_mfold_1(A, B).
v4_mfold_1(A) :- v7_ordinal1(B),l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_mfold_1(A, B).
v3_mfold_1(A, k1_xboole_0) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v5_waybel23(A),v1_tdlat_3(A).
v3_mfold_1(A, B) :- v7_ordinal1(B), ~ (v2_struct_0(C)) ,v2_pre_topc(C),v3_mfold_1(C, B),l1_pre_topc(C),m1_pre_topc(A, C), ~ (v2_struct_0(A)) ,v1_tsep_1(A, C).
v12_vectsp_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v1_midsp_2(A).
v6_group_1(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v2_mod_4(A, B, C).
v1_grcat_1(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v2_mod_4(A, B, C).
v1_mod_4(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v2_mod_4(A, B, C).
v2_mod_4(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v6_group_1(A, B, C),v1_grcat_1(A, B, C),v1_mod_4(A, B, C).
v1_finset_1(A) :- m1_subset_1(A, k6_modal_1),true.
 ~ (v1_moebius1(A))  :- m1_subset_1(A, k5_numbers),v1_int_2(A).
v1_moebius1(A) :- true, ~ (v1_zfmisc_1(A)) ,v7_ordinal1(A),v1_pythtrip(A).
v1_relat_1(A) :- v1_monoid_0(B),l1_struct_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v1_funct_1(A) :- v1_monoid_0(B),l1_struct_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v1_monoid_0(A) :- l1_struct_0(A),v2_monoid_0(A).
v1_finseq_1(A) :- v2_monoid_0(B),l1_struct_0(B),m1_subset_1(A, u1_struct_0(B)),true.
v13_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v2_group_1(A),v3_group_1(A).
v2_group_1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v3_group_1(A),v13_monoid_0(A).
v11_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v13_monoid_0(A).
v12_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v13_monoid_0(A).
v13_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v11_monoid_0(A),v12_monoid_0(A).
v14_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v16_monoid_0(A).
v15_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v16_monoid_0(A).
v16_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v14_monoid_0(A),v15_monoid_0(A).
v1_group_1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v3_group_1(A),v13_monoid_0(A).
v16_monoid_0(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v3_group_1(A),v13_monoid_0(A).
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,l3_algstr_0(B),m1_monoid_0(A, B),true.
v1_monoid_0(A) :- v1_monoid_0(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v2_monoid_0(A) :- v2_monoid_0(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v1_group_1(A) :-  ~ (v2_struct_0(B)) ,v1_group_1(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v3_group_1(A) :-  ~ (v2_struct_0(B)) ,v3_group_1(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v5_group_1(A) :-  ~ (v2_struct_0(B)) ,v5_group_1(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v13_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v13_monoid_0(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v16_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v16_monoid_0(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v17_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v17_monoid_0(B),l3_algstr_0(B),m1_monoid_0(A, B),true.
v1_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v1_monoid_0(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) .
v1_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v1_monoid_0(B),l3_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v2_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v2_monoid_0(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) .
v2_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v2_monoid_0(B),l3_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v4_vectsp_1(A) :-  ~ (v2_struct_0(B)) ,v4_vectsp_1(B),l4_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v5_group_1(A) :-  ~ (v2_struct_0(B)) ,v5_group_1(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) .
v5_group_1(A) :-  ~ (v2_struct_0(B)) ,v5_group_1(B),l3_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v3_group_1(A) :-  ~ (v2_struct_0(B)) ,v3_group_1(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) .
v3_group_1(A) :-  ~ (v2_struct_0(B)) ,v3_group_1(B),l3_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v10_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v10_monoid_0(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) .
v10_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v10_monoid_0(B),l3_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v16_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v16_monoid_0(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) .
v16_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v16_monoid_0(B),l3_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v17_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v4_vectsp_1(B),v17_monoid_0(B),l4_algstr_0(B),m3_monoid_0(A, B), ~ (v2_struct_0(A)) .
v1_group_1(A) :-  ~ (v2_struct_0(B)) ,v1_group_1(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) ,v3_group_1(A),v13_monoid_0(A).
v2_group_1(A) :-  ~ (v2_struct_0(B)) ,v1_group_1(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) ,v3_group_1(A),v13_monoid_0(A).
v16_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v1_group_1(B),l3_algstr_0(B),m2_monoid_0(A, B), ~ (v2_struct_0(A)) ,v3_group_1(A),v13_monoid_0(A).
v1_prob_2(A) :- true,v1_relat_1(A),v3_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
v2_msafree2(A) :- l1_msualg_1(A), ~ (v2_struct_0(A)) ,v11_struct_0(A).
v3_msafree2(A, B) :-  ~ (v2_struct_0(B)) ,l1_msualg_1(B),l3_msualg_1(A, B),v4_msualg_1(A, B),v4_msafree2(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k11_msafree(B, C)), D)),true.
v1_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k11_msafree(B, C)), D)),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k11_msafree(B, C)), D)),true.
v1_finset_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k11_msafree(B, C)), D)),true.
v3_trees_2(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_funct_1(u3_msualg_1(B, k11_msafree(B, C)), D)),v1_relat_1(A),v1_funct_1(A).
v1_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C), ~ (v3_relat_1(C)) ,v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, C)))),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C), ~ (v3_relat_1(C)) ,v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, C)))),true.
v1_finset_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C), ~ (v3_relat_1(C)) ,v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, C)))),true.
v3_trees_2(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C), ~ (v3_relat_1(C)) ,v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, C)))),true.
v3_trees_9(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C), ~ (v3_relat_1(C)) ,v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(A, k3_card_3(u3_msualg_1(B, k1_msafree3(B, C)))),true.
 ~ (v1_xboole_0(A))  :- true,v1_relat_1(A),v1_funct_1(A),v3_trees_2(A).
v1_msualg_6(A, B) :-  ~ (v11_struct_0(B)) ,v1_instalg1(B),l1_msualg_1(B),v1_msualg_6(C, B),l3_msualg_1(C, B),m1_msualg_2(A, B, C),true.
v1_finset_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),m1_subset_1(A, k1_msaterm(B, C)),true.
v2_msscyc_1(A) :- l1_graph_1(A), ~ (v2_struct_0(A)) ,v11_struct_0(A).
v1_xboole_0(A) :-  ~ (v2_struct_0(B)) ,v11_struct_0(B),l1_graph_1(B),m1_graph_1(A, B),true.
v3_msscyc_1(A) :- l1_graph_1(A), ~ (v2_struct_0(A)) ,v11_struct_0(A).
 ~ (v11_struct_0(A))  :- l1_graph_1(A), ~ (v2_struct_0(A)) , ~ (v3_msscyc_1(A)) .
v2_msscyc_1(A) :- l1_graph_1(A), ~ (v2_struct_0(A)) ,v3_msscyc_1(A).
v2_finset_1(A) :- true,v1_relat_1(A),v3_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
v2_finset_1(A) :- v1_relat_1(B),v4_relat_1(B, C),v1_funct_1(B),v1_partfun1(B, C),v2_finset_1(B),m3_pboole(A, C, B),true.
v1_mssubfam(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m3_pboole(A, B, k1_mboolean(B, C)),v2_mssubfam(A, B, C).
v3_mssubfam(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m3_pboole(A, B, k1_mboolean(B, C)),v4_mssubfam(A, B, C).
v5_mssubfam(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m3_pboole(A, B, k1_mboolean(B, C)),v4_mssubfam(A, B, C).
v2_relat_1(A) :- v1_relat_1(B),v4_relat_1(B, C),v1_funct_1(B),v1_partfun1(B, C),m3_pboole(A, C, k1_mboolean(C, B)),v5_mssubfam(A, C, B).
v6_mssubfam(A, B, C) :- v1_relat_1(C),v4_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, B),m3_pboole(A, B, k1_mboolean(B, C)),v2_mssubfam(A, B, C).
v2_relat_1(A) :- v1_relat_1(B),v4_relat_1(B, C),v1_funct_1(B),v1_partfun1(B, C),m3_pboole(A, C, k1_mboolean(C, B)),v6_mssubfam(A, C, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,l1_msualg_1(B),v4_msualg_1(C, B),l3_msualg_1(C, B),m1_subset_1(A, k2_relat_1(u3_msualg_1(B, C))),true.
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,l1_msualg_1(B),v4_msualg_1(C, B),l3_msualg_1(C, B),m1_subset_1(A, k2_relat_1(k6_finseq_2(u1_struct_0(B), u3_msualg_1(B, C)))),true.
v2_funcop_1(A) :- v1_relat_1(B),v4_relat_1(B, C),v1_funct_1(B),v1_partfun1(B, C),v1_relat_1(D),v4_relat_1(D, C),v1_funct_1(D),v1_partfun1(D, C),m1_msualg_4(A, C, B, D),true.
v1_msualg_4(A, u1_struct_0(B), u3_msualg_1(B, C)) :-  ~ (v2_struct_0(B)) ,l1_msualg_1(B),l3_msualg_1(C, B),m1_msualg_4(A, u1_struct_0(B), u3_msualg_1(B, C), u3_msualg_1(B, C)),v2_msualg_4(A, B, C).
v1_msualg_6(A, B) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),l3_msualg_1(A, B),v4_msualg_1(A, B).
v2_msualg_6(A, B, C) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v4_msualg_1(C, B),l3_msualg_1(C, B),m1_msualg_4(A, u1_struct_0(B), u3_msualg_1(B, C), u3_msualg_1(B, C)),v2_msualg_4(A, B, C),v3_msualg_6(A, B, C).
v3_msualg_6(A, B, C) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v4_msualg_1(C, B),l3_msualg_1(C, B),m1_msualg_4(A, u1_struct_0(B), u3_msualg_1(B, C), u3_msualg_1(B, C)),v2_msualg_4(A, B, C),v2_msualg_6(A, B, C).
v2_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v4_msualg_1(C, B),l3_msualg_1(C, B),m1_msualg_4(A, u1_struct_0(B), u3_msualg_1(B, C), u3_msualg_1(B, C)),v2_msualg_4(A, B, C).
v4_lattice3(A) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),v4_lattice3(B),l3_lattices(B),m2_nat_lat(A, B),v1_msualg_7(A, B).
v4_lattice3(A) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),v4_lattice3(B),l3_lattices(B),m2_nat_lat(A, B),v2_msualg_7(A, B).
v1_finseq_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),v4_msualg_1(C, B),l3_msualg_1(C, B),m1_subset_1(D, u4_struct_0(B)),m1_subset_1(A, k3_msualg_1(B, D, C)),true.
v1_relat_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),m1_subset_1(C, u1_struct_0(B)),m1_pralg_2(D, E, B),m1_subset_1(A, k1_funct_1(k10_pralg_2(E, B, D), C)),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,l1_msualg_1(B),m1_subset_1(C, u1_struct_0(B)),m1_pralg_2(D, E, B),m1_subset_1(A, k1_funct_1(k10_pralg_2(E, B, D), C)),true.
v1_mycielsk(A) :- l1_orders_2(A),v8_struct_0(A).
v2_mycielsk(A) :- l1_orders_2(A),v8_struct_0(A).
v2_struct_0(A) :- m1_mycielsk(A, k6_numbers),true.
 ~ (v2_struct_0(A))  :-  ~ (v1_xboole_0(B)) ,v7_ordinal1(B),m1_mycielsk(A, B),true.
v8_struct_0(A) :- v7_ordinal1(B),m1_mycielsk(A, B),true.
 ~ (v1_xboole_0(A))  :- true, ~ (v4_card_3(A)) .
v3_ordinal1(A) :- true,v7_ordinal1(A).
 ~ (v3_xxreal_0(A))  :- m1_subset_1(A, k5_numbers),true.
 ~ (v3_xxreal_0(A))  :- true,v7_ordinal1(A).
 ~ (v1_xboole_0(A))  :- true,v7_ordinal1(A),v1_int_2(A).
v1_xreal_0(A) :- m1_subset_1(B, k1_zfmisc_1(k1_numbers)),m1_subset_1(A, B),true.
v1_xreal_0(A) :- m1_subset_1(B, k1_zfmisc_1(k5_numbers)),m1_subset_1(A, B),true.
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k6_nat_lat),true.
v7_ordinal1(A) :- m1_subset_1(A, k6_nat_lat),true.
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, u1_struct_0(k10_nat_lat)),true.
v7_ordinal1(A) :- m1_subset_1(A, u1_struct_0(k10_nat_lat)),true.
v3_card_1(A, k3_finseq_1(B)) :-  ~ (v1_xboole_0(B)) ,v1_relat_1(B),v1_funct_1(B),v1_finseq_1(B),v2_prvect_2(B),v1_ndiff_5(B),m1_subset_1(A, u1_struct_0(k14_prvect_2(B))),true.
v1_finset_1(A) :- v7_ordinal1(B),m1_subset_1(A, k4_classes1(B)),true.
v1_finset_1(A) :- m1_subset_1(A, k13_classes2),true.
v1_zfmisc_1(A) :- v1_zfmisc_1(B),v1_zfmisc_1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),true.
v1_zfmisc_1(A) :- v1_zfmisc_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, B))),true.
v1_relat_2(A) :- v1_zfmisc_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, B))),true.
v3_relat_2(A) :- v1_zfmisc_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, B))),true.
v7_relat_2(A) :- v1_zfmisc_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, B))),true.
v8_relat_2(A) :- v1_zfmisc_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, B))),true.
v3_necklace(A) :- v3_necklace(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B).
v1_necklace(A) :- v1_necklace(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B).
v1_neckla_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v7_struct_0(A),v1_orders_2(A).
v1_neckla_3(A) :- l1_orders_2(A),v2_struct_0(A).
v1_neckla_3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v16_waybel_0(A).
v16_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v1_neckla_3(A).
v2_funct_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_zfmisc_1(A).
v1_nfcont_3(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, u1_struct_0(B)))),v1_funct_1(A),v3_funct_1(A).
v1_nfcont_3(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, u1_struct_0(B)))),v1_xboole_0(A),v1_funct_1(A).
v2_nfcont_3(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, u1_struct_0(B)))),v1_xboole_0(A),v1_funct_1(A).
v2_nfcont_3(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, u1_struct_0(B)))),v1_funct_1(A),v3_funct_1(A).
v1_nfcont_3(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v3_normsp_0(B),v4_normsp_0(B),v2_normsp_1(B),l1_normsp_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, u1_struct_0(B)))),v1_funct_1(A),v2_nfcont_3(A, B).
v1_nfcont_4(A, B) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_euclid(B)))),v1_funct_1(A),v3_funct_1(A).
v1_nfcont_4(A, B) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_euclid(B)))),v1_xboole_0(A),v1_funct_1(A).
v2_nfcont_4(A, B) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_euclid(B)))),v1_xboole_0(A),v1_funct_1(A).
v2_nfcont_4(A, B) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_euclid(B)))),v1_funct_1(A),v3_funct_1(A).
v1_nfcont_4(A, B) :- m1_subset_1(B, k5_numbers),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_euclid(B)))),v1_funct_1(A),v2_nfcont_4(A, B).
v3_filter_0(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v13_lattices(A),v1_lattice2(A).
v14_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v3_filter_0(A).
v3_oposet_1(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v4_oposet_1(A).
v3_orders_2(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v6_oposet_1(A).
v4_orders_2(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v6_oposet_1(A).
v5_orders_2(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v6_oposet_1(A).
v6_oposet_1(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A).
v3_necklace(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v8_oposet_1(A).
v3_necklace(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v8_oposet_1(A).
v3_orders_2(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v6_oposet_1(A).
v4_orders_2(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v6_oposet_1(A).
v5_orders_2(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v6_oposet_1(A).
v2_wellord1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v1_orders_1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v2_orders_1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v3_orders_1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v2_orders_2(A) :- l1_orders_2(A),v3_orders_2(A).
v6_orders_2(A, B) :- l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v1_orders_3(A) :- l1_orders_2(A),v2_struct_0(A).
v3_orders_2(A) :- l1_orders_2(A),v2_struct_0(A).
v4_orders_2(A) :- l1_orders_2(A),v2_struct_0(A).
v5_orders_2(A) :- l1_orders_2(A),v2_struct_0(A).
v3_orders_2(A) :- m1_orders_4(A),true.
v4_orders_2(A) :- m1_orders_4(A),true.
v5_orders_2(A) :- m1_orders_4(A),true.
v16_waybel_0(A) :- m1_orders_4(A), ~ (v2_struct_0(A)) .
v16_waybel_0(A) :-  ~ (v2_struct_0(B)) ,v16_waybel_0(B),l1_orders_2(B),m1_yellow_0(A, B), ~ (v2_struct_0(A)) ,v4_yellow_0(A, B).
v8_struct_0(A) :- v8_struct_0(B),l1_orders_2(B),m1_yellow_0(A, B),true.
v1_ordinal1(A) :- true,v3_ordinal1(A).
v2_ordinal1(A) :- true,v3_ordinal1(A).
v3_ordinal1(A) :- true,v1_ordinal1(A),v2_ordinal1(A).
v3_ordinal1(A) :- true,v1_xboole_0(A).
v5_ordinal1(A) :- true,v1_xboole_0(A).
v3_ordinal1(A) :- v3_ordinal1(B),m1_subset_1(A, B),true.
v3_ordinal1(A) :- true,v7_ordinal1(A).
v7_ordinal1(A) :- true,v1_xboole_0(A).
v7_ordinal1(A) :- m1_subset_1(A, k4_ordinal1),true.
v1_ordinal2(A) :- v3_ordinal1(B),v5_ordinal1(A),v1_relat_1(A),v5_relat_1(A, B),v1_funct_1(A).
v3_ordinal1(A) :- v3_ordinal1(B),m1_subset_1(A, B),true.
v5_ordinal1(A) :-  ~ (v1_xboole_0(B)) ,v1_classes2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_ordinal1(B), k2_ordinal1(B)))),v1_funct_1(A),v1_funct_2(A, k2_ordinal1(B), k2_ordinal1(B)).
v1_ordinal2(A) :-  ~ (v1_xboole_0(B)) ,v1_classes2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k2_ordinal1(B), k2_ordinal1(B)))),v1_funct_1(A),v1_funct_2(A, k2_ordinal1(B), k2_ordinal1(B)).
v1_ordinal2(A) :- true,v1_xboole_0(A).
v2_ordinal5(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v1_ordinal2(A),v2_ordinal2(A).
v3_ordinal5(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v1_ordinal2(A),v1_ordinal5(A).
v1_finset_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v1_ordinal5(A).
v2_ordinal2(A) :- true,v1_xboole_0(A).
v1_ordinal5(A) :- true,v1_xboole_0(A).
 ~ (v1_xboole_0(A))  :- true,v3_ordinal1(A),v4_ordinal5(A).
v4_ordinal1(A) :- true,v3_ordinal1(A),v4_ordinal5(A).
 ~ (v1_xboole_0(A))  :- true,v3_ordinal1(A),v5_ordinal5(A).
v6_ordinal5(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A),v5_ordinal1(A),v1_ordinal2(A).
v1_ordinal5(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v1_ordinal2(A),v6_ordinal5(A).
v1_ordinal6(A) :- true,v3_ordinal1(A).
v3_ordinal1(A) :- v1_ordinal6(B),m1_subset_1(A, B),true.
v1_ordinal2(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v2_ordinal6(A).
v2_ordinal2(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v1_ordinal2(A),v2_ordinal6(A).
v3_ordinal2(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v1_ordinal2(A),v2_ordinal6(A).
v2_ordinal6(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_ordinal1(A),v1_ordinal2(A),v2_ordinal2(A),v3_ordinal2(A).
v5_ordinal1(A) :-  ~ (v1_xboole_0(C)) ,v1_classes2(C),v3_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k2_ordinal1(C)))),v1_funct_1(A),v1_funct_2(A, B, k2_ordinal1(C)).
v1_ordinal2(A) :-  ~ (v1_xboole_0(C)) ,v1_classes2(C),v3_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, k2_ordinal1(C)))),v1_funct_1(A),v1_funct_2(A, B, k2_ordinal1(C)).
v5_ordinal1(A) :- v3_ordinal1(B),v3_ordinal1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C).
v1_ordinal2(A) :- v3_ordinal1(B),v3_ordinal1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C).
v3_orders_2(A) :- l3_osalg_1(A),v4_osalg_1(A).
v4_orders_2(A) :- l3_osalg_1(A),v4_osalg_1(A).
v5_orders_2(A) :- l3_osalg_1(A),v4_osalg_1(A).
v5_osalg_1(A) :- l3_osalg_1(A), ~ (v2_struct_0(A)) , ~ (v11_struct_0(A)) ,v6_osalg_1(A).
v7_osalg_1(A, B) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v4_osalg_1(B),v5_osalg_1(B),v8_osalg_1(B),l3_osalg_1(B),m1_subset_1(A, u4_struct_0(B)),true.
v8_osalg_1(A) :- l3_osalg_1(A), ~ (v2_struct_0(A)) , ~ (v11_struct_0(A)) ,v4_osalg_1(A),v5_osalg_1(A),v6_osalg_1(A).
v10_osalg_1(A) :- l3_osalg_1(A), ~ (v2_struct_0(A)) , ~ (v11_struct_0(A)) ,v4_osalg_1(A),v5_osalg_1(A),v6_osalg_1(A),v8_osalg_1(A).
v9_osalg_1(A, B) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v4_osalg_1(B),v5_osalg_1(B),v8_osalg_1(B),v10_osalg_1(B),l3_osalg_1(B),m1_subset_1(A, u4_struct_0(B)),true.
v12_osalg_1(A, B) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v1_orders_3(B),v4_osalg_1(B),v5_osalg_1(B),l3_osalg_1(B),l3_msualg_1(A, B),true.
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v4_osalg_1(B),v5_osalg_1(B),l3_osalg_1(B),m1_subset_1(A, k7_osalg_1(B)),true.
v13_osalg_1(A, B) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v4_osalg_1(B),v5_osalg_1(B),l3_osalg_1(B),v12_osalg_1(C, B),v13_osalg_1(C, B),l3_msualg_1(C, B),m1_msualg_2(A, B, C),v12_osalg_1(A, B).
v11_osalg_1(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),l1_orders_2(B),v1_relat_1(C),v4_relat_1(C, u1_struct_0(B)),v1_funct_1(C),v1_partfun1(C, u1_struct_0(B)),v1_relat_1(D),v4_relat_1(D, u1_struct_0(B)),v1_funct_1(D),v1_partfun1(D, u1_struct_0(B)),m1_msualg_4(A, u1_struct_0(B), C, D),v1_osalg_4(A, B, C, D).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),l1_orders_2(B),m1_subset_1(A, k2_osalg_4(B)),true.
v2_osalg_4(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_orders_3(A).
v1_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v2_osalg_4(B),l1_orders_2(B),m1_subset_1(A, k2_osalg_4(B)),true.
v3_msualg_4(A, B, C) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v4_osalg_1(B),v5_osalg_1(B),l3_osalg_1(B),v4_msualg_1(C, B),v12_osalg_1(C, B),l3_msualg_1(C, B),m1_osalg_4(A, B, C),v2_msualg_4(A, B, C),v3_osalg_4(A, B, C).
v3_osalg_4(A, B, C) :-  ~ (v2_struct_0(B)) , ~ (v11_struct_0(B)) ,v4_osalg_1(B),v5_osalg_1(B),l3_osalg_1(B),v4_msualg_1(C, B),v12_osalg_1(C, B),v13_osalg_1(C, B),l3_msualg_1(C, B),m1_osalg_4(A, B, C),v2_msualg_4(A, B, C),v3_msualg_4(A, B, C).
v1_partfun1(A, B) :- v1_xboole_0(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),true.
 ~ (v1_partfun1(A, B))  :-  ~ (v1_xboole_0(B)) ,v1_xboole_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),true.
v1_relat_2(A) :- true,v1_relat_1(A),v3_relat_2(A),v8_relat_2(A).
v2_relat_1(A) :- true,v1_relat_1(A),v1_partfun3(A).
v4_partfun3(A) :- true,v1_relat_1(A),v1_partfun3(A).
v2_relat_1(A) :- true,v1_relat_1(A),v2_partfun3(A).
v3_partfun3(A) :- true,v1_relat_1(A),v2_partfun3(A).
v2_relat_2(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v5_relat_2(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v8_relat_2(A) :- true,v1_xboole_0(A),v1_relat_1(A).
 ~ (v3_relat_1(A))  :-  ~ (v1_xboole_0(B)) ,v1_relat_1(A),v2_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
 ~ (v2_relat_1(A))  :-  ~ (v1_xboole_0(B)) ,v1_relat_1(A),v3_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
v1_funcop_1(A) :- v1_relat_1(B),v4_relat_1(B, C),v1_funct_1(B),v1_partfun1(B, C),v1_relat_1(D),v4_relat_1(D, C),v1_funct_1(D),v1_partfun1(D, C),m2_pboole(A, C, B, D),true.
 ~ (v3_relat_1(A))  :-  ~ (v1_xboole_0(B)) ,v1_relat_1(A),v2_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
v2_orders_2(A) :- l1_orders_2(A),v2_struct_0(A).
v1_pcs_0(A) :- true,v1_relat_1(A),v1_yellow16(A).
v3_pcs_0(A) :- l1_pcs_0(A),v4_pcs_0(A).
v4_pcs_0(A) :- l1_pcs_0(A),v2_struct_0(A).
v5_pcs_0(A) :- l1_pcs_0(A),v2_struct_0(A).
v6_pcs_0(A) :- l1_pcs_0(A),v2_struct_0(A).
v9_pcs_0(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v10_pcs_0(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v11_pcs_0(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v2_pralg_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v7_pcs_0(A).
v3_orders_2(A) :- l2_pcs_0(A),v14_pcs_0(A).
v4_orders_2(A) :- l2_pcs_0(A),v14_pcs_0(A).
v4_pcs_0(A) :- l2_pcs_0(A),v14_pcs_0(A).
v6_pcs_0(A) :- l2_pcs_0(A),v14_pcs_0(A).
v13_pcs_0(A) :- l2_pcs_0(A),v14_pcs_0(A).
v14_pcs_0(A) :- l2_pcs_0(A),v3_orders_2(A),v4_orders_2(A),v4_pcs_0(A),v6_pcs_0(A),v13_pcs_0(A).
v3_orders_2(A) :- l2_pcs_0(A),v15_pcs_0(A).
v4_orders_2(A) :- l2_pcs_0(A),v15_pcs_0(A).
v5_pcs_0(A) :- l2_pcs_0(A),v15_pcs_0(A).
v6_pcs_0(A) :- l2_pcs_0(A),v15_pcs_0(A).
v13_pcs_0(A) :- l2_pcs_0(A),v15_pcs_0(A).
v15_pcs_0(A) :- l2_pcs_0(A),v3_orders_2(A),v4_orders_2(A),v5_pcs_0(A),v6_pcs_0(A),v13_pcs_0(A).
v1_yellow_1(A) :- true,v1_relat_1(A),v16_pcs_0(A).
v7_pcs_0(A) :- true,v1_relat_1(A),v16_pcs_0(A).
v16_pcs_0(A) :- true,v1_relat_1(A),v17_pcs_0(A).
v5_waybel_3(A) :- true,v1_relat_1(A),v17_pcs_0(A).
v1_pcs_0(A) :- true,v1_relat_1(A),v17_pcs_0(A).
v9_pcs_0(A) :- true,v1_relat_1(A),v17_pcs_0(A).
v11_pcs_0(A) :- true,v1_relat_1(A),v17_pcs_0(A).
v21_pcs_0(A, B) :- l1_pcs_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v1_partfun1(A, k1_numbers) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v2_fdiff_1(A).
v3_pdiff_6(A, B, C) :- v7_ordinal1(B),v7_ordinal1(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_euclid(B), k1_euclid(C)))),v1_funct_1(A),v1_funct_2(A, k1_euclid(B), k1_euclid(C)),v1_pdiff_6(A, C, B),v2_pdiff_6(A, C, B).
v2_lopban_1(A, k4_real_ns1(B), k4_real_ns1(C)) :- m1_subset_1(B, k5_numbers),m1_subset_1(C, k5_numbers),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k4_real_ns1(B)), u1_struct_0(k4_real_ns1(C))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k4_real_ns1(B)), u1_struct_0(k4_real_ns1(C))),v1_grcat_1(A, k4_real_ns1(B), k4_real_ns1(C)),v1_lopban_1(A, k4_real_ns1(B), k4_real_ns1(C)).
v2_pralg_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v11_pencil_1(A).
v4_waybel_3(A) :- true,v1_relat_1(A),v14_pencil_1(A).
v4_waybel_3(A) :- true,v1_relat_1(A),v1_funct_1(A),v15_pencil_1(A).
v11_pencil_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v15_pencil_1(A).
v12_pencil_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v11_pencil_1(A),v15_pencil_1(A).
v14_pencil_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v11_pencil_1(A),v15_pencil_1(A).
v2_relat_1(A) :-  ~ (v1_xboole_0(B)) ,v1_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B), ~ (v13_pencil_1(A)) ,v16_pencil_1(A, B).
v1_finset_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v2_finseq_1(A).
v2_finseq_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A).
v7_struct_0(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v6_struct_0(A),v13_algstr_0(A),v1_vectsp_1(A),v3_vectsp_1(A),v3_rlvect_1(A),v4_rlvect_1(A).
 ~ (v6_struct_0(A))  :- l6_algstr_0(A), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) ,v13_algstr_0(A),v1_vectsp_1(A),v3_vectsp_1(A),v3_rlvect_1(A),v4_rlvect_1(A).
v1_vectsp_2(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v6_algstr_0(A),v33_algstr_0(A),v3_group_1(A),v5_group_1(A),v1_vectsp_1(A),v4_vectsp_1(A),v1_algstr_1(A).
 ~ (v1_polynom2(A, B))  :-  ~ (v1_xboole_0(B)) ,v1_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B),v6_valued_0(A),v2_pre_poly(A),v2_polynom7(A, B).
v1_polynom1(A, k15_pre_poly(B), C) :-  ~ (v2_struct_0(C)) ,l2_struct_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k15_pre_poly(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, k15_pre_poly(B), u1_struct_0(C)),v3_polynom7(A, B, C).
v3_polynom7(A, B, C) :-  ~ (v2_struct_0(C)) ,l2_struct_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k15_pre_poly(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, k15_pre_poly(B), u1_struct_0(C)),v4_polynom7(A, B, C).
v1_vectsp_2(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v33_algstr_0(A),v3_group_1(A),v1_vectsp_1(A),v4_vectsp_1(A),v3_rlvect_1(A),v4_rlvect_1(A).
v1_yellow_0(A) :- l1_orders_2(A),v1_poset_1(A).
v5_orders_3(A, B, C) :-  ~ (v2_struct_0(B)) ,v1_orders_2(B),v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_poset_1(B),l1_orders_2(B), ~ (v2_struct_0(C)) ,v1_orders_2(C),v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),v1_poset_1(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v2_poset_1(A, B, C).
v2_pralg_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_pralg_1(A).
v4_relat_1(A, k9_card_3(B)) :- v4_funct_1(B), ~ (v1_xboole_0(B)) ,v2_card_3(B),m1_subset_1(A, B),true.
v1_partfun1(A, k9_card_3(B)) :- v4_funct_1(B), ~ (v1_xboole_0(B)) ,v2_card_3(B),m1_subset_1(A, B),true.
v1_finset_1(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k5_trees_3(B)),true.
v1_finseq_1(A) :- m1_subset_1(A, k3_finseq_2(B)),true.
v1_finset_1(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(B, k1_zfmisc_1(k5_finsub_1(C))),m1_subset_1(A, B),true.
v1_pre_poly(A) :- m1_finseq_1(A, k3_finseq_2(B)),true.
v1_funcop_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_pre_poly(A).
v1_pre_poly(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A).
v1_card_3(A) :- m1_finseq_1(A, k5_numbers),true.
v2_pre_poly(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finset_1(A).
v2_pre_poly(A) :- v1_finset_1(B),v1_relat_1(A),v4_relat_1(A, B),v1_funct_1(A),v1_partfun1(A, B).
v4_funct_1(A) :- m1_subset_1(A, k1_zfmisc_1(k15_pre_poly(B))),true.
v4_relat_1(A, B) :- m1_subset_1(C, k1_zfmisc_1(k15_pre_poly(B))),m1_subset_1(A, C),true.
v1_partfun1(A, B) :-  ~ (v1_xboole_0(C)) ,m1_subset_1(C, k1_zfmisc_1(k15_pre_poly(B))),m1_subset_1(A, C),true.
v6_valued_0(A) :-  ~ (v1_xboole_0(C)) ,m1_subset_1(C, k1_zfmisc_1(k15_pre_poly(B))),m1_subset_1(A, C),true.
v2_pre_poly(A) :-  ~ (v1_xboole_0(C)) ,m1_subset_1(C, k1_zfmisc_1(k15_pre_poly(B))),m1_subset_1(A, C),true.
v6_valued_0(A) :- v1_funct_1(B),v1_funct_2(B, C, k5_numbers),m1_subset_1(B, k1_zfmisc_1(k2_zfmisc_1(C, k5_numbers))),m1_subset_1(A, k18_pre_poly(C, B)),true.
v1_pre_poly(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(C, k5_numbers),m1_finseq_1(A, k4_finseq_2(C, B)),true.
v2_pre_topc(A) :- v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v4_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v7_pre_topc(A) :- l1_pre_topc(A),v11_pre_topc(A).
v9_pre_topc(A) :- l1_pre_topc(A),v11_pre_topc(A).
v11_pre_topc(A) :- l1_pre_topc(A),v7_pre_topc(A),v9_pre_topc(A).
v7_pre_topc(A) :- l1_pre_topc(A),v12_pre_topc(A).
v10_pre_topc(A) :- l1_pre_topc(A),v12_pre_topc(A).
v12_pre_topc(A) :- l1_pre_topc(A),v7_pre_topc(A),v10_pre_topc(A).
v6_pre_topc(A) :- l1_pre_topc(A),v7_pre_topc(A).
v7_pre_topc(A) :- l1_pre_topc(A),v8_pre_topc(A).
v2_finsub_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))), ~ (v1_xboole_0(A)) ,v1_prob_1(A, B),v4_prob_1(A, B).
v12_valued_0(A) :-  ~ (v1_xboole_0(B)) , ~ (v1_xboole_0(C)) ,v1_prob_1(C, B),v4_prob_1(C, B),m1_subset_1(C, k1_zfmisc_1(k1_zfmisc_1(B))),m2_prob_1(A, B, C),true.
v5_anproj_2(A) :- l1_collsp(A), ~ (v2_struct_0(A)) ,v2_collsp(A),v3_collsp(A),v4_collsp(A),v2_anproj_2(A),v3_anproj_2(A), ~ (v7_anproj_2(A)) .
v1_finseq_1(A) :- v1_relat_1(B),v2_relat_1(B),v1_funct_1(B), ~ (v1_xboole_0(B)) ,v1_finseq_1(B),m1_prvect_1(A, B),true.
v1_finseq_1(A) :- v1_relat_1(B),v2_relat_1(B),v1_funct_1(B), ~ (v1_xboole_0(B)) ,v1_finseq_1(B),m2_prvect_1(A, B),true.
v1_finseq_1(A) :- v1_relat_1(B),v2_relat_1(B),v1_funct_1(B), ~ (v1_xboole_0(B)) ,v1_finseq_1(B),m1_prvect_2(A, B, C),true.
v1_prvect_2(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finseq_1(A),v2_prvect_2(A).
v2_pscomp_1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_compts_1(A).
v3_seq_2(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_pscomp_1(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v1_pscomp_1(A, B).
v5_measure6(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_pscomp_1(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v1_pscomp_1(A, B).
v6_measure6(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_pscomp_1(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), k1_numbers))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), k1_numbers),v1_pscomp_1(A, B).
v2_relat_1(A) :- v1_setfam_1(B),m1_finseq_1(A, B),true.
 ~ (v2_struct_0(A))  :- l1_unialg_1(A),v4_unialg_1(A).
v2_relat_1(A) :- m2_pua2mss1(A, B),true.
v2_funct_1(A) :- m2_pua2mss1(A, B),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,m2_pua2mss1(A, B),true.
v7_ordinal1(A) :- true,v1_pythtrip(A).
v1_finset_1(A) :- m1_pythtrip(A),true.
v4_quantal1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v6_quantal1(A).
v5_quantal1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v6_quantal1(A).
v6_quantal1(A) :- l3_algstr_0(A), ~ (v2_struct_0(A)) ,v4_quantal1(A),v5_quantal1(A).
v9_quantal1(A) :- l2_quantal1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v4_lattice3(A),v7_quantal1(A),v8_quantal1(A).
v10_quantal1(A) :- l2_quantal1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v4_lattice3(A),v7_quantal1(A),v8_quantal1(A).
v1_quaterni(A) :- m1_subset_1(A, u1_struct_0(k15_quatern2)),true.
v1_quaterni(A) :- m1_subset_1(A, u1_struct_0(k16_quatern2)),true.
v1_quaterni(A) :- m1_subset_1(A, k1_quaterni),true.
v3_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v33_algstr_0(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_group_1(A),v5_group_1(A),v4_vectsp_1(A),v5_vectsp_1(A).
v1_vectsp_2(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v33_algstr_0(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_group_1(A),v5_group_1(A),v4_vectsp_1(A),v5_vectsp_1(A).
v1_grcat_1(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_quofield(A, B, C).
v6_group_1(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_quofield(A, B, C).
v1_group_6(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_quofield(A, B, C).
v1_quofield(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_grcat_1(A, B, C),v6_group_1(A, B, C),v1_group_6(A, B, C).
v2_quofield(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v4_quofield(A, B, C).
v3_quofield(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v4_quofield(A, B, C).
v4_quofield(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v2_quofield(A, B, C),v3_quofield(A, B, C).
v1_xreal_0(A) :- true,v1_rat_1(A).
v1_rat_1(A) :- true,v1_int_1(A).
v2_rcomp_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v1_rcomp_1(A).
v3_rcomp_1(A) :- v1_xreal_0(B),m1_rcomp_1(A, B),true.
v3_topmetr(A) :- m1_topmetr(A, k8_metric_1),true.
v6_xxreal_2(A) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k2_topalg_2))),v2_connsp_1(A, k2_topalg_2).
v2_connsp_1(A, k2_topalg_2) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k2_topalg_2))),v6_xxreal_2(A).
v1_xreal_0(A) :- m1_subset_1(A, k1_numbers),true.
v1_realset2(A) :- l6_algstr_0(A), ~ (v6_struct_0(A)) ,v13_algstr_0(A),v33_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v3_group_1(A),v5_group_1(A),v4_vectsp_1(A),v5_vectsp_1(A).
v33_algstr_0(A) :- l6_algstr_0(A), ~ (v6_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_vectsp_1(A),v1_realset2(A).
v3_group_1(A) :- l6_algstr_0(A), ~ (v6_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_vectsp_1(A),v1_realset2(A).
v5_group_1(A) :- l6_algstr_0(A), ~ (v6_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_vectsp_1(A),v1_realset2(A).
v4_vectsp_1(A) :- l6_algstr_0(A), ~ (v6_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_vectsp_1(A),v1_realset2(A).
v1_relat_1(A) :- true,v1_xboole_0(A).
v1_relat_1(A) :- v1_relat_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_relat_1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v2_relat_1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v4_relat_1(A, B) :- v1_relat_1(C),v4_relat_1(C, B),m1_subset_1(A, k1_zfmisc_1(C)),true.
v5_relat_1(A, B) :- v1_relat_1(C),v5_relat_1(C, B),m1_subset_1(A, k1_zfmisc_1(C)),true.
v1_xboole_0(A) :- v1_xboole_0(B),v1_relat_1(A),v4_relat_1(A, B).
v1_xboole_0(A) :- v1_xboole_0(B),v1_relat_1(A),v5_relat_1(A, B).
v1_relat_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),true.
v4_relat_1(A, B) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),true.
v5_relat_1(A, C) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),true.
v1_xboole_0(A) :- v1_xboole_0(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),true.
v1_xboole_0(A) :- v1_xboole_0(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),true.
 ~ (v1_xboole_0(A))  :- v1_relat_1(B),m1_rewrite1(A, B),true.
v2_relat_2(A) :- true,v1_relat_1(A),v3_rewrite1(A).
v1_rewrite1(A) :- true,v1_relat_1(A),v3_rewrite1(A).
v3_rewrite1(A) :- true,v1_relat_1(A),v2_relat_2(A),v1_rewrite1(A).
v2_rewrite1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v3_rewrite1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v2_rewrite1(A) :- true,v1_relat_1(A),v3_rewrite1(A).
v7_rewrite1(A) :- true,v1_relat_1(A),v8_rewrite1(A).
v8_rewrite1(A) :- true,v1_relat_1(A),v7_rewrite1(A).
v9_rewrite1(A) :- true,v1_relat_1(A),v7_rewrite1(A).
v7_rewrite1(A) :- true,v1_relat_1(A),v6_rewrite1(A).
v5_rewrite1(A) :- true,v1_relat_1(A),v8_rewrite1(A).
v4_rewrite1(A) :- true,v1_relat_1(A),v5_rewrite1(A).
v8_rewrite1(A) :- true,v1_relat_1(A),v2_rewrite1(A),v4_rewrite1(A).
v6_rewrite1(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v7_rewrite1(A) :- true,v1_relat_1(A),v3_rewrite1(A),v9_rewrite1(A).
v3_rewrite1(A) :- true,v1_relat_1(A),v10_rewrite1(A).
v7_rewrite1(A) :- true,v1_relat_1(A),v10_rewrite1(A).
v10_rewrite1(A) :- true,v1_relat_1(A),v3_rewrite1(A),v7_rewrite1(A).
v1_rewrite2(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v1_rewrite2(A) :- m1_finseq_1(A, k2_catalan2(B)),true.
v3_seq_2(A) :- true,v1_relat_1(A),v1_funct_1(A),v3_funct_1(A),v3_valued_0(A).
v1_rfunct_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v9_valued_0(A).
v1_rfunct_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v10_valued_0(A).
 ~ (v9_valued_0(A))  :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A), ~ (v1_rfunct_2(A)) .
 ~ (v10_valued_0(A))  :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A), ~ (v1_rfunct_2(A)) .
v3_funct_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v9_valued_0(A),v10_valued_0(A).
v9_valued_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A).
v10_valued_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k1_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A).
v3_valued_0(A) :-  ~ (v1_xboole_0(B)) ,v3_membered(C),m1_subset_1(A, k4_rfunct_3(B, C)),true.
v1_subset_1(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,l4_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_ring_1(A, B).
v1_ring_1(A, B) :-  ~ (v2_struct_0(B)) ,l4_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_ring_1(A, B).
v2_ring_1(A, B) :-  ~ (v2_struct_0(B)) ,l4_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_subset_1(A, u1_struct_0(B)),v1_ring_1(A, B).
v1_subset_1(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_ring_1(A, B).
v3_ring_1(A, B) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_ring_1(A, B).
v4_ring_1(A, B) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_subset_1(A, u1_struct_0(B)),v3_ring_1(A, B).
v2_ring_1(A, B) :-  ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v3_group_1(B),v5_group_1(B),v4_vectsp_1(B),v5_vectsp_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),l6_algstr_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v1_ideal_1(A, B),v2_ideal_1(A, B),v3_ideal_1(A, B),v4_ring_1(A, B).
v1_grcat_1(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_ringcat1(A, B, C).
v6_group_1(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_ringcat1(A, B, C).
v1_group_6(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_ringcat1(A, B, C).
v1_ringcat1(A, B, C) :-  ~ (v2_struct_0(B)) ,l6_algstr_0(B), ~ (v2_struct_0(C)) ,l6_algstr_0(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_grcat_1(A, B, C),v6_group_1(A, B, C),v1_group_6(A, B, C).
v1_rlaffin1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v1_rlaffin1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_rlvect_3(A, B).
v2_rlaffin1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_xboole_0(A).
v1_finset_1(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v1_rlvect_5(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_rlaffin1(A, B).
v4_pre_topc(A, k15_euclid(B)) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))),v2_rusub_4(A, k15_euclid(B)).
v3_rltopsp1(A, B) :-  ~ (v2_struct_0(B)) ,l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v2_rltopsp1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_rltopsp1(A, B).
v9_rltopsp1(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v6_rltopsp1(B),v7_rltopsp1(B),l1_rltopsp1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v8_pre_topc(A) :- l1_rltopsp1(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v7_pre_topc(A),v13_algstr_0(A),v2_rlvect_1(A),v3_rlvect_1(A),v4_rlvect_1(A),v5_rlvect_1(A),v6_rlvect_1(A),v7_rlvect_1(A),v8_rlvect_1(A),v6_rltopsp1(A),v7_rltopsp1(A).
v4_rlvect_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v9_rlvect_1(A).
v9_rlvect_1(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v2_rlvect_1(A),v4_rlvect_1(A).
v12_algstr_0(A) :- l2_algstr_0(A), ~ (v2_struct_0(A)) ,v13_algstr_0(A),v2_rlvect_1(A).
v1_rlvect_5(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v1_rlvect_5(B),l1_rlvect_1(B),m1_rlsub_1(A, B),true.
v14_lattices(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v6_robbins1(A),v7_robbins1(A).
v7_robbins1(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v6_robbins1(A).
v6_robbins1(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v5_robbins1(A),v9_robbins1(A).
v6_robbins1(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v5_robbins1(A).
v6_lattices(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v10_robbins1(A).
v6_robbins1(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v17_lattices(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v6_robbins1(A),v10_robbins1(A).
v17_lattices(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v5_robbins1(A),v10_robbins1(A).
v5_robbins1(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v4_lattices(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v1_robbins2(A).
v5_lattices(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v1_robbins2(A).
v5_robbins1(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v1_robbins2(A).
v1_robbins2(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v5_robbins1(A).
v17_lattices(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v10_robbins1(A),v1_robbins2(A).
v1_robbins2(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v4_lattices(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v2_robbins2(A),v3_robbins2(A).
v5_lattices(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v2_robbins2(A),v3_robbins2(A).
v6_robbins1(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v2_robbins2(A),v3_robbins2(A).
v2_robbins2(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v6_robbins1(A).
v3_robbins2(A) :- l2_robbins1(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v5_lattices(A),v6_robbins1(A).
v17_lattices(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v10_robbins1(A),v2_robbins2(A),v3_robbins2(A).
v2_robbins2(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v3_robbins2(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v1_robbins3(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v2_robbins3(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v3_robbins3(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A).
v10_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v9_lattices(A),v1_robbins3(A),v2_robbins3(A),v3_robbins3(A).
v1_oposet_1(A) :- l2_qmax_1(A), ~ (v2_struct_0(A)) ,v6_oposet_1(A),v10_oposet_1(A).
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,l3_lattices(B),m2_robbins3(A, B),true.
v7_lattices(A) :-  ~ (v2_struct_0(B)) ,v7_lattices(B),l3_lattices(B),m2_robbins3(A, B),true.
v5_lattices(A) :-  ~ (v2_struct_0(B)) ,v5_lattices(B),l3_lattices(B),m2_robbins3(A, B),true.
v6_lattices(A) :-  ~ (v2_struct_0(B)) ,v6_lattices(B),l3_lattices(B),m2_robbins3(A, B),true.
v4_lattices(A) :-  ~ (v2_struct_0(B)) ,v4_lattices(B),l3_lattices(B),m2_robbins3(A, B),true.
v9_lattices(A) :-  ~ (v2_struct_0(B)) ,v9_lattices(B),l3_lattices(B),m2_robbins3(A, B),true.
v8_lattices(A) :-  ~ (v2_struct_0(B)) ,v8_lattices(B),l3_lattices(B),m2_robbins3(A, B),true.
v8_robbins1(A) :- l4_robbins1(A),v13_struct_0(A, 1).
v10_robbins1(A) :- l4_robbins1(A),v13_struct_0(A, 1).
v8_robbins3(A) :- l4_robbins1(A),v13_struct_0(A, 1).
v9_robbins3(A) :- l4_robbins1(A),v13_struct_0(A, 1).
v6_oposet_1(A) :- l2_qmax_1(A),v13_struct_0(A, 1),v3_orders_2(A).
v7_oposet_1(A) :- l2_qmax_1(A),v13_struct_0(A, 1),v3_orders_2(A).
v10_oposet_1(A) :- l2_qmax_1(A),v13_struct_0(A, 1),v3_orders_2(A).
v10_robbins3(A) :- l3_robbins3(A),v13_struct_0(A, 1),v3_orders_2(A).
v11_robbins3(A) :- l3_robbins3(A),v13_struct_0(A, 1),v3_orders_2(A).
v1_lattice3(A) :- l3_robbins3(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v10_robbins3(A).
v2_lattice3(A) :- l3_robbins3(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v10_robbins3(A).
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,l4_robbins1(B),m3_robbins3(A, B),true.
v7_lattices(A) :-  ~ (v2_struct_0(B)) ,v7_lattices(B),l4_robbins1(B),m3_robbins3(A, B),true.
v5_lattices(A) :-  ~ (v2_struct_0(B)) ,v5_lattices(B),l4_robbins1(B),m3_robbins3(A, B),true.
v6_lattices(A) :-  ~ (v2_struct_0(B)) ,v6_lattices(B),l4_robbins1(B),m3_robbins3(A, B),true.
v4_lattices(A) :-  ~ (v2_struct_0(B)) ,v4_lattices(B),l4_robbins1(B),m3_robbins3(A, B),true.
v8_lattices(A) :-  ~ (v2_struct_0(B)) ,v8_lattices(B),l4_robbins1(B),m3_robbins3(A, B),true.
v9_lattices(A) :-  ~ (v2_struct_0(B)) ,v9_lattices(B),l4_robbins1(B),m3_robbins3(A, B),true.
v9_robbins3(A) :-  ~ (v2_struct_0(B)) ,v9_robbins3(B),l4_robbins1(B),m3_robbins3(A, B),true.
v3_orders_2(A) :- l3_robbins3(A), ~ (v2_struct_0(A)) ,v6_lattices(A),v8_lattices(A),v9_lattices(A),v10_robbins3(A).
v4_orders_2(A) :- l3_robbins3(A), ~ (v2_struct_0(A)) ,v5_lattices(A),v10_robbins3(A).
v5_orders_2(A) :- l3_robbins3(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v10_robbins3(A).
v8_robbins3(A) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),v10_robbins1(B),v8_robbins3(B),v9_robbins3(B),l4_robbins1(B),m3_robbins3(A, B),true.
v10_robbins1(A) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),v10_robbins1(B),v8_robbins3(B),v9_robbins3(B),l4_robbins1(B),m3_robbins3(A, B),true.
v12_oposet_1(A) :-  ~ (v2_struct_0(B)) ,v10_lattices(B),v10_robbins1(B),v8_robbins3(B),v9_robbins3(B),l4_robbins1(B),m3_robbins3(A, B),v10_robbins3(A).
v10_robbins1(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v8_robbins3(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v9_robbins3(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A).
v3_robbins3(A) :- l4_robbins1(A),v13_struct_0(A, 1).
v13_lattices(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v10_robbins1(A),v8_robbins3(A),v9_robbins3(A).
v14_lattices(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v10_robbins1(A),v8_robbins3(A),v9_robbins3(A).
v1_robbins4(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v5_lattices(A),v6_lattices(A),v8_lattices(A),v9_lattices(A),v2_robbins4(A).
v2_robbins4(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v5_lattices(A),v6_lattices(A),v8_lattices(A),v9_lattices(A),v1_robbins4(A).
v1_robbins4(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v12_lattices(A),v10_robbins1(A),v8_robbins3(A),v9_robbins3(A).
v9_robbins3(A) :- l4_robbins1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v10_robbins1(A),v1_robbins3(A),v3_robbins3(A),v1_robbins4(A).
 ~ (v7_struct_0(A))  :- l1_orders_2(A),v3_orders_2(A), ~ (v1_orders_3(A)) .
v1_orders_3(A) :- l1_orders_2(A),v7_struct_0(A),v3_orders_2(A).
v1_roughs_1(A) :- l1_orders_2(A),v1_orders_3(A).
 ~ (v1_orders_3(A))  :- l1_orders_2(A), ~ (v1_roughs_1(A)) .
v1_prob_2(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v3_roughs_1(A) :- l1_orders_2(A),v2_roughs_1(A).
 ~ (v4_roughs_1(A, B))  :-  ~ (v2_struct_0(B)) ,v1_orders_3(B),v2_roughs_1(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v1_finset_1(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(B)),v3_card_1(A, 1).
v1_rusub_4(A) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),v2_bhsp_1(B),v1_rusub_4(B),l1_bhsp_1(B),m1_rusub_1(A, B),true.
v1_valued_0(A) :- m1_finseq_1(A, k2_numbers),true.
v1_scm_halt(A) :- true, ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v6_amistd_1(A, k2_tarski(k4_numbers, k3_finseq_2(k4_numbers)), k1_scmfsa_2).
v2_scm_halt(A) :- true, ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v7_amistd_1(A, k2_tarski(k4_numbers, k3_finseq_2(k4_numbers)), k1_scmfsa_2).
v1_scm_halt(A) :- true, ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v2_scm_halt(A).
v1_scm_halt(A) :- true, ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v3_scm_halt(A).
v3_scm_halt(A) :- true, ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v1_scmfsa6b(A).
v3_scm_halt(A) :- true, ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v1_scmfsa7b(A),v1_scm_halt(A).
v6_amistd_1(A, k2_tarski(k4_numbers, k3_finseq_2(k4_numbers)), k1_scmfsa_2) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)), ~ (v1_xboole_0(A)) ,v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v7_amistd_1(A, k2_tarski(k4_numbers, k3_finseq_2(k4_numbers)), k1_scmfsa_2).
v6_amistd_1(A, k2_tarski(k4_numbers, k3_finseq_2(k4_numbers)), k1_scmfsa_2) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)), ~ (v1_xboole_0(A)) ,v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v1_scmfsa6b(A).
v1_scmfsa6b(A) :- true, ~ (v1_xboole_0(A)) ,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmfsa_2)),v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v6_amistd_1(A, k2_tarski(k4_numbers, k3_finseq_2(k4_numbers)), k1_scmfsa_2),v1_scmfsa7b(A).
v1_scmpds_4(A) :- true,v1_relat_1(A),v4_relat_1(A, k5_numbers),v5_relat_1(A, u1_compos_1(k1_scmpds_2)), ~ (v1_xboole_0(A)) ,v1_funct_1(A),v1_finset_1(A),v1_afinsq_1(A),v2_scmpds_4(A).
v1_scmring1(A) :- l1_struct_0(A),v7_struct_0(A).
v1_seq_2(A) :- true,v1_relat_1(A),v1_funct_1(A),v3_valued_0(A),v3_seq_2(A).
v2_seq_2(A) :- true,v1_relat_1(A),v1_funct_1(A),v3_valued_0(A),v3_seq_2(A).
v3_seq_2(A) :- true,v1_relat_1(A),v1_funct_1(A),v3_valued_0(A),v1_seq_2(A),v2_seq_2(A).
v4_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers).
v3_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v4_seq_2(A).
v4_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers).
v4_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v9_valued_0(A),v1_seq_2(A).
v4_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v10_valued_0(A),v2_seq_2(A).
v4_seq_2(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v1_funct_2(A, k5_numbers, k1_numbers),v3_seq_2(A),v1_seqm_3(A).
v9_valued_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A).
v10_valued_0(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v3_funct_1(A).
v3_funct_1(A) :- m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, k1_numbers))),v1_funct_1(A),v9_valued_0(A),v10_valued_0(A).
v6_valued_0(A) :- m1_finseq_1(A, k5_numbers),true.
v3_valued_0(A) :- m1_finseq_1(A, k1_numbers),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_setfam_1(B),m1_subset_1(A, B),true.
 ~ (v1_xboole_0(A))  :- true, ~ (v2_setfam_1(A)) .
 ~ (v2_setfam_1(A))  :- true, ~ (v1_xboole_0(A)) ,v1_setfam_1(A).
 ~ (v2_setfam_1(A))  :- true, ~ (v1_zfmisc_1(A)) .
v4_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v6_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v11_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v1_sheffer1(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v2_sheffer1(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v3_sheffer1(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v4_sheffer1(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A).
v10_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v6_lattices(A),v11_lattices(A),v1_sheffer1(A),v2_sheffer1(A),v3_sheffer1(A),v4_sheffer1(A).
v17_lattices(A) :- l3_lattices(A), ~ (v2_struct_0(A)) ,v4_lattices(A),v6_lattices(A),v11_lattices(A),v1_sheffer1(A),v2_sheffer1(A),v3_sheffer1(A),v4_sheffer1(A).
v10_sheffer1(A) :- l1_sheffer1(A),v13_struct_0(A, 1).
v11_sheffer1(A) :- l1_sheffer1(A),v13_struct_0(A, 1).
v12_sheffer1(A) :- l1_sheffer1(A),v13_struct_0(A, 1).
v4_lattices(A) :- l2_lattices(A),v13_struct_0(A, 1).
v5_lattices(A) :- l2_lattices(A),v13_struct_0(A, 1).
v6_lattices(A) :- l1_lattices(A),v13_struct_0(A, 1).
v7_lattices(A) :- l1_lattices(A),v13_struct_0(A, 1).
v8_lattices(A) :- l3_lattices(A),v13_struct_0(A, 1).
v9_lattices(A) :- l3_lattices(A),v13_struct_0(A, 1).
v17_lattices(A) :- l3_lattices(A),v13_struct_0(A, 1).
v1_sheffer2(A) :- l1_sheffer1(A), ~ (v2_struct_0(A)) ,v7_struct_0(A).
v10_lattices(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v9_sheffer1(A),v10_sheffer1(A),v11_sheffer1(A),v12_sheffer1(A).
v17_lattices(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v9_sheffer1(A),v10_sheffer1(A),v11_sheffer1(A),v12_sheffer1(A).
v10_sheffer1(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A),v9_sheffer1(A).
v11_sheffer1(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A),v9_sheffer1(A).
v12_sheffer1(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A),v9_sheffer1(A).
v1_sheffer2(A) :- l1_sheffer1(A), ~ (v2_struct_0(A)) ,v10_sheffer1(A),v11_sheffer1(A),v12_sheffer1(A).
v10_sheffer1(A) :- l1_sheffer1(A), ~ (v2_struct_0(A)) ,v1_sheffer2(A).
v11_sheffer1(A) :- l1_sheffer1(A), ~ (v2_struct_0(A)) ,v1_sheffer2(A).
v12_sheffer1(A) :- l1_sheffer1(A), ~ (v2_struct_0(A)) ,v1_sheffer2(A).
v10_lattices(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v9_sheffer1(A),v1_sheffer2(A).
v17_lattices(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v9_sheffer1(A),v1_sheffer2(A).
v1_sheffer2(A) :- l3_sheffer1(A), ~ (v2_struct_0(A)) ,v10_lattices(A),v17_lattices(A),v8_robbins1(A),v9_sheffer1(A).
v1_lexbfs(A) :- true,v1_xboole_0(A).
v1_lexbfs(A) :- v1_lexbfs(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v1_classes1(A) :- true,v1_xboole_0(A).
 ~ (v1_xboole_0(A))  :- true, ~ (v1_setfam_1(A)) .
 ~ (v1_setfam_1(A))  :- true, ~ (v1_xboole_0(A)) ,v1_classes1(A).
 ~ (v3_pencil_1(A))  :- l1_pre_topc(A), ~ (v5_simplex0(A)) .
 ~ (v3_pencil_1(A))  :- l1_pre_topc(A), ~ (v4_simplex0(A)) .
 ~ (v5_simplex0(A))  :- l1_pre_topc(A), ~ (v3_pencil_1(A)) ,v4_simplex0(A).
 ~ (v5_simplex0(A))  :- l1_pre_topc(A), ~ (v3_pencil_1(A)) ,v1_matroid0(A).
v1_matroid0(A) :- l1_pre_topc(A),v4_simplex0(A).
v2_simplex0(A) :- l1_pre_topc(A),v4_simplex0(A).
v4_matroid0(A) :- l1_pre_topc(A),v2_simplex0(A).
v3_simplex0(A) :- l1_pre_topc(A),v2_simplex0(A).
v3_matroid0(A) :- l1_pre_topc(A),v1_matroid0(A),v3_simplex0(A).
v1_finset_1(A) :- v2_simplex0(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_tops_2(A, B).
v1_lexbfs(A) :- v3_matroid0(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_tops_2(A, B).
v3_pre_topc(A, B) :-  ~ (v5_simplex0(B)) ,l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_pencil_1(A) :- v3_pencil_1(B),m1_simplex0(B, C),m2_simplex0(A, C, B),true.
v2_simplex0(A) :- v2_simplex0(B),m1_simplex0(B, C),m2_simplex0(A, C, B),true.
v4_matroid0(A) :- v4_matroid0(B),m1_simplex0(B, C),m2_simplex0(A, C, B),true.
v1_finset_1(A) :- true,v1_int_1(A).
v1_xboole_0(A) :-  ~ (v3_pencil_1(B)) ,v1_matroid0(B),l1_pre_topc(B),m3_simplex0(A, B, k2_xxreal_3(1)),true.
 ~ (v4_simplex0(A))  :-  ~ (v2_struct_0(B)) ,l1_rlvect_1(B), ~ (v4_simplex0(C)) ,m1_simplex0(C, u1_struct_0(B)),m1_simplex1(A, B, C),true.
v1_simplex1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),m1_simplex0(A, u1_struct_0(B)),v4_simplex0(A).
v2_simplex1(A, B) :-  ~ (v2_struct_0(B)) ,l1_rlvect_1(B),m1_simplex0(A, u1_struct_0(B)),v4_simplex0(A).
v1_simplex1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),v1_simplex1(C, B),m1_simplex0(C, u1_struct_0(B)),m2_simplex0(A, u1_struct_0(B), C),true.
v2_simplex1(A, B) :-  ~ (v2_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v5_rlvect_1(B),v6_rlvect_1(B),v7_rlvect_1(B),v8_rlvect_1(B),l1_rlvect_1(B),v2_simplex1(C, B),m1_simplex0(C, u1_struct_0(B)),m2_simplex0(A, u1_struct_0(B), C),true.
v6_tbsp_1(A, B) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_finset_1(A).
v1_simplex2(A, B) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),l1_metric_1(B),v1_simplex2(C, B),m1_simplex0(C, D),m2_simplex0(A, D, C),true.
v1_simplex2(A, k14_euclid(B)) :- v7_ordinal1(B),m1_simplex0(A, u1_struct_0(k15_euclid(B))),v2_simplex2(A, B).
v2_simplex2(A, B) :- v7_ordinal1(B),m1_simplex0(A, u1_struct_0(k15_euclid(B))),v2_simplex0(A).
v2_simplex2(A, B) :- v7_ordinal1(B),v2_simplex2(C, B),m1_simplex0(C, u1_struct_0(k15_euclid(B))),m1_simplex1(A, k15_euclid(B), C),true.
v2_compts_1(A, k15_euclid(B)) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(B)))),v4_pre_topc(A, k15_euclid(B)),v1_jordan2c(A, B).
 ~ (v2_sppol_1(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))), ~ (v1_zfmisc_1(A)) ,v1_sppol_1(A).
 ~ (v1_sppol_1(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))), ~ (v1_zfmisc_1(A)) ,v2_sppol_1(A).
 ~ (v1_zfmisc_1(A))  :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v4_topreal1(A).
v2_funct_1(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v4_topreal1(A).
v1_topreal1(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v4_topreal1(A).
v2_topreal1(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v4_topreal1(A).
v3_topreal1(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v4_topreal1(A).
v4_topreal1(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))), ~ (v1_zfmisc_1(A)) ,v2_funct_1(A),v1_topreal1(A),v2_topreal1(A),v3_topreal1(A).
 ~ (v1_xboole_0(A))  :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v4_topreal1(A).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal4(A).
 ~ (v1_zfmisc_1(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal4(A).
v2_compts_1(A, k15_euclid(2)) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal4(A).
 ~ (v3_funct_1(A))  :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))),v1_sprect_1(A).
v1_finseq_6(A, u1_struct_0(k15_euclid(2))) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))), ~ (v1_xboole_0(A)) ,v1_sprect_1(A).
v1_topreal1(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))), ~ (v1_xboole_0(A)) ,v1_sprect_1(A).
v2_topreal1(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))), ~ (v1_xboole_0(A)) ,v1_sprect_1(A).
v1_goboard5(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))), ~ (v1_xboole_0(A)) ,v1_sprect_1(A).
v2_goboard5(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))), ~ (v1_xboole_0(A)) ,v1_sprect_1(A).
v1_sprect_2(A) :- m1_finseq_1(A, u1_struct_0(k15_euclid(2))), ~ (v1_xboole_0(A)) ,v1_finseq_6(A, u1_struct_0(k15_euclid(2))),v1_topreal1(A),v2_topreal1(A),v1_goboard5(A),v1_sprect_1(A).
v1_finseq_1(A) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, u4_struct_0(k8_stacks_1(B))),true.
v7_ordinal1(A) :- v7_ordinal1(B),m1_subset_1(A, B),true.
v7_struct_0(A) :- l1_struct_0(A),v2_struct_0(A).
 ~ (v2_struct_0(A))  :- l1_struct_0(A), ~ (v7_struct_0(A)) .
 ~ (v7_struct_0(A))  :- l4_struct_0(A), ~ (v6_struct_0(A)) .
v8_struct_0(A) :- l1_struct_0(A),v2_struct_0(A).
 ~ (v2_struct_0(A))  :- l1_struct_0(A), ~ (v8_struct_0(A)) .
v8_struct_0(A) :- l1_struct_0(A),v7_struct_0(A).
 ~ (v7_struct_0(A))  :- l1_struct_0(A), ~ (v8_struct_0(A)) .
v13_struct_0(A, k1_xboole_0) :- l1_struct_0(A),v2_struct_0(A).
v2_struct_0(A) :- l1_struct_0(A),v13_struct_0(A, k1_xboole_0).
v13_struct_0(A, 1) :- l1_struct_0(A), ~ (v2_struct_0(A)) ,v7_struct_0(A).
 ~ (v2_struct_0(A))  :- l1_struct_0(A),v13_struct_0(A, 1).
v7_struct_0(A) :- l1_struct_0(A),v13_struct_0(A, 1).
v1_xboole_0(A) :- v1_xboole_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(B)), ~ (v1_subset_1(A, B)) .
v1_subset_1(A, B) :-  ~ (v1_xboole_0(B)) ,m1_subset_1(A, k1_zfmisc_1(B)),v1_xboole_0(A).
 ~ (v1_subset_1(A, B))  :- v1_xboole_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v1_zfmisc_1(A) :- v1_zfmisc_1(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v1_finset_1(A) :- m1_subset_1(A, k1_substlat(B, C)),true.
v4_funct_1(A) :- m1_subset_1(A, k1_substlat(B, C)),true.
v1_supinf_2(A, B, C) :- m1_subset_1(C, k1_zfmisc_1(k7_numbers)),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C),v3_supinf_2(A, B, C).
v2_supinf_2(A, B, C) :- m1_subset_1(C, k1_zfmisc_1(k7_numbers)),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C),v3_supinf_2(A, B, C).
v3_supinf_2(A, B, C) :- m1_subset_1(C, k1_zfmisc_1(k7_numbers)),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C),v1_supinf_2(A, B, C),v2_supinf_2(A, B, C).
v3_taxonom2(A) :- true,v1_zfmisc_1(A).
v3_tbsp_1(A, B) :-  ~ (v2_struct_0(B)) ,v8_metric_1(B),v9_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, k5_numbers, u1_struct_0(B)),v2_tbsp_1(A, B).
v6_tbsp_1(A, B) :-  ~ (v2_struct_0(B)) ,l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v6_tbsp_1(A, B) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),v8_metric_1(B),v9_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_finset_1(A).
v2_pre_topc(A) :- l1_pre_topc(A),v1_tdlat_3(A).
v2_pre_topc(A) :- l1_pre_topc(A),v2_tdlat_3(A).
v3_tdlat_3(A) :- l1_pre_topc(A),v1_tdlat_3(A).
v3_tdlat_3(A) :- l1_pre_topc(A),v2_tdlat_3(A).
v1_borsuk_1(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v1_tsep_1(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v1_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v2_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v3_tdlat_3(A) :- l1_pre_topc(A),v2_pre_topc(A),v1_tdlat_3(A).
v3_tdlat_3(A) :- l1_pre_topc(A),v2_pre_topc(A),v2_tdlat_3(A).
v3_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
v1_borsuk_1(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tsep_1(A, B).
v1_tsep_1(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_borsuk_1(A, B).
v4_tdlat_3(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v5_tdlat_3(A).
v5_tdlat_3(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_tdlat_3(A).
v4_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v4_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tsep_1(A, B).
v5_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v5_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
v1_wellord1(A) :- v7_ordinal1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(k15_pre_poly(B), k15_pre_poly(B)))),v1_partfun1(A, k15_pre_poly(B)),v1_relat_2(A),v4_relat_2(A),v8_relat_2(A),v2_bagorder(A, B).
v1_tdlat_3(A) :- l1_pre_topc(A),v13_struct_0(A, 1),v2_pre_topc(A).
v2_tdlat_3(A) :- l1_pre_topc(A),v13_struct_0(A, 1),v2_pre_topc(A).
v7_struct_0(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A),v2_tdlat_3(A).
 ~ (v7_struct_0(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A), ~ (v1_tdlat_3(A)) .
 ~ (v7_struct_0(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A), ~ (v2_tdlat_3(A)) .
 ~ (v1_tdlat_3(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A), ~ (v3_tdlat_3(A)) .
 ~ (v2_tdlat_3(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A), ~ (v3_tdlat_3(A)) .
v1_xboole_0(A) :- v3_card_1(B, 1),m1_subset_1(A, k1_zfmisc_1(B)),v1_subset_1(A, B).
 ~ (v1_subset_1(A, B))  :- v3_card_1(B, 1),m1_subset_1(A, k1_zfmisc_1(B)), ~ (v1_xboole_0(A)) .
v1_zfmisc_1(A) :- v3_card_1(B, 1),m1_subset_1(A, k1_zfmisc_1(B)), ~ (v1_xboole_0(A)) , ~ (v1_subset_1(A, B)) .
v1_subset_1(A, B) :-  ~ (v1_zfmisc_1(B)) ,m1_subset_1(A, k1_zfmisc_1(B)), ~ (v1_xboole_0(A)) ,v1_zfmisc_1(A).
 ~ (v1_zfmisc_1(A))  :-  ~ (v1_zfmisc_1(B)) ,m1_subset_1(A, k1_zfmisc_1(B)), ~ (v1_xboole_0(A)) , ~ (v1_subset_1(A, B)) .
 ~ (v1_subset_1(A, u1_struct_0(B)))  :- v13_struct_0(B, 1),l1_struct_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) .
v1_zfmisc_1(A) :- v13_struct_0(B, 1),l1_struct_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) , ~ (v1_subset_1(A, u1_struct_0(B))) .
v1_subset_1(A, u1_struct_0(B)) :-  ~ (v7_struct_0(B)) ,l1_struct_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v1_zfmisc_1(A).
 ~ (v1_zfmisc_1(A))  :-  ~ (v7_struct_0(B)) ,l1_struct_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) , ~ (v1_subset_1(A, u1_struct_0(B))) .
 ~ (v1_tex_2(A, B))  :- v13_struct_0(B, 1),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
v7_struct_0(A) :- v13_struct_0(B, 1),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v1_tex_2(A, B)) .
v1_tex_2(A, B) :-  ~ (v7_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v7_struct_0(A).
 ~ (v7_struct_0(A))  :-  ~ (v7_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v1_tex_2(A, B)) .
v2_pre_topc(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B),v1_tdlat_3(A).
v2_pre_topc(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B),v2_tdlat_3(A).
 ~ (v1_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_pre_topc(A)) .
 ~ (v2_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_pre_topc(A)) .
v3_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B),v1_tdlat_3(A).
 ~ (v1_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v3_tdlat_3(A)) .
v3_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B),v2_tdlat_3(A).
 ~ (v2_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v3_tdlat_3(A)) .
v7_struct_0(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tdlat_3(A),v2_tdlat_3(A).
 ~ (v1_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) ,v2_tdlat_3(A).
 ~ (v2_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) ,v1_tdlat_3(A).
v1_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v7_struct_0(A),v2_pre_topc(A).
v2_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v7_struct_0(A),v2_pre_topc(A).
v1_borsuk_1(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_tex_2(A, B)) .
v1_tsep_1(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_tex_2(A, B)) .
v1_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_tsep_1(A, B)) .
v1_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_borsuk_1(A, B)) .
v7_struct_0(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v2_tdlat_3(A).
 ~ (v2_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) .
v7_struct_0(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tdlat_3(A).
 ~ (v1_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) .
 ~ (v1_borsuk_1(A, B))  :-  ~ (v7_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tex_2(A, B).
 ~ (v1_tsep_1(A, B))  :-  ~ (v7_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tex_2(A, B).
v7_struct_0(A) :-  ~ (v7_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tdlat_3(A).
v1_tex_2(A, B) :-  ~ (v7_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tdlat_3(A).
v1_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v4_tex_2(A, B).
 ~ (v4_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v1_tdlat_3(A)) .
 ~ (v1_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v4_tex_2(A, B).
 ~ (v4_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_2(A, B).
v4_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_tex_2(A, B)) .
v1_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v4_tex_2(A, B)) .
v7_struct_0(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v4_tex_2(A, B).
 ~ (v4_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) .
v4_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v7_struct_0(A).
 ~ (v7_struct_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v2_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v4_tex_2(A, B)) .
v1_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B), ~ (v1_tdlat_3(B)) ,v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v4_tex_2(A, B).
 ~ (v4_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B), ~ (v1_tdlat_3(B)) ,v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v1_tex_2(A, B)) .
 ~ (v7_struct_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B), ~ (v2_tdlat_3(B)) ,v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v4_tex_2(A, B).
 ~ (v4_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B), ~ (v2_tdlat_3(B)) ,v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v7_struct_0(A).
 ~ (v1_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_borsuk_1(A, B),v1_tex_3(A, B).
 ~ (v1_borsuk_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_2(A, B),v1_tex_3(A, B).
 ~ (v1_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_borsuk_1(A, B),v1_tex_2(A, B).
v1_tex_3(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v2_tex_3(A, B).
 ~ (v2_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_tex_3(A, B)) .
v2_tex_3(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_tex_2(A, B)) .
v1_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_tex_3(A, B)) .
v2_tex_3(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tsep_1(A, B),v1_tex_3(A, B).
 ~ (v1_tsep_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_3(A, B), ~ (v2_tex_3(A, B)) .
 ~ (v1_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tsep_1(A, B), ~ (v2_tex_3(A, B)) .
 ~ (v3_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tsep_1(A, B).
 ~ (v1_tsep_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v3_tex_3(A, B).
 ~ (v3_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v2_tex_3(A, B).
 ~ (v2_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v3_tex_3(A, B).
v3_tex_3(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v4_tex_3(A, B).
 ~ (v4_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v3_tex_3(A, B)) .
 ~ (v1_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v4_tex_3(A, B).
 ~ (v4_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_3(A, B).
v4_tex_3(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_borsuk_1(A, B),v3_tex_3(A, B).
 ~ (v1_borsuk_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v3_tex_3(A, B), ~ (v4_tex_3(A, B)) .
 ~ (v3_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_borsuk_1(A, B), ~ (v4_tex_3(A, B)) .
 ~ (v3_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
 ~ (v1_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_2(A, B).
 ~ (v1_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_3(A, B).
 ~ (v4_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
 ~ (v2_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_2(A, B).
 ~ (v1_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v2_tex_3(A, B).
 ~ (v1_borsuk_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v3_tex_3(A, B).
 ~ (v3_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_borsuk_1(A, B).
 ~ (v1_tsep_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_2(A, B),v1_tex_3(A, B).
 ~ (v1_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tsep_1(A, B),v1_tex_3(A, B).
 ~ (v1_tex_3(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v3_tdlat_3(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tsep_1(A, B),v1_tex_2(A, B).
v2_tdlat_3(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B),v4_tex_4(A, B).
 ~ (v4_tex_4(A, B))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_tdlat_3(A)) .
v4_tex_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tsep_1(A, B),v2_tdlat_3(A).
 ~ (v2_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_tsep_1(A, B), ~ (v4_tex_4(A, B)) .
 ~ (v1_tsep_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v2_tdlat_3(A), ~ (v4_tex_4(A, B)) .
v4_tex_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_borsuk_1(A, B),v2_tdlat_3(A).
 ~ (v2_tdlat_3(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v1_borsuk_1(A, B), ~ (v4_tex_4(A, B)) .
 ~ (v1_borsuk_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v2_tdlat_3(A), ~ (v4_tex_4(A, B)) .
v1_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v2_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v3_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v4_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v5_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v6_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v7_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v8_relat_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v5_pre_topc(A, k2_borsuk_1(k5_topmetr, k5_topmetr), k15_euclid(B)) :- v7_ordinal1(B),m1_subset_1(C, u1_struct_0(k15_euclid(B))),m1_subset_1(D, u1_struct_0(k15_euclid(B))),m1_borsuk_2(E, k15_euclid(B), C, D),m1_borsuk_2(F, k15_euclid(B), C, D),m1_borsuk_6(A, k15_euclid(B), C, D, E, F),true.
v1_borsuk_2(A) :- m1_subset_1(B, k5_numbers),m1_pre_topc(A, k15_euclid(B)), ~ (v2_struct_0(A)) ,v1_topalg_2(A, B).
v5_pre_topc(A, k2_borsuk_1(k5_topmetr, k5_topmetr), B) :- m1_subset_1(C, k5_numbers), ~ (v2_struct_0(B)) ,v1_topalg_2(B, C),m1_pre_topc(B, k15_euclid(C)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(E, u1_struct_0(B)),m1_borsuk_2(F, B, D, E),m1_borsuk_2(G, B, D, E),m1_borsuk_6(A, B, D, E, F, G),true.
v1_borsuk_2(A) :- m1_pre_topc(A, k2_topalg_2), ~ (v2_struct_0(A)) ,v2_topalg_2(A).
v5_pre_topc(A, k2_borsuk_1(k5_topmetr, k5_topmetr), B) :-  ~ (v2_struct_0(B)) ,v2_topalg_2(B),m1_pre_topc(B, k2_topalg_2),m1_subset_1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_borsuk_2(E, B, C, D),m1_borsuk_2(F, B, C, D),m1_borsuk_6(A, B, C, D, E, F),true.
v1_connsp_1(A) :- l1_pre_topc(A),v2_struct_0(A).
v1_borsuk_2(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v7_struct_0(A),v2_pre_topc(A).
v5_pre_topc(A, k5_topmetr, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(C, u1_struct_0(B)),m1_borsuk_2(A, B, C, C),true.
v1_topdim_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_finset_1(A).
v2_topdim_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_xboole_0(A).
v3_topdim_1(A) :- l1_pre_topc(A),v2_struct_0(A),v2_pre_topc(A).
v1_topdim_1(A, B) :- v2_pre_topc(B),v3_topdim_1(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v1_topdim_2(A, B) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v1_finset_1(A).
v1_topdim_1(A, B) :- v2_pre_topc(B),v3_pcomps_1(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_card_3(A).
v1_orders_4(A) :- l1_struct_0(A),v8_struct_0(A).
v4_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_topgen_1(A, B).
v2_topgen_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_topgen_1(A, B).
v5_topgen_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_pre_topc(A, B),v2_topgen_1(A, B).
v5_topgen_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
 ~ (v2_topgen_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v6_topgen_1(A, B).
 ~ (v6_topgen_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v2_topgen_1(A, B).
v6_topgen_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_pre_topc(A, B) :- v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v4_pre_topc(A, B) :- v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v7_topgen_1(A) :- l1_pre_topc(A),v2_pre_topc(A),v1_orders_4(A).
v1_topgen_2(A) :- l1_pre_topc(A),v8_struct_0(A).
 ~ (v8_struct_0(A))  :- l1_pre_topc(A), ~ (v1_topgen_2(A)) .
v5_waybel23(A) :- l1_pre_topc(A),v8_struct_0(A).
 ~ (v1_finset_1(A))  :- true, ~ (v4_card_3(A)) .
 ~ (v1_zfmisc_1(A))  :- true, ~ (v1_finset_1(A)) .
v7_topgen_1(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v5_waybel23(A).
v3_topgen_4(A, B) :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))), ~ (v1_xboole_0(A)) ,v1_prob_1(A, B),v4_prob_1(A, B).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),v3_topgen_4(A, B).
v2_topgen_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_prob_1(A, u1_struct_0(B)),v1_topgen_4(A, B),v3_topgen_4(A, u1_struct_0(B)).
v4_topgen_4(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_prob_1(A, u1_struct_0(B)),v1_topgen_4(A, B),v3_topgen_4(A, u1_struct_0(B)).
v1_topgen_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_prob_1(A, u1_struct_0(B)),v2_topgen_4(A, B),v4_topgen_4(A, u1_struct_0(B)).
v3_topgen_4(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_prob_1(A, u1_struct_0(B)),v2_topgen_4(A, B),v4_topgen_4(A, u1_struct_0(B)).
v1_tops_2(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_xboole_0(A).
v2_tops_2(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_xboole_0(A).
v4_card_3(A) :- true,v1_xboole_0(A).
v5_topgen_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v6_topgen_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v6_pre_topc(A) :- l1_pre_topc(A),v2_pre_topc(A),v7_topgen_4(A).
v7_topgen_4(A) :- l1_pre_topc(A),v2_pre_topc(A),v7_pre_topc(A).
v3_topgen_4(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))), ~ (v1_xboole_0(A)) ,v1_prob_1(A, u1_struct_0(B)),v4_prob_1(A, u1_struct_0(B)).
v8_topgen_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v5_topgen_4(A, B).
v8_topgen_4(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v6_topgen_4(A, B).
v9_pre_topc(A) :- l1_pre_topc(A),v2_pre_topc(A),v1_topgen_5(A).
v1_topgen_5(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v12_pre_topc(A).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(C, u1_struct_0(B)),m1_connsp_2(A, B, C),true.
v2_funct_1(A) :- l1_pre_topc(B),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v3_tops_2(A, B, C).
v2_funct_2(A, u1_struct_0(C)) :- l1_pre_topc(B),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v3_tops_2(A, B, C).
v5_pre_topc(A, B, C) :- l1_pre_topc(B),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v3_tops_2(A, B, C).
v1_t_0topsp(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B), ~ (v2_struct_0(C)) ,l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v3_tops_2(A, B, C).
v3_tops_2(A, B, B) :- l1_pre_topc(B),m2_topgrp_1(A, B),true.
v1_topgrp_1(A) :- l1_pre_topc(A),v13_struct_0(A, 1).
v2_group_1(A) :- l3_algstr_0(A),v13_struct_0(A, 1).
v3_group_1(A) :- l3_algstr_0(A),v13_struct_0(A, 1).
v5_group_1(A) :- l3_algstr_0(A),v13_struct_0(A, 1).
v1_topgrp_1(A) :- l1_topgrp_1(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v2_group_1(A),v3_group_1(A),v4_topgrp_1(A).
v9_pre_topc(A) :- l1_topgrp_1(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v2_group_1(A),v3_group_1(A),v3_topgrp_1(A),v4_topgrp_1(A).
v3_topmetr(A) :- v3_topmetr(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v5_topreal1(A).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal2(A).
v2_compts_1(A, k15_euclid(2)) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal2(A).
 ~ (v1_sppol_1(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal2(A).
 ~ (v2_sppol_1(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal2(A).
v2_connsp_1(A, B) :- l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))), ~ (v1_sppol_1(A)) .
 ~ (v1_xboole_0(A))  :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))), ~ (v2_sppol_1(A)) .
v3_pre_topc(A, k15_euclid(2)) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v2_topreal4(A, k15_euclid(2)).
v2_connsp_1(A, k15_euclid(2)) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v2_topreal4(A, k15_euclid(2)).
v2_topreal4(A, k15_euclid(2)) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v3_pre_topc(A, k15_euclid(2)),v2_connsp_1(A, k15_euclid(2)).
v1_sppol_1(A) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_xboole_0(A).
v2_sppol_1(A) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_xboole_0(A).
v2_connsp_1(A, k15_euclid(2)) :- m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k15_euclid(2)))),v1_topreal2(A).
v1_rcomp_1(A) :- m1_subset_1(A, k1_zfmisc_1(k1_numbers)),v1_finset_1(A).
v2_monoid_0(A) :- v2_pre_topc(B),v2_monoid_0(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v1_tdlat_3(A) :- l1_pre_topc(A),v2_struct_0(A),v2_pre_topc(A).
v2_tdlat_3(A) :- l1_pre_topc(A),v2_struct_0(A),v2_pre_topc(A).
v5_pre_topc(A, B, C) :- v2_pre_topc(B),v1_tdlat_3(B),l1_pre_topc(B),v2_pre_topc(C),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)).
v5_pre_topc(A, B, C) :- v2_pre_topc(B),l1_pre_topc(B),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_xboole_0(A),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)).
 ~ (v2_struct_0(A))  :- m1_pre_topc(A, k15_euclid(2)),v1_toprealb(A).
v1_compts_1(A) :- m1_pre_topc(A, k15_euclid(2)),v1_toprealb(A).
v1_borsuk_2(A) :- m1_pre_topc(A, k15_euclid(2)),v1_toprealb(A).
v1_xboole_0(A) :- v1_xboole_0(B),m1_subset_1(A, B),true.
v1_toprealc(A) :- l1_struct_0(A),v2_toprealc(A).
v1_toprealc(A) :- v2_pre_topc(B),v1_toprealc(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v2_toprealc(A) :- v2_pre_topc(B),v2_toprealc(B),l1_pre_topc(B),m1_pre_topc(A, B),true.
v3_pre_topc(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v2_tops_1(A, B) :- l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v2_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_tops_1(A, B).
v3_tops_1(A, B) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_pre_topc(A, B),v2_tops_1(A, B).
v1_xboole_0(A) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_pre_topc(A, B),v3_tops_1(A, B).
v1_finset_1(A) :-  ~ (v1_xboole_0(B)) ,v1_finset_1(B),v1_trees_1(B),m4_trees_1(A, B),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_trees_1(B),m1_trees_2(A, B),v2_trees_2(A, B).
v1_trees_3(A) :- true,v1_xboole_0(A).
v2_trees_3(A) :- true,v1_xboole_0(A).
v3_trees_3(A) :- true,v1_xboole_0(A).
v1_trees_3(A) :- true,v2_trees_3(A).
v1_trees_3(A) :- v1_trees_3(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v2_trees_3(A) :- v2_trees_3(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_trees_3(A) :- v3_trees_3(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
 ~ (v1_xboole_0(A))  :-  ~ (v1_xboole_0(B)) ,v1_trees_3(B),m1_subset_1(A, B),true.
v1_trees_1(A) :-  ~ (v1_xboole_0(B)) ,v1_trees_3(B),m1_subset_1(A, B),true.
v1_finset_1(A) :-  ~ (v1_xboole_0(B)) ,v2_trees_3(B),m1_subset_1(A, B),true.
v4_funct_1(A) :- true,v3_trees_3(A).
v3_trees_2(A) :-  ~ (v1_xboole_0(B)) ,v3_trees_3(B),m1_subset_1(A, B),true.
v3_trees_3(A) :-  ~ (v1_xboole_0(B)) ,m1_trees_3(A, B),true.
v5_relat_1(A, B) :-  ~ (v1_xboole_0(B)) , ~ (v1_xboole_0(C)) ,m1_trees_3(C, B),m1_subset_1(A, C),true.
v3_trees_2(A) :-  ~ (v1_xboole_0(B)) ,v1_trees_1(B), ~ (v1_xboole_0(C)) ,m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(B, C))),v1_funct_1(A),v1_funct_2(A, B, C).
v4_trees_3(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A).
v5_trees_3(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A).
v6_trees_3(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_xboole_0(A).
v4_trees_3(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_trees_3(A).
v4_trees_3(A) :-  ~ (v1_xboole_0(B)) ,v1_trees_3(B),m1_finseq_1(A, B),true.
v5_trees_3(A) :-  ~ (v1_xboole_0(B)) ,v2_trees_3(B),m1_finseq_1(A, B),true.
v6_trees_3(A) :-  ~ (v1_xboole_0(B)) ,v3_trees_3(B),m1_finseq_1(A, B),true.
v6_trees_3(A) :-  ~ (v1_xboole_0(B)) ,v3_trees_3(B),m1_subset_1(C, k1_zfmisc_1(B)),m1_finseq_1(A, C),true.
v1_trees_2(A) :- true, ~ (v1_xboole_0(A)) ,v1_finset_1(A),v1_trees_1(A).
v1_trees_9(A) :- true, ~ (v1_xboole_0(A)) ,v1_trees_1(A),v1_trees_2(A).
v2_trees_9(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_finset_1(A),v3_trees_2(A).
v3_trees_9(A) :- true,v1_relat_1(A),v1_funct_1(A),v3_trees_2(A),v2_trees_9(A).
v1_finset_1(A) :-  ~ (v1_xboole_0(B)) , ~ (v1_xboole_0(C)) ,m1_subset_1(C, k1_zfmisc_1(k5_trees_3(B))),m1_subset_1(A, C),true.
v1_finset_1(A) :- v1_relat_1(B),v1_funct_1(B),v1_finset_1(B),v3_trees_2(B),m1_subset_1(A, k3_trees_9(B)),true.
v1_relat_1(A) :- v7_ordinal1(B),m1_subset_1(A, k1_funct_1(k4_triang_1, B)),true.
v1_funct_1(A) :- v7_ordinal1(B),m1_subset_1(A, k1_funct_1(k4_triang_1, B)),true.
v6_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v7_struct_0(A).
v6_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A).
 ~ (v1_tdlat_3(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A), ~ (v6_pre_topc(A)) .
 ~ (v6_pre_topc(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) ,v2_pre_topc(A),v2_tdlat_3(A).
v7_struct_0(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v6_pre_topc(A),v2_tdlat_3(A).
 ~ (v2_tdlat_3(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) , ~ (v7_struct_0(A)) ,v2_pre_topc(A),v6_pre_topc(A).
v1_tdlat_3(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v6_pre_topc(A),v3_tdlat_3(A).
 ~ (v6_pre_topc(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A), ~ (v1_tdlat_3(A)) ,v3_tdlat_3(A).
 ~ (v3_tdlat_3(A))  :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v6_pre_topc(A), ~ (v1_tdlat_3(A)) .
v6_pre_topc(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v6_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
 ~ (v6_pre_topc(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B), ~ (v6_pre_topc(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v1_tex_2(A, B)) .
v1_tex_2(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B), ~ (v6_pre_topc(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v6_pre_topc(A).
v6_pre_topc(A) :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) ,v2_tsp_2(A, B).
 ~ (v2_tsp_2(A, B))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) , ~ (v6_pre_topc(A)) .
v1_tex_3(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v2_tsp_2(A, B).
 ~ (v2_tsp_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v1_tex_3(A, B)) .
 ~ (v1_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tsep_1(A, B),v2_tsp_2(A, B).
 ~ (v2_tsp_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tsep_1(A, B),v1_tex_2(A, B).
 ~ (v1_tsep_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_2(A, B),v2_tsp_2(A, B).
 ~ (v1_tex_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_borsuk_1(A, B),v2_tsp_2(A, B).
 ~ (v2_tsp_2(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_borsuk_1(A, B),v1_tex_2(A, B).
 ~ (v1_borsuk_1(A, B))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B),v1_tex_2(A, B),v2_tsp_2(A, B).
v2_unialg_2(A) :-  ~ (v2_struct_0(B)) ,v2_unialg_1(B),v3_unialg_1(B),v4_unialg_1(B),v2_unialg_2(B),l1_unialg_1(B),m1_unialg_2(A, B),true.
v5_valued_0(A) :- true,v1_relat_1(A),v6_valued_0(A).
v4_valued_0(A) :- true,v1_relat_1(A),v5_valued_0(A).
v3_valued_0(A) :- true,v1_relat_1(A),v4_valued_0(A).
v2_valued_0(A) :- true,v1_relat_1(A),v3_valued_0(A).
v1_valued_0(A) :- true,v1_relat_1(A),v3_valued_0(A).
v6_valued_0(A) :- true,v1_xboole_0(A),v1_relat_1(A).
v1_valued_0(A) :- v1_relat_1(B),v1_valued_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v2_valued_0(A) :- v1_relat_1(B),v2_valued_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_valued_0(A) :- v1_relat_1(B),v3_valued_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v4_valued_0(A) :- v1_relat_1(B),v4_valued_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v5_valued_0(A) :- v1_relat_1(B),v5_valued_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v6_valued_0(A) :- v1_relat_1(B),v6_valued_0(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v1_valued_0(A) :- v1_membered(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),true.
v2_valued_0(A) :- v2_membered(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),true.
v3_valued_0(A) :- v3_membered(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),true.
v4_valued_0(A) :- v4_membered(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),true.
v5_valued_0(A) :- v5_membered(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),true.
v6_valued_0(A) :- v6_membered(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),true.
v7_valued_0(A) :- true,v1_zfmisc_1(A),v1_relat_1(A),v1_funct_1(A),v2_valued_0(A).
v8_valued_0(A) :- true,v1_zfmisc_1(A),v1_relat_1(A),v1_funct_1(A),v2_valued_0(A).
v9_valued_0(A) :- true,v1_relat_1(A),v1_funct_1(A),v2_valued_0(A),v7_valued_0(A).
v10_valued_0(A) :- true,v1_relat_1(A),v1_funct_1(A),v2_valued_0(A),v8_valued_0(A).
v5_relat_1(A, B) :-  ~ (v1_xboole_0(B)) ,v1_relat_1(C),v4_relat_1(C, k5_numbers),v5_relat_1(C, B),v1_funct_1(C),v1_partfun1(C, k5_numbers),m1_valued_0(A, C),true.
v3_funct_1(A) :-  ~ (v1_xboole_0(B)) ,v1_funct_1(C),v3_funct_1(C),v1_funct_2(C, k5_numbers, B),m1_subset_1(C, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, B))),m1_valued_0(A, C),true.
v2_relat_1(A) :-  ~ (v2_setfam_1(B)) ,v2_relat_1(C),v1_funct_1(C),v1_funct_2(C, k5_numbers, B),m1_subset_1(C, k1_zfmisc_1(k2_zfmisc_1(k5_numbers, B))),m1_valued_0(A, C),true.
v1_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k2_numbers).
v2_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k7_numbers).
v3_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k1_numbers).
v4_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k3_numbers).
v5_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k4_numbers).
v6_valued_0(A) :- true,v1_relat_1(A),v5_relat_1(A, k5_numbers).
v5_valued_2(A) :- true,v6_valued_2(A).
v4_valued_2(A) :- true,v5_valued_2(A).
v3_valued_2(A) :- true,v4_valued_2(A).
v1_valued_2(A) :- true,v3_valued_2(A).
v2_valued_2(A) :- true,v3_valued_2(A).
v6_valued_2(A) :- true,v1_xboole_0(A).
v4_funct_1(A) :- true,v1_valued_2(A).
v4_funct_1(A) :- true,v2_valued_2(A).
v1_valued_2(A) :- v1_valued_2(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v2_valued_2(A) :- v2_valued_2(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v3_valued_2(A) :- v3_valued_2(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v4_valued_2(A) :- v4_valued_2(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v5_valued_2(A) :- v5_valued_2(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v6_valued_2(A) :- v6_valued_2(B),m1_subset_1(A, k1_zfmisc_1(B)),true.
v1_valued_0(A) :- v1_valued_2(B),m1_subset_1(A, B),true.
v2_valued_0(A) :- v2_valued_2(B),m1_subset_1(A, B),true.
v3_valued_0(A) :- v3_valued_2(B),m1_subset_1(A, B),true.
v4_valued_0(A) :- v4_valued_2(B),m1_subset_1(A, B),true.
v5_valued_0(A) :- v5_valued_2(B),m1_subset_1(A, B),true.
v6_valued_0(A) :- v6_valued_2(B),m1_subset_1(A, B),true.
v7_valued_2(A) :- v1_valued_2(B),v1_relat_1(A),v5_relat_1(A, B),v1_funct_1(A).
v11_valued_2(A) :- true,v1_relat_1(A),v12_valued_2(A).
v10_valued_2(A) :- true,v1_relat_1(A),v11_valued_2(A).
v9_valued_2(A) :- true,v1_relat_1(A),v10_valued_2(A).
v8_valued_2(A) :- true,v1_relat_1(A),v9_valued_2(A).
v7_valued_2(A) :- true,v1_relat_1(A),v9_valued_2(A).
v12_valued_2(A) :- true,v1_relat_1(A),v1_xboole_0(A).
v7_valued_2(A) :- v1_valued_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A).
v8_valued_2(A) :- v2_valued_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A).
v9_valued_2(A) :- v3_valued_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A).
v10_valued_2(A) :- v4_valued_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A).
v11_valued_2(A) :- v5_valued_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A).
v12_valued_2(A) :- v6_valued_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(C, B))),v1_funct_1(A).
v2_vectmetr(A) :- l1_metric_1(A), ~ (v2_struct_0(A)) ,v6_metric_1(A),v7_metric_1(A),v8_metric_1(A),v9_metric_1(A),v1_vectmetr(A).
v2_funct_1(A) :-  ~ (v2_struct_0(B)) ,v6_metric_1(B),v7_metric_1(B),l1_metric_1(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(B)),v3_vectmetr(A, B).
v2_hahnban1(A, C, B) :-  ~ (v2_struct_0(C)) ,v13_algstr_0(C),v3_rlvect_1(C),v4_rlvect_1(C),l6_algstr_0(C), ~ (v2_struct_0(B)) ,v4_rlvect_1(B),l1_vectsp_1(B, C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_grcat_1(A, B, C).
v2_hahnban1(A, C, B) :-  ~ (v2_struct_0(C)) ,v13_algstr_0(C),v3_rlvect_1(C),v4_rlvect_1(C),v3_group_1(C),v4_vectsp_1(C),v5_vectsp_1(C),l6_algstr_0(C), ~ (v2_struct_0(B)) ,v13_algstr_0(B),v3_rlvect_1(B),v4_rlvect_1(B),v8_vectsp_1(B, C),v9_vectsp_1(B, C),v10_vectsp_1(B, C),v11_vectsp_1(B, C),l1_vectsp_1(B, C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v1_hahnban1(A, C, B).
v3_funct_1(A) :-  ~ (v2_struct_0(C)) , ~ (v6_struct_0(C)) ,v13_algstr_0(C),v33_algstr_0(C),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v3_group_1(C),v5_group_1(C),v4_vectsp_1(C),v5_vectsp_1(C),l6_algstr_0(C), ~ (v2_struct_0(B)) , ~ (v7_struct_0(B)) ,v13_algstr_0(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),v8_vectsp_1(B, C),v9_vectsp_1(B, C),v10_vectsp_1(B, C),v11_vectsp_1(B, C),l1_vectsp_1(B, C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_zfmisc_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)).
 ~ (v3_funct_1(A))  :-  ~ (v2_struct_0(C)) , ~ (v6_struct_0(C)) ,l6_algstr_0(C), ~ (v7_struct_0(B)) ,l1_vectsp_1(B, C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v2_hahnban1(A, C, B), ~ (v2_vectsp10(A, C, B)) .
v1_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v5_vectsp_1(A).
v2_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v5_vectsp_1(A).
v5_vectsp_1(A) :- l6_algstr_0(A), ~ (v2_struct_0(A)) ,v1_vectsp_1(A),v2_vectsp_1(A).
v3_vectsp_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v4_vectsp_1(A).
v6_vectsp_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v4_vectsp_1(A).
v1_group_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v3_vectsp_1(A),v6_vectsp_1(A).
v1_group_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v4_vectsp_1(A).
v3_vectsp_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v5_group_1(A),v6_vectsp_1(A).
v4_vectsp_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v5_group_1(A),v6_vectsp_1(A).
v4_vectsp_1(A) :- l4_algstr_0(A), ~ (v2_struct_0(A)) ,v5_group_1(A),v3_vectsp_1(A).
v1_vectsp_7(A, B, C) :-  ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v33_algstr_0(B),v3_group_1(B),v5_group_1(B),v4_vectsp_1(B),v5_vectsp_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),l6_algstr_0(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v8_vectsp_1(C, B),v9_vectsp_1(C, B),v10_vectsp_1(C, B),v11_vectsp_1(C, B),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),l1_vectsp_1(C, B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(C))),v1_xboole_0(A).
v1_matrlin(A, B) :-  ~ (v2_struct_0(B)) , ~ (v6_struct_0(B)) ,v13_algstr_0(B),v33_algstr_0(B),v3_group_1(B),v5_group_1(B),v4_vectsp_1(B),v5_vectsp_1(B),v2_rlvect_1(B),v3_rlvect_1(B),v4_rlvect_1(B),l6_algstr_0(B), ~ (v2_struct_0(C)) ,v13_algstr_0(C),v8_vectsp_1(C, B),v9_vectsp_1(C, B),v10_vectsp_1(C, B),v11_vectsp_1(C, B),v2_rlvect_1(C),v3_rlvect_1(C),v4_rlvect_1(C),v1_matrlin(C, B),l1_vectsp_1(C, B),m1_vectsp_4(A, B, C),true.
v11_quantal1(A) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(B)),v6_waybel_1(A, B).
v5_orders_3(A, B, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(B)),v6_waybel_1(A, B).
 ~ (v2_struct_0(A))  :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),l1_orders_2(B),m1_yellow_0(A, B),v7_yellow_0(A, B).
 ~ (v2_struct_0(A))  :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),l1_orders_2(B),m1_yellow_0(A, B),v8_yellow_0(A, B).
v1_finset_1(A) :- v8_struct_0(B),l1_struct_0(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v12_waybel_0(A, B) :- l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v13_waybel_0(A, B) :- l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v13_waybel_0(A, B) :- v13_struct_0(B, 1),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v2_waybel11(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_waybel11(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v1_waybel11(A, B) :-  ~ (v2_struct_0(B)) ,v8_struct_0(B),v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v3_waybel11(A, B) :- v2_pre_topc(B),v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),l1_waybel_9(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v12_waybel_0(A, B).
v2_waybel_0(A, B) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v2_lattice3(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v12_waybel_0(A, B),v1_waybel_6(A, B).
v1_waybel_6(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v5_orders_2(B),v3_waybel_3(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v12_waybel_0(A, B).
v17_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,v5_orders_2(B),l1_orders_2(B), ~ (v2_struct_0(C)) ,v5_orders_2(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v23_waybel_0(A, B, C).
v18_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,v5_orders_2(B),l1_orders_2(B), ~ (v2_struct_0(C)) ,v5_orders_2(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v23_waybel_0(A, B, C).
v6_waybel_6(A, B) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v11_waybel_1(B),l1_orders_2(B),m1_subset_1(A, u1_struct_0(B)),v1_waybel15(A, B).
v5_orders_3(A, B, C) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v5_orders_2(B),l1_orders_2(B), ~ (v2_struct_0(C)) ,v3_orders_2(C),v5_orders_2(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v22_waybel_0(A, B, C).
v5_orders_3(A, B, C) :- v2_pre_topc(B),v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v24_waybel_0(B),v4_waybel11(B),l1_waybel_9(B),v2_pre_topc(C),v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),v1_lattice3(C),v2_lattice3(C),v24_waybel_0(C),v4_waybel11(C),l1_waybel_9(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v5_pre_topc(A, B, C).
v22_waybel_0(A, B, C) :- v2_pre_topc(B),v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),v4_waybel11(B),l1_waybel_9(B),v2_pre_topc(C),v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),v1_lattice3(C),v2_lattice3(C),v3_lattice3(C),v4_waybel11(C),l1_waybel_9(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v5_pre_topc(A, B, C).
v5_pre_topc(A, B, C) :- v2_pre_topc(B),v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),v4_waybel11(B),l1_waybel_9(B),v2_pre_topc(C),v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),v1_lattice3(C),v2_lattice3(C),v3_lattice3(C),v4_waybel11(C),l1_waybel_9(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v22_waybel_0(A, B, C).
v2_pralg_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_waybel18(A).
v1_waybel19(A) :- l1_waybel_9(A),v13_struct_0(A, 1),v2_pre_topc(A),v3_orders_2(A).
v6_pre_topc(A) :- l1_waybel_9(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_waybel19(A).
v1_yellow_0(A) :-  ~ (v2_struct_0(B)) ,v1_yellow_0(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v3_waybel_3(A) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v3_waybel_3(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v7_pre_topc(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A),v2_waybel19(A).
v1_compts_1(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A),v2_waybel19(A).
v8_pre_topc(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A),v3_waybel_3(A),v2_waybel19(A).
v5_orders_3(A, B, C) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v2_lattice3(B),l1_orders_2(B),v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),v2_lattice3(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v19_waybel_0(A, B, C).
v19_waybel_0(A, B, C) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v2_lattice3(B),l1_orders_2(B),v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),v2_yellow_0(C),v2_lattice3(C),l1_orders_2(C),m1_waybel21(A, B, C),true.
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v2_yellow_0(B),l1_orders_2(B),m1_subset_1(A, u1_struct_0(k2_yellow_1(k8_waybel_0(B)))),true.
v1_waybel23(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_waybel23(A, B).
v2_waybel23(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v4_waybel23(A, B).
v2_waybel_0(A, B) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v2_lattice3(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_waybel23(A, B).
v1_waybel_0(A, B) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v2_waybel23(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v6_waybel23(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v7_waybel23(A, B).
v2_waybel23(A, B) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v3_waybel_3(B),l1_orders_2(B),m1_waybel23(A, B),true.
 ~ (v1_xboole_0(A))  :-  ~ (v7_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v1_yellow_0(B),v3_waybel_3(B),l1_orders_2(B),m1_waybel23(A, B),true.
v5_orders_3(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,v3_orders_2(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v3_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)).
v6_pre_topc(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A),v4_waybel11(A).
v2_waybel18(A) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),v3_waybel_3(B),l1_orders_2(B),m1_yellow_9(A, B),v4_waybel11(A).
v5_orders_3(A, k1_waybel25(B), k1_waybel25(C)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B), ~ (v2_struct_0(C)) ,v2_pre_topc(C),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k1_waybel25(B)), u1_struct_0(k1_waybel25(C))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k1_waybel25(B)), u1_struct_0(k1_waybel25(C))),v5_pre_topc(A, k1_waybel25(B), k1_waybel25(C)).
v13_waybel_0(A, k1_waybel25(B)) :- v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k1_waybel25(B)))),v3_pre_topc(A, k1_waybel25(B)).
v1_yellow14(A, k3_waybel24(B, k1_waybel25(C))) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B), ~ (v2_struct_0(C)) ,v2_pre_topc(C),l1_pre_topc(C),l1_waybel_0(A, k3_waybel24(B, k1_waybel25(C))), ~ (v2_struct_0(A)) ,v4_orders_2(A),v7_waybel_0(A).
v1_waybel25(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v7_struct_0(A),v2_pre_topc(A),v6_pre_topc(A).
v1_waybel25(A) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),l1_orders_2(B),m1_yellow_9(A, B),v4_waybel11(A).
v3_yellow_6(A, k1_waybel25(B)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_waybel25(B),l1_pre_topc(B),l1_waybel_0(A, k1_waybel25(B)), ~ (v2_struct_0(A)) ,v4_orders_2(A),v7_waybel_0(A),v10_waybel_0(A, k1_waybel25(B)).
v22_waybel_0(A, k1_waybel25(B), k1_waybel25(C)) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v1_waybel25(B),l1_pre_topc(B), ~ (v2_struct_0(C)) ,v2_pre_topc(C),v6_pre_topc(C),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(k1_waybel25(B)), u1_struct_0(k1_waybel25(C))))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(k1_waybel25(B)), u1_struct_0(k1_waybel25(C))),v5_pre_topc(A, k1_waybel25(B), k1_waybel25(C)).
v1_waybel25(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v6_pre_topc(A),v2_waybel18(A).
v1_relat_1(A) :- v2_pre_topc(B),l1_pre_topc(B),v2_pre_topc(C),l1_pre_topc(C),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(k2_borsuk_1(B, C)))),true.
v1_relat_1(A) :- v2_pre_topc(B),l1_pre_topc(B),v2_pre_topc(C),l1_pre_topc(C),m1_subset_1(A, u1_pre_topc(k2_borsuk_1(B, C))),true.
v1_waybel27(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v2_waybel27(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v3_waybel27(A) :- true,v1_xboole_0(A),v1_relat_1(A),v1_funct_1(A).
v1_monoid_0(A) :-  ~ (v2_struct_0(B)) ,v1_monoid_0(B),l1_orders_2(B),m1_yellow_0(A, B), ~ (v2_struct_0(A)) .
v6_pre_topc(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v4_waybel11(A),v3_waybel_3(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A).
v2_waybel18(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v4_waybel11(A),v3_waybel_3(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A).
v24_waybel_0(A) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v24_waybel_0(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v2_pre_topc(A) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v24_waybel_0(B),l1_orders_2(B),m1_yellow_9(A, B),v4_waybel11(A).
v7_struct_0(A) :- v13_struct_0(B, 1),l1_orders_2(B),m1_yellow_9(A, B),true.
v4_waybel11(A) :- l1_waybel_9(A),v13_struct_0(A, 1),v2_pre_topc(A),v3_orders_2(A).
v2_waybel19(A) :- l1_waybel_9(A),v7_struct_0(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A).
v8_pre_topc(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A),v2_waybel19(A),v3_waybel_3(A).
v3_waybel_3(A) :- l1_waybel_9(A),v2_pre_topc(A),v8_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A),v2_waybel19(A),v2_waybel_2(A).
v1_waybel30(A) :- l1_waybel_9(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v3_waybel30(A).
v1_waybel30(A) :- l1_waybel_9(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v2_waybel30(A).
v1_waybel30(A) :- l1_waybel_9(A), ~ (v2_struct_0(A)) ,v2_tdlat_3(A).
v3_waybel30(A) :- l1_waybel_9(A), ~ (v2_struct_0(A)) ,v2_tdlat_3(A).
v2_waybel30(A) :- l1_waybel_9(A),v13_struct_0(A, 1),v2_pre_topc(A),v3_orders_2(A).
v3_waybel30(A) :- l1_waybel_9(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_lattice3(A),v2_waybel19(A),v3_waybel_3(A).
v3_waybel_8(A) :- l1_orders_2(A),v8_struct_0(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A).
v1_waybel32(A) :- l1_waybel_9(A),v13_struct_0(A, 1),v2_pre_topc(A),v3_orders_2(A).
v2_pre_topc(A) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v24_waybel_0(B),l1_orders_2(B),m1_yellow_9(A, B),v4_waybel11(A).
v24_waybel_0(A) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v24_waybel_0(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v3_waybel11(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v24_waybel_0(B),l1_waybel_9(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v12_waybel_0(A, B).
v2_pre_topc(A) :- l1_waybel_9(A), ~ (v2_struct_0(A)) ,v1_waybel33(A).
v1_waybel33(A) :- l1_waybel_9(A),v7_struct_0(A),v2_pre_topc(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A).
v25_waybel_0(A) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v25_waybel_0(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v2_lattice3(A) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v2_lattice3(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v5_waybel34(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v18_waybel_0(A, B, C).
v6_yellow_0(A, B) :-  ~ (v2_struct_0(B)) ,v5_orders_2(B),v1_yellow_0(B),l1_orders_2(B),m1_yellow_0(A, B),v6_waybel34(A, B).
v7_waybel34(A, B) :-  ~ (v2_struct_0(B)) ,v5_orders_2(B),v1_yellow_0(B),l1_orders_2(B),m1_yellow_0(A, B),v6_waybel34(A, B).
v6_waybel34(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_yellow_0(A, B),v8_yellow_0(A, B).
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,v5_orders_2(B),v1_yellow_0(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B),v7_waybel34(A, B).
v1_yellow_0(A) :-  ~ (v2_struct_0(B)) ,v5_orders_2(B),v1_yellow_0(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B),v7_waybel34(A, B).
v6_waybel34(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_yellow_0(B),v1_lattice3(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B),v6_yellow_0(A, B),v7_waybel34(A, B).
v4_waybel34(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v18_waybel_0(A, B, C).
v20_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v4_waybel34(A, B, C).
v5_waybel34(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v4_waybel34(A, B, C).
v1_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_waybel35(A, B).
v2_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_waybel35(A, B).
v4_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_waybel35(A, B).
v1_waybel35(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_waybel_4(A, B),v2_waybel_4(A, B),v4_waybel_4(A, B).
v5_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v3_waybel_4(A, B),v1_waybel35(A, B).
v1_waybel35(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v5_waybel_4(A, B).
v3_waybel35(A, B, C) :- l1_orders_2(B),m1_subset_1(C, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),m1_waybel35(A, B, C),v1_zfmisc_1(A).
v1_waybel_0(A, B) :- l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v2_waybel_0(A, B) :- l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v3_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_yellow_0(A, B),v7_yellow_0(A, B).
v4_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_yellow_0(A, B),v8_yellow_0(A, B).
v10_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),l1_waybel_0(A, B), ~ (v2_struct_0(A)) ,v7_waybel_0(A),v8_waybel_0(A, B).
v11_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),l1_waybel_0(A, B), ~ (v2_struct_0(A)) ,v7_waybel_0(A),v9_waybel_0(A, B).
v16_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v7_struct_0(A),v3_orders_2(A).
v19_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v17_waybel_0(A, B, C).
v21_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v17_waybel_0(A, B, C).
v20_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v18_waybel_0(A, B, C).
v22_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v18_waybel_0(A, B, C).
v2_funct_1(A) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v23_waybel_0(A, B, C).
v5_orders_3(A, B, C) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B), ~ (v2_struct_0(C)) ,l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v23_waybel_0(A, B, C).
v2_yellow_0(A) :- l1_orders_2(A),v3_orders_2(A),v1_lattice3(A),v24_waybel_0(A).
v24_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v3_lattice3(A).
v25_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v3_lattice3(A).
v1_yellow_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v25_waybel_0(A).
v3_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v1_yellow_0(A),v24_waybel_0(A).
v2_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v5_orders_2(A),v25_waybel_0(A).
v1_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v5_orders_2(A),v2_yellow_0(A),v25_waybel_0(A).
v17_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),l1_orders_2(B), ~ (v2_struct_0(C)) ,v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v4_waybel_1(A, B, C).
v18_waybel_0(A, B, C) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),l1_orders_2(B), ~ (v2_struct_0(C)) ,v3_orders_2(C),v4_orders_2(C),v5_orders_2(C),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v5_waybel_1(A, C, B).
v6_waybel_1(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(B)),v7_waybel_1(A, B).
v6_waybel_1(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(B)),v8_waybel_1(A, B).
v3_orders_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v9_waybel_1(A).
v4_orders_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v9_waybel_1(A).
v5_orders_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v9_waybel_1(A).
v1_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v9_waybel_1(A).
v2_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v9_waybel_1(A).
v2_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v9_waybel_1(A).
v2_yellow_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v9_waybel_1(A).
v3_orders_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v4_orders_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v5_orders_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v1_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v2_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v3_yellow_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v2_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v10_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v11_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_yellow_0(A),v2_waybel_1(A),v10_waybel_1(A).
v9_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v11_waybel_1(A).
v1_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v16_waybel_0(A).
v2_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v16_waybel_0(A).
v2_waybel_1(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v10_waybel_1(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v1_waybel_2(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v24_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v2_waybel_2(A).
v1_waybel_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v2_waybel_2(A).
v2_waybel_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v24_waybel_0(A),v1_waybel_2(A).
v2_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v9_waybel_1(A),v3_lattice3(A).
v2_waybel_2(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v9_waybel_1(A),v3_lattice3(A).
v9_waybel_1(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v2_waybel_1(A),v3_lattice3(A),v2_waybel_2(A).
v1_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v16_waybel_0(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v2_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v16_waybel_0(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),true.
v3_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_yellow_0(A),v16_waybel_0(A),v24_waybel_0(A).
v2_waybel_3(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v24_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v3_waybel_3(A).
v2_waybel_3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v3_waybel_3(A).
v3_waybel_3(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_yellow_0(A),v1_lattice3(A),v24_waybel_0(A),v2_waybel_3(A).
v2_waybel_3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v3_lattice3(A),v16_waybel_0(A).
v9_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v8_pre_topc(A),v1_compts_1(A).
v10_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v8_pre_topc(A),v1_compts_1(A).
v6_waybel_3(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v8_pre_topc(A),v1_compts_1(A).
v1_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v5_waybel_4(A, B).
v2_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v5_waybel_4(A, B).
v3_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v5_waybel_4(A, B).
v4_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v5_waybel_4(A, B).
v5_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_waybel_4(A, B),v2_waybel_4(A, B),v3_waybel_4(A, B),v4_waybel_4(A, B).
v8_relat_2(A) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_yellow_0(B),v1_lattice3(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v1_waybel_4(A, B),v2_waybel_4(A, B).
v2_waybel_2(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_yellow_0(A),v1_lattice3(A),v2_lattice3(A),v3_waybel_3(A).
v9_waybel_4(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v8_waybel_4(A, B).
v8_waybel_4(A, B) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(B)))),v5_waybel_4(A, B),v6_waybel_4(A, B),v9_waybel_4(A, B).
v2_relat_1(A) :-  ~ (v1_xboole_0(B)) ,v1_relat_1(C),v2_relat_1(C),v4_relat_1(C, D),v1_funct_1(C),v1_partfun1(C, D),m2_pboole(A, D, C, k7_funcop_1(D, B)),true.
v1_waybel_5(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A).
v3_lattice3(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v1_waybel_5(A).
v3_waybel_3(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v1_waybel_5(A).
v9_waybel_1(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v1_waybel_5(A).
v1_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v6_orders_2(A, B).
v2_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v6_orders_2(A, B).
v1_subset_1(A, u1_struct_0(B)) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v2_waybel_0(A, B),v13_waybel_0(A, B),v3_waybel_7(A, B).
v3_waybel_3(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v2_waybel_8(A).
v24_waybel_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v2_waybel_8(A).
v1_waybel_8(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v2_waybel_8(A).
v2_waybel_8(A) :- l1_orders_2(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A),v3_waybel_8(A).
v3_waybel_8(A) :- l1_orders_2(A),v7_struct_0(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A).
v8_pre_topc(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v8_pre_topc(B),l1_pre_topc(B),m1_pre_topc(A, B), ~ (v2_struct_0(A)) .
v5_valued_0(A) :- m1_finseq_1(A, k4_numbers),true.
v3_valued_0(A) :- m1_finseq_1(A, k5_numbers),true.
v3_valued_0(A) :- true,v1_relat_1(A),v1_funct_1(A),v5_valued_0(A).
v7_ordinal1(A) :- true,v1_xboolean(A).
v1_xcmplx_0(A) :- m1_subset_1(A, k1_numbers),true.
v1_xcmplx_0(A) :- true,v7_ordinal1(A).
v1_xcmplx_0(A) :- m1_subset_1(A, k2_numbers),true.
v1_xreal_0(A) :- true,v7_ordinal1(A).
v1_xcmplx_0(A) :- true,v1_xreal_0(A).
v1_xxreal_0(A) :- true,v1_xreal_0(A).
v1_xxreal_0(A) :- m1_subset_1(A, k1_numbers),true.
v1_xxreal_0(A) :- m1_subset_1(A, k7_numbers),true.
v1_xxreal_0(A) :- true,v7_ordinal1(A).
 ~ (v1_xboole_0(A))  :- true,v1_xxreal_0(A),v2_xxreal_0(A).
 ~ (v3_xxreal_0(A))  :- true,v1_xxreal_0(A),v2_xxreal_0(A).
v2_xxreal_0(A) :- true, ~ (v1_xboole_0(A)) ,v1_xxreal_0(A), ~ (v3_xxreal_0(A)) .
 ~ (v1_xboole_0(A))  :- true,v1_xxreal_0(A),v3_xxreal_0(A).
 ~ (v2_xxreal_0(A))  :- true,v1_xxreal_0(A),v3_xxreal_0(A).
v3_xxreal_0(A) :- true, ~ (v1_xboole_0(A)) ,v1_xxreal_0(A), ~ (v2_xxreal_0(A)) .
 ~ (v2_xxreal_0(A))  :- true,v1_xboole_0(A),v1_xxreal_0(A).
 ~ (v3_xxreal_0(A))  :- true,v1_xboole_0(A),v1_xxreal_0(A).
v1_xboole_0(A) :- true,v1_xxreal_0(A), ~ (v2_xxreal_0(A)) , ~ (v3_xxreal_0(A)) .
v2_membered(A) :- m1_subset_1(A, k5_finsub_1(k7_numbers)),true.
v1_xxreal_2(A) :- true,v2_membered(A), ~ (v1_xboole_0(A)) ,v1_finset_1(A).
v2_xxreal_2(A) :- true,v2_membered(A), ~ (v1_xboole_0(A)) ,v1_finset_1(A).
v1_xxreal_2(A) :- true,v6_membered(A), ~ (v1_xboole_0(A)) .
v3_xxreal_2(A) :- true,v2_membered(A),v5_xxreal_2(A).
v4_xxreal_2(A) :- true,v2_membered(A),v5_xxreal_2(A).
v5_xxreal_2(A) :- true,v2_membered(A),v3_xxreal_2(A),v4_xxreal_2(A).
v5_xxreal_2(A) :- true,v3_membered(A),v1_finset_1(A).
v2_xxreal_2(A) :- true,v5_membered(A), ~ (v1_xboole_0(A)) ,v4_xxreal_2(A).
v1_xxreal_2(A) :- true,v5_membered(A), ~ (v1_xboole_0(A)) ,v3_xxreal_2(A).
v3_xxreal_2(A) :- true,v6_membered(A).
v3_xxreal_2(A) :- true,v3_membered(A),v1_xxreal_2(A).
v4_xxreal_2(A) :- true,v3_membered(A),v2_xxreal_2(A).
v1_finset_1(A) :- true,v5_membered(A),v5_xxreal_2(A).
v1_finset_1(A) :- true,v6_membered(A),v4_xxreal_2(A).
v3_membered(A) :- true,v2_membered(A),v5_xxreal_2(A).
v6_xxreal_2(A) :- true,v2_membered(A),v1_xboole_0(A).
 ~ (v1_xboole_0(A))  :- true,v2_membered(A),v1_xxreal_2(A).
 ~ (v1_xboole_0(A))  :- true,v2_membered(A),v2_xxreal_2(A).
v2_xxreal_0(A) :- true,v1_xxreal_0(A), ~ (v3_xxreal_0(A)) , ~ (v1_xreal_0(A)) .
v3_xxreal_0(A) :- true,v1_xxreal_0(A), ~ (v2_xxreal_0(A)) , ~ (v1_xreal_0(A)) .
v1_waybel_6(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_xboole_0(A).
v1_yellow11(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v5_orders_2(A),v2_lattice3(A),v2_waybel_1(A).
v1_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v2_yellow11(A, B).
v2_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_orders_2(B),l1_orders_2(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))), ~ (v1_xboole_0(A)) ,v2_yellow11(A, B).
v1_yellow_0(A) :- l1_orders_2(A),v8_struct_0(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v2_lattice3(A).
v3_lattice3(A) :- l1_orders_2(A),v8_struct_0(A),v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v1_lattice3(A),v2_lattice3(A).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_tops_2(A, B),v1_cantor_1(A, B).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(C, u1_struct_0(B)),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),v1_tops_2(A, B),v1_yellow_8(A, B, C).
v4_pre_topc(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v7_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v1_finset_1(A).
v1_tdlat_3(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v8_struct_0(A),v2_pre_topc(A),v7_pre_topc(A).
v1_compts_1(A) :- l1_pre_topc(A),v8_struct_0(A),v2_pre_topc(A).
v7_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A).
v8_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A).
v9_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A).
v10_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v1_tdlat_3(A).
v9_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v12_pre_topc(A).
v8_pre_topc(A) :- l1_pre_topc(A),v2_pre_topc(A),v11_pre_topc(A).
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m1_subset_1(C, u1_struct_0(B)),m1_yellow13(A, B, C),true.
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),m2_yellow13(A, B),true.
v2_yellow13(A) :- l1_waybel_9(A),v13_struct_0(A, 1),v2_pre_topc(A),v3_orders_2(A).
v1_yellow14(A, B) :- v1_monoid_0(B),l1_struct_0(B),l1_waybel_0(A, B),true.
v2_funct_2(A, u1_struct_0(C)) :- l1_orders_2(B),l1_orders_2(C),m1_subset_1(A, k1_zfmisc_1(k2_zfmisc_1(u1_struct_0(B), u1_struct_0(C)))),v1_funct_1(A),v1_funct_2(A, u1_struct_0(B), u1_struct_0(C)),v23_waybel_0(A, B, C).
v3_yellow_0(A) :- l1_orders_2(A),v2_struct_0(A).
v13_waybel_0(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_waybel11(B),l1_waybel_9(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_pre_topc(A, B).
v1_waybel11(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_waybel11(B),l1_waybel_9(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_pre_topc(A, B).
v3_pre_topc(A, B) :-  ~ (v2_struct_0(B)) ,v3_orders_2(B),v4_waybel11(B),l1_waybel_9(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v13_waybel_0(A, B),v1_waybel11(A, B).
v1_finset_1(A) :- v1_finset_1(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(B))),true.
v1_finset_1(A) :- v8_struct_0(B),l1_struct_0(B),m1_subset_1(A, k1_zfmisc_1(k1_zfmisc_1(u1_struct_0(B)))),true.
v1_margrel1(A) :- m1_finseq_1(A, k6_margrel1),true.
v1_yellow_1(A) :- true,v1_relat_1(A),v1_yellow16(A).
v5_waybel_3(A) :- true,v1_relat_1(A),v1_yellow16(A).
v2_yellow18(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v8_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A).
v9_altcat_1(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A),v4_yellow18(A).
v2_yellow18(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A),v4_yellow18(A).
v3_yellow18(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A),v4_yellow18(A).
v4_yellow18(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v9_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A),v2_yellow18(A),v3_yellow18(A).
v1_relat_1(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v2_yellow18(B),l2_altcat_1(B),m1_subset_1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_altcat_1(B, C, D)),true.
v1_funct_1(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v2_yellow18(B),l2_altcat_1(B),m1_subset_1(C, u1_struct_0(B)),m1_subset_1(D, u1_struct_0(B)),m1_subset_1(A, k1_altcat_1(B, C, D)),true.
 ~ (v1_xboole_0(A))  :-  ~ (v2_struct_0(B)) ,l1_struct_0(B), ~ (v2_struct_0(C)) ,v3_orders_2(C),l1_waybel_0(C, B),m1_yellow19(A, B, C),true.
v4_orders_2(A) :-  ~ (v2_struct_0(B)) ,l1_struct_0(B),v4_orders_2(C),l1_waybel_0(C, B),m1_yellow_6(A, B, C),v2_yellow_6(A, B, C).
v2_yellow21(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A),v3_yellow21(A).
v4_yellow18(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A),v2_yellow21(A).
v1_yellow21(A) :- l2_altcat_1(A), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v11_altcat_1(A),v12_altcat_1(A),v2_yellow21(A).
v1_wellord1(A) :- true,v1_relat_1(A),v2_wellord1(A).
v1_relat_2(A) :- true,v1_relat_1(A),v2_wellord1(A).
v4_relat_2(A) :- true,v1_relat_1(A),v2_wellord1(A).
v6_relat_2(A) :- true,v1_relat_1(A),v2_wellord1(A).
v8_relat_2(A) :- true,v1_relat_1(A),v2_wellord1(A).
v3_yellow18(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v3_yellow18(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v3_altcat_2(A, B).
v2_yellow18(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v2_yellow18(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v3_altcat_2(A, B).
v9_altcat_1(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v9_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A).
v1_yellow21(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v1_yellow21(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v3_altcat_2(A, B).
v2_yellow21(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v2_yellow21(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v3_altcat_2(A, B).
v4_yellow21(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v2_yellow21(B),v4_yellow21(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v2_altcat_2(A, B),v3_altcat_2(A, B).
v3_yellow21(A) :-  ~ (v2_struct_0(B)) ,v2_altcat_1(B),v11_altcat_1(B),v12_altcat_1(B),v3_yellow21(B),l2_altcat_1(B),m1_altcat_2(A, B), ~ (v2_struct_0(A)) ,v2_altcat_1(A),v3_altcat_2(A, B).
v1_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_lattice3(A).
v2_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_lattice3(A).
v4_orders_2(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v5_orders_2(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v3_lattice3(A) :- l1_orders_2(A),v13_struct_0(A, 1),v3_orders_2(A).
v3_yellow_0(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_lattice3(A).
v1_yellow_0(A) :- l1_orders_2(A),v3_yellow_0(A).
v2_yellow_0(A) :- l1_orders_2(A),v3_yellow_0(A).
v3_yellow_0(A) :- l1_orders_2(A),v1_yellow_0(A),v2_yellow_0(A).
v3_orders_2(A) :- v3_orders_2(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B).
v4_orders_2(A) :- v4_orders_2(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B).
v5_orders_2(A) :- v5_orders_2(B),l1_orders_2(B),m1_yellow_0(A, B),v4_yellow_0(A, B).
v5_yellow_0(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_yellow_0(A, B),v7_yellow_0(A, B).
v6_yellow_0(A, B) :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_yellow_0(A, B),v8_yellow_0(A, B).
v2_lattice3(A) :- v4_orders_2(B),v5_orders_2(B),v2_lattice3(B),l1_orders_2(B),m1_yellow_0(A, B), ~ (v2_struct_0(A)) ,v4_yellow_0(A, B),v5_yellow_0(A, B).
v1_lattice3(A) :- v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),l1_orders_2(B),m1_yellow_0(A, B), ~ (v2_struct_0(A)) ,v4_yellow_0(A, B),v6_yellow_0(A, B).
v2_pralg_1(A) :- true,v1_relat_1(A),v1_funct_1(A),v1_yellow_1(A).
v3_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A),v4_orders_2(A),v5_orders_2(A),v2_yellow_0(A),v24_waybel_0(A),v25_waybel_0(A).
v1_yellow_3(A) :- l1_orders_2(A),v2_struct_0(A).
 ~ (v2_struct_0(A))  :- l1_orders_2(A), ~ (v1_yellow_3(A)) .
 ~ (v1_yellow_3(A))  :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v3_orders_2(A).
v1_yellow_1(A) :- l1_struct_0(B),m3_yellow_6(A, C, B),true.
v4_waybel_3(A) :- l1_struct_0(B), ~ (v2_struct_0(C)) ,v4_orders_2(C),v7_waybel_0(C),l1_waybel_0(C, B),m3_yellow_6(A, u1_struct_0(C), B),true.
v4_waybel_3(A) :- l1_struct_0(B),m3_yellow_6(A, C, B),true.
v3_yellow_6(A, B) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),l1_pre_topc(B),l1_waybel_0(A, B), ~ (v2_struct_0(A)) ,v4_orders_2(A),v7_waybel_0(A),v1_yellow_6(A, B).
v1_relat_1(A) :-  ~ (v2_struct_0(B)) ,l1_struct_0(B),m4_yellow_6(A, B),true.
v4_yellow_6(A, B) :-  ~ (v2_struct_0(B)) ,l1_struct_0(B),m4_yellow_6(A, B),v8_yellow_6(A, B).
v5_yellow_6(A, B) :-  ~ (v2_struct_0(B)) ,l1_struct_0(B),m4_yellow_6(A, B),v8_yellow_6(A, B).
v6_yellow_6(A, B) :-  ~ (v2_struct_0(B)) ,l1_struct_0(B),m4_yellow_6(A, B),v8_yellow_6(A, B).
v7_yellow_6(A, B) :-  ~ (v2_struct_0(B)) ,l1_struct_0(B),m4_yellow_6(A, B),v8_yellow_6(A, B).
v8_yellow_6(A, B) :-  ~ (v2_struct_0(B)) ,l1_struct_0(B),m4_yellow_6(A, B),v4_yellow_6(A, B),v5_yellow_6(A, B),v6_yellow_6(A, B),v7_yellow_6(A, B).
v3_lattice3(A) :- l1_orders_2(A), ~ (v2_struct_0(A)) ,v1_waybel_5(A).
 ~ (v1_xboole_0(A))  :- l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_yellow_8(A, B).
v1_zfmisc_1(A) :-  ~ (v2_struct_0(B)) ,v2_pre_topc(B),v8_pre_topc(B),l1_pre_topc(B),m1_subset_1(A, k1_zfmisc_1(u1_struct_0(B))),v3_yellow_8(A, B).
v4_yellow_8(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v8_pre_topc(A).
v6_pre_topc(A) :- l1_pre_topc(A), ~ (v2_struct_0(A)) ,v2_pre_topc(A),v4_yellow_8(A).
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,l1_orders_2(B),m1_yellow_9(A, B),true.
v3_orders_2(A) :- v3_orders_2(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v4_orders_2(A) :- v4_orders_2(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v5_orders_2(A) :- v5_orders_2(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v3_lattice3(A) :-  ~ (v2_struct_0(B)) ,v3_lattice3(B),l1_orders_2(B),m1_yellow_9(A, B),true.
v2_pre_topc(A) :- v3_orders_2(B),v4_orders_2(B),v5_orders_2(B),v1_lattice3(B),v2_lattice3(B),v3_lattice3(B),l1_orders_2(B),m1_yellow_9(A, B),v4_waybel11(A).
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),l1_pre_topc(C),m3_yellow_9(A, B, C),true.
 ~ (v2_struct_0(A))  :-  ~ (v2_struct_0(B)) ,l1_pre_topc(B),l1_pre_topc(C),m3_yellow_9(A, C, B),true.
v1_zfmisc_1(A) :- true,v1_xboole_0(A).
 ~ (v1_xboole_0(A))  :- true, ~ (v1_zfmisc_1(A)) .
