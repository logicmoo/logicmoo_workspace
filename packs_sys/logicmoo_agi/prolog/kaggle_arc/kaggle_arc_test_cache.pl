
:- dynamic(arc_test_property/3).
:- multifile(arc_test_property/3).
:- discontiguous(arc_test_property/3).

arc_test_property(t('007bbfb7'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('007bbfb7'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('007bbfb7'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('007bbfb7'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(t('00d62c1b'), grid_hint, output(containsAll([]))).
arc_test_property(t('017c7c7b'), grid_hint, input(grid_size(i, 3, 6))).
arc_test_property(t('017c7c7b'), grid_hint, output(grid_size(o, 3, 9))).
arc_test_property(t('017c7c7b'), grid_hint, output(purportional(1, 2r3))).
arc_test_property(t('045e512c'), grid_hint, input(grid_size(i, 21, 21))).
arc_test_property(t('045e512c'), grid_hint, output(containsAll([]))).
arc_test_property(t('045e512c'), grid_hint, output(grid_size(o, 21, 21))).
arc_test_property(t('0520fde7'), grid_hint, input(grid_size(i, 7, 3))).
arc_test_property(t('0520fde7'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('0520fde7'), grid_hint, input(has_x_columns(silver))).
arc_test_property(t('0520fde7'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('0520fde7'), grid_hint, output(purportional(7r3, 1))).
arc_test_property(t('05269061'), grid_hint, input(grid_size(i, 7, 7))).
arc_test_property(t('05269061'), grid_hint, output(containsAll([]))).
arc_test_property(t('05269061'), grid_hint, output(grid_size(o, 7, 7))).
arc_test_property(t('06df4c85'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('06df4c85'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('06df4c85'), grid_hint, output(containsAll([]))).
arc_test_property(t('06df4c85'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('06df4c85'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('08ed6ac7'), grid_hint, color_change_only).
arc_test_property(t('08ed6ac7'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('08ed6ac7'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('09629e4f'), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(t('09629e4f'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('09629e4f'), grid_hint, input(has_x_columns(silver))).
arc_test_property(t('09629e4f'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('09629e4f'), grid_hint, input(has_y_columns(silver))).
arc_test_property(t('09629e4f'), grid_hint, output(grid_size(o, 11, 11))).
arc_test_property(t('09629e4f'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('09629e4f'), grid_hint, output(has_x_columns(silver))).
arc_test_property(t('09629e4f'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('09629e4f'), grid_hint, output(has_y_columns(silver))).
arc_test_property(t('09629e4f'), grid_hint, output(purportional_mass(19r21))).
arc_test_property(t('0962bcdd'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(t('0962bcdd'), grid_hint, output(containsAll([]))).
arc_test_property(t('0962bcdd'), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(t('0962bcdd'), grid_hint, output(purportional_mass(17r5))).
arc_test_property(t('0a938d79'), grid_hint, output(containsAll([]))).
arc_test_property(t('0b148d64'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('0ca9ddb6'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('0ca9ddb6'), grid_hint, output(containsAll([]))).
arc_test_property(t('0ca9ddb6'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('0d3d703e'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('0d3d703e'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('0d3d703e'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('0d3d703e'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('0dfd9992'), grid_hint, input(grid_size(i, 21, 21))).
arc_test_property(t('0dfd9992'), grid_hint, output(containsAll([]))).
arc_test_property(t('0dfd9992'), grid_hint, output(grid_size(o, 21, 21))).
arc_test_property(t('10fcaaa3'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('11852cab'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('11852cab'), grid_hint, output(containsAll([]))).
arc_test_property(t('11852cab'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('11852cab'), grid_hint, output(purportional_mass(13r10))).
arc_test_property(t('1190e5a7'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('1190e5a7'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('1190e5a7'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('137eaa0f'), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(t('137eaa0f'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('137eaa0f'), grid_hint, output(purportional(11r3, 11r3))).
arc_test_property(t('150deff5'), grid_hint, color_change_only).
arc_test_property(t('178fcbfb'), grid_hint, output(containsAll([]))).
arc_test_property(t('178fcbfb'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('1b2d62fb'), grid_hint, input(grid_size(i, 7, 5))).
arc_test_property(t('1b2d62fb'), grid_hint, input(has_x_columns(blue))).
arc_test_property(t('1b2d62fb'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('1b2d62fb'), grid_hint, output(grid_size(o, 3, 5))).
arc_test_property(t('1b2d62fb'), grid_hint, output(purportional(7r3, 1))).
arc_test_property(t('1b60fb0c'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('1b60fb0c'), grid_hint, output(containsAll([]))).
arc_test_property(t('1b60fb0c'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('1bfc4729'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('1bfc4729'), grid_hint, output(containsAll([]))).
arc_test_property(t('1bfc4729'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('1bfc4729'), grid_hint, output(purportional_mass(26))).
arc_test_property(t('1c786137'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('1cf80156'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('1e32b0e9'), grid_hint, input(grid_size(i, 17, 17))).
arc_test_property(t('1e32b0e9'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('1e32b0e9'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('1e32b0e9'), grid_hint, output(containsAll([]))).
arc_test_property(t('1e32b0e9'), grid_hint, output(grid_size(o, 17, 17))).
arc_test_property(t('1e32b0e9'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('1e32b0e9'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('1f0c79e5'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('1f0c79e5'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('1f642eb9'), grid_hint, color_change_only).
arc_test_property(t('1f642eb9'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('1f642eb9'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('1f85a75f'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('1f85a75f'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(t('1f85a75f'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('1f876c06'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('1f876c06'), grid_hint, input(has_x_columns(bg))).
arc_test_property(t('1f876c06'), grid_hint, input(has_x_columns(black))).
arc_test_property(t('1f876c06'), grid_hint, input(has_y_columns(bg))).
arc_test_property(t('1f876c06'), grid_hint, input(has_y_columns(black))).
arc_test_property(t('1f876c06'), grid_hint, output(containsAll([]))).
arc_test_property(t('1f876c06'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('1fad071e'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('1fad071e'), grid_hint, output(grid_size(o, 5, 1))).
arc_test_property(t('1fad071e'), grid_hint, output(purportional(9r5, 9))).
arc_test_property(t('2013d3e2'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('2013d3e2'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('2013d3e2'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('2013d3e2'), grid_hint, output(purportional(10r3, 10r3))).
arc_test_property(t('2013d3e2'), grid_hint, output(purportional_mass(1r4))).
arc_test_property(t('2204b7a8'), grid_hint, color_change_only).
arc_test_property(t('2204b7a8'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('2204b7a8'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('22168020'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('22168020'), grid_hint, output(containsAll([]))).
arc_test_property(t('22168020'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('22233c11'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('22233c11'), grid_hint, output(containsAll([]))).
arc_test_property(t('22233c11'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('2281f1f4'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('2281f1f4'), grid_hint, output(containsAll([]))).
arc_test_property(t('2281f1f4'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('228f6490'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('228f6490'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('22eb0ac0'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('22eb0ac0'), grid_hint, output(containsAll([]))).
arc_test_property(t('22eb0ac0'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('23581191'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('23581191'), grid_hint, output(containsAll([]))).
arc_test_property(t('23581191'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('23581191'), grid_hint, output(purportional_mass(16))).
arc_test_property(t('239be575'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('239be575'), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(t('23b5c85d'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('253bf280'), grid_hint, output(containsAll([]))).
arc_test_property(t('25d487eb'), grid_hint, output(containsAll([]))).
arc_test_property(t('25d8a9c8'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('25d8a9c8'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('25ff71a9'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('25ff71a9'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('264363fd'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(t('264363fd'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(t('272f95fa'), grid_hint, input(has_x_columns(cyan))).
arc_test_property(t('272f95fa'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('272f95fa'), grid_hint, input(has_y_columns(cyan))).
arc_test_property(t('272f95fa'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('272f95fa'), grid_hint, output(containsAll([]))).
arc_test_property(t('272f95fa'), grid_hint, output(has_x_columns(cyan))).
arc_test_property(t('272f95fa'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('272f95fa'), grid_hint, output(has_y_columns(cyan))).
arc_test_property(t('272f95fa'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('27a28665'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('27a28665'), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(t('27a28665'), grid_hint, output(purportional(3, 3))).
arc_test_property(t('27a28665'), grid_hint, output(purportional_mass(1r5))).
arc_test_property(t('28bf18c6'), grid_hint, input(grid_size(i, 8, 8))).
arc_test_property(t('28bf18c6'), grid_hint, output(grid_size(o, 6, 3))).
arc_test_property(t('28bf18c6'), grid_hint, output(purportional(4r3, 8r3))).
arc_test_property(t('28bf18c6'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('28e73c20'), grid_hint, output(containsAll([]))).
arc_test_property(t('29623171'), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(t('29623171'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('29623171'), grid_hint, input(has_x_columns(silver))).
arc_test_property(t('29623171'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('29623171'), grid_hint, input(has_y_columns(silver))).
arc_test_property(t('29623171'), grid_hint, output(grid_size(o, 11, 11))).
arc_test_property(t('29623171'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('29623171'), grid_hint, output(has_x_columns(silver))).
arc_test_property(t('29623171'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('29623171'), grid_hint, output(has_y_columns(silver))).
arc_test_property(t('29c11459'), grid_hint, input(grid_size(i, 11, 5))).
arc_test_property(t('29c11459'), grid_hint, output(containsAll([]))).
arc_test_property(t('29c11459'), grid_hint, output(grid_size(o, 11, 5))).
arc_test_property(t('29c11459'), grid_hint, output(purportional_mass(11r2))).
arc_test_property(t('29ec7d0e'), grid_hint, input(grid_size(i, 18, 18))).
arc_test_property(t('29ec7d0e'), grid_hint, output(containsAll([]))).
arc_test_property(t('29ec7d0e'), grid_hint, output(grid_size(o, 18, 18))).
arc_test_property(t('2bcee788'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('2bcee788'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('2bee17df'), grid_hint, output(containsAll([]))).
arc_test_property(t('2dc579da'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('2dc579da'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('2dc579da'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('2dd70a9a'), grid_hint, output(containsAll([]))).
arc_test_property(t('2dee498d'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('2dee498d'), grid_hint, output(purportional(3, 1))).
arc_test_property(t('2dee498d'), grid_hint, output(purportional_mass(1r3))).
arc_test_property(t('31aa019c'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('31aa019c'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('321b1fc6'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('321b1fc6'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('321b1fc6'), grid_hint, output(purportional_mass(3r4))).
arc_test_property(t('32597951'), grid_hint, color_change_only).
arc_test_property(t('32597951'), grid_hint, input(grid_size(i, 17, 17))).
arc_test_property(t('32597951'), grid_hint, output(grid_size(o, 17, 17))).
arc_test_property(t('3345333e'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(t('3345333e'), grid_hint, output(grid_size(o, 16, 16))).
arc_test_property(t('3428a4f5'), grid_hint, input(grid_size(i, 5, 13))).
arc_test_property(t('3428a4f5'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('3428a4f5'), grid_hint, input(has_y_columns(yellow))).
arc_test_property(t('3428a4f5'), grid_hint, output(grid_size(o, 5, 6))).
arc_test_property(t('3428a4f5'), grid_hint, output(purportional(1, 13r6))).
arc_test_property(t('3618c87e'), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(t('3618c87e'), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(t('3631a71a'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(t('3631a71a'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(t('363442ee'), grid_hint, input(grid_size(i, 13, 9))).
arc_test_property(t('363442ee'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('363442ee'), grid_hint, input(has_x_columns(silver))).
arc_test_property(t('363442ee'), grid_hint, output(grid_size(o, 13, 9))).
arc_test_property(t('363442ee'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('363442ee'), grid_hint, output(has_x_columns(silver))).
arc_test_property(t('36d67576'), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(t('36d67576'), grid_hint, input(has_y_columns(bg))).
arc_test_property(t('36d67576'), grid_hint, input(has_y_columns(black))).
arc_test_property(t('36d67576'), grid_hint, output(containsAll([]))).
arc_test_property(t('36d67576'), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(t('36fdfd69'), grid_hint, color_change_only).
arc_test_property(t('3906de3d'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('3906de3d'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('39a8645d'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('39a8645d'), grid_hint, input(grid_size(i, 14, 14))).
arc_test_property(t('39a8645d'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('39a8645d'), grid_hint, output(purportional(14r3, 14r3))).
arc_test_property(t('39e1d7f9'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('39e1d7f9'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('39e1d7f9'), grid_hint, output(containsAll([]))).
arc_test_property(t('39e1d7f9'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('39e1d7f9'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('3aa6fb7a'), grid_hint, input(grid_size(i, 7, 7))).
arc_test_property(t('3aa6fb7a'), grid_hint, output(containsAll([]))).
arc_test_property(t('3aa6fb7a'), grid_hint, output(grid_size(o, 7, 7))).
arc_test_property(t('3aa6fb7a'), grid_hint, output(purportional_mass(4r3))).
arc_test_property(t('3ac3eb23'), grid_hint, output(containsAll([]))).
arc_test_property(t('3ac3eb23'), grid_hint, output(purportional_mass(9))).
arc_test_property(t('3af2c5a8'), grid_hint, input(grid_size(i, 4, 3))).
arc_test_property(t('3af2c5a8'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('3af2c5a8'), grid_hint, output(grid_size(o, 8, 6))).
arc_test_property(t('3af2c5a8'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('3af2c5a8'), grid_hint, output(purportional_mass(4))).
arc_test_property(t('3bd67248'), grid_hint, output(containsAll([]))).
arc_test_property(t('3bdb4ada'), grid_hint, input(containsAll([]))).
arc_test_property(t('3befdf3e'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('3befdf3e'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('3c9b0459'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('3c9b0459'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('3e980e27'), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(t('3e980e27'), grid_hint, output(containsAll([]))).
arc_test_property(t('3e980e27'), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(t('3eda0437'), grid_hint, output(containsAll([]))).
arc_test_property(t('3f7978a0'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('3f7978a0'), grid_hint, output(has_x_columns(bg))).
arc_test_property(t('3f7978a0'), grid_hint, output(has_x_columns(black))).
arc_test_property(t('40853293'), grid_hint, output(containsAll([]))).
arc_test_property(t('4093f84a'), grid_hint, input(grid_size(i, 14, 14))).
arc_test_property(t('4093f84a'), grid_hint, output(grid_size(o, 14, 14))).
arc_test_property(t('41e4d17e'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(t('41e4d17e'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(t('4258a5f9'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('4258a5f9'), grid_hint, output(containsAll([]))).
arc_test_property(t('4258a5f9'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('4258a5f9'), grid_hint, output(purportional_mass(9))).
arc_test_property(t('42a50994'), grid_hint, input(containsAll([]))).
arc_test_property(t('4347f46a'), grid_hint, input(containsAll([]))).
arc_test_property(t('444801d8'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('444801d8'), grid_hint, output(containsAll([]))).
arc_test_property(t('444801d8'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('445eab21'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('445eab21'), grid_hint, output(grid_size(o, 2, 2))).
arc_test_property(t('445eab21'), grid_hint, output(purportional(5, 5))).
arc_test_property(t('447fd412'), grid_hint, input(grid_size(i, 12, 14))).
arc_test_property(t('447fd412'), grid_hint, output(containsAll([]))).
arc_test_property(t('447fd412'), grid_hint, output(grid_size(o, 12, 14))).
arc_test_property(t('44d8ac46'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(t('44d8ac46'), grid_hint, output(containsAll([]))).
arc_test_property(t('44d8ac46'), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(t('44f52bb0'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('44f52bb0'), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(t('44f52bb0'), grid_hint, output(purportional(3, 3))).
arc_test_property(t('4522001f'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('4522001f'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('4522001f'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(t('4522001f'), grid_hint, output(purportional_mass(8))).
arc_test_property(t('4612dd53'), grid_hint, output(containsAll([]))).
arc_test_property(t('46442a0e'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('46442a0e'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('46442a0e'), grid_hint, output(purportional_mass(4))).
arc_test_property(t('469497ad'), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(t('46f33fce'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('46f33fce'), grid_hint, output(grid_size(o, 20, 20))).
arc_test_property(t('46f33fce'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('46f33fce'), grid_hint, output(purportional_mass(16))).
arc_test_property(t('47c1f68c'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('47c1f68c'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('484b58aa'), grid_hint, input(grid_size(i, 29, 29))).
arc_test_property(t('484b58aa'), grid_hint, output(containsAll([]))).
arc_test_property(t('484b58aa'), grid_hint, output(grid_size(o, 29, 29))).
arc_test_property(t('48d8fb45'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('48d8fb45'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('48d8fb45'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('48d8fb45'), grid_hint, output(purportional(10r3, 10r3))).
arc_test_property(t('4938f0c2'), grid_hint, output(containsAll([]))).
arc_test_property(t('496994bd'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('496994bd'), grid_hint, output(containsAll([]))).
arc_test_property(t('496994bd'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('496994bd'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('49d1d64f'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('4c4377d9'), grid_hint, input(grid_size(i, 4, 3))).
arc_test_property(t('4c4377d9'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('4c4377d9'), grid_hint, output(grid_size(o, 4, 6))).
arc_test_property(t('4c4377d9'), grid_hint, output(purportional(1, 1r2))).
arc_test_property(t('4c4377d9'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('4c5c2cf0'), grid_hint, output(containsAll([]))).
arc_test_property(t('50846271'), grid_hint, color_change_only).
arc_test_property(t('508bd3b6'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(t('508bd3b6'), grid_hint, output(containsAll([]))).
arc_test_property(t('508bd3b6'), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(t('50cb2852'), grid_hint, color_change_only).
arc_test_property(t('5117e062'), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(t('5117e062'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('5117e062'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('5117e062'), grid_hint, output(purportional(13r3, 13r3))).
arc_test_property(t('539a4f51'), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(t('539a4f51'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('539a4f51'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('53b68214'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('53b68214'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('543a7ed5'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(t('543a7ed5'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(t('54d82841'), grid_hint, output(containsAll([]))).
arc_test_property(t('54d82841'), grid_hint, output(purportional_mass(6r5))).
arc_test_property(t('54d9e175'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('54d9e175'), grid_hint, input(has_x_columns(silver))).
arc_test_property(t('54d9e175'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('54d9e175'), grid_hint, output(has_x_columns(silver))).
arc_test_property(t('5521c0d9'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(t('5521c0d9'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(t('5582e5ca'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('5582e5ca'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('5614dbcf'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('5614dbcf'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('5614dbcf'), grid_hint, output(purportional(3, 3))).
arc_test_property(t('56ff96f3'), grid_hint, output(containsAll([]))).
arc_test_property(t('57aa92db'), grid_hint, output(containsAll([]))).
arc_test_property(t('5ad4f10b'), grid_hint, input(has_x_columns(bg))).
arc_test_property(t('5ad4f10b'), grid_hint, input(has_x_columns(black))).
arc_test_property(t('5ad4f10b'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('5bd6f4ac'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('5bd6f4ac'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('5bd6f4ac'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('5bd6f4ac'), grid_hint, output(purportional(3, 3))).
arc_test_property(t('5c0a986e'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('5c0a986e'), grid_hint, output(containsAll([]))).
arc_test_property(t('5c0a986e'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('5c2c9af4'), grid_hint, input(grid_size(i, 23, 23))).
arc_test_property(t('5c2c9af4'), grid_hint, output(containsAll([]))).
arc_test_property(t('5c2c9af4'), grid_hint, output(grid_size(o, 23, 23))).
arc_test_property(t('5daaa586'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('60b61512'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('60b61512'), grid_hint, output(containsAll([]))).
arc_test_property(t('60b61512'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('6150a2bd'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('6150a2bd'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('623ea044'), grid_hint, output(containsAll([]))).
arc_test_property(t('62c24649'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('62c24649'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('62c24649'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(t('62c24649'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('62c24649'), grid_hint, output(purportional_mass(4))).
arc_test_property(t('63613498'), grid_hint, color_change_only).
arc_test_property(t('63613498'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('63613498'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('6430c8c4'), grid_hint, input(grid_size(i, 4, 9))).
arc_test_property(t('6430c8c4'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('6430c8c4'), grid_hint, input(has_y_columns(yellow))).
arc_test_property(t('6430c8c4'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t('6430c8c4'), grid_hint, output(purportional(1, 9r4))).
arc_test_property(t('6455b5f5'), grid_hint, output(containsAll([]))).
arc_test_property(t('662c240a'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('662c240a'), grid_hint, input(grid_size(i, 3, 9))).
arc_test_property(t('662c240a'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('662c240a'), grid_hint, output(purportional(1, 3))).
arc_test_property(t('662c240a'), grid_hint, output(purportional_mass(1r3))).
arc_test_property(t('67385a82'), grid_hint, color_change_only).
arc_test_property(t('6773b310'), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(t('6773b310'), grid_hint, input(has_x_columns(cyan))).
arc_test_property(t('6773b310'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('6773b310'), grid_hint, input(has_y_columns(cyan))).
arc_test_property(t('6773b310'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('6773b310'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('6773b310'), grid_hint, output(purportional(11r3, 11r3))).
arc_test_property(t('67e8384a'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('67e8384a'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('67e8384a'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(t('67e8384a'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('67e8384a'), grid_hint, output(purportional_mass(4))).
arc_test_property(t('681b3aeb'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('681b3aeb'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('681b3aeb'), grid_hint, output(purportional(10r3, 10r3))).
arc_test_property(t('6855a6e4'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(t('6855a6e4'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(t('694f12f3'), grid_hint, color_change_only).
arc_test_property(t('694f12f3'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('694f12f3'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('6a1e5592'), grid_hint, input(grid_size(i, 15, 10))).
arc_test_property(t('6a1e5592'), grid_hint, output(grid_size(o, 15, 10))).
arc_test_property(t('6c434453'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('6c434453'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('6cdd2623'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('6cf79266'), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(t('6cf79266'), grid_hint, output(containsAll([]))).
arc_test_property(t('6cf79266'), grid_hint, output(grid_size(o, 20, 20))).
arc_test_property(t('6d0160f0'), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(t('6d0160f0'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('6d0160f0'), grid_hint, input(has_x_columns(silver))).
arc_test_property(t('6d0160f0'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('6d0160f0'), grid_hint, input(has_y_columns(silver))).
arc_test_property(t('6d0160f0'), grid_hint, output(grid_size(o, 11, 11))).
arc_test_property(t('6d0160f0'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('6d0160f0'), grid_hint, output(has_x_columns(silver))).
arc_test_property(t('6d0160f0'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('6d0160f0'), grid_hint, output(has_y_columns(silver))).
arc_test_property(t('6d0aefbc'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('6d0aefbc'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('6d0aefbc'), grid_hint, output(grid_size(o, 6, 3))).
arc_test_property(t('6d0aefbc'), grid_hint, output(purportional(1r2, 1))).
arc_test_property(t('6d0aefbc'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('6d58a25d'), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(t('6d58a25d'), grid_hint, input(has_x_columns(bg))).
arc_test_property(t('6d58a25d'), grid_hint, input(has_x_columns(black))).
arc_test_property(t('6d58a25d'), grid_hint, output(containsAll([]))).
arc_test_property(t('6d58a25d'), grid_hint, output(grid_size(o, 20, 20))).
arc_test_property(t('6d58a25d'), grid_hint, output(has_x_columns(bg))).
arc_test_property(t('6d58a25d'), grid_hint, output(has_x_columns(black))).
arc_test_property(t('6d75e8bb'), grid_hint, output(containsAll([]))).
arc_test_property(t('6e02f1e3'), grid_hint, input(containsAll([black, silver]))).
arc_test_property(t('6e02f1e3'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('6e02f1e3'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('6e02f1e3'), grid_hint, output(purportional_mass(1r3))).
arc_test_property(t('6e19193c'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('6e19193c'), grid_hint, output(containsAll([]))).
arc_test_property(t('6e19193c'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('6e82a1ae'), grid_hint, color_change_only).
arc_test_property(t('6e82a1ae'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('6e82a1ae'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('6f8cd79b'), grid_hint, output(containsAll([]))).
arc_test_property(t('6fa7a44f'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('6fa7a44f'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('6fa7a44f'), grid_hint, output(grid_size(o, 3, 6))).
arc_test_property(t('6fa7a44f'), grid_hint, output(purportional(1, 1r2))).
arc_test_property(t('6fa7a44f'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('72322fa7'), grid_hint, output(containsAll([]))).
arc_test_property(t('72ca375d'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('72ca375d'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('73251a56'), grid_hint, input(grid_size(i, 21, 21))).
arc_test_property(t('73251a56'), grid_hint, output(containsAll([]))).
arc_test_property(t('73251a56'), grid_hint, output(grid_size(o, 21, 21))).
arc_test_property(t('7447852a'), grid_hint, output(containsAll([]))).
arc_test_property(t('74dd1130'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('74dd1130'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('75b8110e'), grid_hint, input(grid_size(i, 8, 8))).
arc_test_property(t('75b8110e'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t('75b8110e'), grid_hint, output(purportional(2, 2))).
arc_test_property(t('760b3cac'), grid_hint, input(grid_size(i, 9, 6))).
arc_test_property(t('760b3cac'), grid_hint, output(containsAll([]))).
arc_test_property(t('760b3cac'), grid_hint, output(grid_size(o, 9, 6))).
arc_test_property(t('776ffc46'), grid_hint, color_change_only).
arc_test_property(t('776ffc46'), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(t('776ffc46'), grid_hint, output(grid_size(o, 20, 20))).
arc_test_property(t('77fdfe62'), grid_hint, input(has_x_columns(blue))).
arc_test_property(t('77fdfe62'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('77fdfe62'), grid_hint, input(has_y_columns(blue))).
arc_test_property(t('77fdfe62'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('780d0b14'), grid_hint, input(has_x_columns(bg))).
arc_test_property(t('780d0b14'), grid_hint, input(has_x_columns(black))).
arc_test_property(t('780d0b14'), grid_hint, input(has_y_columns(bg))).
arc_test_property(t('780d0b14'), grid_hint, input(has_y_columns(black))).
arc_test_property(t('7837ac64'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('7837ac64'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('7837ac64'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('794b24be'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('794b24be'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('7b6016b9'), grid_hint, output(containsAll([]))).
arc_test_property(t('7b7f7511'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('7b7f7511'), grid_hint, output(purportional_mass(1r2))).
arc_test_property(t('7c008303'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('7c008303'), grid_hint, input(has_x_columns(cyan))).
arc_test_property(t('7c008303'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('7c008303'), grid_hint, input(has_y_columns(cyan))).
arc_test_property(t('7c008303'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('7c008303'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(t('7c008303'), grid_hint, output(purportional(3r2, 3r2))).
arc_test_property(t('7ddcd7ec'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('7ddcd7ec'), grid_hint, output(containsAll([]))).
arc_test_property(t('7ddcd7ec'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('7df24a62'), grid_hint, input(grid_size(i, 23, 23))).
arc_test_property(t('7df24a62'), grid_hint, output(containsAll([]))).
arc_test_property(t('7df24a62'), grid_hint, output(grid_size(o, 23, 23))).
arc_test_property(t('7f4411dc'), grid_hint, input(containsAll([]))).
arc_test_property(t('7fe24cdd'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('7fe24cdd'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('7fe24cdd'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(t('7fe24cdd'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t('7fe24cdd'), grid_hint, output(purportional_mass(4))).
arc_test_property(t('80af3007'), grid_hint, input(grid_size(i, 18, 16))).
arc_test_property(t('80af3007'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('80af3007'), grid_hint, output(purportional(2, 16r9))).
arc_test_property(t('810b9b61'), grid_hint, color_change_only).
arc_test_property(t('82819916'), grid_hint, output(containsAll([]))).
arc_test_property(t('83302e8f'), grid_hint, output(containsAll([]))).
arc_test_property(t('8403a5d5'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('8403a5d5'), grid_hint, output(containsAll([]))).
arc_test_property(t('8403a5d5'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('846bdb03'), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(t('855e0971'), grid_hint, input(containsAll([]))).
arc_test_property(t('868de0fa'), grid_hint, output(containsAll([]))).
arc_test_property(t('8731374e'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('8731374e'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('88a62173'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('88a62173'), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(t('88a62173'), grid_hint, input(has_x_columns(bg))).
arc_test_property(t('88a62173'), grid_hint, input(has_x_columns(black))).
arc_test_property(t('88a62173'), grid_hint, input(has_y_columns(bg))).
arc_test_property(t('88a62173'), grid_hint, input(has_y_columns(black))).
arc_test_property(t('88a62173'), grid_hint, output(grid_size(o, 2, 2))).
arc_test_property(t('88a62173'), grid_hint, output(purportional(5r2, 5r2))).
arc_test_property(t('890034e9'), grid_hint, input(grid_size(i, 21, 21))).
arc_test_property(t('890034e9'), grid_hint, output(grid_size(o, 21, 21))).
arc_test_property(t('8be77c9e'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('8be77c9e'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('8be77c9e'), grid_hint, output(grid_size(o, 3, 6))).
arc_test_property(t('8be77c9e'), grid_hint, output(purportional(1, 1r2))).
arc_test_property(t('8be77c9e'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('8d5021e8'), grid_hint, input(grid_size(i, 2, 3))).
arc_test_property(t('8d5021e8'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('8d5021e8'), grid_hint, output(grid_size(o, 4, 9))).
arc_test_property(t('8d5021e8'), grid_hint, output(purportional(1r2, 1r3))).
arc_test_property(t('8d5021e8'), grid_hint, output(purportional_mass(6))).
arc_test_property(t('8d510a79'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('8d510a79'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('8d510a79'), grid_hint, input(has_y_columns(silver))).
arc_test_property(t('8d510a79'), grid_hint, output(containsAll([]))).
arc_test_property(t('8d510a79'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('8d510a79'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('8d510a79'), grid_hint, output(has_y_columns(silver))).
arc_test_property(t('8e5a5113'), grid_hint, input(grid_size(i, 11, 3))).
arc_test_property(t('8e5a5113'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('8e5a5113'), grid_hint, output(containsAll([]))).
arc_test_property(t('8e5a5113'), grid_hint, output(grid_size(o, 11, 3))).
arc_test_property(t('8e5a5113'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('8e5a5113'), grid_hint, output(has_x_columns(silver))).
arc_test_property(t('8e5a5113'), grid_hint, output(purportional_mass(11r5))).
arc_test_property(t('8eb1be9a'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('8eb1be9a'), grid_hint, output(containsAll([]))).
arc_test_property(t('8efcae92'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t('8efcae92'), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(t('8f2ea7aa'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t('8f2ea7aa'), grid_hint, output(containsAll([]))).
arc_test_property(t('8f2ea7aa'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('90c28cc7'), grid_hint, input(grid_size(i, 21, 21))).
arc_test_property(t('90f3ed37'), grid_hint, input(grid_size(i, 10, 15))).
arc_test_property(t('90f3ed37'), grid_hint, output(containsAll([]))).
arc_test_property(t('90f3ed37'), grid_hint, output(grid_size(o, 10, 15))).
arc_test_property(t('913fb3ed'), grid_hint, output(containsAll([]))).
arc_test_property(t('913fb3ed'), grid_hint, output(purportional_mass(9))).
arc_test_property(t('91413438'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('91413438'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('91714a58'), grid_hint, input(containsAll([]))).
arc_test_property(t('91714a58'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(t('91714a58'), grid_hint, output(grid_size(o, 16, 16))).
arc_test_property(t('9172f3a0'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('9172f3a0'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t('9172f3a0'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(t('9172f3a0'), grid_hint, output(purportional_mass(9))).
arc_test_property(t('928ad970'), grid_hint, output(containsAll([]))).
arc_test_property(t('93b581b8'), grid_hint, input(grid_size(i, 6, 6))).
arc_test_property(t('93b581b8'), grid_hint, output(containsAll([]))).
arc_test_property(t('93b581b8'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(t('941d9a10'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('941d9a10'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('941d9a10'), grid_hint, input(has_x_columns(silver))).
arc_test_property(t('941d9a10'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('941d9a10'), grid_hint, input(has_y_columns(silver))).
arc_test_property(t('941d9a10'), grid_hint, output(containsAll([]))).
arc_test_property(t('941d9a10'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('941d9a10'), grid_hint, output(has_x_columns(fg))).
arc_test_property(t('941d9a10'), grid_hint, output(has_x_columns(silver))).
arc_test_property(t('941d9a10'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('941d9a10'), grid_hint, output(has_y_columns(silver))).
arc_test_property(t('94f9d214'), grid_hint, input(grid_size(i, 4, 8))).
arc_test_property(t('94f9d214'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t('94f9d214'), grid_hint, output(purportional(1, 2))).
arc_test_property(t('952a094c'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t('952a094c'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t('9565186b'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t('9565186b'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('95990924'), grid_hint, output(containsAll([]))).
arc_test_property(t('95990924'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('963e52fc'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t('963e52fc'), grid_hint, output(purportional(1r2, 1))).
arc_test_property(t('963e52fc'), grid_hint, output(purportional_mass(2))).
arc_test_property(t('97999447'), grid_hint, output(containsAll([]))).
arc_test_property(t('995c5fa3'), grid_hint, input(grid_size(i, 14, 4))).
arc_test_property(t('995c5fa3'), grid_hint, input(has_x_columns(bg))).
arc_test_property(t('995c5fa3'), grid_hint, input(has_x_columns(black))).
arc_test_property(t('995c5fa3'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('995c5fa3'), grid_hint, output(has_y_columns(fg))).
arc_test_property(t('995c5fa3'), grid_hint, output(purportional(14r3, 4r3))).
arc_test_property(t('99b1bc43'), grid_hint, input(grid_size(i, 4, 9))).
arc_test_property(t('99b1bc43'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t('99b1bc43'), grid_hint, input(has_y_columns(yellow))).
arc_test_property(t('99b1bc43'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t('99b1bc43'), grid_hint, output(purportional(1, 9r4))).
arc_test_property(t('99fa7670'), grid_hint, output(containsAll([]))).
arc_test_property(t('9d9215db'), grid_hint, input(grid_size(i, 19, 19))).
arc_test_property(t('9d9215db'), grid_hint, input(has_y_columns(bg))).
arc_test_property(t('9d9215db'), grid_hint, input(has_y_columns(black))).
arc_test_property(t('9d9215db'), grid_hint, output(containsAll([]))).
arc_test_property(t('9d9215db'), grid_hint, output(grid_size(o, 19, 19))).
arc_test_property(t('9d9215db'), grid_hint, output(has_x_columns(bg))).
arc_test_property(t('9d9215db'), grid_hint, output(has_x_columns(black))).
arc_test_property(t('9d9215db'), grid_hint, output(has_y_columns(bg))).
arc_test_property(t('9d9215db'), grid_hint, output(has_y_columns(black))).
arc_test_property(t('9ecd008a'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(t('9ecd008a'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t('9ecd008a'), grid_hint, output(purportional(16r3, 16r3))).
arc_test_property(t('9ecd008a'), grid_hint, output(purportional_mass(9r247))).
arc_test_property(t('9edfc990'), grid_hint, output(containsAll([]))).
arc_test_property(t('9f236235'), grid_hint, input(has_x_columns(fg))).
arc_test_property(t('9f236235'), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(a2fd1cf0), grid_hint, output(containsAll([]))).
arc_test_property(t(a3325580), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(a3df8b1e), grid_hint, output(containsAll([]))).
arc_test_property(t(a3df8b1e), grid_hint, output(purportional_mass(10))).
arc_test_property(t(a416b8f3), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t(a416b8f3), grid_hint, output(purportional(1r2, 1))).
arc_test_property(t(a416b8f3), grid_hint, output(purportional_mass(2))).
arc_test_property(t(a48eeaf7), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(a48eeaf7), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(a5313dff), grid_hint, output(containsAll([]))).
arc_test_property(t(a5f85a15), grid_hint, color_change_only).
arc_test_property(t(a61ba2ce), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(t(a61ba2ce), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t(a61ba2ce), grid_hint, output(purportional(13r4, 13r4))).
arc_test_property(t(a61f2674), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t(a61f2674), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t(a64e4611), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(t(a64e4611), grid_hint, output(containsAll([]))).
arc_test_property(t(a64e4611), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(t(a65b410d), grid_hint, output(containsAll([]))).
arc_test_property(t(a68b268e), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t(a68b268e), grid_hint, input(has_x_columns(blue))).
arc_test_property(t(a68b268e), grid_hint, input(has_x_columns(fg))).
arc_test_property(t(a68b268e), grid_hint, input(has_y_columns(blue))).
arc_test_property(t(a68b268e), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(a68b268e), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t(a68b268e), grid_hint, output(purportional(9r4, 9r4))).
arc_test_property(t(a699fb00), grid_hint, output(containsAll([]))).
arc_test_property(t(a78176bb), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(a78176bb), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(a85d4709), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(a85d4709), grid_hint, output(containsAll([black, silver]))).
arc_test_property(t(a85d4709), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(a85d4709), grid_hint, output(purportional_mass(3))).
arc_test_property(t(a87f7484), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(a87f7484), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(a8c38be5), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t(a8c38be5), grid_hint, output(has_x_columns(fg))).
arc_test_property(t(a8c38be5), grid_hint, output(has_x_columns(silver))).
arc_test_property(t(a8c38be5), grid_hint, output(has_y_columns(fg))).
arc_test_property(t(a8c38be5), grid_hint, output(has_y_columns(silver))).
arc_test_property(t(a8d7556c), grid_hint, input(grid_size(i, 18, 18))).
arc_test_property(t(a8d7556c), grid_hint, output(containsAll([]))).
arc_test_property(t(a8d7556c), grid_hint, output(grid_size(o, 18, 18))).
arc_test_property(t(a9f96cdd), grid_hint, input(grid_size(i, 5, 3))).
arc_test_property(t(a9f96cdd), grid_hint, output(grid_size(o, 5, 3))).
arc_test_property(t(aabf363d), grid_hint, input(grid_size(i, 7, 7))).
arc_test_property(t(aabf363d), grid_hint, output(grid_size(o, 7, 7))).
arc_test_property(t(aabf363d), grid_hint, output(purportional_mass(12r13))).
arc_test_property(t(aba27056), grid_hint, output(containsAll([]))).
arc_test_property(t(ac0a08a4), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(ae3edfdc), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(t(ae3edfdc), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(t(ae4f1146), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(ae4f1146), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t(ae4f1146), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(ae4f1146), grid_hint, output(purportional(3, 3))).
arc_test_property(t(ae4f1146), grid_hint, output(purportional_mass(1r4))).
arc_test_property(t(aedd82e4), grid_hint, color_change_only).
arc_test_property(t(af902bf9), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(af902bf9), grid_hint, output(containsAll([]))).
arc_test_property(t(af902bf9), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(b0c4d837), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(b1948b0a), grid_hint, color_change_only).
arc_test_property(t(b230c067), grid_hint, color_change_only).
arc_test_property(t(b230c067), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(b230c067), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(b27ca6d3), grid_hint, output(containsAll([]))).
arc_test_property(t(b2862040), grid_hint, color_change_only).
arc_test_property(t(b527c5c6), grid_hint, output(containsAll([]))).
arc_test_property(t(b60334d2), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t(b60334d2), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t(b60334d2), grid_hint, output(purportional_mass(8))).
arc_test_property(t(b6afb2da), grid_hint, color_change_only).
arc_test_property(t(b6afb2da), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(b6afb2da), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(b7249182), grid_hint, output(containsAll([]))).
arc_test_property(t(b775ac94), grid_hint, output(containsAll([]))).
arc_test_property(t(b782dc8a), grid_hint, output(containsAll([]))).
arc_test_property(t(b8825c91), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(t(b8825c91), grid_hint, output(grid_size(o, 16, 16))).
arc_test_property(t(b8cdaf2b), grid_hint, output(containsAll([]))).
arc_test_property(t(b91ae062), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(b9b7f026), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(b9b7f026), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(t(ba26e723), grid_hint, color_change_only).
arc_test_property(t(ba26e723), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(ba26e723), grid_hint, input(has_y_columns(yellow))).
arc_test_property(t(ba97ae07), grid_hint, color_change_only).
arc_test_property(t(bb43febb), grid_hint, color_change_only).
arc_test_property(t(bb43febb), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(bb43febb), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(bbc9ae5d), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t(bc1d5164), grid_hint, input(grid_size(i, 7, 5))).
arc_test_property(t(bc1d5164), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(bc1d5164), grid_hint, output(purportional(7r3, 5r3))).
arc_test_property(t(bd4472b8), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(bd4472b8), grid_hint, input(has_y_columns(silver))).
arc_test_property(t(bd4472b8), grid_hint, output(containsAll([]))).
arc_test_property(t(bd4472b8), grid_hint, output(has_y_columns(fg))).
arc_test_property(t(bd4472b8), grid_hint, output(has_y_columns(silver))).
arc_test_property(t(bdad9b1f), grid_hint, input(grid_size(i, 6, 6))).
arc_test_property(t(bdad9b1f), grid_hint, output(containsAll([]))).
arc_test_property(t(bdad9b1f), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(t(bdad9b1f), grid_hint, output(purportional_mass(11r4))).
arc_test_property(t(be94b721), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(c0f76784), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(t(c0f76784), grid_hint, output(containsAll([]))).
arc_test_property(t(c0f76784), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(t(c1d99e64), grid_hint, input(has_x_columns(bg))).
arc_test_property(t(c1d99e64), grid_hint, input(has_x_columns(black))).
arc_test_property(t(c1d99e64), grid_hint, output(containsAll([]))).
arc_test_property(t(c1d99e64), grid_hint, output(has_x_columns(fg))).
arc_test_property(t(c1d99e64), grid_hint, output(has_x_columns(red))).
arc_test_property(t(c3e719e8), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(c3e719e8), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t(c3e719e8), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t(c3e719e8), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(t(c3f564a4), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(t(c3f564a4), grid_hint, output(containsAll([]))).
arc_test_property(t(c3f564a4), grid_hint, output(grid_size(o, 16, 16))).
arc_test_property(t(c444b776), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(c444b776), grid_hint, input(has_y_columns(yellow))).
arc_test_property(t(c444b776), grid_hint, output(containsAll([]))).
arc_test_property(t(c444b776), grid_hint, output(has_y_columns(fg))).
arc_test_property(t(c444b776), grid_hint, output(has_y_columns(yellow))).
arc_test_property(t(c59eb873), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t(c59eb873), grid_hint, output(purportional_mass(4))).
arc_test_property(t(c909285e), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(c909285e), grid_hint, output(grid_size(o, 7, 7))).
arc_test_property(t(c9e6f938), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(c9e6f938), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t(c9e6f938), grid_hint, output(grid_size(o, 6, 3))).
arc_test_property(t(c9e6f938), grid_hint, output(purportional(1r2, 1))).
arc_test_property(t(c9e6f938), grid_hint, output(purportional_mass(2))).
arc_test_property(t(c9f8e694), grid_hint, color_change_only).
arc_test_property(t(c9f8e694), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(t(c9f8e694), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(t(cbded52d), grid_hint, color_change_only).
arc_test_property(t(cbded52d), grid_hint, input(grid_size(i, 8, 8))).
arc_test_property(t(cbded52d), grid_hint, input(has_x_columns(bg))).
arc_test_property(t(cbded52d), grid_hint, input(has_x_columns(black))).
arc_test_property(t(cbded52d), grid_hint, input(has_y_columns(bg))).
arc_test_property(t(cbded52d), grid_hint, input(has_y_columns(black))).
arc_test_property(t(cbded52d), grid_hint, output(grid_size(o, 8, 8))).
arc_test_property(t(cbded52d), grid_hint, output(has_x_columns(bg))).
arc_test_property(t(cbded52d), grid_hint, output(has_x_columns(black))).
arc_test_property(t(cbded52d), grid_hint, output(has_y_columns(bg))).
arc_test_property(t(cbded52d), grid_hint, output(has_y_columns(black))).
arc_test_property(t(cce03e0d), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(cce03e0d), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t(cce03e0d), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t(cce03e0d), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(t(cdecee7f), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(cdecee7f), grid_hint, input(has_x_columns(bg))).
arc_test_property(t(cdecee7f), grid_hint, input(has_x_columns(black))).
arc_test_property(t(cdecee7f), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(cdecee7f), grid_hint, output(purportional(10r3, 10r3))).
arc_test_property(t(ce22a75a), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t(ce22a75a), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(t(ce22a75a), grid_hint, output(purportional_mass(9))).
arc_test_property(t(ce4f8723), grid_hint, input(grid_size(i, 4, 9))).
arc_test_property(t(ce4f8723), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(ce4f8723), grid_hint, input(has_y_columns(yellow))).
arc_test_property(t(ce4f8723), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t(ce4f8723), grid_hint, output(purportional(1, 9r4))).
arc_test_property(t(ce602527), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(ce9e57f2), grid_hint, color_change_only).
arc_test_property(t(ce9e57f2), grid_hint, input(has_x_columns(bg))).
arc_test_property(t(ce9e57f2), grid_hint, input(has_x_columns(black))).
arc_test_property(t(ce9e57f2), grid_hint, output(has_x_columns(bg))).
arc_test_property(t(ce9e57f2), grid_hint, output(has_x_columns(black))).
arc_test_property(t(cf98881b), grid_hint, input(grid_size(i, 14, 4))).
arc_test_property(t(cf98881b), grid_hint, input(has_x_columns(fg))).
arc_test_property(t(cf98881b), grid_hint, input(has_x_columns(red))).
arc_test_property(t(cf98881b), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(t(cf98881b), grid_hint, output(purportional(7r2, 1))).
arc_test_property(t(d037b0a7), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(d037b0a7), grid_hint, output(containsAll([]))).
arc_test_property(t(d037b0a7), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(d06dbe63), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(t(d06dbe63), grid_hint, output(containsAll([]))).
arc_test_property(t(d06dbe63), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(t(d10ecb37), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(d10ecb37), grid_hint, output(grid_size(o, 2, 2))).
arc_test_property(t(d13f3404), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(d13f3404), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(t(d13f3404), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t(d22278a0), grid_hint, output(containsAll([]))).
arc_test_property(t(d23f8c26), grid_hint, input(containsAll([]))).
arc_test_property(t(d2abd087), grid_hint, color_change_only).
arc_test_property(t(d2abd087), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(d2abd087), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(d364b489), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(d364b489), grid_hint, output(containsAll([]))).
arc_test_property(t(d364b489), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(d406998b), grid_hint, color_change_only).
arc_test_property(t(d43fd935), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(d43fd935), grid_hint, output(containsAll([]))).
arc_test_property(t(d43fd935), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(d4469b4b), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(t(d4469b4b), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(d4469b4b), grid_hint, output(purportional(5r3, 5r3))).
arc_test_property(t(d4a91cb9), grid_hint, output(containsAll([]))).
arc_test_property(t(d4f3cd78), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(d4f3cd78), grid_hint, output(containsAll([]))).
arc_test_property(t(d4f3cd78), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(d631b094), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(d6ad076f), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(d6ad076f), grid_hint, output(containsAll([]))).
arc_test_property(t(d6ad076f), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(d89b689b), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(d89b689b), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(d89b689b), grid_hint, output(purportional_mass(1r2))).
arc_test_property(t(d8c310e9), grid_hint, input(grid_size(i, 15, 5))).
arc_test_property(t(d8c310e9), grid_hint, output(containsAll([]))).
arc_test_property(t(d8c310e9), grid_hint, output(grid_size(o, 15, 5))).
arc_test_property(t(d9f24cd1), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(d9f24cd1), grid_hint, output(containsAll([]))).
arc_test_property(t(d9f24cd1), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(d9f24cd1), grid_hint, output(has_x_columns(fg))).
arc_test_property(t(d9f24cd1), grid_hint, output(has_x_columns(red))).
arc_test_property(t(d9fac9be), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(d9fac9be), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(t(dae9d2b5), grid_hint, input(grid_size(i, 6, 3))).
arc_test_property(t(dae9d2b5), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(dae9d2b5), grid_hint, output(purportional(2, 1))).
arc_test_property(t(db3e9e38), grid_hint, output(containsAll([]))).
arc_test_property(t(db93a21d), grid_hint, output(containsAll([]))).
arc_test_property(t(dbc1a6ce), grid_hint, output(containsAll([]))).
arc_test_property(t(dc0a314f), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(t(dc0a314f), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(t(dc0a314f), grid_hint, output(purportional(16r5, 16r5))).
arc_test_property(t(dc0a314f), grid_hint, output(purportional_mass(25r256))).
arc_test_property(t(dc1df850), grid_hint, output(containsAll([]))).
arc_test_property(t(ddf7fa4f), grid_hint, color_change_only).
arc_test_property(t(ddf7fa4f), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(ddf7fa4f), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(de1cd16c), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(de1cd16c), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(t(ded97339), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(ded97339), grid_hint, output(containsAll([]))).
arc_test_property(t(ded97339), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(e179c5f4), grid_hint, output(containsAll([]))).
arc_test_property(t(e21d9049), grid_hint, output(containsAll([]))).
arc_test_property(t(e3497940), grid_hint, input(grid_size(i, 9, 10))).
arc_test_property(t(e3497940), grid_hint, input(has_x_columns(fg))).
arc_test_property(t(e3497940), grid_hint, input(has_x_columns(silver))).
arc_test_property(t(e3497940), grid_hint, output(grid_size(o, 4, 10))).
arc_test_property(t(e3497940), grid_hint, output(purportional(9r4, 1))).
arc_test_property(t(e40b9e2f), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(e40b9e2f), grid_hint, output(containsAll([]))).
arc_test_property(t(e40b9e2f), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(e48d4e1a), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(e48d4e1a), grid_hint, input(has_x_columns(fg))).
arc_test_property(t(e48d4e1a), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(e48d4e1a), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(e48d4e1a), grid_hint, output(has_y_columns(fg))).
arc_test_property(t(e5062a87), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(e5062a87), grid_hint, output(containsAll([]))).
arc_test_property(t(e5062a87), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(e509e548), grid_hint, color_change_only).
arc_test_property(t(e50d258f), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(e50d258f), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(e73095fd), grid_hint, output(containsAll([]))).
arc_test_property(t(e76a88a6), grid_hint, color_change_only).
arc_test_property(t(e76a88a6), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(e76a88a6), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(e8593010), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(e8593010), grid_hint, input(has_x_columns(fg))).
arc_test_property(t(e8593010), grid_hint, input(has_x_columns(silver))).
arc_test_property(t(e8593010), grid_hint, output(containsAll([]))).
arc_test_property(t(e8593010), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(e8593010), grid_hint, output(has_x_columns(fg))).
arc_test_property(t(e8593010), grid_hint, output(has_x_columns(silver))).
arc_test_property(t(e8dc4411), grid_hint, color_change_only).
arc_test_property(t(e9614598), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(e9614598), grid_hint, output(containsAll([]))).
arc_test_property(t(e9614598), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(e9614598), grid_hint, output(purportional_mass(7r2))).
arc_test_property(t(e98196ab), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(t(e98196ab), grid_hint, input(has_y_columns(fg))).
arc_test_property(t(e98196ab), grid_hint, input(has_y_columns(silver))).
arc_test_property(t(e98196ab), grid_hint, output(grid_size(o, 11, 5))).
arc_test_property(t(e98196ab), grid_hint, output(purportional(1, 11r5))).
arc_test_property(t(e9afcf9a), grid_hint, input(grid_size(i, 6, 2))).
arc_test_property(t(e9afcf9a), grid_hint, output(grid_size(o, 6, 2))).
arc_test_property(t(ea32f347), grid_hint, color_change_only).
arc_test_property(t(ea32f347), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(ea32f347), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(ea786f4a), grid_hint, input(containsAll([]))).
arc_test_property(t(eb281b96), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t(ec883f72), grid_hint, output(containsAll([]))).
arc_test_property(t(ed36ccf7), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(t(ed36ccf7), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(ef135b50), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(ef135b50), grid_hint, output(containsAll([]))).
arc_test_property(t(ef135b50), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(f15e1fac), grid_hint, output(containsAll([]))).
arc_test_property(t(f25fbde4), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(t(f25fbde4), grid_hint, output(purportional_mass(4))).
arc_test_property(t(f25ffba3), grid_hint, input(grid_size(i, 4, 10))).
arc_test_property(t(f25ffba3), grid_hint, output(containsAll([]))).
arc_test_property(t(f25ffba3), grid_hint, output(grid_size(o, 4, 10))).
arc_test_property(t(f25ffba3), grid_hint, output(purportional_mass(2))).
arc_test_property(t(f2829549), grid_hint, input(grid_size(i, 7, 4))).
arc_test_property(t(f2829549), grid_hint, input(has_x_columns(blue))).
arc_test_property(t(f2829549), grid_hint, input(has_x_columns(fg))).
arc_test_property(t(f2829549), grid_hint, output(grid_size(o, 3, 4))).
arc_test_property(t(f2829549), grid_hint, output(purportional(7r3, 1))).
arc_test_property(t(f35d900a), grid_hint, output(containsAll([]))).
arc_test_property(t(f5b8619d), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(t(f8a8fe49), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(t(f8a8fe49), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(t(f8b3ba0a), grid_hint, input(has_x_columns(bg))).
arc_test_property(t(f8b3ba0a), grid_hint, input(has_x_columns(black))).
arc_test_property(t(f8b3ba0a), grid_hint, input(has_y_columns(bg))).
arc_test_property(t(f8b3ba0a), grid_hint, input(has_y_columns(black))).
arc_test_property(t(f8b3ba0a), grid_hint, output(grid_size(o, 1, 3))).
arc_test_property(t(f8b3ba0a), grid_hint, output(has_y_columns(fg))).
arc_test_property(t(f8c80d96), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(f8c80d96), grid_hint, output(containsAll([]))).
arc_test_property(t(f8c80d96), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(f8ff0b80), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(t(f8ff0b80), grid_hint, output(grid_size(o, 1, 3))).
arc_test_property(t(f8ff0b80), grid_hint, output(has_y_columns(fg))).
arc_test_property(t(f8ff0b80), grid_hint, output(purportional(12, 4))).
arc_test_property(t(f9012d9b), grid_hint, input(find_ogs_no_pad)).
arc_test_property(t(fafffa47), grid_hint, input(grid_size(i, 3, 6))).
arc_test_property(t(fafffa47), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(fafffa47), grid_hint, output(purportional(1, 2))).
arc_test_property(t(fcc82909), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(t(fcc82909), grid_hint, output(containsAll([]))).
arc_test_property(t(fcc82909), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(t(feca6190), grid_hint, input(grid_size(i, 5, 1))).
arc_test_property(t(feca6190), grid_hint, input(has_x_columns(fg))).
arc_test_property(t(feca6190), grid_hint, output(find_ogs_no_pad)).
arc_test_property(t(ff28f65a), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(t(ff28f65a), grid_hint, output(purportional_mass(1r4))).
arc_test_property(t(ff805c23), grid_hint, input(grid_size(i, 24, 24))).
arc_test_property(t(ff805c23), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(t(ff805c23), grid_hint, output(purportional(24r5, 24r5))).
arc_test_property(v('00576224'), grid_hint, input(grid_size(i, 2, 2))).
arc_test_property(v('00576224'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('00576224'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(v('00576224'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('00576224'), grid_hint, output(purportional_mass(9))).
arc_test_property(v('009d5c81'), grid_hint, input(grid_size(i, 14, 14))).
arc_test_property(v('009d5c81'), grid_hint, output(grid_size(o, 14, 14))).
arc_test_property(v('00dbd492'), grid_hint, output(containsAll([]))).
arc_test_property(v('03560426'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('03560426'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('03560426'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('03560426'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('05a7bcf2'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('05a7bcf2'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v('0692e18c'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('0692e18c'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(v('0692e18c'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('070dd51e'), grid_hint, output(containsAll([]))).
arc_test_property(v('0934a4d8'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('09c534e7'), grid_hint, color_change_only).
arc_test_property(v('0a1d4ef5'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('0a2355a6'), grid_hint, color_change_only).
arc_test_property(v('0b17323b'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('0b17323b'), grid_hint, output(containsAll([]))).
arc_test_property(v('0b17323b'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('0bb8deee'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('0bb8deee'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('0bb8deee'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(v('0becf7df'), grid_hint, color_change_only).
arc_test_property(v('0becf7df'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('0becf7df'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('0c786b71'), grid_hint, input(grid_size(i, 4, 3))).
arc_test_property(v('0c786b71'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('0c786b71'), grid_hint, output(grid_size(o, 8, 6))).
arc_test_property(v('0c786b71'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v('0c786b71'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('0c9aba6e'), grid_hint, input(grid_size(i, 4, 13))).
arc_test_property(v('0c9aba6e'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('0c9aba6e'), grid_hint, input(has_y_columns(orange))).
arc_test_property(v('0c9aba6e'), grid_hint, output(grid_size(o, 4, 6))).
arc_test_property(v('0c9aba6e'), grid_hint, output(purportional(1, 13r6))).
arc_test_property(v('0d87d2a6'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('0d87d2a6'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('0d87d2a6'), grid_hint, output(has_x_columns(blue))).
arc_test_property(v('0d87d2a6'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('0e671a1a'), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(v('0e671a1a'), grid_hint, output(containsAll([]))).
arc_test_property(v('0e671a1a'), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(v('0f63c0b9'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('0f63c0b9'), grid_hint, output(containsAll([]))).
arc_test_property(v('0f63c0b9'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('0f63c0b9'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('103eff5b'), grid_hint, color_change_only).
arc_test_property(v('11e1fe23'), grid_hint, output(containsAll([]))).
arc_test_property(v('11e1fe23'), grid_hint, output(purportional_mass(7r3))).
arc_test_property(v('12422b43'), grid_hint, output(containsAll([]))).
arc_test_property(v('12eac192'), grid_hint, color_change_only).
arc_test_property(v('136b0064'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('136b0064'), grid_hint, input(has_x_columns(yellow))).
arc_test_property(v('136b0064'), grid_hint, output(purportional(15r7, 1))).
arc_test_property(v('13713586'), grid_hint, output(containsAll([]))).
arc_test_property(v('137f0df0'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('137f0df0'), grid_hint, output(containsAll([]))).
arc_test_property(v('137f0df0'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('14754a24'), grid_hint, color_change_only).
arc_test_property(v('15113be4'), grid_hint, color_change_only).
arc_test_property(v('15113be4'), grid_hint, input(grid_size(i, 23, 23))).
arc_test_property(v('15113be4'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('15113be4'), grid_hint, input(has_x_columns(yellow))).
arc_test_property(v('15113be4'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('15113be4'), grid_hint, input(has_y_columns(yellow))).
arc_test_property(v('15113be4'), grid_hint, output(grid_size(o, 23, 23))).
arc_test_property(v('15113be4'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('15113be4'), grid_hint, output(has_x_columns(yellow))).
arc_test_property(v('15113be4'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('15113be4'), grid_hint, output(has_y_columns(yellow))).
arc_test_property(v('15663ba9'), grid_hint, color_change_only).
arc_test_property(v('15696249'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('15696249'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('15696249'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(v('15696249'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('15696249'), grid_hint, output(purportional_mass(3))).
arc_test_property(v('16b78196'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('16b78196'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v('17b80ad2'), grid_hint, output(containsAll([]))).
arc_test_property(v('17cae0c1'), grid_hint, input(grid_size(i, 9, 3))).
arc_test_property(v('17cae0c1'), grid_hint, output(grid_size(o, 9, 3))).
arc_test_property(v('18419cfa'), grid_hint, output(containsAll([]))).
arc_test_property(v('195ba7dc'), grid_hint, input(grid_size(i, 13, 5))).
arc_test_property(v('195ba7dc'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('195ba7dc'), grid_hint, input(has_x_columns(red))).
arc_test_property(v('195ba7dc'), grid_hint, output(grid_size(o, 6, 5))).
arc_test_property(v('195ba7dc'), grid_hint, output(purportional(13r6, 1))).
arc_test_property(v('1990f7a8'), grid_hint, output(grid_size(o, 7, 7))).
arc_test_property(v('1990f7a8'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('1990f7a8'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('1990f7a8'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('1990f7a8'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('19bb5feb'), grid_hint, output(grid_size(o, 2, 2))).
arc_test_property(v('1a2e2828'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('1a2e2828'), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(v('1a6449f1'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('1acc24af'), grid_hint, color_change_only).
arc_test_property(v('1acc24af'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(v('1acc24af'), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(v('1c02dbbe'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('1c02dbbe'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('1c0d0a4b'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('1c0d0a4b'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('1d0a4b61'), grid_hint, input(grid_size(i, 25, 25))).
arc_test_property(v('1d0a4b61'), grid_hint, output(containsAll([]))).
arc_test_property(v('1d0a4b61'), grid_hint, output(grid_size(o, 25, 25))).
arc_test_property(v('1d398264'), grid_hint, output(containsAll([]))).
arc_test_property(v('1da012fc'), grid_hint, color_change_only).
arc_test_property(v('1e81d6f9'), grid_hint, input(containsAll([]))).
arc_test_property(v('1e81d6f9'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('1e81d6f9'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('1e81d6f9'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('1e81d6f9'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('1e81d6f9'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('1e81d6f9'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('1e97544e'), grid_hint, input(grid_size(i, 23, 23))).
arc_test_property(v('1e97544e'), grid_hint, output(containsAll([]))).
arc_test_property(v('1e97544e'), grid_hint, output(grid_size(o, 23, 23))).
arc_test_property(v('2072aba6'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('2072aba6'), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(v('2072aba6'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v('2072aba6'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('212895b5'), grid_hint, output(containsAll([]))).
arc_test_property(v('21f83797'), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(v('21f83797'), grid_hint, output(containsAll([]))).
arc_test_property(v('21f83797'), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(v('21f83797'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('21f83797'), grid_hint, output(has_x_columns(red))).
arc_test_property(v('21f83797'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('21f83797'), grid_hint, output(has_y_columns(red))).
arc_test_property(v('22a4bbc2'), grid_hint, color_change_only).
arc_test_property(v('25094a63'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('25094a63'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v('2546ccf6'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('2546ccf6'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('2546ccf6'), grid_hint, output(containsAll([]))).
arc_test_property(v('2546ccf6'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('2546ccf6'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('256b0a75'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('256b0a75'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('2685904e'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('2685904e'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('2685904e'), grid_hint, input(has_y_columns(silver))).
arc_test_property(v('2685904e'), grid_hint, output(containsAll([]))).
arc_test_property(v('2685904e'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('2685904e'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('2685904e'), grid_hint, output(has_y_columns(silver))).
arc_test_property(v('2697da3f'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('2753e76c'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(v('27a77e38'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('27a77e38'), grid_hint, input(has_y_columns(silver))).
arc_test_property(v('27a77e38'), grid_hint, output(containsAll([]))).
arc_test_property(v('27a77e38'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('27a77e38'), grid_hint, output(has_y_columns(silver))).
arc_test_property(v('27f8ce4f'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('27f8ce4f'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('27f8ce4f'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(v('27f8ce4f'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('281123b4'), grid_hint, input(grid_size(i, 19, 4))).
arc_test_property(v('281123b4'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('281123b4'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(v('281123b4'), grid_hint, output(purportional(19r4, 1))).
arc_test_property(v('29700607'), grid_hint, output(containsAll([]))).
arc_test_property(v('2a5f8217'), grid_hint, color_change_only).
arc_test_property(v('2c0b0aff'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('310f3251'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('3194b014'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('3194b014'), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(v('3194b014'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('3194b014'), grid_hint, output(purportional(20r3, 20r3))).
arc_test_property(v('319f2597'), grid_hint, input(containsAll([]))).
arc_test_property(v('319f2597'), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(v('319f2597'), grid_hint, output(grid_size(o, 20, 20))).
arc_test_property(v('31adaf00'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('31adaf00'), grid_hint, output(containsAll([]))).
arc_test_property(v('31adaf00'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('31d5ba1a'), grid_hint, input(grid_size(i, 5, 6))).
arc_test_property(v('31d5ba1a'), grid_hint, output(grid_size(o, 5, 3))).
arc_test_property(v('31d5ba1a'), grid_hint, output(purportional(1, 2))).
arc_test_property(v('332efdb3'), grid_hint, output(containsAll([]))).
arc_test_property(v('332efdb3'), grid_hint, output(has_x_columns(blue))).
arc_test_property(v('332efdb3'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('332efdb3'), grid_hint, output(has_y_columns(blue))).
arc_test_property(v('332efdb3'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('33b52de3'), grid_hint, color_change_only).
arc_test_property(v('33b52de3'), grid_hint, input(grid_size(i, 23, 23))).
arc_test_property(v('33b52de3'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('33b52de3'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('33b52de3'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('33b52de3'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('33b52de3'), grid_hint, output(grid_size(o, 23, 23))).
arc_test_property(v('33b52de3'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('33b52de3'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('33b52de3'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('33b52de3'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('3490cc26'), grid_hint, output(containsAll([]))).
arc_test_property(v('34b99a2b'), grid_hint, input(grid_size(i, 9, 5))).
arc_test_property(v('34b99a2b'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('34b99a2b'), grid_hint, input(has_x_columns(yellow))).
arc_test_property(v('34b99a2b'), grid_hint, output(grid_size(o, 4, 5))).
arc_test_property(v('34b99a2b'), grid_hint, output(purportional(9r4, 1))).
arc_test_property(v('351d6448'), grid_hint, input(grid_size(i, 13, 15))).
arc_test_property(v('351d6448'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('351d6448'), grid_hint, input(has_y_columns(silver))).
arc_test_property(v('351d6448'), grid_hint, output(grid_size(o, 13, 3))).
arc_test_property(v('351d6448'), grid_hint, output(purportional(1, 5))).
arc_test_property(v('358ba94e'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('358ba94e'), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(v('37d3e8b2'), grid_hint, color_change_only).
arc_test_property(v('3979b1a8'), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(v('3979b1a8'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('3979b1a8'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('3979b1a8'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v('3979b1a8'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('3a301edc'), grid_hint, output(containsAll([]))).
arc_test_property(v('3b4c2228'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('3d31c5b3'), grid_hint, input(grid_size(i, 6, 12))).
arc_test_property(v('3d31c5b3'), grid_hint, output(grid_size(o, 6, 3))).
arc_test_property(v('3d31c5b3'), grid_hint, output(purportional(1, 4))).
arc_test_property(v('3ed85e70'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('3ed85e70'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v('3f23242b'), grid_hint, output(containsAll([]))).
arc_test_property(v('40f6cd08'), grid_hint, color_change_only).
arc_test_property(v('42918530'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('42918530'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('42918530'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('42918530'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('42918530'), grid_hint, output(containsAll([]))).
arc_test_property(v('42918530'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('42918530'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('42918530'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('42918530'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('42a15761'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('42a15761'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('42a15761'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('42a15761'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('456873bc'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('456873bc'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('456873bc'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('456873bc'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('45737921'), grid_hint, color_change_only).
arc_test_property(v('45bbe264'), grid_hint, output(containsAll([]))).
arc_test_property(v('477d2879'), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(v('477d2879'), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(v('47996f11'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('47996f11'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v('48131b3c'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v('4852f2fa'), grid_hint, input(grid_size(i, 9, 9))).
arc_test_property(v('48f8583b'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('48f8583b'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('48f8583b'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(v('48f8583b'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('4aab4007'), grid_hint, input(grid_size(i, 28, 28))).
arc_test_property(v('4aab4007'), grid_hint, output(containsAll([]))).
arc_test_property(v('4aab4007'), grid_hint, output(grid_size(o, 28, 28))).
arc_test_property(v('4acc7107'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('4acc7107'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('4c177718'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('4c177718'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('4c177718'), grid_hint, input(has_y_columns(silver))).
arc_test_property(v('4c177718'), grid_hint, output(grid_size(o, 15, 9))).
arc_test_property(v('4c177718'), grid_hint, output(purportional(1, 5r3))).
arc_test_property(v('4cd1b7b2'), grid_hint, input(grid_size(i, 4, 4))).
arc_test_property(v('4cd1b7b2'), grid_hint, output(containsAll([]))).
arc_test_property(v('4cd1b7b2'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(v('4e45f183'), grid_hint, color_change_only).
arc_test_property(v('4e45f183'), grid_hint, input(grid_size(i, 19, 19))).
arc_test_property(v('4e45f183'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('4e45f183'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('4e45f183'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('4e45f183'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('4e45f183'), grid_hint, output(grid_size(o, 19, 19))).
arc_test_property(v('4e45f183'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('4e45f183'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('4e45f183'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('4e45f183'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('4e469f39'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('4e469f39'), grid_hint, output(containsAll([]))).
arc_test_property(v('4e469f39'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('4f537728'), grid_hint, color_change_only).
arc_test_property(v('4f537728'), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(v('4f537728'), grid_hint, output(grid_size(o, 20, 20))).
arc_test_property(v('4ff4c9da'), grid_hint, color_change_only).
arc_test_property(v('4ff4c9da'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('4ff4c9da'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('4ff4c9da'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('4ff4c9da'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('506d28a5'), grid_hint, input(grid_size(i, 5, 9))).
arc_test_property(v('506d28a5'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('506d28a5'), grid_hint, input(has_y_columns(yellow))).
arc_test_property(v('506d28a5'), grid_hint, output(grid_size(o, 5, 4))).
arc_test_property(v('506d28a5'), grid_hint, output(purportional(1, 9r4))).
arc_test_property(v('50f325b5'), grid_hint, color_change_only).
arc_test_property(v('516b51b7'), grid_hint, color_change_only).
arc_test_property(v('5207a7b5'), grid_hint, output(containsAll([]))).
arc_test_property(v('5289ad53'), grid_hint, output(grid_size(o, 3, 2))).
arc_test_property(v('52fd389e'), grid_hint, input(grid_size(i, 25, 25))).
arc_test_property(v('52fd389e'), grid_hint, output(containsAll([]))).
arc_test_property(v('52fd389e'), grid_hint, output(grid_size(o, 25, 25))).
arc_test_property(v('54db823b'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('54db823b'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('55059096'), grid_hint, output(containsAll([]))).
arc_test_property(v('551d5bf1'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('551d5bf1'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('551d5bf1'), grid_hint, output(containsAll([]))).
arc_test_property(v('575b1a71'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('575b1a71'), grid_hint, output(containsAll([]))).
arc_test_property(v('575b1a71'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('5783df64'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('5833af48'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('5833af48'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('58743b76'), grid_hint, color_change_only).
arc_test_property(v('59341089'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('59341089'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('59341089'), grid_hint, output(grid_size(o, 12, 3))).
arc_test_property(v('59341089'), grid_hint, output(purportional(1r4, 1))).
arc_test_property(v('59341089'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('5a5a2103'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('5a5a2103'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('5a5a2103'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('5a5a2103'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('5af49b42'), grid_hint, output(containsAll([]))).
arc_test_property(v('5b526a93'), grid_hint, output(containsAll([]))).
arc_test_property(v('5b6cbef5'), grid_hint, input(grid_size(i, 4, 4))).
arc_test_property(v('5b6cbef5'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('5b6cbef5'), grid_hint, output(grid_size(o, 16, 16))).
arc_test_property(v('5b6cbef5'), grid_hint, output(purportional(1r4, 1r4))).
arc_test_property(v('5d2a5c43'), grid_hint, input(grid_size(i, 9, 6))).
arc_test_property(v('5d2a5c43'), grid_hint, input(has_x_columns(blue))).
arc_test_property(v('5d2a5c43'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('5d2a5c43'), grid_hint, output(grid_size(o, 4, 6))).
arc_test_property(v('5d2a5c43'), grid_hint, output(purportional(9r4, 1))).
arc_test_property(v('60a26a3e'), grid_hint, output(containsAll([]))).
arc_test_property(v('60c09cac'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v('60c09cac'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('626c0bcc'), grid_hint, color_change_only).
arc_test_property(v('626c0bcc'), grid_hint, input(grid_size(i, 7, 7))).
arc_test_property(v('626c0bcc'), grid_hint, output(grid_size(o, 7, 7))).
arc_test_property(v('62ab2642'), grid_hint, output(containsAll([]))).
arc_test_property(v('62b74c02'), grid_hint, output(containsAll([]))).
arc_test_property(v('639f5a19'), grid_hint, color_change_only).
arc_test_property(v('639f5a19'), grid_hint, input(grid_size(i, 23, 23))).
arc_test_property(v('639f5a19'), grid_hint, output(grid_size(o, 23, 23))).
arc_test_property(v('642248e4'), grid_hint, output(containsAll([]))).
arc_test_property(v('642d658d'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('642d658d'), grid_hint, output(grid_size(o, 1, 1))).
arc_test_property(v('66e6c45b'), grid_hint, input(grid_size(i, 4, 4))).
arc_test_property(v('66e6c45b'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(v('66f2d22f'), grid_hint, input(grid_size(i, 14, 4))).
arc_test_property(v('66f2d22f'), grid_hint, output(grid_size(o, 7, 4))).
arc_test_property(v('66f2d22f'), grid_hint, output(purportional(2, 1))).
arc_test_property(v('67b4a34d'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(v('67b4a34d'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(v('67b4a34d'), grid_hint, output(purportional(4, 4))).
arc_test_property(v('67b4a34d'), grid_hint, output(purportional_mass(1r16))).
arc_test_property(v('68b67ca3'), grid_hint, input(grid_size(i, 6, 6))).
arc_test_property(v('68b67ca3'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('68b67ca3'), grid_hint, output(purportional(2, 2))).
arc_test_property(v('692cd3b6'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('692cd3b6'), grid_hint, output(containsAll([]))).
arc_test_property(v('692cd3b6'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('695367ec'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('69889d6e'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('69889d6e'), grid_hint, output(containsAll([]))).
arc_test_property(v('69889d6e'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('6a11f6da'), grid_hint, input(grid_size(i, 5, 15))).
arc_test_property(v('6a11f6da'), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(v('6a11f6da'), grid_hint, output(purportional(1, 3))).
arc_test_property(v('6df30ad6'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('6df30ad6'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('6ea4a07e'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('6ea4a07e'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('6f473927'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('6f473927'), grid_hint, output(purportional(1r2, 1))).
arc_test_property(v('7039b2d7'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('7039b2d7'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('705a3229'), grid_hint, output(containsAll([]))).
arc_test_property(v('712bf12e'), grid_hint, output(containsAll([]))).
arc_test_property(v('72207abc'), grid_hint, output(containsAll([]))).
arc_test_property(v('72a961c9'), grid_hint, output(containsAll([]))).
arc_test_property(v('73182012'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('73182012'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(v('73182012'), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(v('73182012'), grid_hint, output(purportional(3, 3))).
arc_test_property(v('73182012'), grid_hint, output(purportional_mass(1r4))).
arc_test_property(v('73ccf9c2'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('759f3fd3'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('759f3fd3'), grid_hint, input(has_x_columns(green))).
arc_test_property(v('759f3fd3'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('759f3fd3'), grid_hint, input(has_y_columns(green))).
arc_test_property(v('759f3fd3'), grid_hint, output(containsAll([]))).
arc_test_property(v('759f3fd3'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('759f3fd3'), grid_hint, output(has_x_columns(green))).
arc_test_property(v('759f3fd3'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('759f3fd3'), grid_hint, output(has_y_columns(green))).
arc_test_property(v('762cd429'), grid_hint, output(containsAll([]))).
arc_test_property(v('770cc55f'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('770cc55f'), grid_hint, input(has_y_columns(red))).
arc_test_property(v('770cc55f'), grid_hint, output(containsAll([]))).
arc_test_property(v('770cc55f'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('770cc55f'), grid_hint, output(has_y_columns(red))).
arc_test_property(v('782b5218'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('782b5218'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('7953d61e'), grid_hint, input(grid_size(i, 4, 4))).
arc_test_property(v('7953d61e'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('7953d61e'), grid_hint, output(grid_size(o, 8, 8))).
arc_test_property(v('7953d61e'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v('7953d61e'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('79fb03f4'), grid_hint, output(containsAll([]))).
arc_test_property(v('7bb29440'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('7c8af763'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('7c8af763'), grid_hint, output(containsAll([]))).
arc_test_property(v('7c8af763'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('7c9b52a0'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(v('7d18a6fb'), grid_hint, output(grid_size(o, 7, 7))).
arc_test_property(v('7d18a6fb'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('7d18a6fb'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('7d18a6fb'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('7d18a6fb'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('7d1f7ee8'), grid_hint, color_change_only).
arc_test_property(v('7d419a02'), grid_hint, color_change_only).
arc_test_property(v('7e02026e'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(v('7e02026e'), grid_hint, output(containsAll([]))).
arc_test_property(v('7e02026e'), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(v('7ee1c6ea'), grid_hint, color_change_only).
arc_test_property(v('7ee1c6ea'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('7ee1c6ea'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('817e6c09'), grid_hint, color_change_only).
arc_test_property(v('81c0276b'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('81c0276b'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('833dafe3'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('833dafe3'), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v('833dafe3'), grid_hint, output(purportional_mass(4))).
arc_test_property(v('845d6e51'), grid_hint, color_change_only).
arc_test_property(v('845d6e51'), grid_hint, input(has_y_columns(bg))).
arc_test_property(v('845d6e51'), grid_hint, input(has_y_columns(black))).
arc_test_property(v('845d6e51'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('845d6e51'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('84db8fc4'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('84db8fc4'), grid_hint, output(containsAll([]))).
arc_test_property(v('84db8fc4'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('84f2aca1'), grid_hint, output(containsAll([]))).
arc_test_property(v('8597cfd7'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('8597cfd7'), grid_hint, input(has_y_columns(silver))).
arc_test_property(v('8597cfd7'), grid_hint, output(grid_size(o, 2, 2))).
arc_test_property(v('85b81ff1'), grid_hint, input(grid_size(i, 14, 13))).
arc_test_property(v('85b81ff1'), grid_hint, input(has_x_columns(bg))).
arc_test_property(v('85b81ff1'), grid_hint, input(has_x_columns(black))).
arc_test_property(v('85b81ff1'), grid_hint, output(grid_size(o, 14, 13))).
arc_test_property(v('85b81ff1'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('85b81ff1'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('8719f442'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('8719f442'), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v('8719f442'), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v('8719f442'), grid_hint, output(purportional(1r5, 1r5))).
arc_test_property(v('8719f442'), grid_hint, output(purportional_mass(13))).
arc_test_property(v('88207623'), grid_hint, output(containsAll([]))).
arc_test_property(v('8a371977'), grid_hint, output(containsAll([]))).
arc_test_property(v('8b28cd80'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('8b28cd80'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(v('8b28cd80'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('8ba14f53'), grid_hint, input(grid_size(i, 9, 4))).
arc_test_property(v('8ba14f53'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('8ba14f53'), grid_hint, output(purportional(3, 4r3))).
arc_test_property(v('8dae5dfc'), grid_hint, color_change_only).
arc_test_property(v('8e2edd66'), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v('8e2edd66'), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(v('8e2edd66'), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v('8fbca751'), grid_hint, output(containsAll([]))).
arc_test_property(v('903d1b4a'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(v('903d1b4a'), grid_hint, output(grid_size(o, 16, 16))).
arc_test_property(v('9110e3c5'), grid_hint, input(grid_size(i, 7, 7))).
arc_test_property(v('9110e3c5'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('9110e3c5'), grid_hint, output(purportional(7r3, 7r3))).
arc_test_property(v('917bccba'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(v('917bccba'), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(v('929ab4e9'), grid_hint, input(grid_size(i, 24, 24))).
arc_test_property(v('929ab4e9'), grid_hint, output(grid_size(o, 24, 24))).
arc_test_property(v('92e50de0'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('92e50de0'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('92e50de0'), grid_hint, output(containsAll([]))).
arc_test_property(v('92e50de0'), grid_hint, output(has_x_columns(fg))).
arc_test_property(v('92e50de0'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('9356391f'), grid_hint, input(grid_size(i, 16, 16))).
arc_test_property(v('9356391f'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('9356391f'), grid_hint, input(has_y_columns(silver))).
arc_test_property(v('9356391f'), grid_hint, output(grid_size(o, 16, 16))).
arc_test_property(v('9356391f'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('9356391f'), grid_hint, output(has_y_columns(silver))).
arc_test_property(v('93b4f4b3'), grid_hint, output(has_y_columns(fg))).
arc_test_property(v('93b4f4b3'), grid_hint, output(purportional(2, 1))).
arc_test_property(v('94414823'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('94414823'), grid_hint, output(containsAll([]))).
arc_test_property(v('94414823'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('94414823'), grid_hint, output(purportional_mass(19r11))).
arc_test_property(v('95a58926'), grid_hint, input(has_y_columns(fg))).
arc_test_property(v('95a58926'), grid_hint, input(has_y_columns(silver))).
arc_test_property(v('963f59bc'), grid_hint, output(containsAll([]))).
arc_test_property(v('96a8c0cd'), grid_hint, output(containsAll([]))).
arc_test_property(v('97239e3d'), grid_hint, input(grid_size(i, 17, 17))).
arc_test_property(v('97239e3d'), grid_hint, output(containsAll([]))).
arc_test_property(v('97239e3d'), grid_hint, output(grid_size(o, 17, 17))).
arc_test_property(v('9772c176'), grid_hint, output(containsAll([]))).
arc_test_property(v('981571dc'), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v('981571dc'), grid_hint, output(containsAll([]))).
arc_test_property(v('981571dc'), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v('992798f6'), grid_hint, output(containsAll([]))).
arc_test_property(v('99306f82'), grid_hint, output(containsAll([]))).
arc_test_property(v('9a4bb226'), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v('9a4bb226'), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v('9a4bb226'), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v('9a4bb226'), grid_hint, output(purportional(5, 5))).
arc_test_property(v('9a4bb226'), grid_hint, output(purportional_mass(1r4))).
arc_test_property(v('9b2a60aa'), grid_hint, output(containsAll([]))).
arc_test_property(v('9b365c51'), grid_hint, input(has_x_columns(fg))).
arc_test_property(v('9c1e755f'), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v('9c1e755f'), grid_hint, output(containsAll([]))).
arc_test_property(v('9c1e755f'), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v('9caba7c3'), grid_hint, color_change_only).
arc_test_property(v('9caba7c3'), grid_hint, input(grid_size(i, 19, 19))).
arc_test_property(v('9caba7c3'), grid_hint, output(grid_size(o, 19, 19))).
arc_test_property(v('9ddd00f0'), grid_hint, output(containsAll([]))).
arc_test_property(v('9ddd00f0'), grid_hint, output(has_x_columns(bg))).
arc_test_property(v('9ddd00f0'), grid_hint, output(has_x_columns(black))).
arc_test_property(v('9ddd00f0'), grid_hint, output(has_y_columns(bg))).
arc_test_property(v('9ddd00f0'), grid_hint, output(has_y_columns(black))).
arc_test_property(v('9def23fe'), grid_hint, output(containsAll([]))).
arc_test_property(v('9f27f097'), grid_hint, input(grid_size(i, 12, 12))).
arc_test_property(v('9f27f097'), grid_hint, output(containsAll([]))).
arc_test_property(v('9f27f097'), grid_hint, output(grid_size(o, 12, 12))).
arc_test_property(v(a096bf4d), grid_hint, color_change_only).
arc_test_property(v(a096bf4d), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(a096bf4d), grid_hint, input(has_x_columns(black))).
arc_test_property(v(a096bf4d), grid_hint, input(has_y_columns(bg))).
arc_test_property(v(a096bf4d), grid_hint, input(has_y_columns(black))).
arc_test_property(v(a096bf4d), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(a096bf4d), grid_hint, output(has_x_columns(black))).
arc_test_property(v(a096bf4d), grid_hint, output(has_y_columns(bg))).
arc_test_property(v(a096bf4d), grid_hint, output(has_y_columns(black))).
arc_test_property(v(a3f84088), grid_hint, output(containsAll([]))).
arc_test_property(v(a406ac07), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(a406ac07), grid_hint, output(containsAll([]))).
arc_test_property(v(a406ac07), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(a57f2f04), grid_hint, output(containsAll([]))).
arc_test_property(v(a59b95c0), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v(a59b95c0), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(a680ac02), grid_hint, output(purportional_mass(3r7))).
arc_test_property(v(a8610ef7), grid_hint, color_change_only).
arc_test_property(v(a8610ef7), grid_hint, input(grid_size(i, 6, 6))).
arc_test_property(v(a8610ef7), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(v(a934301b), grid_hint, input(containsAll([]))).
arc_test_property(v(aa18de87), grid_hint, output(containsAll([]))).
arc_test_property(v(aa300dc3), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(aa300dc3), grid_hint, output(containsAll([]))).
arc_test_property(v(aa300dc3), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(ac605cbb), grid_hint, output(containsAll([]))).
arc_test_property(v(ad7e01d0), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(ae58858e), grid_hint, color_change_only).
arc_test_property(v(ae58858e), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(ae58858e), grid_hint, input(has_x_columns(black))).
arc_test_property(v(ae58858e), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(ae58858e), grid_hint, output(has_x_columns(black))).
arc_test_property(v(aee291af), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v(af22c60d), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v(af22c60d), grid_hint, output(containsAll([]))).
arc_test_property(v(af22c60d), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v(af24b4cc), grid_hint, input(grid_size(i, 10, 9))).
arc_test_property(v(af24b4cc), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(af24b4cc), grid_hint, input(has_x_columns(black))).
arc_test_property(v(af24b4cc), grid_hint, input(has_y_columns(bg))).
arc_test_property(v(af24b4cc), grid_hint, input(has_y_columns(black))).
arc_test_property(v(af24b4cc), grid_hint, output(grid_size(o, 5, 4))).
arc_test_property(v(af24b4cc), grid_hint, output(purportional(2, 9r4))).
arc_test_property(v(af24b4cc), grid_hint, output(purportional_mass(1r6))).
arc_test_property(v(b0722778), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(b0722778), grid_hint, input(has_x_columns(black))).
arc_test_property(v(b0722778), grid_hint, input(has_y_columns(bg))).
arc_test_property(v(b0722778), grid_hint, input(has_y_columns(black))).
arc_test_property(v(b0722778), grid_hint, output(purportional(9r2, 1))).
arc_test_property(v(b0722778), grid_hint, output(purportional_mass(1r3))).
arc_test_property(v(b0f4d537), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(b0f4d537), grid_hint, input(has_x_columns(silver))).
arc_test_property(v(b15fca0b), grid_hint, output(containsAll([]))).
arc_test_property(v(b1fc8b8e), grid_hint, input(grid_size(i, 6, 6))).
arc_test_property(v(b1fc8b8e), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(v(b1fc8b8e), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(b1fc8b8e), grid_hint, output(has_x_columns(black))).
arc_test_property(v(b1fc8b8e), grid_hint, output(has_y_columns(bg))).
arc_test_property(v(b1fc8b8e), grid_hint, output(has_y_columns(black))).
arc_test_property(v(b1fc8b8e), grid_hint, output(purportional(6r5, 6r5))).
arc_test_property(v(b20f7c8b), grid_hint, color_change_only).
arc_test_property(v(b20f7c8b), grid_hint, input(grid_size(i, 22, 18))).
arc_test_property(v(b20f7c8b), grid_hint, output(grid_size(o, 22, 18))).
arc_test_property(v(b457fec5), grid_hint, color_change_only).
arc_test_property(v(b4a43f3b), grid_hint, input(grid_size(i, 6, 13))).
arc_test_property(v(b4a43f3b), grid_hint, input(has_y_columns(fg))).
arc_test_property(v(b4a43f3b), grid_hint, input(has_y_columns(silver))).
arc_test_property(v(b4a43f3b), grid_hint, output(grid_size(o, 18, 18))).
arc_test_property(v(b4a43f3b), grid_hint, output(purportional(1r3, 13r18))).
arc_test_property(v(b7cb93ac), grid_hint, output(grid_size(o, 4, 3))).
arc_test_property(v(b7f8a4d8), grid_hint, output(containsAll([]))).
arc_test_property(v(b7fb29bc), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v(b7fb29bc), grid_hint, output(containsAll([]))).
arc_test_property(v(b7fb29bc), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v(b7fb29bc), grid_hint, output(purportional_mass(27r11))).
arc_test_property(v(b942fd60), grid_hint, output(containsAll([]))).
arc_test_property(v(b9630600), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v(b9630600), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v(ba9d41b8), grid_hint, input(containsAll([]))).
arc_test_property(v(bb52a14b), grid_hint, input(grid_size(i, 22, 22))).
arc_test_property(v(bb52a14b), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(bb52a14b), grid_hint, input(has_x_columns(black))).
arc_test_property(v(bb52a14b), grid_hint, output(containsAll([]))).
arc_test_property(v(bb52a14b), grid_hint, output(grid_size(o, 22, 22))).
arc_test_property(v(bb52a14b), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(bb52a14b), grid_hint, output(has_x_columns(black))).
arc_test_property(v(bbb1b8b6), grid_hint, input(grid_size(i, 9, 4))).
arc_test_property(v(bbb1b8b6), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(bbb1b8b6), grid_hint, input(has_x_columns(silver))).
arc_test_property(v(bbb1b8b6), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(v(bbb1b8b6), grid_hint, output(purportional(9r4, 1))).
arc_test_property(v(bc4146bd), grid_hint, input(grid_size(i, 4, 4))).
arc_test_property(v(bc4146bd), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(bc4146bd), grid_hint, output(grid_size(o, 20, 4))).
arc_test_property(v(bc4146bd), grid_hint, output(purportional(1r5, 1))).
arc_test_property(v(bc4146bd), grid_hint, output(purportional_mass(5))).
arc_test_property(v(bd14c3bf), grid_hint, color_change_only).
arc_test_property(v(be03b35f), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(v(be03b35f), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(be03b35f), grid_hint, input(has_x_columns(black))).
arc_test_property(v(be03b35f), grid_hint, input(has_y_columns(bg))).
arc_test_property(v(be03b35f), grid_hint, input(has_y_columns(black))).
arc_test_property(v(be03b35f), grid_hint, output(grid_size(o, 2, 2))).
arc_test_property(v(be03b35f), grid_hint, output(purportional(5r2, 5r2))).
arc_test_property(v(bf699163), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v(bf699163), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v(bf89d739), grid_hint, output(containsAll([]))).
arc_test_property(v(c1990cce), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(c1990cce), grid_hint, input(has_x_columns(red))).
arc_test_property(v(c1990cce), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(c3202e5a), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v(c3202e5a), grid_hint, input(grid_size(i, 23, 23))).
arc_test_property(v(c3202e5a), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(c3202e5a), grid_hint, input(has_y_columns(fg))).
arc_test_property(v(c35c1b4c), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(c35c1b4c), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(c48954c1), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v(c48954c1), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(c48954c1), grid_hint, output(grid_size(o, 9, 9))).
arc_test_property(v(c48954c1), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v(c48954c1), grid_hint, output(purportional_mass(9))).
arc_test_property(v(c64f1187), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(c64f1187), grid_hint, output(has_x_columns(black))).
arc_test_property(v(c64f1187), grid_hint, output(has_y_columns(bg))).
arc_test_property(v(c64f1187), grid_hint, output(has_y_columns(black))).
arc_test_property(v(c663677b), grid_hint, input(grid_size(i, 27, 27))).
arc_test_property(v(c663677b), grid_hint, input(has_x_columns(blue))).
arc_test_property(v(c663677b), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(c663677b), grid_hint, output(containsAll([]))).
arc_test_property(v(c663677b), grid_hint, output(grid_size(o, 27, 27))).
arc_test_property(v(c663677b), grid_hint, output(has_x_columns(blue))).
arc_test_property(v(c663677b), grid_hint, output(has_x_columns(fg))).
arc_test_property(v(c663677b), grid_hint, output(has_y_columns(blue))).
arc_test_property(v(c663677b), grid_hint, output(has_y_columns(fg))).
arc_test_property(v(c6e1b8da), grid_hint, input(grid_size(i, 20, 20))).
arc_test_property(v(c6e1b8da), grid_hint, output(grid_size(o, 20, 20))).
arc_test_property(v(c7d4e6ad), grid_hint, color_change_only).
arc_test_property(v(c7d4e6ad), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(c7d4e6ad), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(c87289bb), grid_hint, output(containsAll([]))).
arc_test_property(v(c8b7cc0f), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v(c92b942c), grid_hint, output(purportional(1r3, 1r3))).
arc_test_property(v(c97c0139), grid_hint, output(containsAll([]))).
arc_test_property(v(ca8de6ea), grid_hint, input(grid_size(i, 5, 5))).
arc_test_property(v(ca8de6ea), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v(ca8de6ea), grid_hint, output(purportional(5r3, 5r3))).
arc_test_property(v(ca8f78db), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v(ca8f78db), grid_hint, output(containsAll([]))).
arc_test_property(v(ca8f78db), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v(cad67732), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(cad67732), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v(cb227835), grid_hint, output(containsAll([]))).
arc_test_property(v(ccd554ac), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(cd3c21df), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v(ce039d91), grid_hint, color_change_only).
arc_test_property(v(ce039d91), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(ce039d91), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(cf133acc), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v(cf133acc), grid_hint, output(containsAll([]))).
arc_test_property(v(cf133acc), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v(cfb2ce5a), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(cfb2ce5a), grid_hint, output(containsAll([]))).
arc_test_property(v(cfb2ce5a), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(d19f7514), grid_hint, input(grid_size(i, 4, 12))).
arc_test_property(v(d19f7514), grid_hint, output(grid_size(o, 4, 6))).
arc_test_property(v(d19f7514), grid_hint, output(purportional(1, 2))).
arc_test_property(v(d282b262), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v(d282b262), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v(d304284e), grid_hint, input(grid_size(i, 28, 23))).
arc_test_property(v(d304284e), grid_hint, output(containsAll([]))).
arc_test_property(v(d304284e), grid_hint, output(grid_size(o, 28, 23))).
arc_test_property(v(d37a1ef5), grid_hint, output(containsAll([]))).
arc_test_property(v(d47aa2ff), grid_hint, input(grid_size(i, 21, 10))).
arc_test_property(v(d47aa2ff), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(d47aa2ff), grid_hint, input(has_x_columns(silver))).
arc_test_property(v(d47aa2ff), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(d47aa2ff), grid_hint, output(purportional(21r10, 1))).
arc_test_property(v(d492a647), grid_hint, output(containsAll([]))).
arc_test_property(v(d4b1c2b1), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v(d56f2372), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v(d5c634a2), grid_hint, output(grid_size(o, 6, 3))).
arc_test_property(v(d5c634a2), grid_hint, output(purportional_mass(1r4))).
arc_test_property(v(d931c21c), grid_hint, output(containsAll([]))).
arc_test_property(v(d94c3b52), grid_hint, color_change_only).
arc_test_property(v(d94c3b52), grid_hint, input(grid_size(i, 25, 17))).
arc_test_property(v(d94c3b52), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(d94c3b52), grid_hint, input(has_x_columns(black))).
arc_test_property(v(d94c3b52), grid_hint, input(has_y_columns(bg))).
arc_test_property(v(d94c3b52), grid_hint, input(has_y_columns(black))).
arc_test_property(v(d94c3b52), grid_hint, output(grid_size(o, 25, 17))).
arc_test_property(v(d94c3b52), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(d94c3b52), grid_hint, output(has_x_columns(black))).
arc_test_property(v(d94c3b52), grid_hint, output(has_y_columns(bg))).
arc_test_property(v(d94c3b52), grid_hint, output(has_y_columns(black))).
arc_test_property(v(da2b0fe3), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(da2b0fe3), grid_hint, output(containsAll([]))).
arc_test_property(v(da2b0fe3), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(da515329), grid_hint, output(containsAll([]))).
arc_test_property(v(dc2aa30b), grid_hint, color_change_only).
arc_test_property(v(dc2aa30b), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(v(dc2aa30b), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(dc2aa30b), grid_hint, input(has_x_columns(black))).
arc_test_property(v(dc2aa30b), grid_hint, input(has_y_columns(bg))).
arc_test_property(v(dc2aa30b), grid_hint, input(has_y_columns(black))).
arc_test_property(v(dc2aa30b), grid_hint, output(grid_size(o, 11, 11))).
arc_test_property(v(dc2aa30b), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(dc2aa30b), grid_hint, output(has_x_columns(black))).
arc_test_property(v(dc2aa30b), grid_hint, output(has_y_columns(bg))).
arc_test_property(v(dc2aa30b), grid_hint, output(has_y_columns(black))).
arc_test_property(v(dc2e9a9d), grid_hint, output(containsAll([]))).
arc_test_property(v(dd2401ed), grid_hint, input(grid_size(i, 15, 7))).
arc_test_property(v(dd2401ed), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(dd2401ed), grid_hint, input(has_x_columns(silver))).
arc_test_property(v(dd2401ed), grid_hint, output(grid_size(o, 15, 7))).
arc_test_property(v(dd2401ed), grid_hint, output(has_x_columns(fg))).
arc_test_property(v(dd2401ed), grid_hint, output(has_x_columns(silver))).
arc_test_property(v(de493100), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v(e0fb7511), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(v(e0fb7511), grid_hint, output(containsAll([]))).
arc_test_property(v(e0fb7511), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(v(e133d23d), grid_hint, input(grid_size(i, 7, 3))).
arc_test_property(v(e133d23d), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(e133d23d), grid_hint, input(has_x_columns(yellow))).
arc_test_property(v(e133d23d), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v(e133d23d), grid_hint, output(purportional(7r3, 1))).
arc_test_property(v(e2092e0c), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v(e2092e0c), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v(e345f17b), grid_hint, input(grid_size(i, 8, 4))).
arc_test_property(v(e345f17b), grid_hint, output(grid_size(o, 4, 4))).
arc_test_property(v(e345f17b), grid_hint, output(purportional(2, 1))).
arc_test_property(v(e4075551), grid_hint, output(containsAll([]))).
arc_test_property(v(e57337a4), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v(e57337a4), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v(e57337a4), grid_hint, output(purportional(5, 5))).
arc_test_property(v(e5790162), grid_hint, output(containsAll([]))).
arc_test_property(v(e5c44e8f), grid_hint, input(grid_size(i, 11, 11))).
arc_test_property(v(e5c44e8f), grid_hint, output(containsAll([]))).
arc_test_property(v(e5c44e8f), grid_hint, output(grid_size(o, 11, 11))).
arc_test_property(v(e619ca6e), grid_hint, output(containsAll([]))).
arc_test_property(v(e633a9e5), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v(e633a9e5), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(e633a9e5), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(v(e633a9e5), grid_hint, output(purportional(3r5, 3r5))).
arc_test_property(v(e633a9e5), grid_hint, output(purportional_mass(25r9))).
arc_test_property(v(e66aafb8), grid_hint, input(grid_size(i, 24, 24))).
arc_test_property(v(e681b708), grid_hint, color_change_only).
arc_test_property(v(e69241bd), grid_hint, output(containsAll([]))).
arc_test_property(v(e6de6e8f), grid_hint, input(grid_size(i, 12, 2))).
arc_test_property(v(e6de6e8f), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(e6de6e8f), grid_hint, input(has_x_columns(black))).
arc_test_property(v(e6de6e8f), grid_hint, output(grid_size(o, 7, 8))).
arc_test_property(v(e6de6e8f), grid_hint, output(purportional(12r7, 1r4))).
arc_test_property(v(e6de6e8f), grid_hint, output(purportional_mass(11r13))).
arc_test_property(v(e760a62e), grid_hint, output(containsAll([]))).
arc_test_property(v(e7639916), grid_hint, output(containsAll([]))).
arc_test_property(v(e78887d1), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(e78887d1), grid_hint, input(has_x_columns(black))).
arc_test_property(v(e78887d1), grid_hint, output(has_x_columns(fg))).
arc_test_property(v(e7a25a18), grid_hint, input(grid_size(i, 14, 14))).
arc_test_property(v(e7b06bea), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(e7dd8335), grid_hint, color_change_only).
arc_test_property(v(e872b94a), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v(e872b94a), grid_hint, output(purportional_mass(0))).
arc_test_property(v(e88171ec), grid_hint, output(containsAll([]))).
arc_test_property(v(e95e3d8e), grid_hint, input(grid_size(i, 22, 22))).
arc_test_property(v(e95e3d8e), grid_hint, output(containsAll([]))).
arc_test_property(v(e95e3d8e), grid_hint, output(grid_size(o, 22, 22))).
arc_test_property(v(e99362f0), grid_hint, input(grid_size(i, 9, 11))).
arc_test_property(v(e99362f0), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(e99362f0), grid_hint, input(has_x_columns(yellow))).
arc_test_property(v(e99362f0), grid_hint, input(has_y_columns(fg))).
arc_test_property(v(e99362f0), grid_hint, input(has_y_columns(yellow))).
arc_test_property(v(e99362f0), grid_hint, output(grid_size(o, 4, 5))).
arc_test_property(v(e99362f0), grid_hint, output(purportional(9r4, 11r5))).
arc_test_property(v(e9ac8c9e), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(e9ac8c9e), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(e9b4f6fc), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(v(e9c9d9a1), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(e9c9d9a1), grid_hint, input(has_x_columns(green))).
arc_test_property(v(e9c9d9a1), grid_hint, input(has_y_columns(fg))).
arc_test_property(v(e9c9d9a1), grid_hint, input(has_y_columns(green))).
arc_test_property(v(e9c9d9a1), grid_hint, output(containsAll([]))).
arc_test_property(v(e9c9d9a1), grid_hint, output(has_x_columns(fg))).
arc_test_property(v(e9c9d9a1), grid_hint, output(has_x_columns(green))).
arc_test_property(v(e9c9d9a1), grid_hint, output(has_y_columns(fg))).
arc_test_property(v(e9c9d9a1), grid_hint, output(has_y_columns(green))).
arc_test_property(v(ea959feb), grid_hint, input(grid_size(i, 25, 22))).
arc_test_property(v(ea959feb), grid_hint, output(grid_size(o, 25, 22))).
arc_test_property(v(ea9794b1), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(ea9794b1), grid_hint, output(grid_size(o, 5, 5))).
arc_test_property(v(ea9794b1), grid_hint, output(purportional(2, 2))).
arc_test_property(v(ecaa0ec1), grid_hint, output(purportional_mass(10r13))).
arc_test_property(v(ed74f2f2), grid_hint, input(grid_size(i, 9, 5))).
arc_test_property(v(ed74f2f2), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(ed74f2f2), grid_hint, input(has_x_columns(black))).
arc_test_property(v(ed74f2f2), grid_hint, output(grid_size(o, 3, 3))).
arc_test_property(v(ed74f2f2), grid_hint, output(purportional(3, 5r3))).
arc_test_property(v(ed98d772), grid_hint, input(grid_size(i, 3, 3))).
arc_test_property(v(ed98d772), grid_hint, output(find_ogs_no_pad)).
arc_test_property(v(ed98d772), grid_hint, output(grid_size(o, 6, 6))).
arc_test_property(v(ed98d772), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v(ed98d772), grid_hint, output(purportional_mass(4))).
arc_test_property(v(ef26cbf6), grid_hint, color_change_only).
arc_test_property(v(ef26cbf6), grid_hint, input(has_x_columns(fg))).
arc_test_property(v(ef26cbf6), grid_hint, input(has_x_columns(yellow))).
arc_test_property(v(ef26cbf6), grid_hint, input(has_y_columns(fg))).
arc_test_property(v(ef26cbf6), grid_hint, input(has_y_columns(yellow))).
arc_test_property(v(ef26cbf6), grid_hint, output(has_x_columns(fg))).
arc_test_property(v(ef26cbf6), grid_hint, output(has_x_columns(yellow))).
arc_test_property(v(ef26cbf6), grid_hint, output(has_y_columns(fg))).
arc_test_property(v(ef26cbf6), grid_hint, output(has_y_columns(yellow))).
arc_test_property(v(f0afb749), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v(f0df5ff0), grid_hint, input(grid_size(i, 15, 15))).
arc_test_property(v(f0df5ff0), grid_hint, output(containsAll([]))).
arc_test_property(v(f0df5ff0), grid_hint, output(grid_size(o, 15, 15))).
arc_test_property(v(f3b10344), grid_hint, output(containsAll([]))).
arc_test_property(v(f3cdc58f), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(f3cdc58f), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(f3e62deb), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(f3e62deb), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(f4081712), grid_hint, input(grid_size(i, 24, 24))).
arc_test_property(v(f45f5ca7), grid_hint, input(grid_size(i, 10, 10))).
arc_test_property(v(f45f5ca7), grid_hint, output(grid_size(o, 10, 10))).
arc_test_property(v(f5aa3634), grid_hint, input(find_ogs_no_pad)).
arc_test_property(v(f5c89df1), grid_hint, input(grid_size(i, 13, 13))).
arc_test_property(v(f5c89df1), grid_hint, output(grid_size(o, 13, 13))).
arc_test_property(v(f8be4b64), grid_hint, output(containsAll([]))).
arc_test_property(v(f9a67cb5), grid_hint, output(containsAll([]))).
arc_test_property(v(f9d67f8b), grid_hint, input(grid_size(i, 30, 30))).
arc_test_property(v(f9d67f8b), grid_hint, output(grid_size(o, 30, 30))).
arc_test_property(v(fafd9572), grid_hint, color_change_only).
arc_test_property(v(fb791726), grid_hint, output(has_y_columns(fg))).
arc_test_property(v(fb791726), grid_hint, output(has_y_columns(green))).
arc_test_property(v(fb791726), grid_hint, output(purportional(1r2, 1r2))).
arc_test_property(v(fd4b2b02), grid_hint, output(containsAll([]))).
arc_test_property(v(fe9372f3), grid_hint, output(containsAll([]))).
arc_test_property(v(fea12743), grid_hint, color_change_only).
arc_test_property(v(fea12743), grid_hint, input(grid_size(i, 11, 16))).
arc_test_property(v(fea12743), grid_hint, input(has_x_columns(bg))).
arc_test_property(v(fea12743), grid_hint, input(has_x_columns(black))).
arc_test_property(v(fea12743), grid_hint, input(has_y_columns(bg))).
arc_test_property(v(fea12743), grid_hint, input(has_y_columns(black))).
arc_test_property(v(fea12743), grid_hint, output(grid_size(o, 11, 16))).
arc_test_property(v(fea12743), grid_hint, output(has_x_columns(bg))).
arc_test_property(v(fea12743), grid_hint, output(has_x_columns(black))).
arc_test_property(v(fea12743), grid_hint, output(has_y_columns(bg))).
arc_test_property(v(fea12743), grid_hint, output(has_y_columns(black))).
arc_test_property(v(ff72ca3e), grid_hint, output(containsAll([]))).
