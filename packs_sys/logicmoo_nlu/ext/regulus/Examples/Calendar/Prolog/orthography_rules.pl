
letter_class('L', "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").
letter_class('C', "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ").
letter_class('V', "aeiouAEIOU").
letter_class('N', "0123456789").

orthography_rewrite(" a V1", " an V1").
orthography_rewrite(" an C1", " a C1").

orthography_rewrite("_", " ").
orthography_rewrite(" s ", "'s ").
orthography_rewrite(" '", "'").
orthography_rewrite(" , ", ", ").
orthography_rewrite("N1 : N2", "N1:N2").
orthography_rewrite("L1 : ", "L1: ").
