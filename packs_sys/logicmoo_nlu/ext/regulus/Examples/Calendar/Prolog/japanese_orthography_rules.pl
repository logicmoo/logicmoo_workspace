
letter_class('L', "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").
letter_class('N', "0123456789").

orthography_rewrite(" , ", ", ").
orthography_rewrite("N1 : N2", "N1:N2").
orthography_rewrite("N1 / N2", "N1/N2").
orthography_rewrite("L1 : ", "L1: ").
