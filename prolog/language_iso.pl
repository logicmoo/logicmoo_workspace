:- module(languages_iso, [language_iso/10]).

% Downloaded excel file from http://www.downloadexcelfiles.com/wo_en/download-excel-file-list-iso-639-codes#.VY1BuN89uLu
% and migrated to prolog automatically from csv file.

% table_column_names(language_iso, [sno, 'language family', 'language name', 'native name', '639-1', '639-2/t', '639-2/b', '639-3', '639-6', notes]).

% For other more formats kindly visit www.downloadexcelfiles.com
% Original source : en.wikipedia.org/wiki/List_of_ISO_639-1_codes

language_iso(1, 'Northwest Caucasian', 'Abkhaz', 'аҧсуа бызшәа, аҧсшәа', ab, abk, abk, abk, abks, '').
language_iso(2, 'Afro-Asiatic', 'Afar', 'Afaraf', aa, aar, aar, aar, aars, '').
language_iso(3, 'Indo-European', 'Afrikaans', 'Afrikaans', af, afr, afr, afr, afrs, '').
language_iso(4, 'Niger–Congo', 'Akan', 'Akan', ak, aka, aka, 'aka + 2', '', 'macrolanguage, Twi is [tw/twi], Fanti is [fat]').
language_iso(5, 'Indo-European', 'Albanian', 'gjuha shqipe', sq, sqi, alb, 'sqi + 4', '–', 'macrolanguage, "Albanian Phylozone" in 639-6').
language_iso(6, 'Afro-Asiatic', 'Amharic', አማርኛ, am, amh, amh, amh, '', '').
language_iso(7, 'Afro-Asiatic', 'Arabic', العربية, ar, ara, ara, 'ara + 30', '', 'macrolanguage, Standard Arabic is [arb]').
language_iso(8, 'Indo-European', 'Aragonese', aragonés, an, arg, arg, arg, '', '').
language_iso(9, 'Indo-European', 'Armenian', 'Հայերեն', hy, hye, arm, hye, '', '').
language_iso(10, 'Indo-European', 'Assamese', অসমীয়া, as, asm, asm, asm, '', '').
language_iso(11, 'Northeast Caucasian', 'Avaric', 'авар мацӀ, магӀарул мацӀ', av, ava, ava, ava, '', '').
language_iso(12, 'Indo-European', 'Avestan', avesta, ae, ave, ave, ave, '', ancient).
language_iso(13, 'Aymaran', 'Aymara', 'aymar aru', ay, aym, aym, 'aym + 2', '', macrolanguage).
language_iso(14, 'Turkic', 'Azerbaijani', 'azərbaycan dili', az, aze, aze, 'aze + 2', '', macrolanguage).
language_iso(15, 'Niger–Congo', 'Bambara', bamanankan, bm, bam, bam, bam, '', '').
language_iso(16, 'Turkic', 'Bashkir', 'башҡорт теле', ba, bak, bak, bak, '', '').
language_iso(17, 'Language isolate', 'Basque', 'euskara, euskera', eu, eus, baq, eus, '', '').
language_iso(18, 'Indo-European', 'Belarusian', 'беларуская мова', be, bel, bel, bel, '', '').
language_iso(19, 'Indo-European', 'Bengali; Bangla', বাংলা, bn, ben, ben, ben, '', '').
language_iso(20, 'Indo-European', 'Bihari', भोजपुरी, bh, bih, bih, '–', '', 'Collective language code for Bhojpuri, Magahi, and Maithili').
language_iso(21, 'Creole', 'Bislama', 'Bislama', bi, bis, bis, bis, '', '').
language_iso(22, 'Indo-European', 'Bosnian', 'bosanski jezik', bs, bos, bos, bos, boss, '').
language_iso(23, 'Indo-European', 'Breton', brezhoneg, br, bre, bre, bre, '', '').
language_iso(24, 'Indo-European', 'Bulgarian', 'български език', bg, bul, bul, bul, buls, '').
language_iso(25, 'Sino-Tibetan', 'Burmese', ဗမာစာ, my, mya, bur, mya, '', '').
language_iso(26, 'Indo-European', 'Catalan; Valencian', 'català, valencià', ca, cat, cat, cat, '', '').
language_iso(27, 'Austronesian', 'Chamorro', 'Chamoru', ch, cha, cha, cha, '', '').
language_iso(28, 'Northeast Caucasian', 'Chechen', 'нохчийн мотт', ce, che, che, che, '', '').
language_iso(29, 'Niger–Congo', 'Chichewa; Chewa; Nyanja', 'chiCheŵa, chinyanja', ny, nya, nya, nya, '', '').
language_iso(30, 'Sino-Tibetan', 'Chinese', '中文 (Zhōngwén), 汉语, 漢語', zh, zho, chi, 'zho + 13', '', macrolanguage).
language_iso(31, 'Turkic', 'Chuvash', 'чӑваш чӗлхи', cv, chv, chv, chv, '', '').
language_iso(32, 'Indo-European', 'Cornish', 'Kernewek', kw, cor, cor, cor, '', '').
language_iso(33, 'Indo-European', 'Corsican', 'corsu, lingua corsa', co, cos, cos, cos, '', '').
language_iso(34, 'Algonquian', 'Cree', 'ᓀᐦᐃᔭᐍᐏᐣ', cr, cre, cre, 'cre + 6', '', macrolanguage).
language_iso(35, 'Indo-European', 'Croatian', 'hrvatski jezik', hr, hrv, hrv, hrv, '', '').
language_iso(36, 'Indo-European', 'Czech', 'čeština, český jazyk', cs, ces, cze, ces, '', '').
language_iso(37, 'Indo-European', 'Danish', dansk, da, dan, dan, dan, '', '').
language_iso(38, 'Indo-European', 'Divehi; Dhivehi; Maldivian;', 'ދިވެހި', dv, div, div, div, '', '').
language_iso(39, 'Indo-European', 'Dutch', 'Nederlands, Vlaams', nl, nld, dut, nld, '', '').
language_iso(40, 'Sino-Tibetan', 'Dzongkha', 'རྫོང་ཁ', dz, dzo, dzo, dzo, '', '').
language_iso(41, 'Indo-European', 'English', 'English', en, eng, eng, eng, engs, '').
language_iso(42, 'Constructed', 'Esperanto', 'Esperanto', eo, epo, epo, epo, '', 'constructed, initiated from L.L. Zamenhof, 1887').
language_iso(43, 'Uralic', 'Estonian', 'eesti, eesti keel', et, est, est, 'est + 2', '', macrolanguage).
language_iso(44, 'Niger–Congo', 'Ewe', 'Eʋegbe', ee, ewe, ewe, ewe, '', '').
language_iso(45, 'Indo-European', 'Faroese', føroyskt, fo, fao, fao, fao, '', '').
language_iso(46, 'Austronesian', 'Fijian', 'vosa Vakaviti', fj, fij, fij, fij, '', '').
language_iso(47, 'Uralic', 'Finnish', 'suomi, suomen kieli', fi, fin, fin, fin, '', '').
language_iso(48, 'Indo-European', 'French', 'français, langue française', fr, fra, fre, fra, fras, '').
language_iso(49, 'Niger–Congo', 'Fula; Fulah; Pulaar; Pular', 'Fulfulde, Pulaar, Pular', ff, ful, ful, 'ful + 9', '', macrolanguage).
language_iso(50, 'Indo-European', 'Galician', galego, gl, glg, glg, glg, '', '').
language_iso(51, 'South Caucasian', 'Georgian', 'ქართული', ka, kat, geo, kat, '', '').
language_iso(52, 'Indo-European', 'German', 'Deutsch', de, deu, ger, deu, deus, '').
language_iso(53, 'Indo-European', 'Greek, Modern', ελληνικά, el, ell, gre, ell, ells, '').
language_iso(54, 'Tupian', 'Guaraní', 'Avañe\'ẽ', gn, grn, grn, 'grn + 5', '', macrolanguage).
language_iso(55, 'Indo-European', 'Gujarati', ગુજરાતી, gu, guj, guj, guj, '', '').
language_iso(56, 'Creole', 'Haitian; Haitian Creole', 'Kreyòl ayisyen', ht, hat, hat, hat, '', '').
language_iso(57, 'Afro-Asiatic', 'Hausa', 'Hausa, 'هَوُسَ'', ha, hau, hau, hau, '', '').
language_iso(58, 'Afro-Asiatic', 'Hebrew (modern)', 'עברית', he, heb, heb, heb, '', '').
language_iso(59, 'Niger–Congo', 'Herero', 'Otjiherero', hz, her, her, her, '', '').
language_iso(60, 'Indo-European', 'Hindi', 'हिन्दी, हिंदी', hi, hin, hin, hin, hins, '').
language_iso(61, 'Austronesian', 'Hiri Motu', 'Hiri Motu', ho, hmo, hmo, hmo, '', '').
language_iso(62, 'Uralic', 'Hungarian', magyar, hu, hun, hun, hun, '', '').
language_iso(63, 'Constructed', 'Interlingua', 'Interlingua', ia, ina, ina, ina, '', 'constructed by International Auxiliary Language Association').
language_iso(64, 'Austronesian', 'Indonesian', 'Bahasa Indonesia', id, ind, ind, ind, '', 'Covered by macrolanguage [ms/msa]').
language_iso(65, 'Constructed', 'Interlingue', 'Originally called Occidental; then Interlingue after WWII', ie, ile, ile, ile, '', 'constructed by Edgar de Wahl, first published in 1922').
language_iso(66, 'Indo-European', 'Irish', 'Gaeilge', ga, gle, gle, gle, '', '').
language_iso(67, 'Niger–Congo', 'Igbo', 'Asụsụ Igbo', ig, ibo, ibo, ibo, '', '').
language_iso(68, 'Eskimo–Aleut', 'Inupiaq', 'Iñupiaq, Iñupiatun', ik, ipk, ipk, 'ipk + 2', '', macrolanguage).
language_iso(69, 'Constructed', 'Ido', 'Ido', io, ido, ido, ido, idos, 'constructed by De Beaufront, 1907, as variation of Esperanto').
language_iso(70, 'Indo-European', 'Icelandic', 'Íslenska', is, isl, ice, isl, '', '').
language_iso(71, 'Indo-European', 'Italian', italiano, it, ita, ita, ita, itas, '').
language_iso(72, 'Eskimo–Aleut', 'Inuktitut', 'ᐃᓄᒃᑎᑐᑦ', iu, iku, iku, 'iku + 2', '', macrolanguage).
language_iso(73, 'Japonic', 'Japanese', '日本語 (にほんご)', ja, jpn, jpn, jpn, '', '').
language_iso(74, 'Austronesian', 'Javanese', 'basa Jawa', jv, jav, jav, jav, '', '').
language_iso(75, 'Eskimo–Aleut', 'Kalaallisut, Greenlandic', 'kalaallisut, kalaallit oqaasii', kl, kal, kal, kal, '', '').
language_iso(76, 'Dravidian', 'Kannada', ಕನ್ನಡ, kn, kan, kan, kan, '', '').
language_iso(77, 'Nilo-Saharan', 'Kanuri', 'Kanuri', kr, kau, kau, 'kau + 3', '', macrolanguage).
language_iso(78, 'Indo-European', 'Kashmiri', 'कश्मीरी, كشميري‎', ks, kas, kas, kas, '', '').
language_iso(79, 'Turkic', 'Kazakh', 'қазақ тілі', kk, kaz, kaz, kaz, '', '').
language_iso(80, 'Austroasiatic', 'Khmer', 'ខ្មែរ, ខេមរភាសា, ភាសាខ្មែរ', km, khm, khm, khm, '', 'a.k.a. Cambodian').
language_iso(81, 'Niger–Congo', 'Kikuyu, Gikuyu', 'Gĩkũyũ', ki, kik, kik, kik, '', '').
language_iso(82, 'Niger–Congo', 'Kinyarwanda', 'Ikinyarwanda', rw, kin, kin, kin, '', '').
language_iso(83, 'Turkic', 'Kyrgyz', 'Кыргызча, Кыргыз тили', ky, kir, kir, kir, '', '').
language_iso(84, 'Uralic', 'Komi', 'коми кыв', kv, kom, kom, 'kom + 2', '', macrolanguage).
language_iso(85, 'Niger–Congo', 'Kongo', 'KiKongo', kg, kon, kon, 'kon + 3', '', macrolanguage).
language_iso(86, 'Language isolate', 'Korean', '한국어 (韓國語), 조선어 (朝鮮語)', ko, kor, kor, kor, '', '').
language_iso(87, 'Indo-European', 'Kurdish', 'Kurdî, كوردی‎', ku, kur, kur, 'kur + 3', '', macrolanguage).
language_iso(88, 'Niger–Congo', 'Kwanyama, Kuanyama', 'Kuanyama', kj, kua, kua, kua, '', '').
language_iso(89, 'Indo-European', 'Latin', 'latine, lingua latina', la, lat, lat, lat, lats, ancient).
language_iso(90, 'Indo-European', 'Luxembourgish, Letzeburgesch', 'Lëtzebuergesch', lb, ltz, ltz, ltz, '', '').
language_iso(91, 'Niger–Congo', 'Ganda', 'Luganda', lg, lug, lug, lug, '', '').
language_iso(92, 'Indo-European', 'Limburgish, Limburgan, Limburger', 'Limburgs', li, lim, lim, lim, '', '').
language_iso(93, 'Niger–Congo', 'Lingala', 'Lingála', ln, lin, lin, lin, '', '').
language_iso(94, 'Tai–Kadai', 'Lao', 'ພາສາລາວ', lo, lao, lao, lao, '', '').
language_iso(95, 'Indo-European', 'Lithuanian', 'lietuvių kalba', lt, lit, lit, lit, '', '').
language_iso(96, 'Niger–Congo', 'Luba-Katanga', 'Tshiluba', lu, lub, lub, lub, '', '').
language_iso(97, 'Indo-European', 'Latvian', 'latviešu valoda', lv, lav, lav, 'lav + 2', '', macrolanguage).
language_iso(98, 'Indo-European', 'Manx', 'Gaelg, Gailck', gv, glv, glv, glv, '', '').
language_iso(99, 'Indo-European', 'Macedonian', 'македонски јазик', mk, mkd, mac, mkd, '', '').
language_iso(100, 'Austronesian', 'Malagasy', 'fiteny malagasy', mg, mlg, mlg, 'mlg + 10', '', macrolanguage).
language_iso(101, 'Austronesian', 'Malay', 'bahasa Melayu, بهاس ملايو‎', ms, msa, may, 'msa + 13', '', 'macrolanguage, Standard Malay is [zsm], Indonesian is [id/ind]').
language_iso(102, 'Dravidian', 'Malayalam', മലയാളം, ml, mal, mal, mal, '', '').
language_iso(103, 'Afro-Asiatic', 'Maltese', 'Malti', mt, mlt, mlt, mlt, '', '').
language_iso(104, 'Austronesian', 'Māori', 'te reo Māori', mi, mri, mao, mri, '', '').
language_iso(105, 'Indo-European', 'Marathi (Marāṭhī)', मराठी, mr, mar, mar, mar, '', '').
language_iso(106, 'Austronesian', 'Marshallese', 'Kajin M̧ajeļ', mh, mah, mah, mah, '', '').
language_iso(107, 'Mongolic', 'Mongolian', 'монгол', mn, mon, mon, 'mon + 2', '', macrolanguage).
language_iso(108, 'Austronesian', 'Nauru', 'Ekakairũ Naoero', na, nau, nau, nau, '', '').
language_iso(109, 'Dené–Yeniseian', 'Navajo, Navaho', 'Diné bizaad, Dinékʼehǰí', nv, nav, nav, nav, '', '').
language_iso(110, 'Indo-European', 'Norwegian Bokmål', 'Norsk bokmål', nb, nob, nob, nob, '', 'Covered by macrolanguage [no/nor]').
language_iso(111, 'Niger–Congo', 'North Ndebele', isiNdebele, nd, nde, nde, nde, '', '').
language_iso(112, 'Indo-European', 'Nepali', नेपाली, ne, nep, nep, nep, '', '').
language_iso(113, 'Niger–Congo', 'Ndonga', 'Owambo', ng, ndo, ndo, ndo, '', '').
language_iso(114, 'Indo-European', 'Norwegian Nynorsk', 'Norsk nynorsk', nn, nno, nno, nno, '', 'Covered by macrolanguage [no/nor]').
language_iso(115, 'Indo-European', 'Norwegian', 'Norsk', no, nor, nor, 'nor + 2', '', 'macrolanguage, Bokmål is [nb/nob], Nynorsk is [nn/nno]').
language_iso(116, 'Sino-Tibetan', 'Nuosu', 'ꆈꌠ꒿ Nuosuhxop', ii, iii, iii, iii, '', 'Standard form of Yi languages').
language_iso(117, 'Niger–Congo', 'South Ndebele', isiNdebele, nr, nbl, nbl, nbl, '', '').
language_iso(118, 'Indo-European', 'Occitan', 'occitan, lenga d\'òc', oc, oci, oci, oci, '', '').
language_iso(119, 'Algonquian', 'Ojibwe, Ojibwa', 'ᐊᓂᔑᓈᐯᒧᐎᓐ', oj, oji, oji, 'oji + 7', '', macrolanguage).
language_iso(120, 'Indo-European', 'Old Church Slavonic, Church Slavonic, Old Bulgarian', 'ѩзыкъ словѣньскъ', cu, chu, chu, chu, '', 'ancient, in use by Orthodox Church').
language_iso(121, 'Afro-Asiatic', 'Oromo', 'Afaan Oromoo', om, orm, orm, 'orm + 4', '', macrolanguage).
language_iso(122, 'Indo-European', 'Oriya', ଓଡ଼ିଆ, or, ori, ori, ori, '', '').
language_iso(123, 'Indo-European', 'Ossetian, Ossetic', 'ирон æвзаг', os, oss, oss, oss, '', '').
language_iso(124, 'Indo-European', 'Panjabi, Punjabi', 'ਪੰਜਾਬੀ, پنجابی‎', pa, pan, pan, pan, '', '').
language_iso(125, 'Indo-European', 'Pāli', पाऴि, pi, pli, pli, pli, '', ancient).
language_iso(126, 'Indo-European', 'Persian (Farsi)', 'فارسی', fa, fas, per, 'fas + 2', '', macrolanguage).
language_iso(127, 'Indo-European', 'Polish', 'język polski, polszczyzna', pl, pol, pol, pol, pols, '').
language_iso(128, 'Indo-European', 'Pashto, Pushto', 'پښتو', ps, pus, pus, 'pus + 3', '', macrolanguage).
language_iso(129, 'Indo-European', 'Portuguese', português, pt, por, por, por, '', '').
language_iso(130, 'Quechuan', 'Quechua', 'Runa Simi, Kichwa', qu, que, que, 'que + 44', '', macrolanguage).
language_iso(131, 'Indo-European', 'Romansh', 'rumantsch grischun', rm, roh, roh, roh, '', '').
language_iso(132, 'Niger–Congo', 'Kirundi', 'Ikirundi', rn, run, run, run, '', '').
language_iso(133, 'Indo-European', 'Romanian', 'limba română', ro, ron, rum, ron, '', '[mo] for Moldavian has been withdrawn, recommending [ro] also for Moldavian').
language_iso(134, 'Indo-European', 'Russian', 'русский язык', ru, rus, rus, rus, '', '').
language_iso(135, 'Indo-European', 'Sanskrit (Saṁskṛta)', 'संस्कृतम्', sa, san, san, san, '', 'ancient, still spoken').
language_iso(136, 'Indo-European', 'Sardinian', sardu, sc, srd, srd, 'srd + 4', '', macrolanguage).
language_iso(137, 'Indo-European', 'Sindhi', 'सिन्धी, سنڌي، سندھی‎', sd, snd, snd, snd, '', '').
language_iso(138, 'Uralic', 'Northern Sami', 'Davvisámegiella', se, sme, sme, sme, '', '').
language_iso(139, 'Austronesian', 'Samoan', 'gagana fa\'a Samoa', sm, smo, smo, smo, '', '').
language_iso(140, 'Creole', 'Sango', 'yângâ tî sängö', sg, sag, sag, sag, '', '').
language_iso(141, 'Indo-European', 'Serbian', 'српски језик', sr, srp, srp, srp, '', 'The ISO 639-2/T code srp deprecated the ISO 639-2/B code scc').
language_iso(142, 'Indo-European', 'Scottish Gaelic; Gaelic', 'Gàidhlig', gd, gla, gla, gla, '', '').
language_iso(143, 'Niger–Congo', 'Shona', chiShona, sn, sna, sna, sna, '', '').
language_iso(144, 'Indo-European', 'Sinhala, Sinhalese', සිංහල, si, sin, sin, sin, '', '').
language_iso(145, 'Indo-European', 'Slovak', 'slovenčina, slovenský jazyk', sk, slk, slo, slk, '', '').
language_iso(146, 'Indo-European', 'Slovene', 'slovenski jezik, slovenščina', sl, slv, slv, slv, '', '').
language_iso(147, 'Afro-Asiatic', 'Somali', 'Soomaaliga, af Soomaali', so, som, som, som, '', '').
language_iso(148, 'Niger–Congo', 'Southern Sotho', 'Sesotho', st, sot, sot, sot, '', '').
language_iso(149, 'Turkic', 'South Azerbaijani', 'تورکجه‎', az, azb, azb, azb, '', '').
language_iso(150, 'Indo-European', 'Spanish; Castilian', 'español, castellano', es, spa, spa, spa, '', '').
language_iso(151, 'Austronesian', 'Sundanese', 'Basa Sunda', su, sun, sun, sun, '', '').
language_iso(152, 'Niger–Congo', 'Swahili', 'Kiswahili', sw, swa, swa, 'swa + 2', '', macrolanguage).
language_iso(153, 'Niger–Congo', 'Swati', 'SiSwati', ss, ssw, ssw, ssw, '', '').
language_iso(154, 'Indo-European', 'Swedish', 'Svenska', sv, swe, swe, swe, '', '').
language_iso(155, 'Dravidian', 'Tamil', 'தமிழ்,' ta, tam, tam, tam, '', '').
language_iso(156, 'Dravidian', 'Telugu', తెలుగు, te, tel, tel, tel, '', '').
language_iso(157, 'Indo-European', 'Tajik', 'тоҷикӣ, toğikī, تاجیکی‎', tg, tgk, tgk, tgk, '', '').
language_iso(158, 'Tai–Kadai', 'Thai', ไทย, th, tha, tha, tha, '', '').
language_iso(159, 'Afro-Asiatic', 'Tigrinya', ትግርኛ, ti, tir, tir, tir, '', '').
language_iso(160, 'Sino-Tibetan', 'Tibetan Standard, Tibetan, Central', 'བོད་ཡིག', bo, bod, tib, bod, '', '').
language_iso(161, 'Turkic', 'Turkmen', 'Türkmen, Түркмен', tk, tuk, tuk, tuk, '', '').
language_iso(162, 'Austronesian', 'Tagalog', 'Wikang Tagalog, ᜏᜒᜃᜅ᜔ ᜆᜄᜎᜓᜄ᜔', tl, tgl, tgl, tgl, '', 'Note: Filipino (Pilipino) has the code [fil]').
language_iso(163, 'Niger–Congo', 'Tswana', 'Setswana', tn, tsn, tsn, tsn, '', '').
language_iso(164, 'Austronesian', 'Tonga (Tonga Islands)', 'faka Tonga', to, ton, ton, ton, '', '').
language_iso(165, 'Turkic', 'Turkish', 'Türkçe', tr, tur, tur, tur, '', '').
language_iso(166, 'Niger–Congo', 'Tsonga', 'Xitsonga', ts, tso, tso, tso, '', '').
language_iso(167, 'Turkic', 'Tatar', 'татар теле, tatar tele', tt, tat, tat, tat, '', '').
language_iso(168, 'Niger–Congo', 'Twi', 'Twi', tw, twi, twi, twi, '', 'Covered by macrolanguage [ak/aka]').
language_iso(169, 'Austronesian', 'Tahitian', 'Reo Tahiti', ty, tah, tah, tah, '', 'One of the Reo Mā`ohi (languages of French Polynesia)').
language_iso(170, 'Turkic', 'Uyghur, Uighur', 'Uyƣurqə, ئۇيغۇرچە‎', ug, uig, uig, uig, '', '').
language_iso(171, 'Indo-European', 'Ukrainian', 'українська мова', uk, ukr, ukr, ukr, '', '').
language_iso(172, 'Indo-European', 'Urdu', 'اردو', ur, urd, urd, urd, '', '').
language_iso(173, 'Turkic', 'Uzbek', 'O‘zbek, 'Ўзбек', أۇزبېك‎', uz, uzb, uzb, 'uzb + 2', '', macrolanguage).
language_iso(174, 'Niger–Congo', 'Venda', 'Tshivenḓa', ve, ven, ven, ven, '', '').
language_iso(175, 'Austroasiatic', 'Vietnamese', 'Tiếng Việt', vi, vie, vie, vie, '', '').
language_iso(176, 'Constructed', 'Volapük', 'Volapük', vo, vol, vol, vol, '', constructed).
language_iso(177, 'Indo-European', 'Walloon', walon, wa, wln, wln, wln, '', '').
language_iso(178, 'Indo-European', 'Welsh', 'Cymraeg', cy, cym, wel, cym, '', '').
language_iso(179, 'Niger–Congo', 'Wolof', 'Wollof', wo, wol, wol, wol, '', '').
language_iso(180, 'Indo-European', 'Western Frisian', 'Frysk', fy, fry, fry, fry, '', '').
language_iso(181, 'Niger–Congo', 'Xhosa', isiXhosa, xh, xho, xho, xho, '', '').
language_iso(182, 'Indo-European', 'Yiddish', 'ייִדיש', yi, yid, yid, 'yid + 2', '', macrolanguage).
language_iso(183, 'Niger–Congo', 'Yoruba', 'Yorùbá', yo, yor, yor, yor, '', '').
language_iso(184, 'Tai–Kadai', 'Zhuang, Chuang', 'Saɯ cueŋƅ, Saw cuengh', za, zha, zha, 'zha + 16', '', macrolanguage).
language_iso(185, 'Niger–Congo', 'Zulu', isiZulu, zu, zul, zul, zul, '', '').
