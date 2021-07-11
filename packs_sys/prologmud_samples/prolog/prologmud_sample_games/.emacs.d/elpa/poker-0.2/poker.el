;;; poker.el --- Texas hold 'em poker

;; Copyright (C) 2014, 2016  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Version: 0.2
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; poker.el provides Texas hold 'em poker gameplay for Emacs.

;;; Todo:

;; * Provide a better user interface.  A buffer should be used to keep
;;   the state and visual representation of a table/game.
;; * Smarter AIs.

;;; Requires:

(require 'cl-lib)
(require 'cookie1)
(require 'ert)

;;; Compatibility:

(eval-and-compile
  (unless (fboundp 'cookie-shuffle-vector)
    (defalias 'cookie-shuffle-vector 'shuffle-vector)))

;;; Constants:

(defconst poker-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))
(defconst poker-suits '(clubs diamonds hearts spades))
(defconst poker-deck (cl-loop for card from 0 to 51 collect card))
(defconst poker-unicode-cards
  (let ((unicode-suit '((clubs . #xD0) (diamonds . #XC0)
			(hearts . #XB0) (spades . #XA0))))
    (apply #'vector
	   (cl-loop for suit in poker-suits
		    nconc
		    (cl-loop for rank in poker-ranks
			     collect
			     (logior #x1f000
				     (cdr (assq suit unicode-suit))
				     (cond
				      ((eq rank 'ace)   #x1)
				      ((eq rank 'jack)  #xB)
				      ((eq rank 'queen) #xD)
				      ((eq rank 'king)  #XE)
				      (t                rank))))))))
(defconst poker-pre-flop-starting-hands
  '((AA 0.8551 0.7375 0.6422 0.5622 0.4946 0.4388 0.3907 0.349 0.3134 0.2828)
    (KK 0.8273 0.692 0.586 0.5022 0.4331 0.3785 0.332 0.2951 0.2638 0.2386)
    (QQ 0.8017 0.6536 0.5387 0.4525 0.3829 0.3298 0.2878 0.2535 0.2265 0.2045)
    (JJ 0.7781 0.6166 0.496 0.4072 0.3406 0.2904 0.2511 0.2214 0.1984 0.181)
    (TT 0.7538 0.5807 0.4568 0.3689 0.3044 0.2577 0.2238 0.1979 0.1771 0.1626)
    (99 0.7251 0.541 0.4159 0.33 0.2709 0.2293 0.1989 0.1767 0.1605 0.1483)
    (88 0.6965 0.5052 0.3808 0.2995 0.2443 0.2084 0.1818 0.1634 0.1499 0.1391)
    (AKs 0.6787 0.5185 0.425 0.3655 0.3216 0.2876 0.2602 0.2362 0.2179 0.2004)
    (AQs 0.6718 0.5059 0.4104 0.3492 0.3056 0.272 0.2451 0.2228 0.2042 0.1884)
    (77 0.6674 0.47 0.3492 0.2728 0.2231 0.1912 0.1685 0.1524 0.141 0.1331)
    (AJs 0.6637 0.495 0.398 0.3365 0.2927 0.2591 0.2337 0.2131 0.1956 0.1813)
    (AK 0.6614 0.4933 0.3965 0.334 0.29 0.2556 0.2257 0.2027 0.183 0.1647)
    (ATs 0.6575 0.4845 0.388 0.3266 0.2827 0.2508 0.2259 0.2057 0.1888 0.1747)
    (AQ 0.6532 0.4804 0.3804 0.3175 0.2726 0.2376 0.2101 0.1858 0.1683 0.1521)
    (AJ 0.6467 0.4673 0.3673 0.3029 0.258 0.2241 0.197 0.1752 0.1569 0.1414)
    (KQs 0.6438 0.4828 0.3937 0.3377 0.2947 0.2639 0.2374 0.2156 0.1977 0.1825)
    (A9s 0.6404 0.4606 0.3617 0.2999 0.2574 0.2263 0.2032 0.184 0.1689 0.1556)
    (66 0.6394 0.4383 0.3205 0.2499 0.2054 0.178 0.1582 0.145 0.1353 0.1278)
    (AT 0.6389 0.4582 0.3555 0.2918 0.2467 0.2134 0.1878 0.1664 0.1492 0.1347)
    (KJs 0.6365 0.4712 0.3824 0.3244 0.2829 0.2513 0.2265 0.2071 0.1893 0.1755)
    (A8s 0.633 0.4531 0.354 0.2915 0.2498 0.219 0.1972 0.1785 0.1638 0.1511)
    (KTs 0.6302 0.4631 0.3713 0.3137 0.2736 0.2418 0.2185 0.1991 0.184 0.1703)
    (A7s 0.6261 0.4434 0.3436 0.2827 0.2425 0.2123 0.1908 0.1732 0.1592 0.1477)
    (KQ 0.6254 0.4559 0.3644 0.3051 0.2626 0.2289 0.2038 0.1814 0.1632 0.1476)
    (A9 0.6208 0.4325 0.3282 0.2635 0.2192 0.1866 0.1631 0.1429 0.127 0.1146)
    (A5s 0.6182 0.4351 0.3396 0.2804 0.2413 0.2142 0.1928 0.1757 0.1621 0.1513)
    (KJ 0.6167 0.4443 0.3509 0.291 0.2484 0.2166 0.1916 0.1702 0.1528 0.1387)
    (A6s 0.6162 0.4318 0.3326 0.2743 0.2351 0.2073 0.1867 0.1697 0.1564 0.1451)
    (QJs 0.6138 0.455 0.3702 0.3156 0.2762 0.2453 0.2211 0.2019 0.1855 0.1721)
    (A8 0.6132 0.4234 0.3185 0.2537 0.2109 0.1794 0.1557 0.1368 0.1216 0.1096)
    (K9s 0.613 0.439 0.3458 0.2865 0.2474 0.2179 0.1955 0.1764 0.1616 0.1501)
    (55 0.6097 0.4074 0.2951 0.2291 0.1908 0.1655 0.1491 0.1365 0.1291 0.1219)
    (KT 0.6095 0.4344 0.3391 0.2795 0.2377 0.2062 0.182 0.1623 0.1463 0.133)
    (A4s 0.6091 0.4264 0.3308 0.2737 0.2366 0.2094 0.1882 0.172 0.1584 0.1474)
    (QTs 0.6076 0.4459 0.3607 0.3052 0.2658 0.237 0.2141 0.1956 0.1803 0.1675)
    (A7 0.6049 0.4124 0.3085 0.2448 0.2018 0.1723 0.1499 0.1312 0.1164 0.1052)
    (A3s 0.6008 0.4184 0.323 0.2675 0.2316 0.204 0.1847 0.1685 0.1555 0.1443)
    (K8s 0.5985 0.4194 0.3251 0.2685 0.2291 0.2015 0.1799 0.1634 0.1499 0.1393)
    (A5 0.5976 0.4047 0.3026 0.2409 0.2012 0.1734 0.1514 0.1336 0.1196 0.1084)
    (A6 0.5945 0.4012 0.2965 0.2347 0.194 0.1654 0.1443 0.1275 0.113 0.1022)
    (QJ 0.5937 0.4274 0.34 0.2836 0.2426 0.2115 0.1873 0.167 0.1503 0.1374)
    (A2s 0.5928 0.4081 0.3147 0.2597 0.2244 0.1985 0.1793 0.1629 0.1503 0.1397)
    (K7s 0.5927 0.4121 0.3174 0.2617 0.224 0.1961 0.1761 0.1594 0.1461 0.1359)
    (K9 0.5922 0.4092 0.3108 0.251 0.21 0.1798 0.1562 0.138 0.123 0.1105)
    (Q9s 0.5909 0.4223 0.3346 0.2789 0.2404 0.2122 0.1898 0.1725 0.1592 0.1476)
    (JTs 0.5894 0.4354 0.3532 0.3017 0.2633 0.2351 0.2124 0.1954 0.1807 0.1688)
    (A4 0.5876 0.3943 0.2932 0.2339 0.1949 0.1673 0.146 0.1295 0.1164 0.105)
    (QT 0.5863 0.4173 0.3283 0.2712 0.2323 0.2017 0.1778 0.1589 0.1448 0.1325)
    (K6s 0.5858 0.4038 0.3101 0.2543 0.2173 0.1918 0.1719 0.1556 0.1434 0.1332)
    (A3 0.5784 0.3846 0.2848 0.2266 0.1894 0.1616 0.1416 0.1249 0.1122 0.1014)
    (K5s 0.5776 0.3947 0.3024 0.2488 0.2136 0.1876 0.1683 0.1531 0.1408 0.1309)
    (44 0.577 0.3741 0.2686 0.2114 0.1776 0.1564 0.1432 0.133 0.1256 0.1194)
    (Q8s 0.5765 0.4026 0.3132 0.2586 0.2226 0.1955 0.1748 0.1593 0.1469 0.1353)
    (K8 0.576 0.3878 0.2887 0.2304 0.19 0.1616 0.1402 0.1234 0.1101 0.0984)
    (J9s 0.5725 0.4117 0.3281 0.2753 0.238 0.2106 0.1896 0.1725 0.1605 0.1493)
    (A2 0.5694 0.3739 0.2755 0.2185 0.1811 0.1552 0.1352 0.1197 0.1065 0.0967)
    (K7 0.5692 0.3787 0.2813 0.2232 0.1835 0.1565 0.1344 0.1187 0.1053 0.095)
    (K4s 0.5691 0.3866 0.2953 0.2425 0.2075 0.1829 0.1635 0.1501 0.1374 0.1286)
    (Q9 0.5686 0.3918 0.3001 0.2436 0.2039 0.1758 0.153 0.1357 0.1211 0.11)
    (JT 0.5667 0.4056 0.3221 0.2696 0.2296 0.2016 0.1779 0.1612 0.147 0.1349)
    (Q7s 0.5614 0.3831 0.2948 0.2419 0.2061 0.1813 0.1627 0.1474 0.1358 0.1262)
    (K6 0.5607 0.3704 0.2729 0.2152 0.1777 0.1498 0.1304 0.1151 0.1023 0.0915)
    (K3s 0.5597 0.3785 0.2889 0.236 0.2025 0.1787 0.1601 0.1459 0.1343 0.1254)
    (J8s 0.557 0.3918 0.3075 0.2559 0.2203 0.1946 0.1748 0.1584 0.1466 0.1367)
    (T9s 0.5558 0.4035 0.3256 0.2742 0.2394 0.2124 0.1918 0.1757 0.1636 0.1529)
    (Q6s 0.5555 0.3762 0.2888 0.2363 0.2023 0.1778 0.1585 0.1449 0.1332 0.1227)
    (K5 0.5538 0.3615 0.2648 0.2087 0.1721 0.1458 0.1263 0.1112 0.0996 0.0892)
    (Q8 0.5531 0.3708 0.2787 0.2229 0.1848 0.1556 0.136 0.1202 0.1069 0.0968)
    (K2s 0.5514 0.3692 0.281 0.2304 0.1985 0.1746 0.157 0.1436 0.1322 0.1231)
    (J9 0.5491 0.38 0.295 0.2415 0.203 0.1747 0.1539 0.1375 0.1233 0.1127)
    (Q5s 0.5484 0.3695 0.2808 0.2313 0.1969 0.1739 0.156 0.1417 0.1303 0.1215)
    (33 0.5454 0.3437 0.2457 0.1946 0.1665 0.1502 0.1385 0.1299 0.1234 0.1176)
    (K4 0.5436 0.3524 0.2563 0.2011 0.1659 0.1406 0.1215 0.107 0.0961 0.0865)
    (J7s 0.542 0.373 0.2874 0.2375 0.2036 0.1794 0.1599 0.1462 0.1349 0.1258)
    (T8s 0.5417 0.3852 0.3055 0.2551 0.2205 0.1962 0.1766 0.1626 0.1506 0.1406)
    (Q4s 0.5392 0.3613 0.2743 0.2248 0.1916 0.1689 0.1527 0.1388 0.1271 0.1189)
    (Q7 0.5371 0.35 0.2581 0.2035 0.1674 0.1411 0.122 0.1079 0.0956 0.0865)
    (K3 0.5354 0.3431 0.248 0.1946 0.1602 0.1353 0.1182 0.104 0.0925 0.0834)
    (J8 0.5321 0.3596 0.2722 0.2202 0.1833 0.1565 0.1372 0.1215 0.1094 0.0993)
    (T9 0.532 0.3736 0.2929 0.2411 0.2038 0.1777 0.1571 0.1415 0.1288 0.1185)
    (Q3s 0.5316 0.3524 0.2671 0.2189 0.1868 0.1654 0.1474 0.135 0.1246 0.1161)
    (Q6 0.5305 0.3429 0.2512 0.1976 0.1619 0.1367 0.1177 0.104 0.0923 0.0833)
    (98s 0.5275 0.3775 0.3009 0.2516 0.2168 0.1916 0.1729 0.1586 0.1464 0.1372)
    (T7s 0.5264 0.3655 0.2859 0.2377 0.2045 0.1817 0.163 0.1496 0.1385 0.1296)
    (J6s 0.5262 0.3532 0.2704 0.2217 0.1894 0.1659 0.1489 0.1359 0.1255 0.1169)
    (K2 0.5254 0.3331 0.24 0.1886 0.1549 0.1317 0.1142 0.1006 0.0891 0.0809)
    (Q5 0.5227 0.3354 0.2444 0.1907 0.1566 0.1326 0.1141 0.1006 0.0903 0.0815)
    (Q2s 0.5224 0.3438 0.2598 0.2137 0.1824 0.1609 0.1441 0.1318 0.122 0.1129)
    (J5s 0.5214 0.348 0.2658 0.2172 0.1855 0.1636 0.1469 0.134 0.1233 0.1147)
    (J7 0.5164 0.3379 0.2518 0.1995 0.1651 0.1403 0.1216 0.1075 0.0967 0.0878)
    (T8 0.5157 0.3522 0.2715 0.2207 0.1851 0.1596 0.1404 0.1262 0.1148 0.1057)
    (Q4 0.5132 0.3249 0.2352 0.1835 0.1506 0.1276 0.1103 0.0973 0.0867 0.0787)
    (J4s 0.5126 0.3397 0.2586 0.2115 0.1814 0.1586 0.1432 0.1306 0.1203 0.1122)
    (22 0.5125 0.3132 0.2256 0.1817 0.1587 0.1448 0.1355 0.1279 0.1224 0.1163)
    (97s 0.5122 0.3594 0.2829 0.2357 0.2028 0.1796 0.1615 0.1481 0.1384 0.1293)
    (T6s 0.5106 0.3468 0.2675 0.2204 0.189 0.1669 0.1505 0.1378 0.1277 0.1188)
    (Q3 0.5044 0.3162 0.2278 0.1774 0.1455 0.1227 0.1062 0.0936 0.0837 0.0757)
    (J3s 0.5043 0.3315 0.2506 0.2054 0.1755 0.1541 0.1394 0.1275 0.118 0.1094)
    (87s 0.5028 0.3567 0.2828 0.2358 0.2033 0.1805 0.1633 0.1514 0.1403 0.132)
    (98 0.5009 0.3446 0.2668 0.2159 0.181 0.1558 0.1372 0.1228 0.1122 0.1037)
    (T7 0.5001 0.332 0.2499 0.2011 0.1671 0.1433 0.1252 0.1119 0.1017 0.0931)
    (J6 0.5 0.3182 0.2334 0.1823 0.1496 0.1259 0.1095 0.0966 0.0863 0.0782)
    (96s 0.4971 0.3404 0.2646 0.2192 0.1884 0.1668 0.1503 0.1377 0.127 0.1188)
    (J2s 0.4954 0.3231 0.2437 0.1999 0.1717 0.1513 0.1359 0.1236 0.1147 0.1066)
    (T5s 0.4952 0.3292 0.2501 0.2063 0.1761 0.1549 0.1396 0.1282 0.1188 0.1109)
    (J5 0.4941 0.3118 0.2268 0.1775 0.1454 0.1231 0.1065 0.094 0.084 0.0764)
    (Q2 0.494 0.3072 0.2206 0.1709 0.1408 0.1189 0.1026 0.0906 0.0804 0.0725)
    (T4s 0.4888 0.3213 0.2456 0.2012 0.1722 0.1513 0.1368 0.1246 0.1151 0.108)
    (86s 0.4859 0.3388 0.2662 0.2214 0.1909 0.1698 0.1538 0.1419 0.1319 0.1236)
    (97 0.4856 0.3255 0.2475 0.1996 0.1664 0.1427 0.126 0.1121 0.1023 0.0944)
    (J4 0.4846 0.3039 0.2184 0.1714 0.139 0.1175 0.1021 0.0904 0.0809 0.0731)
    (T6 0.4834 0.3115 0.2304 0.182 0.151 0.1272 0.1116 0.0997 0.0899 0.0822)
    (95s 0.4814 0.3222 0.2477 0.2033 0.1747 0.1539 0.1385 0.1266 0.1165 0.1096)
    (T3s 0.4798 0.3138 0.2375 0.1955 0.167 0.1478 0.1332 0.1209 0.1128 0.1053)
    (76s 0.4789 0.3382 0.2676 0.223 0.193 0.1722 0.1572 0.1452 0.1356 0.1278)
    (J3 0.4757 0.2944 0.2106 0.1645 0.1336 0.1133 0.0983 0.0862 0.0775 0.0703)
    (87 0.474 0.3238 0.2478 0.1999 0.1677 0.1446 0.1279 0.1153 0.1056 0.0983)
    (T2s 0.4715 0.3056 0.2314 0.1892 0.1628 0.1435 0.1298 0.1183 0.1097 0.1029)
    (85s 0.4703 0.3206 0.2488 0.205 0.177 0.1573 0.1424 0.1304 0.1219 0.1147)
    (96 0.4686 0.3059 0.2284 0.1813 0.1499 0.1283 0.1122 0.1003 0.0912 0.0836)
    (T5 0.4664 0.2914 0.212 0.1656 0.1365 0.1158 0.1004 0.0893 0.0801 0.073)
    (J2 0.4663 0.2856 0.2039 0.1576 0.1293 0.109 0.0945 0.083 0.0748 0.0675)
    (75s 0.4643 0.3202 0.2515 0.2098 0.181 0.1614 0.1472 0.1366 0.1285 0.1198)
    (94s 0.4639 0.3041 0.231 0.1884 0.1609 0.1414 0.1268 0.1159 0.1066 0.1)
    (T4 0.4598 0.2851 0.2057 0.1602 0.1323 0.1114 0.097 0.0854 0.0769 0.07)
    (65s 0.459 0.3217 0.2533 0.2122 0.1848 0.1654 0.1509 0.14 0.1311 0.1242)
    (86 0.458 0.3037 0.2299 0.1833 0.1542 0.1319 0.117 0.1057 0.0973 0.0899)
    (93s 0.4572 0.2974 0.2253 0.1837 0.1563 0.1377 0.1238 0.1129 0.1036 0.0972)
    (84s 0.4532 0.3021 0.2313 0.1905 0.1627 0.144 0.1297 0.1195 0.11 0.1038)
    (95 0.4517 0.2862 0.2096 0.1641 0.1348 0.1135 0.0998 0.0884 0.0796 0.073)
    (76 0.4502 0.3041 0.2314 0.1868 0.1561 0.1357 0.121 0.1099 0.1011 0.0943)
    (T3 0.4499 0.2775 0.1983 0.1542 0.1266 0.107 0.0928 0.0817 0.0742 0.0677)
    (92s 0.4487 0.2899 0.2194 0.1789 0.1523 0.1339 0.1208 0.1103 0.102 0.095)
    (74s 0.4454 0.3023 0.2339 0.1936 0.1668 0.1489 0.1345 0.1243 0.1156 0.1089)
    (54s 0.444 0.3104 0.2435 0.2044 0.1786 0.1607 0.1483 0.1374 0.1288 0.1224)
    (64s 0.4408 0.3049 0.2382 0.1975 0.1717 0.1535 0.1408 0.13 0.1219 0.1154)
    (T2 0.4407 0.2683 0.191 0.1486 0.1214 0.1027 0.0893 0.0794 0.0713 0.0646)
    (85 0.4407 0.2852 0.2114 0.1665 0.1386 0.1189 0.1042 0.0944 0.0862 0.0793)
    (83s 0.4345 0.2832 0.2138 0.1747 0.1498 0.1318 0.1185 0.1087 0.1005 0.0938)
    (75 0.4337 0.2848 0.2133 0.1717 0.1426 0.1241 0.1104 0.1002 0.0927 0.0862)
    (94 0.4332 0.2656 0.1904 0.1475 0.1203 0.1008 0.0872 0.078 0.0698 0.0635)
    (65 0.4281 0.2868 0.2169 0.1749 0.1468 0.1281 0.1149 0.1055 0.0979 0.091)
    (82s 0.4279 0.2772 0.2094 0.1718 0.1463 0.1285 0.1159 0.1062 0.0978 0.0909)
    (73s 0.4276 0.2833 0.2168 0.1773 0.1528 0.1356 0.1224 0.1128 0.1048 0.0985)
    (93 0.4264 0.2592 0.1849 0.1423 0.1153 0.0971 0.0841 0.0739 0.0665 0.0601)
    (53s 0.4248 0.292 0.228 0.1898 0.1658 0.1497 0.1375 0.1278 0.119 0.113)
    (63s 0.4236 0.2853 0.2201 0.1818 0.158 0.141 0.1283 0.1191 0.1107 0.1042)
    (84 0.4216 0.2648 0.1921 0.1486 0.1236 0.1044 0.0917 0.0813 0.0733 0.0676)
    (92 0.4173 0.2511 0.1784 0.1368 0.1108 0.0928 0.0801 0.071 0.0635 0.0569)
    (43s 0.4156 0.2826 0.2189 0.1825 0.1585 0.1431 0.1306 0.1218 0.1136 0.1071)
    (74 0.4141 0.2652 0.1954 0.1535 0.1279 0.1097 0.0973 0.0879 0.0804 0.0742)
    (54 0.4126 0.2739 0.2055 0.1658 0.1406 0.1234 0.1113 0.102 0.0946 0.0891)
    (64 0.4102 0.2676 0.199 0.1594 0.134 0.1165 0.1043 0.0943 0.0871 0.0815)
    (72s 0.4092 0.2647 0.1999 0.1623 0.1399 0.1236 0.1122 0.1026 0.0951 0.0892)
    (52s 0.4076 0.2737 0.2106 0.1748 0.1523 0.1373 0.1252 0.1159 0.109 0.102)
    (62s 0.4052 0.2669 0.2036 0.1666 0.1437 0.1279 0.1163 0.1072 0.0997 0.0931)
    (83 0.4021 0.2436 0.1742 0.1335 0.1084 0.0912 0.0791 0.0707 0.0636 0.0581)
    (42s 0.3982 0.2653 0.2033 0.1684 0.1467 0.1317 0.121 0.1119 0.1052 0.0981)
    (82 0.3962 0.2374 0.1688 0.1292 0.1048 0.0881 0.0765 0.0677 0.0605 0.0548)
    (73 0.3955 0.2447 0.1761 0.1375 0.1123 0.0957 0.0838 0.0749 0.0685 0.0627)
    (53 0.3938 0.2548 0.1883 0.1505 0.1274 0.1118 0.1009 0.0915 0.0854 0.0799)
    (63 0.3911 0.2475 0.1809 0.1419 0.1182 0.1018 0.091 0.0823 0.0752 0.0706)
    (32s 0.3895 0.2562 0.1951 0.1609 0.1406 0.1258 0.1155 0.1066 0.0991 0.0931)
    (43 0.3826 0.2444 0.1789 0.1428 0.12 0.1046 0.0937 0.0855 0.079 0.0735)
    (72 0.3738 0.2244 0.1581 0.1218 0.0989 0.0835 0.0726 0.0647 0.0586 0.0538)
    (52 0.3736 0.2342 0.1694 0.1342 0.1121 0.0975 0.0866 0.0795 0.0734 0.0683)
    (62 0.3709 0.2273 0.1622 0.1257 0.1032 0.088 0.0777 0.0699 0.0639 0.0587)
    (42 0.3631 0.225 0.1632 0.1271 0.1073 0.0932 0.0828 0.0755 0.0693 0.0643)
    (32 0.3539 0.2162 0.1536 0.1202 0.0996 0.0859 0.0769 0.0698 0.0632 0.0584)))

;;; Code:

(defsubst poker-make-card (rank suit)
  "Make a poker card from RANK and SUIT.
RANK is one of `poker-ranks' and SUIT is one of `poker-suits'."
  (cl-assert (memq rank poker-ranks))
  (cl-assert (memq suit poker-suits))
  (+ (* (cl-position suit poker-suits) 13) (cl-position rank poker-ranks)))

(defsubst poker-card-rank (card)
  "The rank (a integer from 0 to 12) of a poker CARD."
  (cl-check-type card (integer 0 51))
  (% card 13))

(defsubst poker-card-suit (card)
  "The suit (an integer from 0 to 3) of a poker CARD."
  (cl-check-type card (integer 0 51))
  (/ card 13))

(defsubst poker-card-name (card)
  "The name of a poker CARD (a string of two characters)."
  (cl-check-type card (integer 0 51))
  (concat (aref ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"]
		(poker-card-rank card))
	  (aref ["c" "d" "h" "s"] (poker-card-suit card))))

(defun poker-card-unicode (card)
  "The Unicode character for a poker CARD."
  (aref poker-unicode-cards card))

(defun poker-hand-value (hand)
  "Calculate the value of a given 5 card poker HAND.
The result is a 24 bit integer where the leftmost 4 bits (0-8) indicate the type
of hand, and the remaining nibbles are rank values of decisive cards.
The highest possible value is therefore #x8CBA98 and the lowest is #x053210."
  (let* ((rank-counts (sort (let ((cards hand) result)
			      (while cards
				(let ((rank (poker-card-rank (car cards))))
				  (unless (rassq rank result)
				    (push (cons (let ((count 1))
						  (dolist (card (cdr cards) count)
						    (when (eq (poker-card-rank card)
							      rank)
						      (setq count (1+ count)))))
						rank)
					  result)))
				(setq cards (cdr cards)))
			      result)
			    (lambda (lhs rhs) (or (> (car lhs) (car rhs))
						  (and (= (car lhs) (car rhs))
						       (> (cdr lhs) (cdr rhs)))))))
	 (ranks-length (length rank-counts))
	 (ranks (mapcar #'cdr rank-counts)))
    (setq rank-counts (mapcar #'car rank-counts))
    (logior (cond
	     ((eq ranks-length 4) #x100000)
	     ((eq ranks-length 5)
	      (let ((straight (or (when (and (eq (nth 0 ranks) 12)
					     (eq (nth 1 ranks) 3))
				    (setq ranks '(3 2 1 0 0)))
				  (eq (- (nth 0 ranks) (nth 4 ranks)) 4)))
		    (flush (let ((suit (poker-card-suit (car hand)))
				 (tail (cdr hand)))
			     (while (and tail
					 (eq suit (poker-card-suit (car tail))))
			       (setq tail (cdr tail)))
			     (not tail))))
		(cond ((and straight flush) #x800000)
		      (straight             #x400000)
		      (flush                #x500000)
		      (t                      0))))
	     ((equal rank-counts '(2 2 1)) #x200000)
	     ((equal rank-counts '(3 1 1)) #x300000)
	     ((equal rank-counts '(3 2)) #x600000)
	     ((equal rank-counts '(4 1)) #x700000))
	    (ash (nth 0 ranks) 16)
	    (ash (nth 1 ranks) 12)
	    (if (> ranks-length 2) (ash (nth 2 ranks) 8) 0)
	    (if (> ranks-length 3) (ash (nth 3 ranks) 4) 0)
	    (if (> ranks-length 4) (nth 4 ranks) 0))))

(defun poker-hand-> (hand1 hand2)
  "Return non-nil if HAND1 is better than HAND2."
  (> (poker-hand-value hand1) (poker-hand-value hand2)))

(defun poker-sort-hands (hands)
  "Sort HANDS (a list of list of cards) according to the value of the individual hands."
  (mapcar #'cdr
	  (cl-sort (mapcar (lambda (hand) (cons (poker-hand-value hand) hand)) hands)
		   #'> :key #'car)))

(defun poker-combinations (n list)
  "A list of all unique ways of taking N different elements from LIST."
  (when list
    (let ((length (length list)))
      (nconc (if (eq n 1)
		 (list (if (cdr list) (list (car list)) list))
	       (if (eq n length)
		   (list list)
		 (mapcar (lambda (rest) (cons (car list) rest))
			 (poker-combinations (1- n) (cdr list)))))
	     (when (> length n) (poker-combinations n (cdr list)))))))

(defun poker-possible-hands (cards)
  "Generate a list of possible 5 card poker hands from CARDS.
CARDS is a list of 5 to 7 poker cards."
  (cl-check-type (length cards) (integer 5 7))
  (cond
   ;; While this could certainly be made generic,
   ;; the performance of this hand-crafted implementation is unmatched.
   ((eq 7 (length cards))
    (let ((car (car cards))
	  (cdr (cdr cards)))
      (let ((cadr (car cdr))
	    (cddr (cdr cdr)))
	(let ((caddr (car cddr))
	      (cdddr (cdr cddr)))
	  (let ((cadddr (car cdddr))
		(cddddr (cdr cdddr)))
	    (let ((caddddr (car cddddr))
		  (cdddddr (cdr cddddr)))
	      (let ((cadddddr (car cdddddr))
		    (cddddddr (cdr cdddddr)))
		(list (list car cadr caddr cadddr caddddr)
		      (list car cadr caddr cadddr cadddddr)
		      (cons car (cons cadr (cons caddr (cons cadddr cddddddr))))
		      (list car cadr caddr caddddr cadddddr)
		      (cons car (cons cadr (cons caddr (cons caddddr cddddddr))))
		      (cons car (cons cadr (cons caddr cdddddr)))
		      (cons car (cons cadr (butlast cdddr)))
		      (cons car (cons cadr (cons cadddr (cons caddddr cddddddr))))
		      (cons car (cons cadr (cons cadddr cdddddr)))
		      (cons car (cons cadr cddddr))
		      (cons car (butlast cddr))
		      (cons car (cons caddr (cons cadddr (cons caddddr cddddddr))))
		      (cons car (cons caddr (cons cadddr cdddddr)))
		      (cons car (cons caddr cddddr))
		      (cons car cdddr)
		      (butlast cdr)
		      (cons cadr (cons caddr (cons cadddr (cons caddddr cddddddr))))
		      (cons cadr (cons caddr (cons cadddr cdddddr)))
		      (cons cadr (cons caddr cddddr))
		      (cons cadr cdddr)
		      cddr))))))))
   (t (poker-combinations 5 cards))))

(defun poker-best-hand (cards)
  "Find the best hand for a number of CARDS (usually a list of 6 or 7 elements)."
  (let ((max 0) (best-hand nil))
    (dolist (hand (poker-possible-hands cards) best-hand)
      (let ((value (poker-hand-value hand)))
	(when (> value max) (setq max value best-hand hand))))))

(defun poker-rank-to-string (rank)
  "The english name of poker card RANK."
  (aref ["2" "3" "4" "5" "6" "7" "8" "9" "10" "jack" "queen" "king" "ace"] rank))

(defun poker-rank-to-plural-string (rank)
  "The plural english name of poker card RANK."
  (concat (poker-rank-to-string rank) "s"))

(defun poker-describe-hand (hand)
  "Return a string description of the value of the given poker HAND.
HAND is a list of 5 poker cards."
  (cl-assert (eq (length hand) 5))
  (pcase (let ((value (poker-hand-value hand)))
	   (cl-loop for i from 5 downto 0 collect (logand (ash value (- (* i 4))) #xf)))
    (`(8 ,high ,_ ,_ ,_ ,_) (pcase high
			      (12 "royal flush")
			      (_ (format "%s high straight flush"
					 (poker-rank-to-string high)))))
    (`(7 ,four ,high 0 0 0) (format "four %s, %s high"
				    (poker-rank-to-plural-string four)
				    (poker-rank-to-string high)))
    (`(6 ,three ,two 0 0 0) (format "full house of %s and %s"
				    (poker-rank-to-plural-string three)
				    (poker-rank-to-plural-string two)))
    (`(5 ,high ,k1 ,k2 ,k3 ,k4) (format "%s high flush, %s %s %s and %s kickers"
					(poker-rank-to-string high)
					(poker-rank-to-string k1)
					(poker-rank-to-string k2)
					(poker-rank-to-string k3)
					(poker-rank-to-string k4)))
    (`(4 ,high ,_ ,_ ,_ ,_) (pcase high
			      (3 "5 high straight (steel wheel)")
			      (_ (format "%s high straight"
					 (poker-rank-to-string high)))))
    (`(3 ,three ,high ,kicker 0 0) (format "three %s, %s high, %s kicker"
					   (poker-rank-to-plural-string three)
					   (poker-rank-to-string high)
					   (poker-rank-to-string kicker)))
    (`(2 ,two1 ,two2 ,high 0 0) (format "wwo pairs of %s and %s, %s high"
				    (poker-rank-to-plural-string two1)
				    (poker-rank-to-plural-string two2)
				    (poker-rank-to-string high)))
    (`(1 ,two ,high ,k1 ,k2 0) (format "a pair of %s, %s high, %s and %s kickers"
				       (poker-rank-to-plural-string two)
				       (poker-rank-to-string high)
				       (poker-rank-to-string k1)
				       (poker-rank-to-string k2)))
    (`(0 ,high ,k1 ,k2 ,k3 ,k4) (format "high card %s, %s %s %s and %s kickers"
					(poker-rank-to-string high)
					(poker-rank-to-string k1)
					(poker-rank-to-string k2)
					(poker-rank-to-string k3)
					(poker-rank-to-string k4)))))

(defun poker-random-deck ()
  "Return a shuffled deck of 52 poker cards."
  (append (cookie-shuffle-vector (apply 'vector poker-deck)) nil))

(defun poker-strength (pocket &optional community opponents)
  "Estimate the strength of POCKET and COMMUNITY cards against number of OPPONENTS.
The optional number of OPPONENTS defaults to 1."
  (or (and (not community)
	   (nth (or opponents 1)
		(assq (poker-starting-hand-name pocket)
		      poker-pre-flop-starting-hands)))
      (let ((wins 0) (iterations 300))
	(dotimes (i iterations)
	  (let ((deck (poker-random-deck))
		(players (make-vector (or opponents 1) nil)))
	    (dolist (card pocket) (setq deck (delete card deck)))
	    (dolist (card community) (setq deck (delete card deck)))
	    (dotimes (cards 2)
	      (dotimes (player (or opponents 1))
		(push (pop deck) (aref players player))))
	    (let ((board (append community nil)))
	      (dotimes (_ (- 5 (length community)))
		(push (pop deck) board))
	      (setq wins (+ wins (caar (cl-sort
					(mapcar (lambda (info)
						  (setcdr info (poker-best-hand
								(append (cdr info) board)))
						  info)
						(nconc (list (cons 1 pocket))
						       (mapcar (lambda (cards)
								 (cons 0 cards))
							       players)))
					#'poker-hand-> :key #'cdr)))))))
	(/ (float wins) iterations))))

(defun poker-starting-hand-name (pocket)
  (cl-assert (eq (length pocket) 2))
  (cl-assert (not (eq (nth 0 pocket) (nth 1 pocket))))
  (let ((rank-name (vector "2" "3" "4" "5" "6" "7" "8" "9" "T"
			   "J" "Q" "K" "A"))
	(rank1 (poker-card-rank (nth 0 pocket)))
	(rank2 (poker-card-rank (nth 1 pocket)))
	(suited (eq (poker-card-suit (nth 0 pocket))
		    (poker-card-suit (nth 1 pocket)))))
    (when (< rank1 rank2)
      (let ((tmp rank1))
	(setq rank1 rank2 rank2 tmp)))
    (if (and (< rank1 (cl-position 10 poker-ranks))
	     (< rank2 (cl-position 10 poker-ranks))
	     (not suited))
	(+ (* (+ rank1 2) 10) (+ rank2 2))
      (intern (concat (aref rank-name rank1)
		      (aref rank-name rank2)
		      (when suited "s"))))))

(defun poker-pre-flop-starting-hands (opponents)
  (let (hands)
    (dolist (rank1 poker-ranks (cl-sort hands #'> :key #'cdr))
      (dolist (rank2 poker-ranks)
	(if (eq rank1 rank2)
	    (push (let ((pocket (list (poker-make-card rank1 'clubs)
				      (poker-make-card rank2 'hearts))))
		    (cons (poker-strength pocket nil opponents)
			  (poker-starting-hand-name pocket)))
		  hands)
	  (when (< (cl-position rank1 poker-ranks)
		   (cl-position rank2 poker-ranks))
	    (let ((tmp rank1))
	      (setq rank1 rank2
		    rank2 tmp)))
	  (dolist (suited '(nil t))
	    (let* ((pocket (list (poker-make-card rank1 'clubs)
				 (poker-make-card rank2 (if suited
							    'clubs 'hearts))))
		   (code (poker-starting-hand-name pocket)))
	      (unless (rassq code hands)
		(accept-process-output)
		(message "%S" code)
		(push (cons (poker-strength pocket nil opponents)
			    code)
		      hands)))))))))

(defun poker-pot-odds (bet pot)
  "Return the odds when BET is added to POT."
  (/ (float bet) (+ pot bet)))

(defun poker-random-fold-call-raise (fold% call% raise%)
  "Randomly choose between FOLD%, CALL% and RAISE%."
  (cl-assert (= (+ fold% call% raise%) 100))
  (let ((value (random 100)))
    (cond
     ((< value fold%) 'fold)
     ((< value (+ fold% call%)) 'call)
     ((< value (+ fold% call% raise%)) 'raise)
     (t (error "Random FCR Error")))))

(defun poker-make-player (name fcr-fn)
  "Create a new poker player with NAME and FCR-FN.
FCR-FN specifies a function to use when a fold-call-raise decision is required."
  (list (cons 'name name)
	(cons 'stack 0)
	(cons 'wagered 0)
	(cons 'pocket nil)
	(cons 'fcr-fn fcr-fn)))

(defun poker-player-name (player)
  "Return the name of poker PLAYER."
  (cdr (assq 'name player)))

(defun poker-player-stack (player)
  "Return the remaining stack of poker PLAYER."
  (cdr (assq 'stack player)))

(defun poker-player-bet (player amount)
  "Make PLAYER bet AMOUNT of chips."
  (let ((actual (min (poker-player-stack player) amount)))
    (when (zerop actual) (message "WARNING: Actual is 0."))
    (unless (zerop actual)
      (cl-decf (cdr (assq 'stack player)) actual)
      (cl-incf (cdr (assq 'wagered player)) actual))
    actual))

(defun poker-player-payout (player amount)
  "Give PLAYER AMOUNT of chips."
  (cl-incf (cdr (assq 'stack player)) amount)
  amount)

(defun poker-player-wagered (player)
  "Return the amount of chips currently wagered by poker PLAYER."
  (cdr (assq 'wagered player)))

(defun poker-player-pocket (player)
  "Return the current pocket (hole) cards of PLAYER."
  (cdr (assq 'pocket player)))

(defun poker-player-fold (player)
  "Make PLAYER fold and forget about their cards."
  (setcdr (assq 'pocket player) nil))

(defun poker-player-active-p (player)
  (and (poker-player-pocket player) (> (poker-player-wagered player) 0)))

(defun poker-player-all-in-p (player)
  (and (poker-player-active-p player) (zerop (poker-player-stack player))))

(defun poker-player-can-bet-p (player)
  (and (poker-player-pocket player) (> (poker-player-stack player) 0)))

(defun poker-player-best-hand (player community)
  (cl-assert (>= (length (poker-player-pocket player)) 2))
  (cl-assert (>= (length community) 3))
  (poker-best-hand (append (poker-player-pocket player) community)))

(defun poker-player-give-card (player card)
  (cl-check-type card (integer 0 51))
  (push card (cdr (assq 'pocket player))))

(defun poker-player-fcr-fn (player)
  (cdr (assq 'fcr-fn player)))

(defun poker-player-fcr (player pot amount-to-call max-raise board opponents)
  (funcall (poker-player-fcr-fn player)
	   player pot amount-to-call max-raise board opponents))

(defun poker-read-fold-call-raise (pot to-call max-raise &optional prompt)
  (let ((cursor-in-echo-area t)
	(map (let ((map (make-sparse-keymap)))
	       (define-key map [?c] 'call)
	       (define-key map [?f] 'fold)
	       (when (> max-raise 0) (define-key map [?r] 'raise))
	       (define-key map [?q] 'quit)
	       map))
	(action nil))
    (while (not action)
      (message (format "%s%d in pot, %d to call: (f)old%s: "
		       (or prompt "") pot to-call
		       (if (> max-raise 0)
			   (if (zerop to-call)
			       ", (c)heck or (r)aise"
			     ", (c)all or (r)aise")
			 (if (zerop to-call)
			     " or (c)heck"
			   " or (c)all"))))
      (setq action (lookup-key map (vector (read-event)))))
    (cond
     ((eq action 'fold) nil)
     ((eq action 'call) to-call)
     ((eq action 'raise) (+ to-call (let ((raise (1+ max-raise)))
				      (while (> raise max-raise)
					(setq raise
					      (read-number (format "Raise by (max %d): "
								   max-raise))))
				      (cl-check-type raise integer)
				      raise))))))

(defun poker-interactive-fcr (player pot due max-raise board opponents)
  (poker-read-fold-call-raise
   pot due max-raise (format "%s%s, %d stack, "
			     (mapconcat #'poker-card-name (poker-player-pocket player) ", ")
			     (if board
				 (concat "(" (mapconcat #'poker-card-name board " ") ")")
			       "")
			     (poker-player-stack player))))

(defun poker-automatic-fcr (player pot due max-raise board &optional opponents)
  (let* ((strength (poker-strength (poker-player-pocket player) board opponents))
	 (pot-odds (poker-pot-odds due pot))
	 (rate-of-return (/ strength pot-odds))
	 (action (cond
		  ((< rate-of-return 0.8) (poker-random-fold-call-raise 95 1 4))
		  ((< rate-of-return 1.0) (poker-random-fold-call-raise 80 15 5))
		  ((< rate-of-return 1.3) (poker-random-fold-call-raise 0 60 40))
		  (t (poker-random-fold-call-raise 0 25 75)))))
    (when (and (memq action '(call raise))
	       (< (- (poker-player-stack player) due) 200) (< strength 0.5))
      (setq action 'fold))
    (when (and (eq action 'raise) (< strength 0.1))
      (setq action 'call))
    (when (and (zerop due) (eq action 'fold))
      (setq action 'call))
    (cond
     ((eq action 'fold) nil)
     ((eq action 'call) due)
     ((eq action 'raise) (+ due (min 100 max-raise))))))

(defun poker-rotate-to-first (player players)
  "Make PLAYER the first element of PLAYERS."
  (let ((position (cl-position player players)))
    (when position
      (let ((shift (- (length players) position)))
	(append (last players shift) (butlast players shift))))))

(defun poker-next-players (player players)
  (cdr (poker-rotate-to-first player players)))

(defun poker-next-player (player players)
  (car (poker-next-players player players)))

(defun poker-pot (players)
  "Return the amount of chips in the pot, the total wagered by all PLAYERS."
  (apply #'+ (mapcar #'poker-player-wagered players)))

(defun poker-current-wager (players)
  "Determine the maximum amount of chips wagered by any of PLAYERS."
  (apply #'max (mapcar #'poker-player-wagered players)))

(defun poker-collect-wager (amount players)
  "Collect AMOUNT of wager from PLAYERS."
  (let ((total 0))
    (dolist (player players total)
      (let ((wagered (assq 'wagered player)))
	(if (> amount (cdr wagered))
	    (progn
	      (setq total (+ total (cdr wagered)))
	      (setcdr wagered 0))
	  (setq total (+ total amount))
	  (setcdr wagered (- (cdr wagered) amount)))))))

(defun poker-distribute-winnings (winners players)
  "Distribute chips to WINNERS from PLAYERS accounting for split-pot rules."
  (cl-assert (not (null winners)))
  (cl-assert (> (length players) 1))
  (if (= (length winners) 1)
      (poker-player-payout (car winners)
			   (poker-collect-wager (poker-player-wagered (car winners))
						players))
    (let* ((lowest (apply #'min (mapcar #'poker-player-wagered winners)))
	   (total (poker-collect-wager lowest players))
	   (each (/ total (length winners)))
	   (leftover (- total (* each (length winners)))))
      (poker-player-payout (car winners) (+ each leftover))
      (dolist (player (cdr winners)) (poker-player-payout player each))
      total)))

(defun poker-player-max-raise (player players)
  "Determine the maximum amount allowed to raise for PLAYER considering PLAYERS stacks."
  (let ((other-stacks (mapcar #'poker-player-stack
			      (cl-remove
			       player
			       (cl-remove-if-not #'poker-player-can-bet-p players)))))
    (min (poker-player-stack player) (if other-stacks (apply #'max other-stacks) 0))))

(defun poker-interactive-p (players)
  (cl-find #'poker-interactive-fcr players :key #'poker-player-fcr-fn))

(defun poker-dealer-ask-player (player players board allow-raise)
  "Ask PLAYER for next action."
  (let ((pot (poker-pot players))
	(max-raise (if allow-raise (poker-player-max-raise player players) 0))
	(amount-to-call (- (poker-current-wager players)
			   (poker-player-wagered player)))
	(opponents (1- (length (cl-remove-if-not #'poker-player-pocket players)))))
    (cl-assert (> opponents 0))
    (let ((decision (poker-player-fcr player pot amount-to-call max-raise
				      board opponents)))
      (cl-assert (or (null decision)
		     (and (integerp decision)
			  (<= (- decision amount-to-call) max-raise))))
      (cond
       ((null decision)
	(message (format "%s folds." (poker-player-name player)))
	(poker-player-fold player))
       ((zerop decision)
	(message "%s checks." (poker-player-name player)))
       ((integerp decision)
	(if (= decision amount-to-call)
	    (message "%s calls %d." (poker-player-name player) decision)
	  (cl-assert (>= decision amount-to-call))
	  (message "%s raises by %d."
		   (poker-player-name player) (- decision amount-to-call)))
	(poker-player-bet player decision))))))

(defun poker-dealer (min-bet deck board players)
  "Deal a round of texas holdem poker with MIN-BET for PLAYERS."
  (cl-assert (> (length players) 1))
  (cond
   ;; pre-flop
   ((and (null board) (zerop (poker-pot players)))
    (let ((blinds players))
      (message "Collecting blinds.")
      (message "%s posts %d small blind." (poker-player-name (car blinds)) (/ min-bet 2))
      (poker-player-bet (car blinds) (/ min-bet 2))
      (message "%s posts %d big blind." (poker-player-name (cadr blinds)) min-bet)
      (poker-player-bet (cadr blinds) min-bet)
      (message "Dealing cards to players.")
      (dotimes (_ 2)
	(dolist (player players) (poker-player-give-card player (pop deck))))

      (message "Initial betting round.")

      (dolist (player (poker-next-players (cadr blinds) players))

	(unless (zerop (poker-player-stack player))
	  (poker-dealer-ask-player player players board t)))

      (when (and (not (zerop (poker-player-stack (cadr blinds))))
		 (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		     (< (poker-player-wagered (cadr blinds))
			(poker-current-wager players))))
	(poker-dealer-ask-player (cadr blinds) players board t))

      (poker-dealer min-bet deck board players)))

   ;; All but one have folded
   ((and (not (zerop (poker-pot players)))
	 (= (length (cl-remove-if-not #'poker-player-active-p players)) 1))
    (let ((winners (cl-remove-if-not #'poker-player-active-p players)))
      (message "%s silently wins %d."
	       (poker-player-name (car winners))
	       (poker-distribute-winnings winners players))
      winners))

   ;; pre-flop, second round of bets, no raises allowed
   ((and (null board) (cl-remove-if
		       (lambda (player)
			 (or (zerop (poker-player-wagered player))
			     (not (poker-player-pocket player))
			     (poker-player-all-in-p player)
			     (= (poker-player-wagered player)
				(poker-current-wager players))))
		       (poker-rotate-to-first (cadr players) players)))

    (message "Pre flop, second round of bets.")

    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (zerop (poker-player-wagered player))
			   (not (poker-player-pocket player))
			   (poker-player-all-in-p player)
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     (poker-rotate-to-first (cadr players) players)))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board nil)))

    (poker-dealer min-bet deck board players))

   ;; flop
   ((null board)
    (dotimes (_ 3) (push (pop deck) board))

    (message "The flop: %s" (mapconcat #'poker-card-name board " "))

    (dolist (player (cl-remove-if-not #'poker-player-can-bet-p players))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board t)))

    (poker-dealer min-bet deck board players))

   ;; flop, second round of bets, no raises allowed
   ((and (= (length board) 3) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-can-bet-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       players))
    (message "The flop, second round of bets.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-can-bet-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     players))
      (poker-dealer-ask-player player players board nil))

    (poker-dealer min-bet deck board players))

   ;; turn
   ((= (length board) 3)
    (push (pop deck) board)

    (message "The turn: %s" (mapconcat #'poker-card-name board " "))

    (setq min-bet (* min-bet 2))

    (dolist (player (cl-remove-if-not #'poker-player-can-bet-p players))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board t)))

    (poker-dealer min-bet deck board players))

   ;; turn, second round of bets, no raises allowed
   ((and (= (length board) 4) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-can-bet-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       players))
    (message "The turn, second round of bets.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-can-bet-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     players))
      (poker-dealer-ask-player player players board nil))

    (poker-dealer min-bet deck board players))

   ;; river
   ((= (length board) 4)
    (push (pop deck) board)
    (message "The river: %s" (mapconcat #'poker-card-name board " "))

    (dolist (player (cl-remove-if-not #'poker-player-can-bet-p players))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board t)))

    (poker-dealer min-bet deck board players))

   ;; river, second round of bets, no raises allowed
   ((and (= (length board) 5) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-can-bet-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       players))
    (message "Last betting round.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-can-bet-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     players))
      (poker-dealer-ask-player player players board nil))

    (poker-dealer min-bet deck board players))

   ;; showdown
   ((= (length board) 5)
    (cl-assert (not (zerop (poker-pot players))))
    (let ((in-play (cl-remove-if-not #'poker-player-active-p players))
	  (groups ())
	  (game-interactive-p (poker-interactive-p players)))
      (unless (> (length in-play) 1)
	(error "In-play to small: %S %S" in-play players))
      (while in-play
	(if (= (length in-play) 1)
	    (progn
	      (message "%s wins %d."
		       (poker-player-name (car in-play))
		       (poker-distribute-winnings in-play players))
	      (when game-interactive-p (sit-for 2))
	      (push in-play groups)
	      (setq in-play nil))
	  (let* ((best-hand-value (poker-hand-value
				   (car
				    (poker-sort-hands
				     (mapcar (lambda (player)
					       (poker-player-best-hand player board))
					     in-play)))))
		 (winners (cl-remove-if (lambda (player)
					  (< (poker-hand-value
					      (poker-player-best-hand player board))
					     best-hand-value))
					in-play)))
	    (dolist (player in-play)
	      (message "%s shows %s, %s."
		       (poker-player-name player)
		       (mapconcat #'poker-card-name (poker-player-pocket player) " ")
		       (poker-describe-hand (poker-player-best-hand player board)))
	      (when game-interactive-p (sit-for 2)))
	    (message "%s wins %d."
		     (mapconcat #'poker-player-name winners ", ")
		     (poker-distribute-winnings winners players))
	    (when game-interactive-p (sit-for 2))
	    (push winners groups))
	  (setq in-play (cl-remove-if-not #'poker-player-active-p players))))

      (cons board (nreverse groups))))

   (t (list 'error min-bet deck board players))))

;;;###autoload
(defun poker (initial-stack min-bet players)
  "Play a game of texas hold 'em poker."
  (interactive (list (read-number "Initial stack: " 1000)
		     (read-number "Minimum bet: " 50)
		     (list (poker-make-player "Angela" #'poker-automatic-fcr)
			   (poker-make-player "Bettina" #'poker-automatic-fcr)
			   (poker-make-player "Christina" #'poker-automatic-fcr)
			   (poker-make-player "Daniela" #'poker-automatic-fcr)
			   (poker-make-player "Emil" #'poker-automatic-fcr)
			   (poker-make-player "Frank" #'poker-automatic-fcr)
			   (poker-make-player "Günther" #'poker-automatic-fcr)
			   (poker-make-player "Harald" #'poker-automatic-fcr)
			   (poker-make-player "Ingrid" #'poker-automatic-fcr)
			   (poker-make-player (user-full-name) #'poker-interactive-fcr))))
  (cl-assert (> (length players) 1))
  (dolist (player players)
    (message "%s receives %d chips." (poker-player-name player) initial-stack)
    (setcdr (assq 'stack player) initial-stack))
  (let ((game-interactive-p (poker-interactive-p players))
	(button-player (nth (random (length players)) players))
	(rounds ())
	(losers ()))
    (setq players (poker-rotate-to-first button-player players))
    (while (and button-player
		(or (not game-interactive-p)
		    (poker-interactive-p players)))
      (message "Round %d, %d players." (1+ (length rounds)) (length players))

      (push (poker-dealer min-bet (poker-random-deck) () players)
	    rounds)

      (mapc #'poker-player-fold players)
      (setq button-player
	    (car-safe (cdr (cl-remove-if (lambda (player)
					   (zerop (poker-player-stack player)))
					 (poker-rotate-to-first button-player players)))))
      (let ((lost (cl-remove-if-not (lambda (player) (zerop (poker-player-stack player)))
				    players)))
	(when lost
	  (setq players (cl-remove-if
			 (lambda (player)
			   (when (member player lost)
			     (message "%s drops out." (poker-player-name player))
			     t))
			 players))
	  (setq losers (nconc losers lost))))
      (message "Remaining players: %s"
	       (mapconcat (lambda (player) (format "%s (%d)"
						   (poker-player-name player)
						   (poker-player-stack player)))
			  (cl-sort (append players nil)
				   #'> :key #'poker-player-stack)
			  " "))
      (when button-player
	(cl-assert (member button-player players))
	(let ((count (length players)))
	  (setq players (poker-rotate-to-first button-player players))
	  (cl-assert (= count (length players)))))

      (accept-process-output)

      (when (and game-interactive-p (not (poker-interactive-p players)))
	(message "You drop out in %s place."
		 (let ((rank (1+ (length players))))
		   (pcase rank
		     (2 "2nd")
		     (3 "3rd")
		     (n (format "%dth" n)))))))

    (when (and game-interactive-p (poker-interactive-p players))
      (message "You are the winner."))

    (cons players rounds)))


;;;###autoload
(define-key menu-bar-games-menu
  [poker] '(menu-item "Texas hold 'em poker" poker
		      :help "Play Texas hold 'em poker"))

;;; Tests:

(ert-deftest poker-combinations ()
  (should (equal 21 (length (poker-combinations 5 (last poker-deck 7)))))
  (should (equal 1326 (length (poker-combinations 2 poker-deck)))))

(ert-deftest poker-possible-hands ()
  (should (equal (poker-possible-hands '(1 2 3 4 5 6 7))
                 (poker-combinations 5 '(1 2 3 4 5 6 7))))
  (should (equal (poker-possible-hands '(1 2 3 4 5 6))
                 (poker-combinations 5 '(1 2 3 4 5 6)))))

(ert-deftest poker-hand-value ()
  (cl-labels ((permute (list)
		(when list
		  (if (not (cdr list)) (list list)
		    (cl-mapcan (lambda (elt)
				 (mapcar (lambda (l) (cons elt l))
					 (permute (remq elt list))))
			       list)))))
    ;; Straight flush
    (dolist (suit poker-suits)
      (dolist (hand (permute (mapcar (lambda (args)
				       (apply #'poker-make-card args))
				     (list (list 'ace suit) (list 'king suit)
					   (list 'queen suit) (list 'jack suit)
					   (list 10 suit)))))
	(should (eq (poker-hand-value hand) #x8cba98))))
    ;; Straight
    (dolist (s1 poker-suits)
      (dolist (s2 poker-suits)
	(dolist (s3 poker-suits)
	  (dolist (s4 poker-suits)
	    (dolist (s5 poker-suits)
	      (unless (and (eq s1 s2) (eq s2 s3) (eq s3 s4) (eq s4 s5))
		(dolist (hand (permute (mapcar (lambda (args)
						 (apply #'poker-make-card args))
					       (list (list 'ace s1) (list 'king s2)
						     (list 'queen s3) (list 'jack s4)
						     (list 10 s5)))))
		  (should (eq (poker-hand-value hand) #x4cba98)))))))))))

(ert-deftest poker ()
  (let ((players (list (poker-make-player "Angela" #'poker-automatic-fcr)
		       (poker-make-player "Bettina" #'poker-automatic-fcr)
		       (poker-make-player "Christoph" #'poker-automatic-fcr)
		       (poker-make-player "Daniela" #'poker-automatic-fcr)
		       (poker-make-player "Emilia" #'poker-automatic-fcr)
		       (poker-make-player "Franz" #'poker-automatic-fcr)
		       (poker-make-player "Günter" #'poker-automatic-fcr)
		       (poker-make-player "Harald" #'poker-automatic-fcr)
		       (poker-make-player "Isabella" #'poker-automatic-fcr)
		       (poker-make-player "Jakob" #'poker-automatic-fcr))))
    (while (> (length players) 1)
      (should (equal (poker-player-stack (caar (poker 1000 100 players)))
		     (* 1000 (length players))))
      (setq players (cdr players)))))

;;;; ChangeLog:

;; 2016-08-11  Mario Lang	<mlang@delysid.org>
;; 
;; 	[poker] Version 0.2, update copyright years and add todo
;; 
;; 2016-08-06  Mario Lang	<mlang@delysid.org>
;; 
;; 	Set default number of iterations to 300
;; 
;; 	Accidentally set too high in previous commit.
;; 
;; 2016-08-06  Mario Lang	<mlang@delysid.org>
;; 
;; 	Add a pre-flop hand strength table and an ert test for poker-hand-value
;; 
;; 	Precalculated pre-flop starting hand values with 1^6 iterations. Adjust
;; 	`poker-strength' to use them when appropriate.
;; 
;; 2016-08-05  Mario Lang	<mlang@delysid.org>
;; 
;; 	Improve performance of poker-hand-value by a factor of 4
;; 
;; 	`cl-count' is unnecessarily expensive, as it at least uses `length' and
;; 	`nthcdr' which we really don't need in this performance cricital code
;; 	path. Rewriting it without `cl-count' turns up another opportunity to
;; 	speed up, as we actually don't need to check the whole list to count
;; 	occurances of unique elements.	For one, we can start counting from 1
;; 	(not 0) if we encounter the first element, and we only need to check the
;; 	rest of the list of cards.  Also, stop using `mapcar' with
;; 	`poker-card-rank' to allow it to actually be inlined.  This turns out to
;; 	make poker-hand-value
;; 	*a lot* faster. Mission accomplished.
;; 
;; 2016-08-04  Mario Lang	<mlang@delysid.org>
;; 
;; 	Improve poker-hand-value performance by 25%
;; 
;; 	Avoid unnecessary calls to poker-card-suit in flush check.
;; 
;; 2016-08-02  Mario Lang	<mlang@delysid.org>
;; 
;; 	Very slightly improve performance
;; 
;; 	* packages/poker/poker.el (poker-hand-value): Use `delete-dups' instead
;; 	of
;; 	`cl-delete-duplicates' and avoid an unnecessary call to `ash'.
;; 
;; 2016-07-11  Paul Eggert	 <eggert@cs.ucla.edu>
;; 
;; 	Fix some quoting problems in doc strings
;; 
;; 	Most of these are minor issues involving, e.g., quoting `like this' 
;; 	instead of 'like this'.	 A few involve escaping ` and ' with a preceding
;; 	\= when the characters should not be turned into curved single quotes.
;; 
;; 2014-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* poker/poker.el (poker-combinations, poker-possible-hands): Fix tests.
;; 
;; 2014-06-18  Mario Lang	<mlang@delysid.org>
;; 
;; 	[poker] Add to the games menu.
;; 
;; 2014-05-18  Mario Lang	<mlang@delysid.org>
;; 
;; 	Add new simple package poker.el.
;; 


(provide 'poker)
;;; poker.el ends here
