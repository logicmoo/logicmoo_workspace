/*
* Copyright (C) 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Mime Types
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(mime_types, [mime_extension/2, mime_magic/2]).


mime_extension(csm, 'application/cu-seeme').
mime_extension(cu, 'application/cu-seeme').
mime_extension(tsp, 'application/dsptype').
mime_extension(spl, 'application/futuresplash').
mime_extension(hqx, 'application/mac-binhex40').
mime_extension(mdb, 'application/msaccess').
mime_extension(doc, 'application/msword').
mime_extension(dot, 'application/msword').
mime_extension(bin, 'application/octet-stream').
mime_extension(oda, 'application/oda').
mime_extension(pdf, 'application/pdf').
mime_extension(pgp, 'application/pgp-signature').
mime_extension(ps, 'application/postscript').
mime_extension(ai, 'application/postscript').
mime_extension(eps, 'application/postscript').
mime_extension(rtf, 'application/rtf').
mime_extension(xls, 'application/vnd.ms-excel').
mime_extension(xlb, 'application/vnd.ms-excel').
mime_extension(ppt, 'application/vnd.ms-powerpoint').
mime_extension(pps, 'application/vnd.ms-powerpoint').
mime_extension(pot, 'application/vnd.ms-powerpoint').
mime_extension(wmlc, 'application/vnd.wap.wmlc').
mime_extension(wmlsc, 'application/vnd.wap.wmlscriptc').
mime_extension(wp5, 'application/wordperfect5.1').
mime_extension(zip, 'application/zip').
mime_extension(wk, 'application/x-123').
mime_extension(bcpio, 'application/x-bcpio').
mime_extension(pgn, 'application/x-chess-pgn').
mime_extension(cpio, 'application/x-cpio').
mime_extension(deb, 'application/x-debian-package').
mime_extension(dcr, 'application/x-director').
mime_extension(dir, 'application/x-director').
mime_extension(dxr, 'application/x-director').
mime_extension(dms, 'application/x-dms').
mime_extension(dvi, 'application/x-dvi').
mime_extension(fig, 'application/x-xfig').
mime_extension(pfa, 'application/x-font').
mime_extension(pfb, 'application/x-font').
mime_extension(gsf, 'application/x-font').
mime_extension(pcf, 'application/x-font').
mime_extension('pcf.Z', 'application/x-font').
mime_extension(gnumeric, 'application/x-gnumeric').
mime_extension(gtar, 'application/x-gtar').
mime_extension(tgz, 'application/x-gtar').
mime_extension(taz, 'application/x-gtar').
mime_extension(hdf, 'application/x-hdf').
mime_extension(phtml, 'application/x-httpd-php').
mime_extension(pht, 'application/x-httpd-php').
mime_extension(php, 'application/x-httpd-php').
mime_extension(php3, 'application/x-httpd-php3').
mime_extension(phps, 'application/x-httpd-php3-source').
mime_extension(php3p, 'application/x-httpd-php3-preprocessed').
mime_extension(php4, 'application/x-httpd-php4').
mime_extension(ica, 'application/x-ica').
mime_extension(jar, 'application/x-java-archive').
mime_extension(ser, 'application/x-java-serialized-object').
mime_extension(class, 'application/x-java-vm').
mime_extension(js, 'application/x-javascript').
mime_extension(chrt, 'application/x-kchart').
mime_extension(kil, 'application/x-killustrator').
mime_extension(kpr, 'application/x-kpresenter').
mime_extension(kpt, 'application/x-kpresenter').
mime_extension(ksp, 'application/x-kspread').
mime_extension(kwd, 'application/x-kword').
mime_extension(kwt, 'application/x-kword').
mime_extension(latex, 'application/x-latex').
mime_extension(lha, 'application/x-lha').
mime_extension(lzh, 'application/x-lzh').
mime_extension(lzx, 'application/x-lzx').
mime_extension(frm, 'application/x-maker').
mime_extension(maker, 'application/x-maker').
mime_extension(frame, 'application/x-maker').
mime_extension(fm, 'application/x-maker').
mime_extension(fb, 'application/x-maker').
mime_extension(book, 'application/x-maker').
mime_extension(fbdoc, 'application/x-maker').
mime_extension(mif, 'application/x-mif').
mime_extension(com, 'application/x-msdos-program').
mime_extension(exe, 'application/x-msdos-program').
mime_extension(bat, 'application/x-msdos-program').
mime_extension(dll, 'application/x-msdos-program').
mime_extension(msi, 'application/x-msi').
mime_extension(nc, 'application/x-netcdf').
mime_extension(cdf, 'application/x-netcdf').
mime_extension(pac, 'application/x-ns-proxy-autoconfig').
mime_extension(o, 'application/x-object').
mime_extension(ogg, 'application/x-ogg').
mime_extension(oza, 'application/x-oz-application').
mime_extension(pl, 'application/x-perl').
mime_extension(pm, 'application/x-perl').
mime_extension(crl, 'application/x-pkcs7-crl').
mime_extension(rpm, 'application/x-redhat-package-manager').
mime_extension(shar, 'application/x-shar').
mime_extension(swf, 'application/x-shockwave-flash').
mime_extension(swfl, 'application/x-shockwave-flash').
mime_extension(sdd, 'application/x-star-office').
mime_extension(sda, 'application/x-star-office').
mime_extension(sit, 'application/x-stuffit').
mime_extension(sv4cpio, 'application/x-sv4cpio').
mime_extension(sv4crc, 'application/x-sv4crc').
mime_extension(tar, 'application/x-tar').
mime_extension(gf, 'application/x-tex-gf').
mime_extension(pk, 'application/x-tex-pk').
mime_extension('PK', 'application/x-tex-pk').
mime_extension(texinfo, 'application/x-texinfo').
mime_extension(texi, 'application/x-texinfo').
mime_extension(~, 'application/x-trash').
mime_extension('%', 'application/x-trash').
mime_extension(bak, 'application/x-trash').
mime_extension(old, 'application/x-trash').
mime_extension(sik, 'application/x-trash').
mime_extension(t, 'application/x-troff').
mime_extension(tr, 'application/x-troff').
mime_extension(roff, 'application/x-troff').
mime_extension(man, 'application/x-troff-man').
mime_extension(me, 'application/x-troff-me').
mime_extension(ms, 'application/x-troff-ms').
mime_extension(ustar, 'application/x-ustar').
mime_extension(src, 'application/x-wais-source').
mime_extension(wz, 'application/x-wingz').
mime_extension(crt, 'application/x-x509-ca-cert').
mime_extension(au, 'audio/basic').
mime_extension(snd, 'audio/basic').
mime_extension(mid, 'audio/midi').
mime_extension(midi, 'audio/midi').
mime_extension(mpga, 'audio/mpeg').
mime_extension(mpega, 'audio/mpeg').
mime_extension(mp2, 'audio/mpeg').
mime_extension(mp3, 'audio/mpeg').
mime_extension(m3u, 'audio/mpegurl').
mime_extension(sid, 'audio/prs.sid').
mime_extension(aif, 'audio/x-aiff').
mime_extension(aiff, 'audio/x-aiff').
mime_extension(aifc, 'audio/x-aiff').
mime_extension(gsm, 'audio/x-gsm').
mime_extension(ra, 'audio/x-pn-realaudio').
mime_extension(rm, 'audio/x-pn-realaudio').
mime_extension(ram, 'audio/x-pn-realaudio').
mime_extension(pls, 'audio/x-scpls').
mime_extension(wav, 'audio/x-wav').
mime_extension(bmp, 'image/bitmap').
mime_extension(gif, 'image/gif').
mime_extension(ief, 'image/ief').
mime_extension(jpeg, 'image/jpeg').
mime_extension(jpg, 'image/jpeg').
mime_extension(jpe, 'image/jpeg').
mime_extension(pcx, 'image/pcx').
mime_extension(png, 'image/png').
mime_extension(tiff, 'image/tiff').
mime_extension(tif, 'image/tiff').
mime_extension(wbmp, 'image/vnd.wap.wbmp').
mime_extension(ras, 'image/x-cmu-raster').
mime_extension(cdr, 'image/x-coreldraw').
mime_extension(pat, 'image/x-coreldrawpattern').
mime_extension(cdt, 'image/x-coreldrawtemplate').
mime_extension(cpt, 'image/x-corelphotopaint').
mime_extension(jng, 'image/x-jng').
mime_extension(pnm, 'image/x-portable-anymap').
mime_extension(pbm, 'image/x-portable-bitmap').
mime_extension(pgm, 'image/x-portable-graymap').
mime_extension(ppm, 'image/x-portable-pixmap').
mime_extension(rgb, 'image/x-rgb').
mime_extension(xbm, 'image/x-xbitmap').
mime_extension(xpm, 'image/x-xpixmap').
mime_extension(xwd, 'image/x-xwindowdump').
mime_extension(csv, 'text/comma-separated-values').
mime_extension(css, 'text/css').
mime_extension(htm, 'text/html').
mime_extension(html, 'text/html').
mime_extension(xhtml, 'text/html').
mime_extension(mml, 'text/mathml').
mime_extension(txt, 'text/plain').
mime_extension(text, 'text/plain').
mime_extension(diff, 'text/plain').
mime_extension(rtx, 'text/richtext').
mime_extension(tsv, 'text/tab-separated-values').
mime_extension(wml, 'text/vnd.wap.wml').
mime_extension(wmls, 'text/vnd.wap.wmlscript').
mime_extension(xml, 'text/xml').
mime_extension('h++', 'text/x-c++hdr').
mime_extension(hpp, 'text/x-c++hdr').
mime_extension(hxx, 'text/x-c++hdr').
mime_extension(hh, 'text/x-c++hdr').
mime_extension('c++', 'text/x-c++src').
mime_extension(cpp, 'text/x-c++src').
mime_extension(cxx, 'text/x-c++src').
mime_extension(cc, 'text/x-c++src').
mime_extension(h, 'text/x-chdr').
mime_extension(csh, 'text/x-csh').
mime_extension(c, 'text/x-csrc').
mime_extension(java, 'text/x-java').
mime_extension(moc, 'text/x-moc').
mime_extension(p, 'text/x-pascal').
mime_extension(pas, 'text/x-pascal').
mime_extension(etx, 'text/x-setext').
mime_extension(sh, 'text/x-sh').
mime_extension(tcl, 'text/x-tcl').
mime_extension(tk, 'text/x-tcl').
mime_extension(tex, 'text/x-tex').
mime_extension(ltx, 'text/x-tex').
mime_extension(sty, 'text/x-tex').
mime_extension(cls, 'text/x-tex').
mime_extension(vcs, 'text/x-vcalendar').
mime_extension(vcf, 'text/x-vcard').
mime_extension(dl, 'video/dl').
mime_extension(fli, 'video/fli').
mime_extension(gl, 'video/gl').
mime_extension(mpeg, 'video/mpeg').
mime_extension(mpg, 'video/mpeg').
mime_extension(mpe, 'video/mpeg').
mime_extension(qt, 'video/quicktime').
mime_extension(mov, 'video/quicktime').
mime_extension(mng, 'video/x-mng').
mime_extension(asf, 'video/x-ms-asf').
mime_extension(asx, 'video/x-ms-asf').
mime_extension(avi, 'video/x-msvideo').
mime_extension(movie, 'video/x-sgi-movie').
mime_extension(vrm, 'x-world/x-vrml').
mime_extension(vrml, 'x-world/x-vrml').
mime_extension(wrl, 'x-world/x-vrml').


mime_magic('<!DOCTYPE HTML', 'text/html').
mime_magic('<!doctype html', 'text/html').
mime_magic('<HTML', 'text/html').
mime_magic('<html', 'text/html').
mime_magic('<HEAD', 'text/html').
mime_magic('<head', 'text/html').
mime_magic('<TITLE', 'text/html').
mime_magic('<title', 'text/html').
mime_magic('<BODY', 'text/html').
mime_magic('<body', 'text/html').

