---+ ERROR: Encoding cannot represent character

SWI-Prolog is a Unicode application. This means all characters are
internally encoded as large integers in a uniform space. This encoding
can represent any character in any language. Streams however are
sequences of bytes, i.e., sequences of integers in the range 0..255.
This implies some encoding is needed to represent Unicode characters.
Various encodings exist, some of which cannot represent all Unicode
characters. SWI-Prolog uses the default encoding of the platform on
which it runs. If it must write a character that does not fit in this
encoding it gives this error message.

There is no easy answer on what to do. Understanding international
character encoding issues is really hard. The first things to know are
in the SWI-Prolog Reference Manual, section [[Wide character
support][</pldoc/man?section=widechars>]].
