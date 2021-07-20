# Build wisitoken-bnf-generate.exe, for generating code from grammar files.
# 
# Instead of using this, you should consider using the complete
# wisitoken development tree; see
# http://stephe-leake.org/ada/wisitoken.html

gprbuild -p -j8 -P wisi.gpr wisitoken-bnf-generate
