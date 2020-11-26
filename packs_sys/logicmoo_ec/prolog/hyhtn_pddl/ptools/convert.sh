#!/usr/bin/csh

echo compile\($1\). >input
echo compile\($2\). >>input
echo compile\(ocl2pddl_p\). >>input
echo convert. >>input
echo convert_p. >>input
echo halt.>>input
# Check arguments
sicstus -l ocl2pddl< input >pddl_domain
rm input

