## create category and division data from Geoff's SPC files
cd classif/SPC
cat CNF_NKS_EPR | sed -e 's/ .*/\tEPR\tEPT\t/'   > ../EPT
cat CNF_NKS_RFO_NEQ_HRN | sed -e 's/ .*/\tCNF\tHNE\t/' > ../HNE
cat CNF_NKS_RFO_NEQ_NHN | sed -e 's/ .*/\tCNF\tNNE\t/' > ../NNE
cat CNF_NKS_RFO_PEQ_NUE | sed -e 's/ .*/\tCNF\tPEQ\t/' > ../PEQ
cat CNF_NKS_RFO_PEQ_UEQ | sed -e 's/ .*/\tUEQ\tUEQ\t/' >../UEQ
cat CNF_NKS_RFO_SEQ_HRN | sed -e 's/ .*/\tCNF\tHEQ\t/' >../HEQ
cat CNF_NKS_RFO_SEQ_NHN | sed -e 's/ .*/\tCNF\tNEQ\t/' >../NEQ
cat CNF_SAT_EPR | sed -e 's/ .*/\tEPR\tEPS\t/' >../EPS
cat CNF_SAT_RFO_EQU_NUE CNF_SAT_RFO_PEQ_UEQ | sed -e 's/ .*/\tSAT\tSEQ\t/' >../SEQ
cat CNF_SAT_RFO_NEQ FOF_CSA_RFO_EQU | sed -e 's/ .*/\tSAT\tSNE\t/' >../SNE
cat FOF_CSA_EPR_EQU FOF_NKU_NUN_EPR_EQU FOF_NKU_NUN_RFO_EQU | sed -e 's/ .*/\tFNT\tFNQ\t/' >../FNQ
cat FOF_CSA_EPR_NEQ FOF_CSA_RFO_NEQ FOF_NKU_NUN_EPR_NEQ FOF_NKU_NUN_RFO_NEQ | sed -e 's/ .*/\tFNT\tFNN\t/' >../FNN
cat FOF_NKC_EPR_EQU FOF_NKC_RFO_EQU | sed -e 's/ .*/\tFOF\tFEQ\t/' >../FEQ
cat FOF_NKC_EPR_NEQ FOF_NKC_RFO_NEQ | sed -e 's/ .*/\tFOF\tFNE\t/' >../FNE
cat FOF_UNS_EPR_EQU FOF_UNS_EPR_NEQ FOF_UNS_RFO_EQU FOF_UNS_RFO_NEQ | sed -e 's/ .*/\tFOF\tNULL\t/' >../NULL
cd ..
cat EPS EPT FEQ FNE FNN FNQ HEQ HNE NEQ NNE PEQ SEQ SNE UEQ NULL | perl -e '%h=(); open(C,"CASC21problems");while(<C>){chop; $h{$_}=();} close(C); while(<>) {chop; (m/^([^\t]+)\t.*/) or die "Bad"; print $_; if(exists $h{$1}) {print "1\n";} else {print "0\n";}}' |sort > ../loadsql/tptp_r1
