:-use_module('utils/drs_to_coreace').


apev:-  
current_prolog_flag(argv, [InFile | [OutFile | _]]),  
open(InFile,read,S),
read(S,DRS),  
    drs_to_coreace(DRS, TextPre1),
concat_atom(TextPre1, '\n', TextPre2),
    atom_concat(TextPre2, '\n', Text),
    close(S),
    open(OutFile,write,S1),
write(S1,Text),
close(S1),
halt(0).
