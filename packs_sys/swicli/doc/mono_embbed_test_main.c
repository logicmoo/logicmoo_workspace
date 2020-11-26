#include <stdio.h>
#include <stdlib.h>
 
#include <glib.h>
#include <mono/jit/jit.h>
#include <mono/metadata/assembly.h>
 
int main(int argc, char *argv[])
{      
    MonoDomain *domain = NULL ;
    domain = mono_jit_init( "date" );
    return EXIT_SUCCESS;
     
}

