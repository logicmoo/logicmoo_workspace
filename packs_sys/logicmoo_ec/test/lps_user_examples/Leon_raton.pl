:- expects_dialect(lps).

maxTime(10).

fluents leon_atrapado, liberar_leon, salvar_leon.

actions precavido, utilizar_dientes, correr.

events liberar, cortar_amarre, 
       escapar_trampa.

initially leon_atrapado.
if leon_atrapado at T1
   then liberar from T1 to T2.
liberar from T1 to T2 if
      precavido from T1 to T2.
liberar terminates leon_atrapado.

liberar initiates liberar_leon.
if liberar_leon at T3 
   then cortar_amarre from T3 to T4.
cortar_amarre from T3 to T4 if
      utilizar_dientes from T3 to T4.
cortar_amarre terminates liberar_leon.

cortar_amarre initiates salvar_leon.
if salvar_leon at T5
   then escapar_trampa from T5 to T6.
escapar_trampa from T5 to T6 if
      correr from T5 to T6.
escapar_trampa terminates salvar_leon.


