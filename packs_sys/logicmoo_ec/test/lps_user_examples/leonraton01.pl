:- expects_dialect(lps).

maxTime(18).

fluents león_atrapado, quiero_liberar_al_león, salvar_al_león.
actions precavido, utilizar_afilados_dientes, correr.
events liberar, cortar_cuerdas, escapar_al_dejar_trampa.
initially león_atrapado.
if león_atrapado at T1
then liberar from T1 to T2.
liberar from T1 to T2
if precavido from T1 to T2.
liberar terminates león_atrapado.
initially quiero_liberar_al_león.
if quiero_liberar_al_león at T3
then cortar_cuerdas from T3 to T4.
cortar_cuerdas from T3 to T4
if utilizar_afilados_dientes from T3 to T4.
cortar_cuerdas terminates quiero_liberar_al_león.
initially salvar_al_león.
if salvar_al_león at T5
then escapar_al_dejar_trampa from T5 to T6.
escapar_al_dejar_trampa from T5 to T6
if correr from T5 to T6.
escapar_al_dejar_trampa terminates salvar_al_león.