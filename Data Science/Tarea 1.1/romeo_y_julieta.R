#### Romeo y Julieta ####

# Romeo y Julieta tienen una cita a una hora determinada, cada uno llegará a la cita con un retraso de
# entre 0 y 1 hora, con cada par de demoras siendo igualmente probables. El primero en llegar esperará por
# 15 minutos y se irá si el otro no llega en ese lapso. ¿Cuál es la probabilidad de que Romeo y Julieta se
# encuentren? Simúlalo en R.


se_encuentran <- is.numeric(0)
no_se_encuentran <- is.numeric(0)
counter <- is.numeric(0)

for (romeo in 0:59){
  for (julieta in 0:59){
    if (abs(romeo-julieta) <= 15){
      se_encuentran = se_encuentran + 1
    }
    else {
      no_se_encuentran = no_se_encuentran + 1
    }
    counter = counter + 1
  }
}

prob_se_encuentran = se_encuentran/(se_encuentran + no_se_encuentran)
