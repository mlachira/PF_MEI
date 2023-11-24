
### MODELO PARA BRASIL ###

# *Se eligio Brasil por ser el pais con mayor numero de casos de zika en el mundo.

library(deSolve)

  # Tiempo para Brasil
times_B<- seq(1, 100 , by = 1)

  # CONDICIONES INICIALES
cond_ini_B<-c( 
  
  #Condiciones para alto riesgo
  S_H = 120000000,
  E_H = 5150000,
  I_H = 3000000,
  R_H = 3290000,
  
  #Condiciones para los vectores
  S_V = 986700,
  E_V = 467900,
  I_V = 393000,
  
  #Condiciones para bajo riesgo
  S_L = 80000000,
  E_L = 2000000, 
  I_L = 575000,  
  R_L = 285000)

I_H = 3000000
I_V = 393000
I_L = 575000

# Las condiciones iniciales varian dependiendo del lugar, en este caso 
# se contemplo a una poblacion total de 214,300,000 habitantes (poco mas del doble que Mexico).
# Los numeros fueron elegidos al azar, manteniendose dentro de la poblacion total real.


  # PARAMETROS
parametro_B<- c(
  Lv <- 8.5 + 35, #Esperanza de vida de mosquitos (dias)
  Lh <- 76.2* 365, # Esperanza de vida de humanos (dias) #### MODIFICAR POR EDO ####
  iph <- 8.5, # Periodo de incubacion en humanos (dias)
  ipv <- 8.5, # Periodo de incubacion en mosquito (dias)
  ifh <- 4.5, # Periodo infeccioso en humanos (dias)
  muh <- 0.0016, # Mortalidad en humanos 
  muv <- 0.0064, # Mortalidad en vectores 
  alfa_H <- 1/iph, # Tasa de incubacion en humanos 
  alfa_V <- 1/ipv, # Tasa de incubacion en mosquitos
  gamma <- 1/ifh, # Tasa de recuperacion en humanos 
  n_H <- 0.0013, # Tasa de natalidad humanos ##### MODIFICAR POR EDO ####
  n_V <- 0.099, # Tasa de natalidad mosquitos 
  tetha <- 0.005, # Tasa de letalidad asociada a zika
  q <- 0.25, # Proporcion de la poblacion que usa condon #### ESTIMADO ####
  p_H <- 0.867, #   Proporcion de la poblacion que nace libre de zika 
  f <- 0.75, # Proporcion de nacidos que se dirigen al grupo de alto riesgo #### ESTIMADO 
  b <- 0.45, #Tasa de picadura de mosquito
  a1 <- 0.044, # Probabilidad de transmision, picadura de mosquito infectado a humano susceptible de bajo riesgo (LV)
  a2 <- 0.047, # Probabilidad de transmision, picadura de mosquito susceptible a humano infectado de bajo riesgo (VL)
  a3 <- 0.033, # Probabilidad de transmision por contacto sexual entre humanos susceptibles e infectados (S-I humanos)
  a4 <- 0.0616, # Probabilidad de transmision, mosquito infectado a humano susceptible de alto riesgo (HV)
  a5 <- 0.0658, # Probabilidad de transmision, mosquito susceptible a humano infectado de alto riesgo (VH)
  c <- 0.0055, # contacto sexual entre humano susceptible y humano infectado 
  Nh <- 214300000, # Poblacion total de humanos  #### MODIFICAR POR EDO ####
  beta1 <- (b*a1*I_V)/Nh, # Tasa de contagio mosquito infectado a persona susceptible de bajo riesgo
  beta2 <- ((c*a3*(I_H + I_L))/(Nh)), # Tasa de contagio de persona infectada a persona susceptible  
  beta3 <- ((b*a2*(I_L))/(Nh)), # Tasa de contagio persona infectada de bajo riesgo a mosquito susceptible
  beta4 <- ((b*a4*I_V)/(Nh)), # Tasa de contagio mosquito infectado a persona susceptible de alto riesgo 
  beta5 <- ((b*a5*(I_H))/(Nh)) # Tasa de contagio persona infectada de alto riesgo a mosquito susceptible
)

# Todas las simulaciones de los lugares contemplan los mismos parametros para casi todo.
# Unicamente se modificaron aquellos que son propios del lugar (Brasil en este caso). 
# Con los parametros establecidos, se procede a la simulacion:


  #SIMULACION

out_B <- ode(y=cond_ini_B,
             times=times_B,
             fun = ZIKA_M, 
             parms  = parametro_B)

plot(out_B,col="blue")
par(mfrow=c(1,1))

graficaTG_B <- matplot(out_B[ , 1], out_B[ , 2:12], type="l", xlab = "tiempo", ylab = "Población", 
                       main= "Gráfica ZIKA Brasil", lwd = 2) 
legend("topright", c("S alto riesgo", "E alto riesgo ", "I alto riesgo ", "R alto riesgo", 
                     "S vectores", "E vectores", "I vectores", 
                     "S bajo riesgo", "E bajo riesgo", "I bajo riesgo", "R bajo riesgo"), col =1:5, 
       lty=1:5, cex=0.5)

# En esta primera grafica obtenemos los valores de todas las variables dentro del modelo general, donde
# escomplicado observar el curso individual de las variables. 
# Por ello, realizamos graficas que acoten la informacion y
# donde se aprecien las variaciones de los individuos infectados de cada grupo
# asi como interacciones entre humanos y vectores en graficas distintas



  # Grafica alto riesgo vs vectores 

graficaHV_B<- matplot(out_B[ , 1], out_B[ , 2:8], type="l", xlab = "tiempo", ylab = "Población", 
                      main = "Gráfica ZIKA Brasil (HV)", lwd = 2) 
legend("topright", c("S alto riesgo", "E alto riesgo ", "I alto riesgo ", "R alto riesgo", 
                     "S vectores", "E vectores", "I vectores"), col =1:7, lty=1:5, cex=0.5)

# Esta grafica ejemplifica la interaccion entre individuos humanos e alto riesgo e individuos de la clase
# vector, esto para observar como se lleva a cabo la interaccion entre estos grupos 
# NOTA: La poblacion de vectores y las fluctuaciones en este grupo tambien dependen de su interaccion
# con los individuos de tipo bajo riesgo por lo que no podriamos reducir esta grafica solamente a 
# alto riesgo vs vectores

  # DISCUSION DE LA GRAFICA: 
# La cantidad de recuperados de alto riesgo casi alcanza la de los expuestos al inicio de la dinamica.
# La cantidad de expuestos de ato riesgo aumenta rapidamente al inicio de la enfermedad y
#disminuye de la misma manera, alcanzando una estabilidad en casi 0 despues de aproximadamente
#40 dias desde el inicio de la dinamica.
# La cantidad de infectados de alto riesgo aumenta al inicio de la dinamica, pero no tanto como los
#variables mencionadas arriba. Igualmente, disminuyen gradualmente al inicio.



  # Grafica bajo riesgo vs vectores 
graficaLV_B<- matplot(out_B[ , 1], out_B[ , 6:12], type="l", xlab = "tiempo", ylab = "Población", 
                      main = "Gráfica ZIKA Brasil (LV)", lwd = 2) 
legend("topright", c("S vectores", "E vectores", "I vectores", 
                     "S bajo riesgo", "E bajo riesgo ", "I bajo riesgo ", "R bajo riesgo"), col =1:7, lty=1:5, cex=0.5)

# Esta grafica ejemplifica la relacion entre individuos humanos de bajo riesgo e individuos de clase
# vector


  # DISCUSION DE LA GRAFICA: 
# La cantidad de susceptibles de bajo riesgo es muy alta al inicio y tiene una disminucion repentida muy al inicio de la dinamica.
#quedandose en 0 casi al instante.
# La cantidad de vectores susceptibles es la mas alta y esta aumenta gradualmente en el tiempo,
#manteniendose alto y disminuyendo ligeramente.
# La cantidad de infectados de bajo riesgo es alta al inicio, pero disminuye rapidamente, 
#antes de los 40 dias pasados desde el inicio de la dinamica.


  # Grafica infectados (L-H-V)
graficaINF_B<- matplot(out_B[ , 1],out_B[ , c(4, 8, 11)], type="l", xlab = "Tiempo", ylab = "Población", 
                       main= "Gráfica ZIKA Brasil (infectados)", lwd = 1) 
legend("topright", c( "I alto riesgo ", "I vectores","I bajo riesgo"), col = 1:3, lty=1:5, cex=0.5)

# Esta grafica nos muestra las variaciones en los niveles de individuos infectados
# pertenecientes a cada una de las clasificaciones dentro del modelo 

  # DISCUSION DE LA GRAFICA: 
# La cantidad de infectados de alto riesgo es la mayor, aumentando rapidamente (su pico es
#aproximadamente a los 10 dias) y disminuyendo al mismo ritmo, estabilizandose en 0 a los 40 dias.
# Le siguen los individuos de bajo riesgo, los cuales muestran un comportamiento muy similar.
# Por ultimo, los vectores infectados son muy pocos, lo cual tiene sentido si se considera que
#la cantidad de mosquitos es mucho menor que la de la poblacion de personas de Brasil.


  # ESTIMACION DEL R0
contact_rate_ZK_B<- (beta1 + beta2 + beta3 + beta4 + beta5)
transmission_probability_ZK_B <- (a1 + a2 + a3 + a4 + a5)
infectious_period_ZK_B <- ifh
R0B= contact_rate_ZK_B * transmission_probability_ZK_B * infectious_period_ZK_B
R_0B <- c("RO=", R0B)
R_0B

# DISCUSION DEL R0 #
# El R0 resultante es de "0.000635151162105693"
# Un R0 estima la velocidad con la que una enfermedad se esparce en una poblacion a traves del tiempo
# Es decir, es el numero de casos en promedio que surgen de un individuo.
# Un R0 mayor a 1 indica que una persona es capaz de infectar a mas de un individuo.
# Sin embargo, en este caso el R0 es menor a 1, por lo que el punto de equilibrio del sistema es free-disease, es decir, la enfermedad no es endemica.
