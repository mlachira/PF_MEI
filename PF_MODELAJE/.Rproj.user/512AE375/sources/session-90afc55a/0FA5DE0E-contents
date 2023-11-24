
### MODELO PARA VERACRUZ ###

# *Veracruz fue escogido por ser el estado con mas casos de zika registrados en Mexico.

library(deSolve)

  # Tiempo para Veracruz
times_veracruz <- seq(1, 100 , by = 1)

  # CONDICIONES INICIALES
cond_ini_veracruz <-c( 
  
  #Condiciones para los de alto riesgo
  S_H = 2000000,
  E_H = 1000000,
  I_H = 800000,
  R_H = 200000,
  
  #Condiciones para los vectores
  S_V = 400000,
  E_V = 300000,
  I_V = 500000,
  
  #Condiciones para los de bajo riesgo
  S_L = 1500000,
  E_L = 900000, 
  I_L = 400000,  
  R_L = 63000)

I_H = 800000
I_L = 400000
I_V = 50000

# Las condiciones iniciales varian dependiendo del lugar, en este caso 
# se contemplo a una poblacion total de 8,063,000 habitantes.
# Los numeros fueron elegidos al azar, manteniendose dentro de la poblacion total real.


  # PARAMETROS
parametro_veracruz <- c(
  Lv <- 8.5 + 35, #Esperanza de vida de mosquitos (dias)
  Lh <- 74.9 * 365, # Esperanza de vida de humanos (dias) #### MODIFICAR POR EDO ####
  iph <- 8.5, # Periodo de incubacion en humanos (dias)
  ipv <- 8.5, # Periodo de incubacion en mosquito (dias)
  ifh <- 4.5, # Periodo infeccioso en humanos (dias)
  muh <- 0.0016, # Mortalidad en humanos 
  muv <- 0.0064, # Mortalidad en vectores 
  alfa_H <- 1/iph, # Tasa de incubacion en humanos 
  alfa_V <- 1/ipv, # Tasa de incubacion en mosquitos
  gamma <- 1/ifh, # Tasa de recuperacion en humanos 
  n_H <- 0.0502, #Tasaa de natalidad humanos ##### MODIFICAR POR EDO ####
  n_V <- 0.099, # Tasa de natalidad mosquitos 
  tetha <- 0.005, # Tasa de letalidad asociada a zika
  q <- 0.25, # Proporcion de la poblacion que usa condon #### ESTIMADO ####
  p_H <- 0.867, #   Proporcion de la poblacion que nace libre de zika 
  f <- 0.47, # Proporcion de nacidos que se dirigen al grupo de alto riesgo #### ESTIMADO ####
  b <- 0.45, #Tasa de picadura de mosquito
  a1 <- 0.044, # Probabilidad de transmision, picadura de mosquito infectado a humano susceptible de bajo riesgo (LV)
  a2 <- 0.047, # Probabilidad de transmision, picadura de mosquito susceptible a humano infectado debajo riesgo (VL)
  a3 <- 0.033, # Probabilidad de transmision por contacto sexual entre humanos susceptibles e infectados (S-I humanos)
  a4 <- 0.0616, # Probabilidad de transmision, mosquito infectado a humano susceptible de alto riesgo (HV)
  a5 <- 0.0658, # Probabilidad de transmision, mosquito susceptible a humano infectado de alto riesgo (VH)
  c <- 0.0055, # contacto sexual entre humano susceptible y humano infectado 
  Nh <- 8063000, # Poblacion total de humanos  #### MODIFICAR POR EDO ####
  beta1 <- (b*a1*I_V)/Nh, # Tasa de contagio mosquito infectado a persona susceptible de bajo riesgo
  beta2 <- ((c*a3*(I_H + I_L))/(Nh)), # Tasa de contagio de persona infectada a persona susceptible  
  beta3 <- ((b*a2*(I_L))/(Nh)), # Tasa de contagio persona infectada de bajo riesgo a mosquito susceptible
  beta4 <- ((b*a4*I_V)/(Nh)), # Tasa de contagio mosquito infectado a persona susceptible de alto riesgo 
  beta5 <- ((b*a5*(I_H))/(Nh)) # Tasa de contagio persona infectada de alto riesgo a mosquito susceptible
)


# Todas las simulaciones de los lugares contemplan los mismos parametros para casi todo.
# Unicamente se modificaron aquellos que son propios del lugar (Veracruz en este caso). 
# Con los parametros establecidos, se procede a la simulacion:


  # SIMULACION
out_veracruz <- ode(y=cond_ini_veracruz,
                    times=times_veracruz,
                    fun = ZIKA_M, 
                    parms  = parametro_veracruz)

plot(out_veracruz,col="black")
par(mfrow=c(1,1))

graficaTG_veracruz <- matplot(out_veracruz[ , 1], out_veracruz[ , 2:8], type="l", xlab = "tiempo", 
                              ylab = "Poblacion", 
                              main= "Grafica ZIKA Veracruz", lwd = 2) 
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

graficaHV_veracruz <- matplot(out_veracruz[ , 1], out_veracruz[ , 2:8], type="l", xlab = "tiempo", ylab = "Población", main = "Gráfica ZIKA Veracruz (HV)", lwd = 2) 
legend("topright", c("S alto riesgo", "E alto riesgo ", "I alto riesgo ", "R alto riesgo", 
                     "S vectores", "E vectores", "I vectores"), col =1:7, lty=1:5, cex=0.5)

# Esta grafica ejemplifica la interaccion entre individuos humanos e alto riesgo e individuos de la clase
# vector, esto para observar como se lleva a cabo la interaccion entre estos grupos 
# NOTA: La poblacion de vectores y las fluctuaciones en este grupo tambien dependen de su interaccion
# con los individuos de tipo bajo riesgo por lo que no podriamos reducir esta grafica solamente a 
# alto riesgo vs vectores

  # DISCUSION DE LA GRAFICA: 
# El valor que sobresale es el de los recuperados de alto riesgo, los cuales aumentan rapidamente al inicio
#y se mantienen altos a lo largo del curso de la enfermedad en la poblacion.
# Hay muchos individuos expuestos de alto riesgo muy al inicio de la dinamica, disminuyendo rapidamente 
#hasta quedarse estables aproximadamente despues de 40 dias desde el inicio.
# Los susceptibles de alto riesgo aumentan al inicio ligeramente, manteniendo en esos mismos valores 
#a lo largo del tiempo (es decir, quedandose estables).
# Los infectados de alto riesgo aumentan muy al inicio, bajando antes de aproximandamente 20 dias desde el inicio de la dinamica.




  # Grafica bajo riesgo vs vectores 
graficaLV_veracruz <- matplot(out_veracruz[ , 1], out_veracruz[ , 6:12], type="l", xlab = "tiempo", ylab = "Población",  main = "Gráfica ZIKA Veracruz (LV)", lwd = 2) 
legend("topright", c("S vectores", "E vectores", "I vectores", 
                     "S bajo riesgo", "E bajo riesgo ", "I bajo riesgo ", "R bajo riesgo"), col =1:7, 
       lty=1:5, cex=0.5)

# Esta grafica ejemplifica la relacion entre individuos humanos de bajo riesgo e individuos de clase
# vector

  # DISCUSION DE LA GRAFICA: 
# La cantidad de vectores susceptibles es mayor que la de cualquier otra variable.
# Le siguen los expuestos de alto riesgo, quienes aumentan rapidamente muy al inicio y disminuyen igual de rapido
#bajando a aproximadamente 250,000 casos en 20 dias (el pico supera los 2,000,000)
# La cantidad de vectores infectados casi alcanza el millon, manteniendose relativamente estable en el tiempo
#se aprecia una ligera disminucion eventual, bajando a casi la mitad de casos en 100 dias desde el inicio.



  # Grafica infectados (L-H-V)
graficaINF_veracruz <- matplot(out_veracruz[ , 1],out_veracruz[ , c(4, 8, 11)], type="l", xlab = "Tiempo", ylab = "Población",  main= "Gráfica ZIKA Veracruz (infectados)", lwd = 1) 
legend("topright", c( "I alto riesgo ", "I vectores","I bajo riesgo"), col = 1:3, lty=1:5, cex=0.5)

# Esta grafica nos muestra las variaciones en los niveles de individuos infectados
# pertenecientes a cada una de las clasificaciones dentro del modelo 

  # DISCUSION DE LA GRAFICA: 
# La cantidad de infectados de alto riesgo y la de vectores infectados casi alcanza los mismos valores, ambos altos.
# Hay ligeramente mas infectados de alto riesgo que vectores infectados.
# Los infectados de alto riesgo aumentan rapidamente muy al inicio, bajando igual de rapido y
#estabilizandose en casi 0 casos en aproximadamente 40 dias desde el inicio de la dinamica.
# Los infectados de bajo riesgo son los mas bajos, pero no por mucho,
#estabilizandose casi de la misma manera y tiempo que los de alto riesgo.
# Los vectores infectados se mantienen altos en el tiempo, disminuyendo de manera mucho mas gradual que las demas variables.


  # ESTIMACION DEL R0
contact_rate_ZK_V<- (beta1 + beta2 + beta3 + beta4 + beta5)
transmission_probability_ZK_V<- (a1 + a2 + a3 + a4 + a5)
infectious_period_ZK_V<- ifh
R0V= contact_rate_ZK_V * transmission_probability_ZK_V * infectious_period_ZK_V
c("RO=", R0V)

  # DISCUSION DEL R0 #
# El R0 resultante es de 0.00487453780726777"
# Un R0 estima la velocidad con la que una enfermedad se esparce en una poblacion a traves del tiempo
# Es decir, es el numero de casos en promedio que surgen de un individuo.
# Un R0 mayor a 1 indica que una persona es capaz de infectar a mas de un individuo.
# Sin embargo, en este caso el R0 es menor a 1, por lo que el punto de equilibrio del sistema es free-disease, es decir, la enfermedad no es endemica.
# Se puede decir que la dinamica del zika en Veracruz no es tan notable, pero si mucho mayor que la de Queretaro.

