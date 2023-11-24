
### MODELO PARA QUERETARO ###

library(deSolve)

times_qro<- seq(1, 100 , by = 1)


# Condiciones iniciales
cond_ini_qro<-c( 
  
  #Condiciones para alto riesgo
  S_H = 700000,
  E_H = 12000,
  I_H = 10000,
  R_H = 6000,
  
  #Condiciones para vectores
  S_V = 9000,
  E_V = 400,
  I_V = 3000,
  
  #Condiciones para bajo riesgo
  S_L = 300000,
  E_L = 8000, 
  I_L = 10000,  
  R_L = 4000)

I_H = 10000
I_V = 3000
I_L = 10000

# Las condiciones iniciales varian dependiendo del lugar, en este caso 
# se contemplo a una poblacion total de 1,050,000 habitantes.
# Los numeros fueron elegidos al azar, manteniendose dentro de la poblacion total real.


# PARAMETROS
parametro_qro<- c(
  Lv <- 8.5 + 35, #Esperanza de vida de mosquitos (dias)
  Lh <- 75.9* 365, # Esperanza de vida de humanos (dias) #### DIFERENTE PARA CADA LUGAR ####
  iph <- 8.5, # Periodo de incubacion en humanos (dias)
  ipv <- 8.5, # Periodo de incubacion en mosquito (dias)
  ifh <- 4.5, # Periodo infeccioso en humanos (dias)
  muh <- 0.0016, # Mortalidad en humanos 
  muv <- 0.0064, # Mortalidad en vectores 
  alfa_H <- 1/iph, # Tasa de incubacion en humanos 
  alfa_V <- 1/ipv, # Tasa de incubacion en mosquitos
  gamma <- 1/ifh, # Tasa de recuperacion en humanos 
  n_H <- 0.0463, # Tasa de natalidad humanos ##### DIFERENTE PARA CADA LUGAR ####
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
  Nh <- 1050000, # Poblacion total de humanos  #### DIFERENTE PARA CADA LUGAR ####
  beta1 <- (b*a1*I_V)/Nh, # Tasa de contagio mosquito infectado a persona susceptible de bajo riesgo
  beta2 <- ((c*a3*(I_H + I_L))/(Nh)), # Tasa de contagio de persona infectada a persona susceptible  
  beta3 <- ((b*a2*(I_L))/(Nh)), # Tasa de contagio persona infectada de bajo riesgo a mosquito susceptible
  beta4 <- ((b*a4*I_V)/(Nh)), # Tasa de contagio mosquito infectado a persona susceptible de alto riesgo 
  beta5 <- ((b*a5*(I_H))/(Nh)) # Tasa de contagio persona infectada de alto riesgo a mosquito susceptible
)

# Todas las simulaciones de los lugares contemplan los mismos parametros para casi todo.
# Unicamente se modificaron aquellos que son propios del lugar (Qro en este caso). 
# Con los parametros establecidos, se procede a la simulacion:


# SIMULACION

out <- ode(y=cond_ini_qro,
           times=times_qro,
           fun = ZIKA_M, 
           parms  = parametro_qro)

plot(out,col="blue")
par(mfrow=c(1,1))

graficaTG <- matplot(out[ , 1], out[ , 2:12], type="l", xlab = "tiempo", ylab = "Población", 
                     main= "Gráfica ZIKA Querétaro", lwd = 2) 
legend("topright", c("S alto riesgo", "E alto riesgo ", "I alto riesgo ", "R alto riesgo", 
                     "S vectores", "E vectores", "I vectores", 
                     "S bajo riesgo", "E bajo riesgo", "I bajo riesgo", "R bajo riesgo"), col =1:5, lty=1:5, cex=0.5)

# En esta primera grafica obtenemos los valores de todas las variables dentro del modelo general, donde
# escomplicado observar el curso individual de las variables. 
# Por ello, realizamos graficas que acoten la informacion y
# donde se aprecien las variaciones de los individuos infectados de cada grupo
# asi como interacciones entre humanos y vectores en graficas distintas



# Grafica alto riesgo vs vectores 

graficaHV_qro<- matplot(out[ , 1], out[ , 2:8], type="l", xlab = "tiempo", ylab = "Población", 
                        main = "Gráfica ZIKA Querétaro (HV)", lwd = 2) 
legend("topright", c("S alto riesgo", "E alto riesgo ", "I alto riesgo ", "R alto riesgo", 
                     "S vectores", "E vectores", "I vectores"), col =1:7, 
       lty=1:5, cex=0.5)

# Esta grafica ejemplifica la interaccion entre individuos humanos e alto riesgo e individuos de la clase
# vector, esto para observar como se lleva a cabo la interaccion entre estos grupos 
# NOTA: La poblacion de vectores y las fluctuaciones en este grupo tambien dependen de su interaccion
# con los individuos de tipo bajo riesgo por lo que no podriamos reducir esta grafica solamente a 
# alto riesgo vs vectores


# DISCUSION DE LA GRAFICA: 
#La grafica muestra que los recuperados de alto riesgo aumentan rapidamente al inicio de la enfermedad y se mantienen altos al pasar los dias.
#Es decir, los individuos que no usaron repelente, en promedio, se recuperan rapidamente.
#La cantidad de susceptibles disminuye rapidamente al inicio y se mantiene baja a lo largo del tiempo (al menos por los 100 dias que se muestran).
#Los infectados suben rapidamente, pero no mas que los expuestos, alcanzando su pico en aproximadamente 10 dias y disminuyendo a partir de ello, manteniendose bajo
#La representacion de los vectores en general (susceptibles, infectados y expuestos) es baja en el tiempo, lo cual se explica con la poca cantidad 
#de mosquitos que hay en comparacion con la de personas.



# Grafica bajo riesgo vs vectores 
graficaLV_qro<- matplot(out[ , 1], out[ , 6:12], type="l", xlab = "tiempo", ylab = "Población", 
                        main = "Gráfica ZIKA Querétaro (LV)", lwd = 2) 
legend("topright", c("S vectores", "E vectores", "I vectores", 
                     "S bajo riesgo", "E bajo riesgo ", "I bajo riesgo ", "R bajo riesgo"), col =1:7, 
       lty=1:5, cex=0.5)

# Esta grafica ejemplifica la relacion entre individuos humanos de bajo riesgo e individuos de clase
# vector

# DISCUSION DE LA GRAFICA: 
#Hay una mayor cantidad de vectores susceptibles que de individuos expuestos que no usaron repelente (bajo riesgo).
#Los infectados de bajo riesgo aumentan al inicio del tiempo y alcanzan su pico aproximadamente despues de 10 dias, disminuyendo desde ahi.
#La cantidad de vectores infectados y expuestos es baja por la cantidad tan baja que hay de mosquitos en general en comparacion con los humanos.
#De manera general, son los vectores susceptibles los que estan mas presentes y que se mantienen a lo largo del tiempo en comparacion con los individuos de bajo riesgo.



# Grafica infectados (L-H-V)
graficaINF_qro<- matplot(out[ , 1],out[ , c(4, 8, 11)], type="l", xlab = "Tiempo", ylab = "Población", 
                         main= "Gráfica ZIKA Querétaro (infectados)", lwd = 1)
legend("topright", c( "I alto riesgo ", "I vectores","I bajo riesgo"), col = 1:3, lty=1:5,
       cex=0.5)

# Esta grafica nos muestra las variaciones en los niveles de individuos infectados
# pertenecientes a cada una de las clasificaciones dentro del modelo 

# DISCUSION DE LA GRAFICA: 
#Existe una mayor cantidad de individuos de alto riesgo infectados que de cualquier otro tipo.
#El pico de casos es alcanzado despues de aproximadamente 10 dias para los infectados de alto y bajo riesgo.
#No se aprecian tantos vectores infectados por la cantidad de mosquitos que hay en relacion a la de humanos, que es mucho mayor.
#Existen mas infectados de bajo riesgo que vectores infectados, sin embargo hay aproximadamente mas del doble de infectados de alto riesgo.


# ESTIMACION DEL R0 
contact_rate_ZK<- (beta1 + beta2 + beta3 + beta4 + beta5)
transmission_probability_ZK<- (a1 + a2 + a3 + a4 + a5)
infectious_period_ZK<- ifh
R0= contact_rate_ZK * transmission_probability_ZK * infectious_period_ZK
R_0 <- c("RO=", R0)
R_0

# DISCUSION DEL R0
#El R0 resultante es de "0.000704412025714286"
#Un R0 estima la velocidad con la que una enfermedad se esparce en una poblacion a traves del tiempo
#Es decir, es el numero de casos en promedio que surgen de un individuo.
#Un R0 mayor a 1 indica que una persona es capaz de infectar a mas de un individuo.
#Sin embargo, en este caso el R0 es menor a 1, por lo que el punto de equilibrio del sistema es free-disease, es decir, la enfermedad no es endemica.
#Se puede decir que la dinamica del zika en Queretaro no es tan notable, con la aparicion de casos siendo baja.

