
### MODELO PARA MEXICO (PAIS) ###

library(deSolve)

# TIEMPO MX
tiempo_mx<- seq(1, 100 , by = 1)

# CONDICIONES INICIALES MX
iniciales_mx<-c( 
 
  #Condiciones para alto riesgo
  S_H = 60000000, # 100 millones
  E_H = 9000000, # 16 millones
  I_H = 5500000,
  R_H = 1000000,
  
  #Condiciones para vectores
  S_V = 3758900,
  E_V = 2008900,
  I_V = 4578600,
  
  #Condiciones para bajo riesgo
  S_L = 40000000,
  E_L = 7000000, 
  I_L = 4000000,  
  R_L = 200000)

I_H = 5500000
I_V = 4578600
I_L = 4000000

# En este caso, asi como en los otros, las condiciones iniciales varian, aqui 
# mucho mas porque la poblacion de todo el pais es mucho mayor, los numero
# fueron elegidos al azar, en este caso tomando en cuenta la poblacion total 
# haciendo que la poblacion de humanos coincidiera

# PARAMETROS (mx)
pars_mx <- c(
  Lv <- 8.5 + 35, #Esperanza de vida de mosquitos (dias)
  Lh <- 75*365, # Esperanza de vida de humanos (dias) #### MODIFICAR POR EDO ####
  iph <- 8.5, # Periodo de incubacion en humanos (dias)
  ipv <- 8.5, # Periodo de incubacion en mosquito (dias)
  ifh <- 4.5, # Periodo infeccioso en humanos (dias)
  muh <- 0.0016, # Mortalidad en humanos 
  muv <- 0.0064, # Mortalidad en vectores 
  alfa_H <- 1/iph, # Tasa de incubacion en humanos 
  alfa_V <- 1/ipv, # Tasa de incubacion en mosquitos
  gamma <- 1/ifh, # Tasa de recuperacion en humanos 
  n_H <- 0.0545, # Tasa de natalidad humanos ##### MODIFICAR POR EDO ####
  n_V <- 0.099, # Tasa de natalidad mosquitos 
  tetha <- 0.005, # Tasa de letalidad asociada a zika 
  q <- 0.25, # Proporcion de la poblacion que usa condon
  p_H <- 0.867, #   Proporcion de la poblacion que nace libre de zika 
  f <- 0.47, # Proporcion de nacidos que se dirigen al grupo de alto riesgo #### ESTIMADO ####
  b <- 0.45, #Tasa de picadura de mosquito
  a1 <- 0.044, # Probabilidad de transmision, picadura de mosquito infectado a humano susceptible de bajo riesgo (LV)
  a2 <- 0.047, # Probabilidad de transmision, picadura de mosquito susceptible a humano infectado de bajo riesgo (VL)
  a3 <- 0.033, # Probabilidad de transmision por contacto sexual entre humanos susceptibles e infectados (S-I humanos)
  a4 <- 0.0616, # Probabilidad de transmision, mosquito infectado a humano susceptible de alto riesgo (HV)
  a5 <- 0.0658, # Probabilidad de transmision, mosquito susceptible a humano infectado de alto riesgo (VH)
  c <- 0.0055, # contacto sexual entre humano susceptible y humano infectado 
  Nh <- 126700000, # Poblacion total de humanos  #### MODIFICAR POR EDO ####
  beta1 <- (b*a1*I_V)/Nh, # Tasa de contagio mosquito infectado a persona susceptible de bajo riesgo
  beta2 <- ((c*a3*(I_H + I_L))/(Nh)), # Tasa de contagio de persona infectada a persona susceptible  
  beta3 <- ((b*a2*(I_L))/(Nh)), # Tasa de contagio persona infectada de bajo riesgo a mosquito susceptible
  beta4 <- ((b*a4*I_V)/(Nh)), # Tasa de contagio mosquito infectado a persona susceptible de alto riesgo 
  beta5 <- ((b*a5*(I_H))/(Nh)) # Tasa de contagio persona infectada de alto riesgo a mosquito susceptible
)

# Como en las simulaciones anteriores respetamos los mismo valores para las variables
# modificando unicamente aquellas que corresponden, en este caso, al pais. 
# ya que establecimos los parametros es necesario realizar la simulacion


#SIMULACION

mx <- ode(iniciales_mx, tiempo_mx, ZIKA_M, parms = pars_mx)

mx_zikam <- matplot(mx[ , 1], mx[ , 2:12], type="l", xlab = "Tiempo", ylab = "Población", 
                    main= "Gráfica ZIKA para México", lwd = 2) 
legend("topright", c("S alto riesgo", "E alto riesgo ", "I alto riesgo ", "R alto riesgo", 
                     "S vectores", "E vectores", "I vectores", 
                     "S bajo riesgo", "E bajo riesgo", "I bajo riesgo", "R bajo riesgo"), col = 1:11, lty=1:5, cex=0.5)
# En esta primera grafica obtenemos los valores de todas las variables dentro del modelo 
# se puede apreciar poco el curso de cada una de las lineas. 
# Podemos observar que la poblacion de susceptibles de alto riesgo decrece aunque no lo hace 
# en picada, es progresivo el cambio, mientras estos bajan, los individuos en incubacion aumentan 
# hasta que comienzan a hacerlo los individuos infectados. 
# Resulta algo complicado ver todas las interacciones en la grafica por lo que vamos a haCer una grafica
# donde se aprecien las variaciones de los individuos infectados de cada grupo
# asi como interacciones entre humanos y vectores en graficas distintas


# Grafica alto riesgo vs vectores 

mx_HV <- matplot(mx[ , 1], mx[ , 2:8], type="l", xlab = "tiempo", ylab = "Población", 
                 main = "Gráfica ZIKA México (HV)", lwd = 2) 
legend("topright", c("S alto riesgo", "E alto riesgo ", "I alto riesgo ", "R alto riesgo", 
                     "S vectores", "E vectores", "I vectores"), col =1:7, lty=1:7, cex=0.5)

# Esta grafica ejemplifica la interaccion entre individuos humanos e alto riesgo e individuos de la clase
# vector, esto para observar como se lleva a cabo la interaccion entre estos grupos 
# NOTA: La poblacion de vectores y las fluctuaciones en este grupo tambien dependen de su interaccion
# con los individuos de tipo bajo riesgo por lo que no podriamos reducir esta grafica solamente a 
# alto riesgo vs vectores

# DISCUSION DE LA GRAFICA: 
# Aqui podemos observar las interacciones entre individuos de alto riesgo y los vectores
# se aprecia que los individuos susceptibles incian con un buen numero en el compartimento 
# pero comienza su descenso gradual hasta llegar a su equilibrio mas o menos en el agno 
# 30, mientras estos disminuyen aumentan los individuos que experimentan la incubacion del patogeno 
# y conforme este grupo desciende en numero comienza a aumentar el tamagno de la poblacion de los infectados
# aunque es importante establecer que este cambio no sucede tan marcado, la curva de los infectados 
# es muy pequegnita en comparacion al resto en la imagen 
# A resaltar - que los individuos rapidamente obtienen inmunidad hacia el patogeno 


# Grafica bajo riesgo vs vectores 
mx_LV<- matplot(mx[ , 1], mx[ , 6:12], type="l", xlab = "tiempo", ylab = "Población", 
                main = "Gráfica ZIKA México (LV)", lwd = 2) 
legend("topright", c("S vectores", "E vectores", "I vectores", 
                     "S bajo riesgo", "E bajo riesgo ", "I bajo riesgo ", "R bajo riesgo"), col =1:7, 
       lty=1:5, cex=0.5)

# Esta grafica ejemplifica la relacion entre individuos humanos de bajo riesgo e individuos de clase
# vector

# DISCUSION DE LA GRAFICA: 
# En esta grafica se observa un comportamiento similar al de la grafica pasada, podemos observar como 
# la poblacion de individuos en bajo riesgo susceptibles desciende, en esta, a comparacion de la anterior
# pueden observarse mucho mejor las curvas e interaccion entre las poblaciones, ello puede deberse
# a que la poblacion en bajo riesgo dentro del modelo es menor por lo que se aprecia mejoria 
# en la dimension de la grafica
# aqui tambien los individuos experimntan una recuperacion rapida, alcanzando el equilirbio aprox en el 
# agno 70. 


# Grafica infectados (L-H-V)
mxINF <- matplot(mx[ , 1], mx[ , c(4, 8, 11)], type="l", xlab = "Tiempo", ylab = "Población", 
                 main= "Gráfica ZIKA México (infectados)", lwd = 1) 
legend("topright", c( "I alto riesgo ", "I vectores","I bajo riesgo"), col = 1:3, lty=1:5, cex=0.5)

# Esta grafica nos muestra las variaciones en los niveles de individuos infectados
# pertenecientes a cada una de las clasificaciones dentro del modelo 

# DISCUSION DE LA GRAFICA: 
# Esta grafica solamente nos da la imagen de los individuos infectados por cada uno de los compartimentos 
# teniendo las distintas variaciones en una sola imagen
# podemos observar que los individuos infectados de alto riesgo son los que presentan mayor numero
# en comparacion a los de bajo riesgo lo que tiene sentido porque son estos precisamente los que se encuentran
# mas expuestos al patogeno y por tanto presentan masprobabilidad de ser infectados posterior a 
# una picadura 
# por otra parte es necesario resaltar que la poblacion de vectores no experimenta una curva 
# parecida a la de sus semejantes por lo que estimamos que esta poblacion no varia tanto en el tiempo
# ellopuede deberse y muy probablemente se deba a la beta asociada a la infeccion de mosquitos 
# susceptibles despues de picar a un humano infectado

# CALCULO DE R0 
contact_rate_ZK_MX<- (beta1 + beta2 + beta3 + beta4 + beta5)
transmission_probability_ZK_MX <- (a1 + a2 + a3 + a4 + a5)
infectious_period_ZK_MX <- ifh
R0X = contact_rate_ZK_MX * transmission_probability_ZK_MX * infectious_period_ZK_MX
R0X <- c(paste0("RO = ", R0X)) 
R0X

# Igual que en el ejercicio anterior y en los siguientes calculamos el R0 tomando en cuenta 
# las tasas de contagio, este proceso es identico para todos los casos de este proyecto 
# el cambio en las tasas en funcion de la poblacion del lugar    
