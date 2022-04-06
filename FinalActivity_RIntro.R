attach(Maquinas)
View(Maquinas)
names(Maquinas)

attach(Cafe)
View(Cafe)
names(Cafe)
#1 - #########################################################################################################
#A - De forma manual realice la base de datos en R.
Nombre =c("Juan","Manuel","José","Luis","Ana","Gabriel","Lina","Luz")
Salario = c(120,180,500,130,156,135,135,230)
Genero = c("M","M","M","M","F","M","F","F")
Empleados <- data.frame(Nombre,Salario,Genero)
Empleados 

#B - Mediante comandos en R, obtenga el nombre de la persona con el salario más alto.
for (i in 1:nrow(Empleados)) if(Salario[i] == max(Salario)) { print(Empleados[i,])}

#C - Usando comandos de R, muestre los resultados de Lina.
Empleados[Empleados$Nombre == "Lina", ]

#D - Usando comandos de R muestre, los resultados para las personas que son de género Femenino.
Empleados[Empleados$Genero == "F", ]

#E - Usando comandos de R muestre los resultados de las personas que tienen un salario superior a 200.
for (i in 1:nrow(Empleados)) if(Salario[i] > 200) { print(Empleados[i,])}
#2 - #######################################################################################################
#Realice una función que obtenga el valor de la factorial de un número.
  fact <- function(numero){
    num <- 1
    if (numero < 0) {
      print("Imposible calcular el factorial. Ingrese número > 0")
    }else if (numero == 0) {
      print("El Factorial es 1")
    }else{
      for (i in 1:numero) {
        num=  num * i
        print(num)
      }
    }     
  }
  fact(5)
#3 - ###########################################################################################################
#4 - ###########################################################################################################
# MEDIA ########################################################################################################

Media_M1 <- Maquinas[Maquinas$Maquina == "Uno", ]
Media_M1
M1 <- Media_M1$Resistencia
M1
mean(M1)

Media_M2 <- Maquinas[Maquinas$Maquina == "Dos", ]
Media_M2
M2 <- Media_M2$Resistencia
M2
mean(M2)

Media_M3 <- Maquinas[Maquinas$Maquina == "Tres", ]
Media_M3
M3 <- Media_M3$Resistencia
M3
mean(M3)

Media_M4 <- Maquinas[Maquinas$Maquina == "Cuatro", ]
Media_M4
M4 <- Media_M4$Resistencia
M4
mean(M4)

Media_M5 <- Maquinas[Maquinas$Maquina == "Cinco", ]
Media_M5
M5 <- Media_M5$Resistencia
M5
mean(M5)

Media_M6 <- Maquinas[Maquinas$Maquina == "Seis", ]
Media_M6
M6 <- Media_M6$Resistencia
M6
mean(M6)

# MEDIANA ######################################################################################################

Mediana_M1 <- Maquinas[Maquinas$Maquina == "Uno", ]
Mediana_M1
mM1 <- Mediana_M1$Resistencia
mM1
Med_M1<-median(mM1)
print(Med_M1)

Mediana_M2 <- Maquinas[Maquinas$Maquina == "Dos", ]
Mediana_M2
mM2 <- Mediana_M2$Resistencia
mM2
Med_M2<-median(mM2)
print(Med_M2)

Mediana_M3 <- Maquinas[Maquinas$Maquina == "Tres", ]
Mediana_M3
mM3 <- Mediana_M3$Resistencia
mM3
Med_M3<-median(mM3)
print(Med_M3)

Mediana_M4 <- Maquinas[Maquinas$Maquina == "Cuatro", ]
Mediana_M4
mM4 <- Mediana_M4$Resistencia
mM4
Med_M4<-median(mM4)
print(Med_M4)

Mediana_M5 <- Maquinas[Maquinas$Maquina == "Cinco", ]
Mediana_M5
mM5 <- Mediana_M5$Resistencia
mM5
Med_M5<-median(mM5)
print(Med_M5)

Mediana_M6 <- Maquinas[Maquinas$Maquina == "Seis", ]
Mediana_M6
mM6 <- Mediana_M6$Resistencia
mM6
Med_M6<-median(mM6)
print(Med_M6)

# DESVIACIÓN ESTÁNDAR ##########################################################################################

SD1 <- Maquinas[Maquinas$Maquina == "Uno", ]
SD1
sd1 <- SD1$Resistencia
sd1

sd(sd1)

SD2 <- Maquinas[Maquinas$Maquina == "Dos", ]
SD2
sd2 <- SD2$Resistencia
sd2

sd(sd2)

SD3 <- Maquinas[Maquinas$Maquina == "Tres", ]
SD3
sd3 <- SD3$Resistencia
sd3

sd(sd3)

SD4 <- Maquinas[Maquinas$Maquina == "Cuatro", ]
SD4
sd4 <- SD4$Resistencia
sd4

sd(sd4)

SD5 <- Maquinas[Maquinas$Maquina == "Cinco", ]
SD5
sd5 <- SD5$Resistencia
sd5

sd(sd5)

SD6 <- Maquinas[Maquinas$Maquina == "Seis", ]
SD6
sd6 <- SD6$Resistencia
sd6

sd(sd6)
#5 - ###########################################################################################################
#Obtenga una función que indique el tipo de sobrepeso 
#de un paciente de acuerdo con su índice de masa corporal.

install.packages("svDialogs")
library(svDialogs)

IMC=function(peso,altura){
  
  peso <- dlgInput(message="Ingrese peso: ")$res
  p <- as.numeric(peso)
  
  altura <- dlgInput(message="Ingrese número: ")$res
  a <- as.numeric(altura)
  
  z=p/(a^2)
  if(z >= 18.5 & z <=24.9 ){
    print("Normopeso")
  }else if(z>=25 & z<=26.9){
    print("Sobrepeso grado 1")
  }else if(z>=27 & z<=29.9){
    print("Sobrepeso grado 2")
  }else if(z>=30 & z<=34.9 ){
    print("Obesidad tipo 1")
  }else if (z>=35 & z<=39.9) {
    print("Obesidad tipo 2")
  }else if(z>=40 & z<=49.9){
    print("Obesidad tipo 3 (mórbida)")
  }else if(z>50){
    print("Obesidad tipo 4 (extrema)")
    }else print("Bajo Peso")
}
IMC()
