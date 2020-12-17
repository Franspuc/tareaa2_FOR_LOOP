#1# Se junto la base de datos en excel y se le agrego la variable tamanio 

#2##TIPO DE DATOS DE LAS VARIABLES##


str(basepaises)

colnames(basepaises)

 

##3##

for (i in unique(basepaises$pais)){
  if(i %in% c('peru','chile')){
    largo=length(basepaises$pais[which(basepaises$pais==i)])
    print(paste('El largo de',i,'es',largo))
  }}

##4##

maximo=basepaises$ingresos[1]
for (i in 2:length(basepaises$ingresos)){
  if(basepaises$ingresos[i]>=maximo){
    maximo=basepaises$ingresos[i]
  }
  else if(basepaises$ingresos[i]<maximo){
    maximo=maximo
  }
}
print(paste('El Ingreso maximo es:',maximo))

##5##

basepaises$nueva_variable=0
for (i in 1:length(basepaises$nueva_variable)){
  if(basepaises$pais[i]=='chile'){basepaises$nueva_variable[i]=basepaises$tasa_interes[i]*0.1}
  else if(basepaises$pais[i]=='peru'){basepaises$nueva_variable[i]=basepaises$tasa_interes[i]*0.3}
  else if(basepaises$pais[i]=='colombia'){basepaises$nueva_variable[i]=basepaises$tasa_interes[i]/10}
}

   #para corroborar la nueva base
    
View(basepaises)


##6##

for(i in basepaises$exportaciones){
  if(round(i,1)>2.1){
    basepaises$exportaciones[which(basepaises$exportaciones==i)]=1
  }
  else if(round(i,1)==2.1){
    basepaises$exportaciones[which(basepaises$exportaciones==i)]=3
  }
  else if(round(i,1)<2.1){
    basepaises$exportaciones[which(basepaises$exportaciones==i)]=2
  }
}

##7 opcional##

plot(basepaises$fecha,basepaises$ingresos)

install.packages("tidyverse")
install.packages("data.table")

library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)


basepaises%>%filter(ingresos>12)


basepaises %>% ggplot(aes(x=fecha,y=ingresos))+ geom_point()
