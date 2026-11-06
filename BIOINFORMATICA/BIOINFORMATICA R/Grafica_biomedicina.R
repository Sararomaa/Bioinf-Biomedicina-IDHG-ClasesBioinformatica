# Instalar librerías sino lo están ya
if(!require("ggplot2")) install.packages("ggplot2",dependencies=TRUE)
if(!require("dplyr")) install.packages("dplyr",dependencies=TRUE)

# Cargar librerías
library(ggplot2)
library(dplyr)

# Boxplot glucosa
n <- 50
glucosa_sanos <- rnorm(n,mean=95,sd=10)
glucosa_diabeticos <- rnorm(n,mean=160,sd=20)

df_glucosa <- data.frame(glucosa=c(glucosa_sanos,glucosa_diabeticos),grupo = factor(rep(c("Sanos","Diabeticos"),each=n)))

ggplot(df_glucosa,aes(x=grupo,y=glucosa,fill=grupo))+
geom_boxplot(outlier.color = "red",alpha=0.7) +
scale_fill_manual(values=c("#66c2a5","#fc8d62"))+
labs(title="Comparación de glucosa en sangre",
y="Glucosa(mg(dL)",x="")+
theme_minimal(base_size=14)+
theme(plot.title=element_text(hjust=0.5))

# Dispersión presión vs edad
set.seed(123)
n<-100
edad <- runif(n,20,80)
genero <- sample(c("Femenino","Masculino"),n,replace=TRUE)
presion <- 0.6*edad+rnorm(n,0,10)

df_presion <- data.frame(edad=edad,presion=presion,genero=genero)

ggplot(df_presion,aes(x=edad,y=presion,color=genero))+
geom_point(size=3,alpha=0.7)+
geom_smooth(method="lm",se=FALSE,linetype="dashed")+
scale_color_manual(values=c("Femenino" = "#8da0cb","Masculino"="#e78ac3"))+
labs(title="Relación entre presión arterial y edad",
x="Edad(años)",y="Presión arterial (mmHg)")+
theme_minimal(base_size=14)+
theme(plot.title=element_text(hjust=0.5))