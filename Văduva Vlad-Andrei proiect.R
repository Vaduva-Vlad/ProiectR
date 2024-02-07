library(MASS)
library(ggplot2)
View(Rabbit)

#Prima faza: Doza din ce in ce mai mare de fenilbiguanida + substanta salina (de control)
#A doua faza: Doza din ce in ce mai mare de fenilbiguanida + MDL (Medicament de reglare a tensiunii arteriale)
#Se observa diferentele de tensiune arteriala in ambele faze

#Impartim setul de date in 2 parti. Una va contine rezultatul primei faze, iar cealalta rezultatul celei de a doua faze.
control<-subset(Rabbit,Treatment=="Control")
mdl<-subset(Rabbit,Treatment=="MDL")
control
mdl

# Grafice
line_graph<-function(rabbit,treatment){
  r<-subset(Rabbit,Treatment==treatment & Animal==rabbit)
  r
  Dose<-r$Dose
  BPchange<-r$BPchange
  data<-data.frame(Dose,BPchange)
  ggplot(data, aes(x=Dose, y=BPchange)) +
    geom_line()+
    geom_point()
}

# Crestere constanta pana la penultima doza. La ultima doza incepe sa isi piarda efectul
line_graph("R1","Control")
line_graph("R2","Control")
line_graph("R3","Control")
line_graph("R4","Control")
line_graph("R5","Control")

# Crestere lenta la inceput, crestere brusca dupa ultima doza, datorata depasirii pragului medicamentului MDL
line_graph("R1","MDL")
line_graph("R2","MDL")
line_graph("R3","MDL")
line_graph("R4","MDL")
line_graph("R5","MDL")


median(control$BPchange)
quantile(control$BPchange,prob=c(.25,.5,.75))
boxplot(control$BPchange)
mean(control$BPchange)

median(mdl$BPchange)
quantile(mdl$BPchange,prob=c(.25,.5,.75))
boxplot(mdl$BPchange)
mean(mdl$BPchange)

hist(Rabbit$Dose,breaks=15)

#Test t pentru esantioane dependente
#H0: BPchange este mai mare in prima faza
#Ha: BPchange este mai mic in prima faza
test1<-t.test(Rabbit$BPchange[Rabbit$Treatment=="Control"],Rabbit$BPchange[Rabbit$Treatment=="MDL"],alternative="less",conf.level=0.05,paired=TRUE)
if(test1$p.value<=0.95) print("Respingem ipoteza nula") else  print("Nu respingem ipoteza nula")

correlation=cor(Rabbit$BPchange,Rabbit$Dose)
#0.831961
correlation

model=lm(BPchange ~ Dose, data=Rabbit)
#Mediana valorilor reziduu = -1.534 => Modelul tinde si sa subestimeze, dar si sa supraestimeze.
#Residual standard error (Deviatia standard a reziduu-rilor) = 6.423
model
#Formula rezultata: y=2.03587+x*0.13992
summary(model)
#F-statistic: 130.4 on 1 and 58 DF,  p-value: < 2.2e-16 => respingem ipoteza nula =>exista o relatie intre cele 2 variabile

plot(BPchange ~ Dose, data=Rabbit)
abline(model, col = "red")

#MSE - eroarea medie patratica = 39.88163
mean(model$residuals^2)

v=c(10,25,75,115,150,190)
new_dose=rep(v, times = 10)
model=lm(BPchange ~ new_dose, data=Rabbit)
model
#Formula rezultata: y=-2.77908+x*0.14865
summary(model)