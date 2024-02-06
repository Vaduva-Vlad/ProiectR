library(MASS)
library(ggplot2)
View(Rabbit)

#Prima faza: Doza din ce in ce mai mare de fenilbiguanida + substanta salina (de control)
#A doua faza: Doza din ce in ce mai mare de fenilbiguanida + MDL (Medicament de reglare a tensiunii arteriale)
#Se observa diferentele de tensiune arteriala in ambele faze

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

# Crestere lenta la inceput, crestere brusca dupa ultima doza, datorata depasirii pragului medicamentului MDL
line_graph("R1","MDL")


median(control$BPchange)
quantile(control$BPchange,prob=c(.25,.5,.75))
boxplot(control$BPchange)
mean(control$BPchange)

median(mdl$BPchange)
quantile(mdl$BPchange,prob=c(.25,.5,.75))
boxplot(mdl$BPchange)
mean(mdl$BPchange)

#Test t pentru esantioane dependente
#H0: BPchange este mai mare in prima faza
#Ha: BPchange este mai mic in prima faza
test1<-t.test(Rabbit$BPchange[Rabbit$Treatment=="Control"],Rabbit$BPchange[Rabbit$Treatment=="MDL"],alternative="less",conf.level=0.05,paired=TRUE)
if(test1$p.value<=0.95) print("Respingem ipoteza nula") else  print("Nu respingem ipoteza nula")

correlation=cor(Rabbit$BPchange,Rabbit$Dose)
correlation

model=lm(BPchange ~ Dose, data=Rabbit)
#Residual standard error = 6.423
#Mediana valorilor reziduu = -1.534 => Modelul tinde si sa subestimeze, dar si sa supraestimeze.
model
summary(model)

plot(BPchange ~ Dose, data=Rabbit)
abline(model, col = "red")

#MSE - eroarea medie patratica = 39.88163
mean(model$residuals^2)

v=c(10,25,75,115,150,190)
new_dose=rep(v, times = 10)
model=lm(BPchange ~ new_dose, data=Rabbit)
model