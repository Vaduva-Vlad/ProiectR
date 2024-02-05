library(MASS)
library(ggplot2)
View(Rabbit)

control<-subset(Rabbit,Treatment=="Control")
mdl<-subset(Rabbit,Treatment=="MDL")
control
mdl

# Grafice
line_graph<-function(rabbit,treatment){
  r<-subset(Rabbit,Treatment==treatment & Animal==rabbit)
  r
  Doza<-r$Dose
  Schimbari_tensiune<-r$BPchange
  data<-data.frame(Doza,Schimbari_tensiune)
  ggplot(data, aes(x=Doza, y=Schimbari_tensiune)) +
    geom_line()+
    geom_point()
}

# Crestere constanta pana la penultima doza. La ultima doza incepe sa isi piarda efectul
line_graph("R1","Control")

# Crestere lenta la inceput, crestere brusca dupa ultima doza, datorata depasirii pragului medicamentului MDL
line_graph("R1","MDL")


boxplot(control$BPchange)
mediana<-median(control$BPchange)
mediana
quantile(control$BPchange,prob=c(.25,.5,.75))
mean(control$BPchange)

boxplot(mdl$BPchange)
mediana<-median(mdl$BPchange)
mediana
quantile(mdl$BPchange,prob=c(.25,.5,.75))
mean(mdl$BPchange)

hist(control$BPchange)
hist(mdl$BPchange)
hist(Rabbit$Dose)

#Test t pentru esantioane dependente
#H0: Schimbarile in tensiunea arteriala sunt egale in ambele faze de testare
#Ha: Schimbarile in tensiunea arteriala sunt diferite in fiecare faza de testare
test1<-t.test(Rabbit$BPchange[Rabbit$Treatment=="Control"],Rabbit$BPchange[Rabbit$Treatment=="MDL"],alternative="two.sided",conf.level=0.05,paired=TRUE)
if(test1$p.value<=0.95) print("Respingem ipoteza nula") else  print("Nu respingem ipoteza nula")

correlation=cor(Rabbit$BPchange,Rabbit$Dose)
correlation
model=lm(BPchange ~ Dose,data=Rabbit)
#Residual standard error = 6.423
#Mediana valorilor reziduu = -1.534 => Modelul tinde si sa subestimeze, dar si sa supraestimeze.
model
summary(model)

plot(BPchange ~ Dose, data=Rabbit)
abline(model, col = "red")
