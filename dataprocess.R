library(ggplot2)
data("mtcars")
head(mtcars)

nt_mtcars<-subset(mtcars,select = c(1:7))
pairs(nt_mtcars, panel = panel.smooth, col =  mtcars$am +2)


figure1<-ggplot(mtcars,aes(x=factor(am, labels = c("Auto","Manual")),y=mpg,fill=factor(am, labels = c("Auto","Manual"))))
figure1<-figure1+geom_boxplot()
figure1<-figure1+scale_fill_discrete(name = "Transmission Type")
figure1<-figure1 + theme_bw() + xlab("Transmission Type") + ylab("Miles Per Gallon")
figure1

fit1<-lm(mpg ~ factor(am), data = mtcars)
summary(fit1)

fit4<-lm(mpg ~ factor(am) + cyl + hp + wt, data = mtcars)
summary(fit4)

fit6<-lm(mpg ~ factor(am) + cyl + hp + wt + disp +qsec, data = mtcars)
summary(fit6)

anova(fit1,fit4,fit6)

par(mfrow = c(2, 2))
plot(fit4)