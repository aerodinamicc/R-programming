#lm() takes formula as argument: y ~ x will be translated to y=a0+a1x
model <- lm(Petal.Width ~ Petal.Length, data = iris)

#ACTION 1
setwd("E:/R/EnvMon/")
glb <- read_csv("GLB.Ts+dSST.csv", skip = 1,
                na = "***")
ggplot(glb, aes(x = Jan, y = Apr)) +
  geom_point() + 
  geom_smooth(method = "lm")

lm(Jan ~ Apr, data = glb)
