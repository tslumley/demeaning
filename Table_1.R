set.seed(2025-9-2)
r<-replicate(100000, {
	x<-rnorm(100)
	y<-rbinom(100,1,.5)
	coef(glm(y~x,family=binomial()))[2]
})

mean(r)
median(r)
sd(r)
mad(r)

