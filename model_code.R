set.seed(1)
x <- 1:150
y <- rbinom(length(x), size = 1, prob = 0.7)
y <- y * rgamma(length(x), shape = 0.4)
non_zero <- ifelse(y > 0, 1, 0)
d <- data.frame(x, y, non_zero)
head(d)
#   x       y non_zero
# 1 1 0.08736        1
# 2 2 0.28773        1
# 3 3 0.05731        1
# 4 4 0.00000        0
# 5 5 1.07186        1
# 6 6 0.00000        0
library(ggplot2)
p <- ggplot(d, aes(x, y, colour = as.factor(non_zero))) + geom_point()
print(p)

m1 <- glm(non_zero ~ 1, data = d, family = binomial(link = logit))
m2 <- glm(y ~ 1, data = subset(d, non_zero == 1), family = Gamma(link = log))
log(mean(plogis(coef(m1))))+log(mean(exp(coef(m2))))
exp(-1.244899)

coef(m2)
