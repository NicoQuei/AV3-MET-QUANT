########################################
# QUESTÃO 2 – Poisson
########################################

x = rpois(n, lambda = 4)

plot(table(x)/n, type="h", main="Distribuição Poisson", xlab="X", ylab="P(X)")

plot(ecdf(x), main="Distribuição Acumulada - Poisson", xlab="X", ylab="F(X)")


########################################
# QUESTÃO 3 – Normal N(65,5)
########################################

x = rnorm(n, 65, 5)

hist(x, freq = FALSE, main="Histograma N(65,5)")
plot(function(t) dnorm(t, 65, 5), 40, 90, add = TRUE)


#######################################
# QUESTÃO 4 – Binomial p=0.85 n=20
########################################

p = 0.85
n = 20

# P(X >= 16)
prob_satisfatorio = pbinom(15, n, p, lower.tail = FALSE)
prob_satisfatorio


########################################
# QUESTÃO 5 – Poisson λ=2
########################################

lambda = 2

# a) P(X >= 2)
P_a = ppois(1, lambda, lower.tail = FALSE)

# b) P(X >= 5)
P_b = ppois(4, lambda, lower.tail = FALSE)

P_a; P_b


########################################
# QUESTÃO 6 – Binomial p=0.9 n=10
########################################

p = 0.9
n = 10

# a) P(X >= 8)
P_a = pbinom(7, n, p, lower.tail = FALSE)

# b) errar > 2 = acertar < 8
P_b = pbinom(7, n, p, lower.tail = TRUE)

# c) média e dp
media = n * p
dp = sqrt(n*p*(1-p))

# d) P(X >= 9)
P_d = pbinom(8, n, p, lower.tail = FALSE)

P_a; P_b; media; dp; P_d


########################################
# QUESTÃO 7 – Normal μ=2060 σ=150
########################################

mu = 2060
dp = 150

# a) P(X > 1900)
P_a = pnorm(1900, mu, dp, lower.tail = FALSE)

# b) 1800 < X < 1900
P_b = pnorm(1900, mu, dp) - pnorm(1800, mu, dp)

# c) valor 2.5% inferior
P_c = qnorm(0.025, mu, dp)

# d) no máximo 1 lâmpada >1800 em 4
p_individual = pnorm(1800, mu, dp, lower.tail = FALSE)
P_d = pbinom(1, 4, p_individual)

P_a; P_b; P_c; P_d


########################################
# QUESTÃO 8 – Normal μ=2.5 σ=0.4
########################################

mu = 2.5
dp = 0.4

# a)
P_a = pnorm(2, mu, dp)

# b)
P_b = pnorm(3, mu, dp) - pnorm(2, mu, dp)

# c)
P_c = pnorm(3.2, mu, dp, lower.tail = FALSE)

# d)
esperado_lentas = 500 * P_c

# e)
P_e = qnorm(0.9, mu, dp)

P_a; P_b; P_c; esperado_lentas; P_e


########################################
# QUESTÃO 9 – Fórmula Excel
########################################
# n = ( N*p*(1-p)*Z^2 ) / ( (N-1)*e^2 + p*(1-p)*Z^2 )


########################################
# QUESTÃO 10 – renda normal
########################################

media = 3000   # insira valor dado
dp = 800
n = 200

renda = rnorm(n, media, dp)

summary(renda)

t.test(renda)


########################################
# QUESTÃO 11 – p = 1%, erro = 2.5%
########################################

p = 0.01
e = 0.025
Z = 1.96

n = (Z^2 * p * (1-p)) / e^2
n