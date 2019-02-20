
# ------------------------------------------Questão01----------------------------------------------------
# 01) Utilize uma Distribuição Uniforme para criar uma base de dados aleatória. 
# A base deve ser composta por 50 elementos entre os valores 6 e 18. 
# Em seguida, calcule o que se pede.

database <- seq(from = 6, to = 18, by = 0.244)
length(database)
database

mean(database) # Média
var(database) # Variância 
sd(database) # Desvio padrão 
median(database) # Mediana 
quantile(database) # Quartis 
100*sd(database)/mean(database) # Coeficiente de variação

# ------------------------------------------Questão02--------------------------------------------------------------
# 02) Realize o download da base de dados localizada no link http://archive.ics.uci.edu/ml/datasets/Wine+Quality. 
# Dois arquivos devem ser baixados, o que avalia a qualidade dos vinhos tintose outro sobre a qualidade dos vinhos 
# brancos (winequality-red.csve winequality-white.csvrespectivamente). Faça o que se pedepara ambas basesde dados.

wine_red <- read.csv("../datasets/winequality-red.csv", sep = ";")
wine_white <- read.csv("../datasets/winequality-white.csv", sep = ";")

head(wine_red, n = 10)

head(wine_white)

# a) Obtenha os parâmetros estatísticos com a função summary.

summary(wine_red)

summary(wine_white)

# b) Plote umgráfico de pontos dos parâmetros de qualidade por pH.

plot(wine_red[,12],wine_red[,9],type="p", main = "Vinho vermelho", xlab = "Qualidade", ylab = "pH", col="blue")

plot(wine_white[,12],wine_white[,9],type="p", main = "Vinho branco", xlab = "Qualidade", ylab = "pH", col="green")

# c) Plote gráficos de barras para os parâmetros pH, qualidade e álcool.

wine_red_aux <- data.frame(pH=wine_red[,9], qualidade=wine_red[,12], alcool=wine_red[,11])
barplot(colSums(wine_red_aux), main = "Vinho vermelho")

wine_white_aux <- data.frame(pH=wine_white[,9], qualidade=wine_white[,12], alcool=wine_white[,11])
barplot(colSums(wine_white_aux), main = "Vinho branco")

# d) Plote histogramas para os parâmetros açúcar residual e volatilidade ácida com 10 intervalos.

hist(wine_red[,2], main = "Vinho vermelho", xlab = "Volatilidade acida")

hist(wine_red[,4], main = "Vinho vermelho", xlab = "Açúcar residual")

hist(wine_white[,2], main = "Vinho branco", xlab = "Volatilidade acida")

hist(wine_white[,4], main = "Vinho branco", xlab = "Açucar residual")

library("ISwR")

# e) Calcule a correlação entre os parâmetros qualidade e pH;densidade e álcool;
# açúcar residual e qualidade. Interprete cada resultado com um breve comentário.

print("Vinho vermelho --BEGIN--")
# qualidade e pH
cor(wine_red[,12], wine_red[,9])
cor.test(wine_red[,12], wine_red[,9])

# densidade e álcool
cor(wine_red[,8], wine_red[,11])
cor.test(wine_red[,8], wine_red[,11])

# açúcar residual e qualidade
cor(wine_red[,4], wine_red[,12])
cor.test(wine_red[,4], wine_red[,12])

print("Vinho vermelho --END--")

print("Vinho branco --BEGIN--")
# qualidade e pH
cor(wine_white[,12], wine_white[,9])
cor.test(wine_white[,12], wine_white[,9])

# densidade e álcool
cor(wine_white[,8], wine_white[,11])
cor.test(wine_white[,8], wine_white[,11])

# açúcar residual e qualidade
cor(wine_white[,4], wine_white[,12])
cor.test(wine_white[,4], wine_white[,12])

print("Vinho branco --END--")

# ------------------------------------------Questão03--------------------------------------------------------------
# 03)Implemente uma função em R que calcule a moda de vetoresde dados criados pelo usuário. Cada vetor deve conter 15 elementos, 
# contendo apenas números inteiros positivos e/ou negativos. Crie três vetores para simulação, sendo um amodal, 
# um modal e um bimodal para teste do seu programa.

# Função que calcula a moda
calc_moda <- function(vec){
 if(length(vec) != 15){
     print("Vetor inválido! O vetor deve conter 15 elementos.")
     return(FALSE)
 }
    
 if(typeof(vec[0]) == 'character'){
     print("Vetor inválido! O vetor deve conter apenas inteiros.")
     return(FALSE)     
 }

 z <- table(as.vector(vec))
 names(z)[z == max(z)]
 
 return(z)
}

# Vetores de exemplo
amodal<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
modal<-c(1,1,3,4,5,6,7,8,9,10,11,12,13,14,15)
bimodal<-c(1,1,3,3,5,6,7,8,9,10,11,12,13,14,15)

calc_moda(amodal)
calc_moda(modal)
calc_moda(bimodal)

# ------------------------------------------Questão04--------------------------------------------------------------
# A tabela a seguir apresenta a quantidade de itens vendidos em um restaurante por um período
# de tempo. Devido às rasuras alguns dados estão faltantes. Importe os dados no formato XLSX
# para o RStudio®. Calcule média, mediana, variância e desvio padrão da Variável Quantidade.
# Obs.: ao criar a planilha no editor utilize apenas DUAS colunas para inserir os dados.

# PS está em CSV pq nao consegui instalar a LIB de xls do R 3.4

df <- data.frame(read.csv("questao_04.csv", sep = ";"))
head(df)
qtd <- na.omit(df[,2])
qtd

mean(qtd) # Média
var(qtd) # Variância 
sd(qtd) # Desvio padrão 
median(qtd) # Mediana

# ------------------------------------------Questão05--------------------------------------------------------------
# Calcule as probabilidades dos seguintes eventos. Para cada letra crie o espaço amostral e o
# evento desejado.

# a) Obter três caras no lançamento de 6 vezes de uma moeda justa.

S1 <- tosscoin(6, makespace = TRUE)
head(S1)
E1 <- subset(S1, isrep(S1, vals = "H", nrep = 3))
head(E1)
Prob(E1)

# b) Obter quatro coroas no lançamento de 7 vezes de uma moeda com p_cara = 0.65 e
# p_coroa= 0.35.

S2 <- probspace(tosscoin(7),probs = c(0.65,0.35)) #cara 65% e coroa 35%
head(S2)

E2 <- subset(S2, isrep(S2, vals = "T", nrep = 4))
head(E2)
Prob(E2)

# c) Obter a soma do resultado do lançamento de 4 vezes de um dado com 8 faces maior que
# 22.
S3 <-rolldie(4, nsides = 8, makespace = TRUE)
head(S3)

E3 <- subset(S3, X1+X2+X3+X4 > 22)
head(E3)
Prob(E3) #resultado

# d) Considere uma urna com 04 bolas verdes, 06 bolas azuis, 02 bolas pretas e 03 bolas
# vermelhas. Determine a probabilidade de se retirada a sequencia nesta ordem (preta,
# azul, verde, vermelha e azul) após cinco retiradas sem reposição dessa urna

Balls <- rep(c("verde","azul","preta", "vermelha"),times = c(4,6,2,3))
S4 <- urnsamples(Balls, size = 5, replace = FALSE, ordered = TRUE)
E4 <- probspace(S4)
R4 <- Prob(E4, isin(E4, c("preta","azul","verde","vermelha","azul"), ordered =
TRUE))

R4

# ------------------------------------------Questão06--------------------------------------------------------------
# 06) Para a base de dados beaver1 presente no R faça o que se pede

beav <- beaver1
head(beav)

# a) Plote um gráfico entre as variáveis tempo (eixo X) e temperatura (eixo Y).

plot(beav[,2], beav[,3], type = "b", main = "Beaver1", xlab = "Tempo", ylab = "Temperatura", col=c("red", "blue"))

# b) Faça uma regressão linear considerando Y=f(X)

time<-beav[,2]
temp<-beav[,3]
x1<-time
y1<-temp

beav.lm <- lm(y1 ~ x1, data=beav)
coef1<- coef(beav.lm)
coef1
plot(x1,y1,type="p",xlab = "Tempo", ylab = "Temperatura", col=c("red", "blue"))
abline(coef1)

# c) Com base na regressão preveja o valor da temperatura para o tempo 960 e 1680.

# d) Calcule o vetor de predições

prev1<- fitted(beav.lm)
head(prev1, 10)

# e) Calcule o vetor de resíduos

res1 <- residuals(beav.lm)
head(res1, 10)

# f) Intervalo de confiança para os níveis de 95% e 70%.
conf1 <- confint(beav.lm, level = 0.95) #intervalo de confiança de 95%
conf2 <- confint(beav.lm, level = 0.70) #intervalo de confiança de 70%

conf1 # 95
conf2 # 70

# g) Utilizando a função ci.plot() plote os gráficos dos intervalos junto com os dados.

plot(beav.lm, conf.level = 0.70)

plot(beav.lm, conf.level = 0.95)
