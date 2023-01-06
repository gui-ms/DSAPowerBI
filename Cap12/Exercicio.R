

notas <- read.csv("notas.csv", fileEncoding = "windows-1252")

# Exercício 1

str(notas)
summary(notas)

# Exercício 2

mediaA <- mean(notas$TurmaA)
print(mediaA)
mediaB <- mean(notas$TurmaB)
print(mediaB)

# Exercício 3

desvioA <- sd(notas$TurmaA)
print(desvioA)
desvioB <- sd(notas$TurmaB)
print(desvioB)
# Resposta: Turma A apresentou maior variabilidade como mostrado
# ao calcular o desvio padrão de cada turma. As notas da turma B 
# ficam mais próximas do valor da média.

# Exercício 4 (Coeficiente de Variação - CV)

cvA <- desvioA/mediaA*100
cvB <- desvioB/mediaB*100

cvA
cvB

# Exercício 5
# Cálculo de moda

moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

resultado <- moda(notas$TurmaA)
print(resultado)

resultado <- moda(notas$TurmaB)
print(resultado)

