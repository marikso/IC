# Instalação dos pacotes caso não estejam instalados
if (!require(haven))
  install.packages("haven")
if (!require(dplyr))
  install.packages("dplyr")
if (!require(car))
  install.packages("car")
if (!require(rstudioapi))
  install.packages("rstudioapi")

# Ativação dos pacotes necessários
library(haven)
library(dplyr)
library(car)
library(rstudioapi)

# read_sav() lê arquivo SSPS
banco_jovens <- read_sav("banco.sav")

# Função apenas para exibir o menu de interação
menu <- function() {
  cat("\n===== Digite o número correspondente ao que deseja =====\n")
  cat("1  - Frequencia do tipo de escola\n")
  cat("2  - Porcentagem de tipos de escola\n")
  cat("3  - Escolas e quantos alunos participaram de cada instituição\n")
  cat("4  - Porcentagem de alunos que participaram em cada instituição\n")
  cat("5  - Frequência do turno de aula\n")
  cat("6  - Porcentagem de alunos em cada turno\n")
  cat("7  - Frequência de pesquisas realizadas por cada pesquisador\n")
  cat("8  - Porcentagem de pesquisas realizadas por cada pesquisador\n")
  cat("9  - Frequência de cada idade\n")
  cat("10 - Porcentagem de cada idade\n")

  cat("0  - Sair\n")
  cat("Sua opção:\n")
}

menu1 <- function(opcao) {
  if (opcao == 1 || opcao == 2) {
    banco_jovens$Tipoescola <- as_factor(banco_jovens$Tipoescola)
    fr_tipo_escola <- table(banco_jovens$Tipoescola)
    if (opcao == 1) {
      print(fr_tipo_escola)
    } else {
      pr_tipo_escola <- prop.table(fr_tipo_escola) * 100
      print(pr_tipo_escola)
    }

  } else if (opcao == 3 || opcao == 4) {
    banco_jovens$Nomeescola <- as_factor(banco_jovens$Nomeescola)
    escolas <- table(banco_jovens$Nomeescola)

    if (opcao == 4) {
      print(escolas)
    } else {
      pr_escolas <- prop.table(escolas) * 100
      print(pr_escolas)
    }
  } else if (opcao == 5 || opcao == 6) {
    banco_jovens$Turno <- as_factor(banco_jovens$Turno)
    fr_turno <- table(banco_jovens$Turno)
    if (opcao == 6) {
      print(fr_turno)
    } else {
      pr_turno <- prop.table(fr_turno) * 100
      print(pr_turno)
    }
  }
}

menu2 <- function(opcao) {
  if (opcao == 7 || opcao == 8) {
    banco_jovens$Pesquisador <- as_factor(banco_jovens$Pesquisador)
    fr_pesquisador <- table(banco_jovens$Pesquisador)

    if (opcao == 7) {
      print(fr_pesquisador)
      cat("\n| Pesquisador |\n-------------\n| Frequência |\n")
    } else {
      pr_pesquisador <- prop.table(fr_pesquisador) * 100
      cat("\n| Pesquisador |\n-------------\n| Porcentagem |\n")
      print(pr_pesquisador)
    }
  } else if (opcao == 9 || opcao == 10) {
    # Converte q1.idade para fator
    banco_jovens$q1.idade <- as_factor(banco_jovens$q1.idade)
    # Usamos addNA() para incluir valores NA como uma categoria
    fr_idade <- table(addNA(banco_jovens$q1.idade))
    if (opcao == 9) {
      print(fr_idade)
    } else if (opcao == 10) {
      pr_idade <- prop.table(fr_idade) * 100
      print(pr_idade)
    }
  }
}


# Iniciar variável para entrar no loop do while()
opcao <- -1

# Enquanto o valor de "opcao" for diferente de 0,
# o programa continua a interagir com o usuário
while (opcao != 0) {
  # Exibe menu
  menu()

  # Lê a entrada do usuário
  opcao <- as.integer(readline(prompt = ""))

  # Sequências de if/else se comportam de acordo com a escolha do usuário
  menu1(opcao)
  menu2(opcao)
}

cat("===== Encerrando =====")