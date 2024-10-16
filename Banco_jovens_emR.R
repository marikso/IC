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
banco <- read_sav("banco.sav")

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
  cat("11 - Frequência de gênero\n")
  cat("12 - Porcentagem de gênero\n")
  cat("13 - Frequência no interesse político\n")
  cat("14 - Porcentagem de frequência em interesse político\n")
  cat("15 - Frequência em conversa política com os pais\n")
  cat("16 - Porcentagem em conversa política com os pais\n")

  cat("0  - Sair\n")
  cat("Sua opção:\n")
}

menu1 <- function(opcao) {
  if (opcao == 1 || opcao == 2) {
    banco$Tipoescola <- as_factor(banco$Tipoescola)
    fr_tipo_escola <- table(banco$Tipoescola)
    if (opcao == 1) {
      print(fr_tipo_escola)
    } else {
      pr_tipo_escola <- prop.table(fr_tipo_escola) * 100
      print(pr_tipo_escola)
    }

  } else if (opcao == 3 || opcao == 4) {
    banco$Nomeescola <- as_factor(banco$Nomeescola)
    escolas <- table(banco$Nomeescola)

    if (opcao == 4) {
      print(escolas)
    } else {
      pr_escolas <- prop.table(escolas) * 100
      print(pr_escolas)
    }
  } else if (opcao == 5 || opcao == 6) {
    banco$Turno <- as_factor(banco$Turno)
    fr_turno <- table(banco$Turno)
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
    banco$Pesquisador <- as_factor(banco$Pesquisador)
    fr_pesquisador <- table(banco$Pesquisador)

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
    banco$q1.idade <- as_factor(banco$q1.idade)
    # Usamos addNA() para incluir valores NA como uma categoria
    fr_idade <- table(addNA(banco$q1.idade))
    if (opcao == 9) {
      print(fr_idade)
    } else if (opcao == 10) {
      pr_idade <- prop.table(fr_idade) * 100
      print(pr_idade)
    }
  } else if (opcao == 11 || opcao == 12) {
    banco$q2.sexo <- as_factor(banco$q2.sexo)
    fr_sexo <- table(addNA(banco$q2.sexo))
    if (opcao == 11) {
      print(fr_sexo)
    } else {
      pr_sexo <- prop.table(fr_sexo) * 100
      print(pr_sexo)
    }
  }
}

menu3 <- function(opcao) {
  if (opcao == 13 || opcao == 14) {
    banco$q3.interessepolitica <- as_factor(banco$q3.interessepolitica)
    fr_interesse_pol <- table(addNA(banco$q3.interessepolitica))

    if (opcao == 13) {
      print(fr_interesse_pol)
    } else {
      pr_interesse_pol <- prop.table(fr_interesse_pol) * 100
      print(pr_interesse_pol)
    }
  } else if (opcao == 15 || 16) {
    banco$q4.1conversapoliticaPAIS <- as_factor(banco$q4.1conversapoliticaPAIS)
    fr_interesse_pol_pais <- table(addNA(banco$q4.1conversapoliticaPAIS))

    if (opcao == 15) {
      print(fr_interesse_pol_pais)
    } else {
      pr_interesse_pol_pais <- prop.table(fr_interesse_pol_pais) * 100
      print(pr_interesse_pol_pais)
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
  menu3(opcao)
}

cat("===== Encerrando =====")