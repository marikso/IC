# Instalação dos pacotes caso não estejam instalados
if (!require(haven)) install.packages("haven")
if (!require(dplyr)) install.packages("dplyr")
if (!require(car)) install.packages("car")
if (!require(rstudioapi)) install.packages("rstudioapi")

# Ativação dos pacotes necessários
library(haven)
library(dplyr)
library(car)
library(rstudioapi)

# Carregar arquivo SPSS
banco <- read_sav("C:/Users/maria/Desktop/IC/banco.sav")

# Função para exibir o menu
menu <- function() {
  cat("\n===== Digite o número correspondente ao que deseja =====\n")
  cat("1  - Porcentagem de tipos de escola\n")
  cat("2  - Porcentagem de alunos que participaram em cada instituição\n")
  cat("3  - Porcentagem de alunos em cada turno\n")
  cat("4  - Porcentagem de cada idade\n")
  cat("5  - Porcentagem de gênero\n")
  cat("6  - Porcentagem de frequência em interesse político\n")
  cat("7  - Porcentagem em conversa política com os pais\n")
  cat("8  - Porcentagem em conversa política com familiares\n")
  cat("9  - Porcentagem em conversa política com amigos\n")
  cat("10 - Porcentagem em conversa política nas redes sociais\n")
  cat("11 - Porcentagem em conversa política com colegas\n")
  cat("12 - Porcentagem em conversa política com professores\n")
  cat("13 - Porcentagem em formar opinião 1LUGAR\n")
  cat("0  - Sair\n")
  cat("Sua opção:\n")
}

# Função para calcular e exibir frequência e proporção, excluindo "Não respondeu"
calcular_e_imprimir <- function(coluna, mostrar_proporcao = TRUE) {
  # Converte a coluna em fator e remove NA
  coluna <- as_factor(coluna)
  coluna <- coluna[!is.na(coluna) & coluna != "Não respondeu"]  # Filtra valores NA e "Não respondeu"
  
  # Verifica se há dados restantes após a remoção de NA e "Não respondeu"
  if (length(coluna) == 0) {
    cat("Não há dados disponíveis para exibir.\n")
    return()
  }
  
  # Calcula frequências
  fr <- table(coluna)
  
  # Calcula e exibe proporções, se necessário
  if (mostrar_proporcao) {
    pr <- prop.table(fr) * 100
    cat("Proporções (%):\n")
    print(pr)
  }
  
  # Exibe total
  cat("Total de respostas:", sum(fr), "\n")
}



# Função para executar a opção escolhida
executar_opcao <- function(opcao) {
  if (opcao == 1) {
    calcular_e_imprimir(banco$Tipoescola)
  } else if (opcao == 2) {
    calcular_e_imprimir(banco$Nomeescola)
  } else if (opcao == 3) {
    calcular_e_imprimir(banco$Turno)
  } else if (opcao == 4) {
    calcular_e_imprimir(banco$q1.idade)
  } else if (opcao == 5) {
    calcular_e_imprimir(banco$q2.sexo)
  } else if (opcao == 6) {
    calcular_e_imprimir(banco$q3.interessepolitica)
  } else if (opcao == 7) {
    calcular_e_imprimir(banco$q4.1conversapoliticaPAIS)
  } else if (opcao == 8) {
    calcular_e_imprimir(banco$q4.2conversapoliticaFAMILIARES)
  } else if (opcao == 9) {
    calcular_e_imprimir(banco$q4.3conversapoliticaAMIGOS)
  } else if (opcao == 10) {
    calcular_e_imprimir(banco$q4.4conversapoliticaREDESSOCIAIS)
  } else if (opcao == 11) {
    calcular_e_imprimir(banco$q4.5conversapoliticaCOLEGASESCOLA)
  } else if (opcao == 12) {
    calcular_e_imprimir(banco$q4.6conversapoliticaPROFESSORES)
  } else if (opcao == 13) {
    calcular_e_imprimir(banco$q5.1formaropiniao1LUGAR)
  } else {
    cat("Opção inválida.\n")
  }
}

# Loop do menu
opcao <- -1
while (opcao != 0) {
  menu()
  entrada <- readline()
  opcao <- suppressWarnings(as.integer(entrada))

  if (!is.na(opcao) && opcao >= 0) {
    if (opcao == 0) {
      cat("===== Encerrando =====\n")
    } else {
      executar_opcao(opcao)
    }
  } else {
    cat("Opção inválida, tente novamente.\n")
  }
}
