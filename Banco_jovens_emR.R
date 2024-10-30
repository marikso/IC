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
  cat("17 - Frequência em conversa política com familiares\n")
  cat("18 - Porcentagem em conversa política com familiares\n")
  cat("19 - Frequência em conversa política com amigos\n")
  cat("20 - Porcentagem em conversa política com amigos\n")
  cat("21 - Frequência em conversa política nas redes sociais\n")
  cat("22 - Porcentagem em conversa política nas redes sociais\n")
  cat("23 - Frequência em conversa política com colegas\n")
  cat("24 - Porcentagem em conversa política com colegas\n")
  cat("25 - Frequência em conversa política com professores\n")
  cat("26 - Porcentagem em conversa política com professores\n")
  cat("27 - Frequência em formar opinião 1LUGAR\n")
  cat("28 - Porcentagem em formar opinião 1LUGAR\n")

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
  } else if (opcao == 15 || opcao == 16) {
    banco$q4.1conversapoliticaPAIS <- as_factor(banco$q4.1conversapoliticaPAIS)
    fr_interesse_pol_pais <- table(addNA(banco$q4.1conversapoliticaPAIS))

    if (opcao == 15) {
      print(fr_interesse_pol_pais)
    } else {
      pr_interesse_pol_pais <- prop.table(fr_interesse_pol_pais) * 100
      print(pr_interesse_pol_pais)
    }
  } else if (opcao == 17 || opcao  == 18) {
    banco$q4.2conversapoliticaFAMILIARES <- as_factor(banco$q4.2conversapoliticaFAMILIARES)
    fr_interesse_pol_fam <- table(addNA(banco$q4.2conversapoliticaFAMILIARES))

    if (opcao == 17) {
      print(fr_interesse_pol_fam)
    } else {
      pr_interesse_pol_fam <- prop.table(fr_interesse_pol_fam) * 100
      print(pr_interesse_pol_fam)
    }
  } else if (opcao == 19 || opcao == 20){
    banco$q4.3conversapoliticaAMIGOS <- as_factor(banco$q4.3conversapoliticaAMIGOS)
    fr_interesse_pol_amigos <- table(addNA(banco$q4.3conversapoliticaAMIGOS))

    if (opcao ==  19){
      print(fr_interesse_pol_amigos)
    } else {
      pr_interesse_pol_amigos <- prop.table(fr_interesse_pol_amigos) * 100
      print(pr_interesse_pol_amigos)
    }

  } else if (opcao == 21 ||  opcao == 22) {
    banco$q4.4conversapoliticaREDESSOCIAIS <- as_factor(banco$q4.4conversapoliticaREDESSOCIAIS)
    fr_interesse_pol_redesSoc <- table(addNA(banco$q4.4conversapoliticaREDESSOCIAIS))

    if (opcao == 21) {
      print(fr_interesse_pol_redesSoc)
    } else {
      pr_interesse_pol_redesSoc <- prop.table(fr_interesse_pol_redesSoc) * 100
      print(pr_interesse_pol_redesSoc)
    }
  } else if (opcao == 23 || opcao == 24) {
    banco$q4.5conversapoliticaCOLEGASESCOLA <- as_factor(banco$q4.5conversapoliticaCOLEGASESCOLA)
    fr_interesse_pol_colegasEscola <- table(addNA(banco$q4.5conversapoliticaCOLEGASESCOLA))

    if (opcao == 23) {
      print(fr_interesse_pol_colegasEscola)
    } else {
      pr_interesse_pol_colegasEscola <- prop.table(fr_interesse_pol_colegasEscola) * 100
      print(pr_interesse_pol_colegasEscola)    
    }
  } else if (opcao == 25 || opcao == 26) {
    banco$q4.6conversapoliticaPROFESSORES <- as_factor(banco$q4.6conversapoliticaPROFESSORES)
    fr_interesse_pol_prof <- table(addNA(banco$q4.6conversapoliticaPROFESSORES))

    if (opcao == 25) {
      print(fr_interesse_pol_prof)
    } else {
      pr_interesse_pol_prof <- prop.table(fr_interesse_pol_prof) * 100
      print(pr_interesse_pol_prof)
    }
  }
}

menu4 <- function(opcao) {
  if (opcao == 27 || opcao == 28) {
    banco$q5.1formaropiniao1LUGAR <- as_factor(banco$q5.1formaropiniao1LUGAR)
    fr_forma_opiniao_1lugar <- table(addNA(banco$q5.1formaropiniao1LUGAR))
    
    if (opcao == 27) {
      print(fr_forma_opiniao_1lugar)
    } else {
      pr_forma_opiniao_1lugar <- prop.table(fr_forma_opiniao_1lugar) * 100
      print(pr_forma_opiniao_1lugar)
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

  # Lê a entrada do usuário e tenta converter para inteiro
  entrada <- readline(prompt = "")
  opcao <- suppressWarnings(as.integer(entrada))

  # Verifica se a conversão foi bem-sucedida
  if (is.na(opcao)) {
    cat("Opção inválida, tente novamente.\n")
  } else {
    # Sequências de if/else se comportam de acordo com a escolha do usuário
    menu1(opcao)
    menu2(opcao)
    menu3(opcao)
    menu4(opcao)

  }
}
cat("===== Encerrando =====")