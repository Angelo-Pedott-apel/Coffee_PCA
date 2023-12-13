library(readxl)
library(prospectr)

Dados <- read_excel("C:/Users/VAIO/Desktop/Dump³/Dados para teste MatLab.xlsx")

typeof(Dados)

Dados_s_label <- Dados[,-1]

typeof(Dados_s_label)
Num_am_modelo <- round(nrow(Dados_s_label)*3/4)

typeof(Num_am_modelo)

Seleção <- kenStone(Dados_s_label,
                    Num_am_modelo,
                    metric = "euclid")

Modelo <-data.frame(Amostras_modelo=Seleção$model)
Teste <-data.frame(Amostras_teste=Seleção$test)


Dados_espe <- read_excel("C:/Users/VAIO/Desktop/Dump³/Dados somente especiais.xlsx")

Dados_espe_s_labels <- Dados_espe[,-1]

Num_am_modelo_espe <- round(nrow(Dados_espe_s_labels)*3/4)

Seleção_espe <- kenStone(Dados_espe_s_labels,
                    Num_am_modelo_espe,
                    metric = "euclid")

Modelo_espe <-data.frame(Amostras_modelo=Seleção_espe$model)
Teste_espe <-data.frame(Amostras_teste=Seleção_espe$test)

Dados_trad <- read_excel("C:/Users/VAIO/Desktop/Dump³/Dados somente Tradicionais.xlsx")

Dados_trad_s_labels <- Dados_trad[,-1]

Num_am_modelo_trad <- round(nrow(Dados_trad_s_labels)*3/4)

Seleção_trad <- kenStone(Dados_trad_s_labels,
                         Num_am_modelo_trad,
                         metric = "euclid")

Modelo_trad <-data.frame(Amostras_modelo=Seleção_trad$model)
Teste_trad <-data.frame(Amostras_teste=Seleção_trad$test)