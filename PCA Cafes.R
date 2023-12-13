library(readxl)                  #Para leitura de Arquivos Excel 
library(ggplot2)                 #Biblioteca de gráficos
library(corrplot)                #Biblioteca para análise de correlação 
library(PerformanceAnalytics)    #Biblioteca para análise de correlação (2)
library(PTCA4CATA)               #Biblioteca suplementar para análise
library(data4PCCAR)              #Biblioteca suplementar para PCA
library(ExPosition)              #Biblioteca para cálculo do PCA
library(InPosition)              #Biblioteca para cálculo do PCA
library(graphics)                #Biblioteca padrão do R.
library(dplyr)                   #Biblioteca para manipulação dos dados
library(tidyverse)               #Coleção de bibliotecas 
library(data.table)              #Biblioteca para manipulação de dados
library(knitr)                   #Biblioteca para tabelas
library(kableExtra)              #Biblioteca para tabelas com barras de rolagem
library(gridExtra)               #Biblioteca para plotar dados
require(ggplotify)               #Biblioteca para plotar dados

#Acesso dos dados do Excel
Dados.puros <- read_excel("Planhilha de Dados.xlsx", 
                        sheet = "Tabela de Dados", range = "A2:EE20")
#kable

viewer <- kable(Dados.puros)

#Cria um Viewer dos dados

scroll_box(viewer, width = "100%", height = "300px")

#transforma em formato data.frame

Tabela.Trabalho.0 <- as.data.frame(Dados.puros)


#Retira a marca dos cafés

Tabela.Trabalho.1 <- Tabela.Trabalho.0[, -1]

#Retira o tipo dos cafés

Tabela.Trabalho.Numeros <- Tabela.Trabalho.1[,-1]

#Sumário dos dados numéricos

summary(Tabela.Trabalho.Numeros)

# transforma Tabela.Trabalho.0 em data table

Tabela.Trabalho.0.as.dt <- as.data.table(Tabela.Trabalho.0)

# Cria uma tabela com as médias de cada café

Tabela.Trabalho.2 <- Tabela.Trabalho.0 %>% 
  group_by(café) %>%
  summarise_all("mean")

#Adicionando nome as tabelas de médias
Tabela.Trabalho.3 <- as.data.frame(Tabela.Trabalho.2)

cafe.media <- as.data.frame(Tabela.Trabalho.3[,-1])

row.names(cafe.media) <- Tabela.Trabalho.3$café

#Somente dados númericos
cafe.processado <-  Tabela.Trabalho.Numeros


#tabela de correlação

Café.Correlacao <- cor(cafe.processado)

plot.correlacao <-corrplot(Café.Correlacao,
                     method = "color" ,
                     tl.col = "black")

plot.correlacao <- recordPlot()


#Realização do PCA com inferência
pca.prep.inf <- epPCA.inference.battery(cafe.processado, 
                                        center = TRUE, 
                                        scale = FALSE,
                                        DESIGN = Tabela.Trabalho.0$café)

inf.scree <- PlotScree(ev = pca.prep.inf$Fixed.Data$ExPosition.Data$eigs,
                       p.ev = pca.prep.inf$Inference.Data$components$p.vals,
                       plotKaiser = TRUE)

#Plot das amostras por café
cor.produto <- dplyr::recode(Tabela.Trabalho.0$café,
                            'Mon' = 'green3',
                            'Pri' = 'blue',
                            'Orf' = 'Pink',
                            'Pon' = 'firebrick1',
                            'Oct' = 'grey50',
                            'Dut' = 'orange',
                            'Sop' = 'darkorchid',
                            'Ode' = 'black',
                            'Min' = 'yellow')

cor.produto.media <- dplyr::recode(Tabela.Trabalho.0$café,
                                  'Mon' = 'green3',
                                  'Pri' = 'blue',
                                  'Orf' = 'brown',
                                  'Pon' = 'firebrick1',
                                  'Oct' = 'grey50',
                                  'Dut' = 'orange',
                                  'Sop' = 'darkorchid',
                                  'Ode' = 'black',
                                  'Min' = 'yellow')

cor.cafe.classificacao <- dplyr::recode(Tabela.Trabalho.0$café,
                                          'Mon' = 'blue',
                                          'Pri' = 'blue',
                                          'Orf' = 'blue',
                                          'Pon' = 'blue',
                                          'Oct' = 'blue',
                                          'Dut' = 'blue',
                                          'Sop' = 'blue',
                                          'Ode' = 'green',
                                          'Min' = 'green')

#Aquisição das médias dos cafés (cafe1,cafe2...)

media.produto <- as.data.frame(pca.prep.inf$Fixed.Data$ExPosition.Data$fi)

#Display para Cafés (cafe1 cafe2 etc...)

#Encontrar as médias
media.grupo <- aggregate(media.produto,
                        by = list(Tabela.Trabalho.0$café),
                        mean)

#Limpeza do dados
cafe.processado.media <- media.grupo[,-1]
row.names(cafe.processado.media) <- media.grupo[, 1]

#Display dos dados
viewer2 <- kable(cafe.processado.media)
scroll_box(viewer2, width = "910px", height = "200px")


#Cria o plot baseado nos tipos de café
fi.mean.plot <- createFactorMap(cafe.processado.media,
                                alpha.points = 0.8,
                                col.points = cor.produto.media,
                                col.labels = cor.produto.media,
                                pch = 17,
                                cex = 3,
                                text.cex = 3)

t1.bMap <- createFactorMap(pca.prep.inf$Fixed.Data$ExPosition.Data$fi, 
                           axis1 = 1, 
                           axis2 = 2, 
                           title = "Observation Factor Scores (Colored by coffees)",
                           col.points = cor.produto,
                           col.labels = cor.produto,
                           cex = 2.5,
                           text.cex = 3,
                           display.labels = FALSE)

t2.bMap <- createFactorMap(pca.prep.inf$Fixed.Data$ExPosition.Data$fi, 
                           axis1 = 1, 
                           axis2 = 2, 
                           title = "Observations Factor Scores (Colored by Products) with TI and Means",
                           col.points = cor.produto,
                           col.labels = cor.produto,
                           cex = 2.5,
                           text.cex = 3,
                           display.labels = FALSE)

t3.bMap <- createFactorMap(pca.prep.inf$Fixed.Data$ExPosition.Data$fi, 
                           axis1 = 1, 
                           axis2 = 2, 
                           title = "Observations Factor Scores (Colored by Class)",
                           col.points = cor.cafe.classificacao,
                           col.labels = cor.cafe.classificacao,
                           cex = 2.5,
                           text.cex = 3,
                           display.labels = FALSE)

row.label <- createxyLabels.gen(1,2,
                                lambda = round(pca.prep.inf$Fixed.Data$ExPosition.Data$eigs),
                                tau = round(pca.prep.inf$Fixed.Data$ExPosition.Data$t),
                                axisName = "Component")          

#Desenha o mapa F1xF2
row.fscore2 <- t1.bMap$zeMap + row.label
row.fscore2

#Display por classificação (Especial, não especial)

media.tipo <- as.data.frame(pca.prep.inf$Fixed.Data$ExPosition.Data$fi)

#Extração das médias
media.grupo.tipo <- aggregate(media.tipo,
                             by = list(Tabela.Trabalho.0$Tipo),
                             mean)

#Limpeza dos dados
t.media.grupo.tipo <- media.grupo.tipo[, -1]
Indicacao.classificacao <- ifelse(media.grupo.tipo[1] == 1, "Especial", "Tradicional")
row.names(t.media.grupo.tipo) <- Indicacao.classificacao

#Display dos dados
d <- kable(t.media.grupo.tipo)
scroll_box(d, width = "910px", height = "200px")

#Cor dos cafés por classificação
cor.classificacao.media <- dplyr::recode(media.grupo.tipo$Group.1,
                                      '1' = 'blue',
                                      '0' = 'green',)

#Cria o mapa dos fatores por classificação
fi.mean.plot.type <- createFactorMap(t.media.grupo.tipo,
                                     alpha.points = 0.8,
                                     col.points = cor.classificacao.media,
                                     col.labels = cor.classificacao.media,
                                     pch = 17,
                                     cex = 3,
                                     text.cex = 3)

t1.cMap <- createFactorMap(pca.prep.inf$Fixed.Data$ExPosition.Data$fi, 
                           axis1 = 1, 
                           axis2 = 2, 
                           title = "Observations Factor Scores (Colored by Type - Especial vs tradicional)",
                           col.points = cor.cafe.classificacao,
                           col.labels = cor.cafe.classificacao,
                           cex = 2.5,
                           text.cex = 3,
                           display.labels = FALSE)

t2.cMap <- createFactorMap(pca.prep.inf$Fixed.Data$ExPosition.Data$fi, 
                           axis1 = 1, 
                           axis2 = 2, 
                           title = "Observations Factor Scores (Colored by Type) with TI and Means",
                           col.points = cor.cafe.classificacao,
                           col.labels = cor.cafe.classificacao,
                           cex = 2.5,
                           text.cex = 3,
                           display.labels = FALSE)

t3.cMap <- createFactorMap(pca.prep.inf$Fixed.Data$ExPosition.Data$fi, 
                           axis1 = 1, 
                           axis2 = 2, 
                           title = "Observations Factor Scores (Colored by Type) with BI and Means",
                           col.points = cor.cafe.classificacao,
                           col.labels = cor.cafe.classificacao,
                           cex = 2.5,
                           text.cex = 3,
                           display.labels = FALSE)

row.label <- createxyLabels.gen(1,2,
                                lambda = round(pca.prep.inf$Fixed.Data$ExPosition.Data$eigs),
                                tau = round(pca.prep.inf$Fixed.Data$ExPosition.Data$t),
                                axisName = "Component")

#Desenha o mapa dos fatores

row.fscore3 <- t1.cMap$zeMap + row.label
row.fscore3

#Criação do mapa de tolerância para classificação

TIplot.type <- MakeToleranceIntervals(pca.prep.inf$Fixed.Data$ExPosition.Data$fi,
                                      design = as.factor(Tabela.Trabalho.0$Tipo),
                                      names.of.factors =  c("Dim1","Dim2"), 
                                      col = cor.classificacao.media,
                                      line.size = .50, 
                                      line.type = 3,
                                      alpha.ellipse = .2,
                                      alpha.line    = .4,
                                      p.level       = .95)

row.full.3TI <- t2.cMap$zeMap_background + t2.cMap$zeMap_dots + fi.mean.plot.type$zeMap_dots + fi.mean.plot.type$zeMap_text + TIplot.type + row.label
row.full.3TI

row.full.3TI <- recordPlot()

#Código para realizar a criação da figura
require(ggplot2)
library(gridExtra)
require(ggplotify)

#designa cores diferentes para cada variável

var <- colnames(Dados.puros)
var.color <- prettyGraphsColorSelection(n.colors = ncol(Dados.puros)-1)

#Escores das variáveis 
my.fj.plot <- createFactorMap(pca.prep.inf$Fixed.Data$ExPosition.Data$fj,
                              title = "Variables Factor Scores",
                              axis1 = 1, axis2 = 2,
                              pch = 19,
                              cex = 1,
                              text.cex = 3,
                              col.points = var.color,
                              col.labels = var.color,
)

fj.plot <- my.fj.plot$zeMap + row.label
fj.plot

#circulo de carga
cor.loading <- cor(cafe.processado, pca.prep.inf$Fixed.Data$ExPosition.Data$fi)
row.names(cor.loading) <- row.names(cor.loading)

loading.plot <- createFactorMap(cor.loading,
                                constraints = list(minx = -1, miny = -1,
                                                   maxx = 1, maxy = 1),
                                col.points = var.color,
                                col.labels = var.color,
                                title = "Loadings Circle",
                                max.overlaps = 200)

LoadingMapWithCircles <- loading.plot$zeMap + 
  addArrows(cor.loading, color = var.color) + 
  addCircleOfCor() + xlab("Component 1") + ylab("Component 2")
LoadingMapWithCircles

signed.ctrJ <- pca.prep.inf$Fixed.Data$ExPosition.Data$cj * sign(pca.prep.inf$Fixed.Data$ExPosition.Data$fj)


#Plot das contribuições fator 1
ctrJ.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 4,
                         color4bar = var.color,
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
) + ggtitle("Contribution barplots", subtitle = 'Component 1: Variable Contributions (Signed)')

#Plot das contribuições fator 2

ctrJ.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 4,
                         color4bar = var.color,
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
) + ggtitle("",subtitle = 'Component 2: Variable Contributions (Signed)')

BR <- pca.prep.inf$Inference.Data$fj.boots$tests$boot.ratios
laDim = 1

#Plot do bootstrap para fator 1
ba001.BR1 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 4,
                            color4bar = var.color,
                            ylab = 'Bootstrap ratios'
                            #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
) + ggtitle("Bootstrap ratios", subtitle = paste0('Component ', laDim))

#Plot do bootstrap para fator 1
laDim = 2
ba002.BR2 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 4,
                            color4bar = var.color,
                            ylab = 'Bootstrap ratios'
                            #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
) + ggtitle("",subtitle = paste0('Component ', laDim))

final.output.barplots <- grid.arrange(
  ctrJ.1,
  ctrJ.2,
  ba001.BR1,
  ba002.BR2,
  ncol = 2,nrow = 2,
  top = "Barplots for variables"
)

newfinal.output.barplots <- grid.arrange(
  ctrJ.1,
  ctrJ.2,
  ncol = 1,nrow = 2,
  top = "Barplots for variables"
)


