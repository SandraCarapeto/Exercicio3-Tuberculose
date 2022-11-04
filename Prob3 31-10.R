setwd("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose")

library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(measurements)
library(lubridate)
library(sf)
library(leaflet)
library(htmltools)
library(RColorBrewer)

# Docs
P_DESA_Abates <- fread("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose/ProjetoTuberculose_2022_DESA (1) - Abates Pesquisa Mycobc 2010-2021.csv", encoding = "UTF-8") %>% unique
# meter com os nomes certos - encoding
cod_me <- fread("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose/ProjetoTuberculose_2022_DESA (1) - Cod Me DiCo.csv") %>% unique
P_DESA_IDTC <- fread("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose/ProjetoTuberculose_2022_DESA (1) - IDTC 2010-2021.csv") %>% unique

Correspondecia <- fread("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose/Correspondências freg 2013-14.csv") %>% unique

Total_Caract_Expl <- fread("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose/FicheiroTotalCaracterizaçãoExplo.csv") %>% unique

Total_Ent_Expl <- fread("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose/FicheiroTotalEntidadesExploraco.csv") %>% unique

Total_Tipo_Ent_Expl <- fread("C:/Users/Sandr/Desktop/Epi/Problema 3 Dados Tuberculose/FicheiroTotalTipos Entidade Explo.csv")%>% unique 

#Adicionamos o sufixo PT a marca de exploracao
P_DESA_Codme <- mutate(cod_me, ME = paste("PT", cod_me$ME, sep = '')) %>% unique #Dont run twice, you'll add another PT
#OU 
# Pode ser com o paste 0 Ver o que é!
# JUNTAR TABELAS 
#Primeiro junção de tabela Abates com o Codigo, fazendo corresponder o ME_alt ao cod
#O mesmo foi feito para IDTC
P_DESA_Abates <- inner_join(P_DESA_Abates, P_DESA_Codme, by= c("ME_alt" = "Cód")) %>% unique
P_DESA_IDTC <- inner_join(P_DESA_IDTC, P_DESA_Codme, by= c("ME_alt" = "Cód")) %>% unique
# Mantem o que é comum para as duas tabelas, juntar as coordenadas e os codigos de freguesia. O inner_join para diminuir o máximo
# de duplicados.

#Agora temos de juntar as coordenadas geográficas à tabela, através dos ficheiros com todas as explorações do país
# CEX_MAR_EXP corresponde a código oficial de marca de exploração na folha Total_Caract_Expl
# Juntando as colunas CEX_COD_FRE, CEX_COD_CON e CEX_COD_DIS obtemos o código DiCoFre
# Juntando as colunas CEX_GRA_N, CEX_MIN_N, CEX_SEG_N obtemos as coordenadas geograficas N e W
# Latitude é N e longitude é W
# Mas não é tão simples como juntar as colunas de GRA_, MIN_, SEG_, estas têm de ser convertidas
# Juntar as coordenadas em "dd mm ss", para utilizar library(measurements), função conv_unit()
# Portanto, transformar dados de deg_min_sec para dec_deg: conv_unit( *coluna*, "deg_min_sec","dec_deg")

Total_Caract_Expl <- unite(Total_Caract_Expl, "LATITUDE", CEX_GRA_N, CEX_MIN_N, CEX_SEG_N, sep = ' ') %>% 
  unite("LONGITUDE", CEX_GRA_W, CEX_MIN_W, CEX_SEG_W, sep = ' ') 

Total_Caract_Expl <- mutate(Total_Caract_Expl, LATITUDE = conv_unit(Total_Caract_Expl$LATITUDE, from = "deg_min_sec", to = "dec_deg")) %>% 
  mutate(LONGITUDE = conv_unit(Total_Caract_Expl$LONGITUDE, from = "deg_min_sec", to = "dec_deg"))

Total_Caract_Expl <- mutate(Total_Caract_Expl, LATITUDE = round(as.numeric(Total_Caract_Expl$LATITUDE), digit = 5)) %>% 
  mutate(LONGITUDE = round(as.numeric(Total_Caract_Expl$LONGITUDE), digit = 5))
#OU
# Tambem podia ser feito por aqui, mas fica mais bonito naquelas 3 linhas
# Total_Caract_Expl$LATITUDE<- paste(Total_Caract_Expl$CEX_GRA_N,Total_Caract_Expl$CEX_MIN_N, Total_Caract_Expl$CEX_SEG_N, sep=" ")
# Total_Caract_Expl$LONGITUDE<- paste(Total_Caract_Expl$CEX_GRA_W,Total_Caract_Expl$CEX_MIN_W, Total_Caract_Expl$CEX_SEG_W, sep=" ")

# Total_Caract_Expl$LATITUDE<- conv_unit(Total_Caract_Expl$LATITUDE, "deg_min_sec", "dec_deg")
# Total_Caract_Expl$LONGITUDE<- conv_unit(Total_Caract_Expl$LONGITUDE, "deg_min_sec", "dec_deg")
#OU
# Podia ser manual: graus + minutos /60 e numero de segundos/3600

#Isto é só para arrumar a casa
P_DESA_IDTC <- select(P_DESA_IDTC, -DiCo.x)
P_DESA_Abates <- select(P_DESA_Abates, -DiCo.x)

#Converter coluna das datas no formato correto - EU NÃO PRECISO DO ABAIXO PORQUE JÁ ESTÁ BEM CONVERTIDO!
# P_DESA_CARACT$DAT_ALT <- as_datetime(P_DESA_CARACT$DAT_ALT, format="%Y-%m-%d %H:%M")

#Esta linha de codigo so da uma coluna com marcas de exploracao mais recentes, mantendo todas as outras coluns atraves do keep_all = TRUE
Total_Caract_Expl_ordenado <- Total_Caract_Expl %>% arrange(desc(DAT_ALT)) %>% distinct(CEX_MAR_EXP, .keep_all = TRUE)
#Ordenar mantendo a mais recente, 177 mil explorações que têm positivo
Total_Caract_Expl_ordenado <- unite(Total_Caract_Expl_ordenado, "DiCoFre", CEX_COD_DIS, CEX_COD_CON, CEX_COD_FRE, sep = "")
#Juntar colunas

# Vou tentar tirar aqui apenas os que me interessa
select_Total_Caract_Expl_ordenado <- select(Total_Caract_Expl_ordenado, CEX_MAR_EXP, DiCoFre, LATITUDE, LONGITUDE)
names(select_Total_Caract_Expl_ordenado)[names(select_Total_Caract_Expl_ordenado) == 'CEX_MAR_EXP'] <- 'ME'
# Mudou o nome da coluna.

#Com o left_join, juntar as tabelas e obtemos todos os dados já com coordenadas certinhas e ME oficiais
P_DESA_Abates1 <- left_join(P_DESA_Abates, select_Total_Caract_Expl_ordenado)

P_DESA_IDTC1 <- left_join(P_DESA_IDTC, select_Total_Caract_Expl_ordenado)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://www.dgterritorio.gov.pt/cartografia/cartografia-tematica/caop e escolhem a versão que quiserem (eu escolhi 2020)
# Nao me lembro se era preciso usar uma versao mais antiga da carta tho
# Atencao que vao descarregar um ficheiro zip e vao ter de manter todos os ficheiros que ele tem juntos na mesma pasta ou acho que depois o mapa nao carrega

#Carregar o mapa
#O ficheiro vai sem extensao porque isto nao estava a conseguir abrir a layer, justificacao:
# https://stackoverflow.com/questions/50949028/readogr-cannot-open-layer-error 
continente<- st_read("Cont_AAD_CAOP2020")

# Em comum do ficheiro do mapa e ficheiros abates e IDTC temos o DiCoFre, juntar por ai
#Como o leaflet nao estava a gostar da festa com os dados como tinhamos antes, tive de ir ao stack overflow investigar a situa?ao de converter os dados em Lat-Long
#Essa conversao e esta linha a seguir
#https://stackoverflow.com/questions/45710087/plotting-sfc-polygon-in-leaflet
# O mapa das CAOP só consigo juntar informação geográfica, coordenadas projetadas para um plano 2D.
continente$geometry <- st_transform(continente$geometry, "+init=epsg:4326")

# Tabela com a soma de abates positivos por Dicofre com a geometria
#Basicamente isso pega em todas as explorações com o mesmo Dicofre e depois soma o total 
#de positivos, respeitando o agrupamento por freguesia
# drop - forma de somar respeitando os grupos
pos_freguesia_ab <- P_DESA_Abates1 %>% group_by(DiCoFre) %>% 
  summarise(Total_Ps = sum(`PM +`, na.rm = TRUE),
            .groups = 'drop')

# Por os 0 como NA para nao aparecerem no mapa
pos_freguesia_ab[pos_freguesia_ab == 0] <- NA

# Merge dos dois ficheiros para dar o ficheiro do mapa
continente_ab <- sp::merge(continente,pos_freguesia_ab, by.x="Dicofre", by.y="DiCoFre")


# Palete de cores para abates
bins_ab <- c(0, 2, 4, 8, 16, 32, 64, 128, Inf)

pal_ab <- colorBin("Greens",continente_ab$Total_Ps, bins_ab, na.color = NA) 

# Texto para o pop up
mytext_ab <- paste(
  "<strong>", "Freguesia: ", "</strong>", continente_ab$Freguesia, "<br/>", 
  "<strong>", "Positivos: ", "</strong>", continente_ab$Total_Ps, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)


#Mapa dos abates
portugal_abates <- leaflet(data = continente_ab) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, fillColor = ~pal_ab(Total_Ps), fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_ab, labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_ab, values = ~Total_Ps, opacity = 0.7, title = NULL,
            position = "bottomright")

portugal_abates

#Tabela com a soma de positivos ao IDTC para cada Dicofre com a geometry associada
pos_freguesia_IDTC <- P_DESA_IDTC1 %>% group_by(DiCoFre) %>% 
  summarise(Total_Ps = sum(Ps, na.rm = TRUE),
            .groups = 'drop')

# Merge para base de dados do mapa
continente_IDTC <- sp::merge(continente,pos_freguesia_IDTC, by.x="Dicofre", by.y="DiCoFre")

# Palete para IDTC
bins_IDTC <- c(0, 10, 20, 40, 80, 160, 320, 640, Inf)
pal_IDTC <- colorBin("Greens",continente_IDTC$Total_Ps, bins_IDTC) 

# Texto para o pop up, # strong para meter a bolt e o br foi copy paste
mytext_IDTC <- paste(
  "<strong>", "Freguesia: ", "</strong>", continente_IDTC$Freguesia, "<br/>", 
  "<strong>", "Positivos: ", "</strong>", continente_IDTC$Total_Ps, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# Mapa de casos IDTC
portugal_IDTC <- leaflet(data = continente_IDTC) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, fillColor = ~pal_IDTC(Total_Ps), fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_IDTC, labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_IDTC, values = ~Total_Ps, opacity = 0.7, title = NULL,
            position = "bottomright")

portugal_IDTC

# Tabela com todos os que ja foram positivos
# Tabela com as unicas colunas que nos interessam
P_DESA_IDTC2 <- select(P_DESA_IDTC1, ME, DiCoFre, LATITUDE, LONGITUDE, Ps, Ano) %>% unique
P_DESA_Abates2 <- select(P_DESA_Abates1, ME, DiCoFre, LATITUDE, LONGITUDE, `PM +`, DtAbate) %>% unique %>% 
  mutate(DtAbate =  as_datetime(DtAbate, format="%d/%m/%Y")) %>% mutate(DtAbate = year(DtAbate))

# Passar os 0's para NA's
P_DESA_IDTC2[P_DESA_IDTC2 == 0] <- NA
P_DESA_Abates2[P_DESA_Abates2 == 0] <- NA

# Mudar os nomes para facilitar visualizacao
names(P_DESA_IDTC2)[names(P_DESA_IDTC2) == 'Ps'] <- 'Pos_IDTC'
names(P_DESA_Abates2)[names(P_DESA_Abates2) == 'PM +'] <- 'Pos_AB'

# Merge das duas tabelas
tabela_TB <- full_join(P_DESA_IDTC2, P_DESA_Abates2)

# Filtrar a tabela para manter apenas as rows que tem um dos testes positivos
tabela_TB_1 <- tabela_TB[(tabela_TB$Pos_IDTC >= 1 | tabela_TB$Pos_AB >= 1),] %>% unique

# Criar duas novas colunas, uma que diz se a exploracao foi positiva no abate ou no IDTC
# e outra para 
tabela_TB_final <- tabela_TB_1 %>% 
  mutate(Positivo = case_when(Pos_IDTC | Pos_AB > 1 ~ TRUE)) %>% # Coluna Positivo ter um caso )
  mutate(Ano_IDTC = case_when(Pos_IDTC >= 1 ~ tabela_TB_1$Ano)) %>% #ano mais recente e tirar NA
  mutate(Ano_AB = case_when(Pos_AB >= 1 ~ tabela_TB_1$DtAbate)) %>%
  select(-Ano, -DtAbate) %>% # tirar o que não interessa
  mutate("Primeiro Ano" = pmin (Ano_IDTC, Ano_AB, na.rm = TRUE)) %>%
  mutate("Ultimo Ano" = pmax(Ano_IDTC, Ano_AB, na.rm = TRUE)) %>% # dar o mais recente
  select(-Ano_IDTC, -Ano_AB) %>% 
  unique

tabela_TB_final <- tabela_TB_final[(tabela_TB_final$Positivo == TRUE)]
# tirar o arrange para ficar ordenado e só como o que interessa.
View(tabela_TB_final)

tabela_TB_final <- tabela_TB_final[(tabela_TB_final$Positivo == TRUE)] %>% 
  arrange(desc("Último Ano")) %>% distinct(ME,.keep_all = TRUE)




