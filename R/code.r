if(!require(pdftools)) install.packages('pdftools')
library(pdftools)
if(!require(readr)) install.packages('readr')
library(readr)
if(!require(stringi)) install.packages('stringi')
library(stringi)
if(!require(stringr)) install.packages('stringr')
library(stringr)
if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if(!require(plotly)) install.packages('plotly')
library(plotly)
if(!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)
if(!require(rgdal)) install.packages('rgdal')
library(rgdal)
if(!require(dplyr)) install.packages('dplyr')
library(dplyr)

file_pdf <- pdf_text("pdf/total-por-municipio.pdf") %>% read_lines()
file_pdf <- file_pdf[-(grep("QUANTIDADE", file_pdf))]
file_pdf <- file_pdf[-(grep("Página", file_pdf))]
file_pdf <- file_pdf[-(grep("Quantidade", file_pdf))]
file_pdf <- strsplit(file_pdf, "  ")

list_pdf <- 0
file_pdf[which((lapply(file_pdf, length)==0) == TRUE)] <- NULL
a <- 1
for(i in 1:length(file_pdf)){
    info_number <- length(file_pdf[[i]][!stri_isempty(file_pdf[[i]])])
    for(j in 1:info_number){
        list_pdf[a] <- str_trim(file_pdf[[i]][!stri_isempty(file_pdf[[i]])][j])
        a <- a + 1
    }
}

data_pdf <- data.frame(stringsAsFactors = FALSE, "City" = list_pdf[grepl("^[[:upper:]]+$", 
str_replace_all(str_replace_all(str_replace_all(list_pdf, fixed(" "), ""),fixed("."), ""),"-","")) == TRUE], 
"Clinic" = 0, "Company" = 0, "Physiotherapist" = 0, "PhilanthropicEntity" = 0, "PublicAgency" = 0, "OccupationalTherapist" = 0)

for(i in 1:(length(list_pdf)+600)){
    aux <- grepl("^[[:upper:]]+$", str_replace_all(str_replace_all(str_replace_all(list_pdf[i], fixed(" "), ""),fixed("."), ""),"-",""))
    if(aux){
        list_pdf<-append(list_pdf, "%", after=i) 
    }
}

marker <- 0
count <- 0
for(i in 1:length(list_pdf)){
    aux <- grepl("^[[:upper:]]+$", str_replace_all(str_replace_all(str_replace_all(list_pdf[i], fixed(" "), ""),fixed("."), ""),"-",""))
    if(aux){
        if(list_pdf[i] == "VILA MARIA"){ 
            marker <- 1
        }
        if(marker == 0){
            aux_2 <- i + 4
        }
        else{
            aux_2 <- i + 2
        }
        while(count < 6 && !grepl("^[[:upper:]]+$", str_replace_all(str_replace_all(str_replace_all(list_pdf[aux_2], fixed(" "), ""),fixed("."), ""),"-",""))){
            if("Consultório" %in% list_pdf[aux_2]){
                data_pdf$Clinic[data_pdf$City==list_pdf[i]] <- list_pdf[aux_2 + 1]
            }    
            if("Empresa" %in% list_pdf[aux_2]){
                data_pdf$Company[data_pdf$City==list_pdf[i]] <- list_pdf[aux_2 + 1]
            }   
            if("Entidade Filantrópica" %in% list_pdf[aux_2]){
                data_pdf$PhilanthropicEntity[data_pdf$City==list_pdf[i]] <- list_pdf[aux_2 + 1]
            }   
            if("Fisioterapeuta" %in% list_pdf[aux_2]){
                data_pdf$Physiotherapist[data_pdf$City==list_pdf[i]] <- list_pdf[aux_2 + 1]
            }   
            if("Órgão Público" %in% list_pdf[aux_2]){
                data_pdf$PublicAgency[data_pdf$City==list_pdf[i]] <- list_pdf[aux_2 + 1]
            }  
            if("Terapeuta Ocupacional" %in% list_pdf[aux_2]){
                data_pdf$OccupationalTherapist[data_pdf$City==list_pdf[i]] <- list_pdf[aux_2 + 1]
            }             
            if(marker == 0){
                aux_2 <- aux_2 + 4
            }
            else{
                aux_2 <- aux_2 + 2
            }
            count <- count + 1
        }
        count <- 0
    }
}

population_rs <- read.csv2('spreadsheets/table200_pop.csv', skip=6, stringsAsFactors = FALSE, encoding="UTF-8")
population_rs<-population_rs[-(497:508),]
colnames(population_rs)[1] <- "Cities"
colnames(population_rs)[3] <- "Population"
population_rs<-population_rs[,-2]

population_rs[,2]<-gsub("[...]","0",population_rs[,2])
population_rs[,2]<-gsub("[-]","0",population_rs[,2])
population_rs[,2]<-as.numeric(as.character(unlist(population_rs[,2])))
population_rs[,1]<-gsub(" [(]RS[)]","",population_rs[,1])

shape_rs <- readOGR("shapes/Municipios_IBGE.shp", "Municipios_IBGE",use_iconv=TRUE, encoding="UTF-8")

shape_rs@data[!shape_rs@data$Label_N %in% population_rs$Cities,]

population_rs[239,1]<-"Maçambara"
population_rs[341,1]<-"Restinga Seca"
population_rs[368,1]<-"Santana do Livramento"
population_rs[482,1]<-"Vespasiano Correa"
population_rs[495,1]<-"Westfalia"

shape_rs@data <- shape_rs@data %>% left_join(population_rs, by = c("Label_N" = "Cities"))
shape_rs@data[is.na(shape_rs@data$Population),12]<-0

data_pdf$City <- tolower(iconv(data_pdf$City,from="UTF-8",to="ASCII//TRANSLIT"))
shape_rs@data$NOME <- tolower(shape_rs@data$NOME)

data_pdf[!data_pdf$City %in% shape_rs@data$NOME,1]

data_pdf$City[28] <- "benjamin constant do sul"
data_pdf$City[41] <- "boa vista das missoes"
data_pdf$City[113] <- "chiapeta"
data_pdf$City[113] <- "dom pedro de alcantara"
data_pdf$City[127] <- "doutor mauricio cardoso"
data_pdf$City[129] <- "eldorado do sul"
data_pdf$City[133] <- "dezesseis de novembro"
data_pdf$City[139] <- "dois irmaos das missoes"
data_pdf$City[160] <- "fazenda vila nova"
data_pdf$City[173] <- "gramado dos loureiros"
data_pdf$City[184] <- "guarani das missoes"
data_pdf$City[214] <- "lagoa dos tres cantos"
data_pdf$City[238] <- "maximiliano de almeida"
data_pdf$City[246] <- "monte alegre dos campos"
data_pdf$City[262] <- "nova esperanca do sul"
data_pdf$City[353] <- "santa vitoria do palmar"
data_pdf$City[358] <- "santana do livramento"
data_pdf$City[364] <- "santa maria do herval"
data_pdf$City[365] <- "santo antonio do palma"
data_pdf$City[371] <- "santo expedito do sul"
data_pdf$City[378] <- "sao francisco de paula"
data_pdf$City[380] <- "sao francisco de assis"
data_pdf$City[384] <- "sao jose do hortencio"
data_pdf$City[393] <- "sao jose dos ausentes"
data_pdf$City[397] <- "sao martinho da serra"
data_pdf$City[399] <- "sao miguel das missoes"
data_pdf$City[402] <- "sao paulo das missoes"
data_pdf$City[409] <- "sao pedro das missoes"
data_pdf$City[416] <- "senador salgado filho"
data_pdf$City[418] <- "santo antonio da patrulha"
data_pdf$City[420] <- "santo antonio das missoes"
data_pdf$City[423] <- "santo antonio de palma"
data_pdf$City[424] <- "santo antonio do planalto"
data_pdf$City[489] <- "vista alegre do prata"

data_pdf <- data_pdf[-356,]
data_pdf <- data_pdf[-238,]
data_pdf <- data_pdf[-375,]
data_pdf <- data_pdf[-435,]
data_pdf <- data_pdf[-435,]

missing_cities <- data.frame(City = shape_rs@data$NOME[!shape_rs@data$NOME %in% data_pdf$City],"Clinic" = 0, "Company" = 0, "Physiotherapist" = 0, "PhilanthropicEntity" = 0, "PublicAgency" = 0, "OccupationalTherapist" = 0)
colnames(missing_cities) <- colnames(data_pdf)
data_pdf <- rbind(data_pdf, missing_cities)

data_pdf$OccupationalTherapist <- as.numeric(as.character(data_pdf$OccupationalTherapist))
data_pdf$OccupationalTherapist[is.na(data_pdf$OccupationalTherapist)]<-0
data_pdf$PublicAgency <- as.numeric(as.character(data_pdf$PublicAgency))
data_pdf$PublicAgency[is.na(data_pdf$PublicAgency)]<-0
data_pdf$PhilanthropicEntity <- as.numeric(as.character(data_pdf$PhilanthropicEntity))
data_pdf$PhilanthropicEntity[is.na(data_pdf$PhilanthropicEntity)]<-0
data_pdf$Physiotherapist <- as.numeric(as.character(data_pdf$Physiotherapist))
data_pdf$Physiotherapist[is.na(data_pdf$Physiotherapist)]<-0
data_pdf$Company <- as.numeric(as.character(data_pdf$Company))
data_pdf$Company[is.na(data_pdf$Company)]<-0
data_pdf$Clinic <- as.numeric(as.character(data_pdf$Clinic))
data_pdf$Clinic[is.na(data_pdf$Clinic)]<-0

shape_rs@data$id <- c(1:nrow(shape_rs@data))
shapefile_df <- fortify(shape_rs, region = 'id') %>% mutate(id = as.numeric(id))
shapefile_df<-sp::merge(shapefile_df, shape_rs@data,all = TRUE,by="id")

data_pdf$Physiotherapist[data_pdf$City=="alegria"] <- 0

data_pdf$Physiotherapist[data_pdf$City=="arroio do sal"] <- 9
data_pdf$Clinic[data_pdf$City=="arroio do sal"] <- 4
data_pdf$PublicAgency[data_pdf$City=="arroio do sal"] <- 1

data_pdf$Physiotherapist[data_pdf$City=="alto feliz"] <- 1
data_pdf$Company[data_pdf$City=="alto feliz"] <- 1

data_pdf$Company[data_pdf$City=="arroio dos ratos"] <- 3
data_pdf$PhilanthropicEntity[data_pdf$City=="arroio dos ratos"] <- 5
data_pdf$Physiotherapist[data_pdf$City=="arroio dos ratos"] <- 7
data_pdf$PublicAgency[data_pdf$City=="arroio dos ratos"] <- 1

data_pdf$Clinic[data_pdf$City=="alto alegre"] <- 1
data_pdf$Company[data_pdf$City=="alto alegre"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="alto alegre"] <- 2
data_pdf$PublicAgency[data_pdf$City=="alto alegre"] <- 2

data_pdf$Clinic[data_pdf$City=="arroio do tigre"] <- 6
data_pdf$Company[data_pdf$City=="arroio do tigre"] <- 1
data_pdf$PhilanthropicEntity[data_pdf$City=="arroio do tigre"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="arroio do tigre"] <- 6
data_pdf$PublicAgency[data_pdf$City=="arroio do tigre"] <- 1

data_pdf$Company[data_pdf$City=="alpestre"] <- 2
data_pdf$PhilanthropicEntity[data_pdf$City=="alpestre"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="alpestre"] <- 2
data_pdf$PublicAgency[data_pdf$City=="alpestre"] <- 1

data_pdf$Clinic[data_pdf$City=="cambara do sul"] <- 1
data_pdf$PhilanthropicEntity[data_pdf$City=="cambara do sul"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="cambara do sul"] <- 3
data_pdf$PublicAgency[data_pdf$City=="cambara do sul"] <- 1

data_pdf$Clinic[data_pdf$City=="cacapava do sul"] <- 10
data_pdf$Company[data_pdf$City=="cacapava do sul"] <- 4
data_pdf$PhilanthropicEntity[data_pdf$City=="cacapava do sul"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="cacapava do sul"] <- 19
data_pdf$PublicAgency[data_pdf$City=="cacapava do sul"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="cacapava do sul"] <- 3

data_pdf$Clinic[data_pdf$City=="campestre da serra"] <- 1
data_pdf$Company[data_pdf$City=="campestre da serra"] <- 0
data_pdf$PhilanthropicEntity[data_pdf$City=="campestre da serra"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="campestre da serra"] <- 1
data_pdf$PublicAgency[data_pdf$City=="campestre da serra"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="campestre da serra"] <- 0

data_pdf$Clinic[data_pdf$City=="caseiros"] <- 4
data_pdf$Company[data_pdf$City=="caseiros"] <- 0
data_pdf$PhilanthropicEntity[data_pdf$City=="caseiros"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="caseiros"] <- 6
data_pdf$PublicAgency[data_pdf$City=="caseiros"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="caseiros"] <- 0

data_pdf$Company[data_pdf$City=="capao bonito do sul"] <- 1

data_pdf$Clinic[data_pdf$City=="catuipe"] <- 5
data_pdf$Company[data_pdf$City=="catuipe"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="catuipe"] <- 9
data_pdf$PublicAgency[data_pdf$City=="catuipe"] <- 1

data_pdf$Clinic[data_pdf$City=="capao da canoa"] <- 23
data_pdf$Company[data_pdf$City=="capao da canoa"] <- 7
data_pdf$PhilanthropicEntity[data_pdf$City=="capao da canoa"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="capao da canoa"] <- 68
data_pdf$PublicAgency[data_pdf$City=="capao da canoa"] <- 3
data_pdf$OccupationalTherapist[data_pdf$City=="capao da canoa"] <- 5

data_pdf$Clinic[data_pdf$City=="caxias do sul"] <- 274
data_pdf$Company[data_pdf$City=="caxias do sul"] <- 87
data_pdf$PhilanthropicEntity[data_pdf$City=="caxias do sul"] <- 6
data_pdf$Physiotherapist[data_pdf$City=="caxias do sul"] <- 803
data_pdf$PublicAgency[data_pdf$City=="caxias do sul"] <- 7
data_pdf$OccupationalTherapist[data_pdf$City=="caxias do sul"] <- 34

data_pdf$Clinic[data_pdf$City=="capao do cipo"] <- 1
data_pdf$Company[data_pdf$City=="capao do cipo"] <- 0
data_pdf$PhilanthropicEntity[data_pdf$City=="capao do cipo"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="capao do cipo"] <- 1
data_pdf$PublicAgency[data_pdf$City=="capao do cipo"] <- 0
data_pdf$OccupationalTherapist[data_pdf$City=="capao do cipo"] <- 0

data_pdf$Clinic[data_pdf$City=="jacutinga"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="jacutinga"] <- 3

data_pdf$Physiotherapist[data_pdf$City=="lajeado do bugre"] <- 0
data_pdf$PublicAgency[data_pdf$City=="lajeado do bugre"] <- 1

data_pdf$Clinic[data_pdf$City=="nonoai"] <- 5
data_pdf$Company[data_pdf$City=="nonoai"] <- 2
data_pdf$PhilanthropicEntity[data_pdf$City=="nonoai"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="nonoai"] <- 9
data_pdf$PublicAgency[data_pdf$City=="nonoai"] <- 1

data_pdf$Clinic[data_pdf$City=="nova petropolis"] <- 20
data_pdf$Company[data_pdf$City=="nova petropolis"] <- 2
data_pdf$PhilanthropicEntity[data_pdf$City=="nova petropolis"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="nova petropolis"] <- 26
data_pdf$PublicAgency[data_pdf$City=="nova petropolis"] <- 3

data_pdf$Clinic[data_pdf$City=="nova alvorada"] <- 2
data_pdf$Company[data_pdf$City=="nova alvorada"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="nova alvorada"] <- 3
data_pdf$PublicAgency[data_pdf$City=="nova alvorada"] <- 1

data_pdf$Clinic[data_pdf$City=="nova prata"] <- 15
data_pdf$Company[data_pdf$City=="nova prata"] <- 11
data_pdf$PhilanthropicEntity[data_pdf$City=="nova prata"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="nova prata"] <- 28
data_pdf$OccupationalTherapist[data_pdf$City=="nova prata"] <- 3

data_pdf$Company[data_pdf$City=="nova araca"] <- 3
data_pdf$Physiotherapist[data_pdf$City=="nova araca"] <- 4

data_pdf$Clinic[data_pdf$City=="nova ramada"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="nova ramada"] <- 2

data_pdf$Clinic[data_pdf$City=="nova bassano"] <- 6
data_pdf$Company[data_pdf$City=="nova bassano"] <- 3
data_pdf$PhilanthropicEntity[data_pdf$City=="nova bassano"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="nova bassano"] <- 5
data_pdf$PublicAgency[data_pdf$City=="nova bassano"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="nova bassano"] <- 1

data_pdf$Clinic[data_pdf$City=="nova roma do sul"] <- 2
data_pdf$Company[data_pdf$City=="nova roma do sul"] <- 0
data_pdf$PhilanthropicEntity[data_pdf$City=="nova roma do sul"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="nova roma do sul"] <- 4
data_pdf$PublicAgency[data_pdf$City=="nova roma do sul"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="nova roma do sul"] <- 0

data_pdf$Clinic[data_pdf$City=="nova santa rita"] <- 2
data_pdf$Company[data_pdf$City=="nova santa rita"] <- 3
data_pdf$PhilanthropicEntity[data_pdf$City=="nova santa rita"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="nova santa rita"] <- 6
data_pdf$PublicAgency[data_pdf$City=="nova santa rita"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="nova santa rita"] <- 1

data_pdf$Physiotherapist[data_pdf$City=="pinheirinho do vale"] <- 2
data_pdf$PublicAgency[data_pdf$City=="pinheirinho do vale"] <- 1

data_pdf$Clinic[data_pdf$City=="porto xavier"] <- 2
data_pdf$Company[data_pdf$City=="porto xavier"] <- 1
data_pdf$PhilanthropicEntity[data_pdf$City=="porto xavier"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="porto xavier"] <- 5
data_pdf$PublicAgency[data_pdf$City=="porto xavier"] <- 2

data_pdf$Clinic[data_pdf$City=="pinheiro machado"] <- 3
data_pdf$Company[data_pdf$City=="pinheiro machado"] <- 1
data_pdf$PhilanthropicEntity[data_pdf$City=="pinheiro machado"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="pinheiro machado"] <- 6
data_pdf$PublicAgency[data_pdf$City=="pinheiro machado"] <- 1

data_pdf$PublicAgency[data_pdf$City=="pouso novo"] <- 1

data_pdf$Clinic[data_pdf$City=="pinto bandeira"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="pinto bandeira"] <- 3
data_pdf$PublicAgency[data_pdf$City=="pinto bandeira"] <- 1

data_pdf$Clinic[data_pdf$City=="presidente lucena"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="presidente lucena"] <- 0
data_pdf$PublicAgency[data_pdf$City=="presidente lucena"] <- 1

data_pdf$Clinic[data_pdf$City=="pinheiro machado"] <- 1
data_pdf$Company[data_pdf$City=="pinheiro machado"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="pinheiro machado"] <- 3

data_pdf$Clinic[data_pdf$City=="piratini"] <- 4
data_pdf$Company[data_pdf$City=="piratini"] <- 1
data_pdf$PhilanthropicEntity[data_pdf$City=="piratini"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="piratini"] <- 6
data_pdf$OccupationalTherapist[data_pdf$City=="piratini"] <- 2

data_pdf$Company[data_pdf$City=="protasio alves"] <- 2
data_pdf$PhilanthropicEntity[data_pdf$City=="protasio alves"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="protasio alves"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="protasio alves"] <- 0

data_pdf$Clinic[data_pdf$City=="segredo"] <- 2
data_pdf$PhilanthropicEntity[data_pdf$City=="segredo"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="segredo"] <- 4
data_pdf$PublicAgency[data_pdf$City=="segredo"] <- 1

data_pdf$Clinic[data_pdf$City=="sao sepe"] <- 16
data_pdf$Company[data_pdf$City=="sao sepe"] <- 3
data_pdf$PhilanthropicEntity[data_pdf$City=="sao sepe"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="sao sepe"] <- 33
data_pdf$PublicAgency[data_pdf$City=="sao sepe"] <- 2
data_pdf$OccupationalTherapist[data_pdf$City=="sao sepe"] <- 8

data_pdf$Clinic[data_pdf$City=="selbach"] <- 5
data_pdf$Physiotherapist[data_pdf$City=="selbach"] <- 6
data_pdf$PublicAgency[data_pdf$City=="selbach"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="selbach"] <- 0

data_pdf$Clinic[data_pdf$City=="sao valentim"] <- 2
data_pdf$Company[data_pdf$City=="sao valentim"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="sao valentim"] <- 3
data_pdf$PublicAgency[data_pdf$City=="sao valentim"] <- 1

data_pdf$Clinic[data_pdf$City=="senador salgado filho"] <- 1
data_pdf$Company[data_pdf$City=="senador salgado filho"] <- 0
data_pdf$PhilanthropicEntity[data_pdf$City=="senador salgado filho"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="senador salgado filho"] <- 2
data_pdf$PublicAgency[data_pdf$City=="senador salgado filho"] <- 0
data_pdf$OccupationalTherapist[data_pdf$City=="senador salgado filho"] <- 0

data_pdf$Clinic[data_pdf$City=="sinimbu"] <- 2
data_pdf$Company[data_pdf$City=="sinimbu"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="sinimbu"] <- 2

data_pdf$Clinic[data_pdf$City=="tapes"] <- 2
data_pdf$Company[data_pdf$City=="tapes"] <- 2
data_pdf$PhilanthropicEntity[data_pdf$City=="tapes"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="tapes"] <- 9
data_pdf$PublicAgency[data_pdf$City=="tapes"] <- 1

data_pdf$Clinic[data_pdf$City=="sobradinho"] <- 14
data_pdf$Company[data_pdf$City=="sobradinho"] <- 1
data_pdf$PhilanthropicEntity[data_pdf$City=="sobradinho"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="sobradinho"] <- 13
data_pdf$PublicAgency[data_pdf$City=="sobradinho"] <- 0

data_pdf$Clinic[data_pdf$City=="taquara"] <- 17
data_pdf$Company[data_pdf$City=="taquara"] <- 11
data_pdf$PhilanthropicEntity[data_pdf$City=="taquara"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="taquara"] <- 45
data_pdf$PublicAgency[data_pdf$City=="taquara"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="taquara"] <- 3

data_pdf$Clinic[data_pdf$City=="soledade"] <- 13
data_pdf$Company[data_pdf$City=="soledade"] <- 3
data_pdf$PhilanthropicEntity[data_pdf$City=="soledade"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="soledade"] <- 30
data_pdf$PublicAgency[data_pdf$City=="soledade"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="soledade"] <- 0

data_pdf$Clinic[data_pdf$City=="taquari"] <- 12
data_pdf$Company[data_pdf$City=="taquari"] <- 7
data_pdf$PhilanthropicEntity[data_pdf$City=="taquari"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="taquari"] <- 44
data_pdf$PublicAgency[data_pdf$City=="taquari"] <- 1

data_pdf$Company[data_pdf$City=="toropi"] <- 1
data_pdf$PublicAgency[data_pdf$City=="toropi"] <- 1

data_pdf$Clinic[data_pdf$City=="trindade do sul"] <- 2
data_pdf$Company[data_pdf$City=="trindade do sul"] <- 1
data_pdf$PhilanthropicEntity[data_pdf$City=="trindade do sul"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="trindade do sul"] <- 5
data_pdf$PublicAgency[data_pdf$City=="trindade do sul"] <- 1

data_pdf$Clinic[data_pdf$City=="torres"] <- 35
data_pdf$Company[data_pdf$City=="torres"] <- 7
data_pdf$PhilanthropicEntity[data_pdf$City=="torres"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="torres"] <- 105
data_pdf$PublicAgency[data_pdf$City=="torres"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="torres"] <- 2

data_pdf$Clinic[data_pdf$City=="triunfo"] <- 1
data_pdf$Company[data_pdf$City=="triunfo"] <- 13
data_pdf$PhilanthropicEntity[data_pdf$City=="triunfo"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="triunfo"] <- 7
data_pdf$PublicAgency[data_pdf$City=="triunfo"] <- 2
data_pdf$OccupationalTherapist[data_pdf$City=="triunfo"] <- 1

data_pdf$Clinic[data_pdf$City=="tramandai"] <- 12
data_pdf$Company[data_pdf$City=="tramandai"] <- 7
data_pdf$PhilanthropicEntity[data_pdf$City=="tramandai"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="tramandai"] <- 43
data_pdf$PublicAgency[data_pdf$City=="tramandai"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="tramandai"] <- 3

data_pdf$Clinic[data_pdf$City=="tucunduva"] <- 2
data_pdf$Company[data_pdf$City=="tucunduva"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="tucunduva"] <- 5
data_pdf$PublicAgency[data_pdf$City=="tucunduva"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="tucunduva"] <- 0

data_pdf$Company[data_pdf$City=="travesseiro"] <- 1
data_pdf$PublicAgency[data_pdf$City=="travesseiro"] <- 0

data_pdf$PublicAgency[data_pdf$City=="tres arroios"] <- 1

data_pdf$Clinic[data_pdf$City=="uruguaiana"] <- 38
data_pdf$Company[data_pdf$City=="uruguaiana"] <- 14
data_pdf$PhilanthropicEntity[data_pdf$City=="uruguaiana"] <- 3
data_pdf$Physiotherapist[data_pdf$City=="uruguaiana"] <- 165
data_pdf$PublicAgency[data_pdf$City=="uruguaiana"] <- 4
data_pdf$OccupationalTherapist[data_pdf$City=="uruguaiana"] <- 6

data_pdf$Clinic[data_pdf$City=="viamao"] <- 11
data_pdf$Company[data_pdf$City=="viamao"] <- 14
data_pdf$PhilanthropicEntity[data_pdf$City=="viamao"] <- 2
data_pdf$Physiotherapist[data_pdf$City=="viamao"] <- 120
data_pdf$PublicAgency[data_pdf$City=="viamao"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="viamao"] <- 15

data_pdf$Clinic[data_pdf$City=="vacaria"] <- 23
data_pdf$Company[data_pdf$City=="vacaria"] <- 7
data_pdf$PhilanthropicEntity[data_pdf$City=="vacaria"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="vacaria"] <- 48
data_pdf$PublicAgency[data_pdf$City=="vacaria"] <- 1
data_pdf$OccupationalTherapist[data_pdf$City=="vacaria"] <- 3

data_pdf$Clinic[data_pdf$City=="vicente dutra"] <- 1
data_pdf$Company[data_pdf$City=="vicente dutra"] <- 0
data_pdf$PhilanthropicEntity[data_pdf$City=="vicente dutra"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="vicente dutra"] <- 3
data_pdf$PublicAgency[data_pdf$City=="vicente dutra"] <- 2
data_pdf$OccupationalTherapist[data_pdf$City=="vicente dutra"] <- 0

data_pdf$Clinic[data_pdf$City=="sao gabriel"] <- 25
data_pdf$Company[data_pdf$City=="sao gabriel"] <- 5
data_pdf$PhilanthropicEntity[data_pdf$City=="sao gabriel"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="sao gabriel"] <- 65
data_pdf$PublicAgency[data_pdf$City=="sao gabriel"] <- 2
data_pdf$OccupationalTherapist[data_pdf$City=="sao gabriel"] <- 1

data_pdf$Clinic[data_pdf$City=="sao leopoldo"] <- 50
data_pdf$Company[data_pdf$City=="sao leopoldo"] <- 34
data_pdf$PhilanthropicEntity[data_pdf$City=="sao leopoldo"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="sao leopoldo"] <- 255
data_pdf$OccupationalTherapist[data_pdf$City=="sao leopoldo"] <- 7

data_pdf$Clinic[data_pdf$City=="sao jeronimo"] <- 4
data_pdf$Company[data_pdf$City=="sao jeronimo"] <- 4
data_pdf$Physiotherapist[data_pdf$City=="sao jeronimo"] <- 10
data_pdf$OccupationalTherapist[data_pdf$City=="sao jeronimo"] <- 2

data_pdf$Clinic[data_pdf$City=="sao loucenco do sul"] <- 9
data_pdf$Company[data_pdf$City=="sao loucenco do sul"] <- 4
data_pdf$PhilanthropicEntity[data_pdf$City=="sao loucenco do sul"] <- 1
data_pdf$Physiotherapist[data_pdf$City=="sao loucenco do sul"] <- 28
data_pdf$PublicAgency[data_pdf$City=="sao loucenco do sul"] <- 2
data_pdf$OccupationalTherapist[data_pdf$City=="sao loucenco do sul"] <- 3

data_pdf$Clinic[data_pdf$City=="sao joao da urtiga"] <- 3
data_pdf$Company[data_pdf$City=="sao joao da urtiga"] <- 0
data_pdf$PhilanthropicEntity[data_pdf$City=="sao joao da urtiga"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="sao joao da urtiga"] <- 3
data_pdf$PublicAgency[data_pdf$City=="sao joao da urtiga"] <- 0
data_pdf$OccupationalTherapist[data_pdf$City=="sao joao da urtiga"] <- 0

shape_rs<-sp::merge(shapefile_df, data_pdf, by.x="NOME", by.y="City")

quantile( shape_rs$Physiotherapist[shape_rs$Physiotherapist>=0], p = (0:5)/5 )

shape_rs$PopulationPerPhysio <- shape_rs$Population/(shape_rs$Physiotherapist+1)
quantile( shape_rs$PopulationPerPhysio[shape_rs$PopulationPerPhysio>=0], p = (0:5)/5 )

shape_rs$cat <- ifelse(shape_rs$Physiotherapist > 33, 5, ifelse(shape_rs$Physiotherapist > 10, 
    4, ifelse(shape_rs$Physiotherapist > 4, 3, ifelse(shape_rs$Physiotherapist > 2, 2, 1))))
shape_rs$cat <- factor(shape_rs$cat, levels = c(5:1), labels = c("34 - 3498", "11 - 33", 
    "5 - 10", "3 - 4", "0 - 2"))

shape_rs$cat2 <- ifelse(shape_rs$PopulationPerPhysio > 2143, 5, ifelse(shape_rs$PopulationPerPhysio > 1314, 
    4, ifelse(shape_rs$PopulationPerPhysio > 1008, 3, ifelse(shape_rs$PopulationPerPhysio > 742, 2, 1))))
shape_rs$cat2 <- factor(shape_rs$cat2, levels = c(5:1), labels = c("2143 - 43652", "1314 - 2142", 
    "1008 - 1313", "742 - 1007", "0 - 741"))

p <- ggplot() +
    geom_polygon( data = shape_rs, aes( fill = cat2, x = long, y = lat, group = group), color = "black", size = 0.1) +
    coord_equal() +
    theme( legend.position = "bottom", legend.title = element_text( size = 18, hjust = 0.5),
        legend.text = element_text( size = 14), plot.title = element_text( size = 18, hjust = 0.5)) +
    labs( x = NULL, y = NULL, title = "Population/Physiotherapists Rate in Rio Grande do Sul - Brazil - Source: Crefito and IBGE, 2019.") +
    scale_fill_manual( values = rev(colorRampPalette(brewer.pal(5, "Spectral"))(5)),
        name = "People / Physiotherapists Rate",
        drop = FALSE,
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit( 6, units = "mm"), keywidth = unit( 40, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0.5,
            nrow = 1,
            byrow = T,
            reverse = T,
            label.position = "bottom"
        )
    )


p2 <- ggplot()+
    geom_polygon(data = shape_rs, aes(fill = cat, group = group, x = long, y = lat), color = "black", size = 0.1) +
    coord_equal() +
    theme(legend.position = "bottom", legend.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 14), plot.title = element_text(size = 18, hjust = 0.5)) +
    labs(x = NULL, y = NULL, title = "Physiotherapists in Rio Grande do Sul - Brazil - Source: Crefito and IBGE, 2019.") +
    scale_fill_manual(
        values = rev(colorRampPalette(brewer.pal(5, "Greens"))(5)),
        name = "Physiotherapists Number",
        drop = FALSE,
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(6, units = "mm"),keywidth = unit(40, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0.5,
            nrow = 1,
            byrow = T,
            reverse = T,
            label.position = "bottom"
        )
    )