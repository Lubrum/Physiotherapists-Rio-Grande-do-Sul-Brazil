# Physiotherapists in Rio Grande do Sul - Brazil

# Summary

This project aims to explore through maps the number of Physiotherapists in the municipalities of Rio Grande do Sul state, from Brazil. The challenge here is that all the data available about the Physiotherapists were found just in .pdf format. You can [download](http://www.crefito5.org.br/wp-content/uploads/2019/06/total-por-municipio.pdf) the file from Crefito - RS to see and understand why is hard to automatic get all the data. In our case, we will see that is not possible.

Open your RStudio or other IDE with R language. Set your directory as the working directory with the RStudio or you can do this using:
```R
setwd("working_directory")
```
We load the packages that we will need first.

```R
if (!require(pdftools)) {
  install.packages("pdftools", repos = "http://cran.us.r-project.org")
  require(pdftools)
}
if (!require(readr)) {
  install.packages("readr", repos = "http://cran.us.r-project.org")
  require(readr)
}
if (!require(stringi)) {
  install.packages("stringi", repos = "http://cran.us.r-project.org")
  require(stringi)
}
if (!require(stringr)) {
  install.packages("stringr", repos = "http://cran.us.r-project.org")
  require(stringr)
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
  require(ggplot2)
}
if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(dplyr)) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
  require(dplyr)
}
```
First we deal with the pdf file loading it to RStudio. 

```R
file_pdf <- pdf_text("Physioterapists_by_city.pdf") %>% read_lines()
```
We got the data in the following format:

![Alt text](figures/figure1.png?raw=true "Title")

Next, we need to remove the lines with useless information. For that, we use *grep* function to get those lines.
```R
file_pdf <- file_pdf[-(grep("QUANTIDADE", file_pdf))]
file_pdf <- file_pdf[-(grep("Página", file_pdf))]
file_pdf <- file_pdf[-(grep("Quantidade", file_pdf))]
```
Now we split each line of the file using *strsplit* function, using spaces as the separator.
```R
file_pdf <- strsplit(file_pdf, "  ")
```
Some lines got the *character(0)*. We set them to NULL to remove them from the list.
```R
file_pdf[which((lapply(file_pdf,length)==0) == TRUE)] <- NULL
```
Now we create a list with each string of the file.
```R
list_pdf <- 0
a <- 1
for(i in 1:length(file_pdf)){
    info_number <- length(file_pdf[[i]][!stri_isempty(file_pdf[[i]])])
    for(j in 1:info_number){
        list_pdf[a] <- str_trim(file_pdf[[i]][!stri_isempty(file_pdf[[i]])][j])
        a <- a + 1
    }
}
```
We will simulate a dataframe with four colunms using the list. If you check the pdf, you will see four colunms, but beside the city names we do not have any information. So, we will insert beside the name of each city a special symbol that we will use after. To identify the name of each citiy, we will use the *grepl* function, that allow us to identify capitalized words (cities have its names capitalized). We use *str_replace_all* to remove spaces and special characters from the city names and *append* to insert the special symbol in the list.
```R
for(i in 1:(length(list_pdf)+600)){
    aux <- grepl("^[[:upper:]]+$", str_replace_all(str_replace_all(str_replace_all(list_pdf[i], fixed(" "), ""),fixed("."), ""),"-",""))
    if(aux){
        list_pdf<-append(list_pdf, "%", after=i) 
    }
}
```
We create a dataframe that will receive all data from the list.
```R
data_pdf <- data.frame(stringsAsFactors = FALSE, "City" = list_pdf[grepl("^[[:upper:]]+$", 
str_replace_all(str_replace_all(str_replace_all(list_pdf, fixed(" "), ""),fixed("."), ""),"-","")) == TRUE], 
"Clinic" = 0, "Company" = 0, "Physiotherapist" = 0, "PhilanthropicEntity" = 0, "PublicAgency" = 0, "OccupationalTherapist" = 0)
```
The most important and difficult part arises: the need to seperate what data is a city, or number, or metadata, and so on. We first check every string of the list. If it is a city, we proceed to the next step: we check if the city is "VILA MARIA". Why? you may ask. This city is a marker of the last page, that has only one colunm, and there the reading pattern changes.
After that, we use an increment to read the lines next to the city name. We use +4 to read exactly the register above the city name, and +2 to do the same with the registers from the last page.
The while loop checks: count > 6 means that we read all six essential information about the city, no need to continue; and if the register is not a city, because if it is, we need to stop because we already read everything for the current city.
Everything in the while loop just verify the information in the current list element to know where to put in the dataframe. See that we almost always have the pattern <Information, value, Information, value>. This "almost" is sadly our source of failing that we will show next.
```R
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
```
You can see that some city names are wrong. To correct them, we will first get the shapefiles data, and with them, the city names. But first, we will get the [population](https://sidra.ibge.gov.br/tabela/200) data from IBGE and merge it with the shapefile (of course, we need first to do some cleaning).
```R
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
```
You can see that we have some replicated data. We removed the replicas with less significance.
```R
data_pdf <- data_pdf[-356,]
data_pdf <- data_pdf[-238,]
data_pdf <- data_pdf[-375,]
data_pdf <- data_pdf[-435,]
data_pdf <- data_pdf[-435,]
```
Now, to avoid problems, we insert new rows with the cities from shapefile not present in the pdf file.
```R
missing_cities <- data.frame(City = shape_rs@data$NOME[!shape_rs@data$NOME %in% data_pdf$City],"Clinic" = 0, "Company" = 0, "Physiotherapist" = 0, "PhilanthropicEntity" = 0, "PublicAgency" = 0, "OccupationalTherapist" = 0)
colnames(missing_cities) <- colnames(data_pdf)
data_pdf <- rbind(data_pdf, missing_cities)
```
The numeric information need to be converted.
```R
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
```
To create maps with *ggplot*, we need to transform the shapefile to dataframe format.
```R
shape_rs@data$id <- c(1:nrow(shape_rs@data))
shapefile_df <- fortify(shape_rs, region = 'id') %>% mutate(id = as.numeric(id))
shapefile_df<-sp::merge(shapefile_df, shape_rs@data,all = TRUE,by="id")
```
And here we need to back again to the *we almost always have the pattern <Information, value, Information, value>*. There are some cases that it is not true. The cases are when we have one City in position 'i' and other City in position 'i + 4'. This problem do not allow us to algorithmically read the data for this cities, corrupting information for some of the next cities from the list too. The remaining alternative was to manually check the pdf file and compare it with the dataframe, and correct the wrong data. You can see an example above, but the full code is available in my project page. 
```R
data_pdf$Physiotherapist[data_pdf$City=="alegria"] <- 0
data_pdf$Physiotherapist[data_pdf$City=="arroio do sal"] <- 9
data_pdf$Clinic[data_pdf$City=="arroio do sal"] <- 4
data_pdf$PublicAgency[data_pdf$City=="arroio do sal"] <- 1
```
At last, we join the data about physiotherapists with the shapefile.
```R
shape_rs<-sp::merge(shapefile_df, data_pdf, by.x="NOME", by.y="City")
```
To conclude, we will analyse two quantiles: one from physiotherapists number and other from the tax population by physiotherapists. With that values we generate the range of values and colors to ggplot.
```R
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
```
And finally, the two final maps about the physiotherapists in Rio Grande do Sul cities, from Brazil.
```R
p <- ggplot() +
    geom_polygon( data = shape_rs, aes( fill = cat2, group = group, x = long, y = lat)) +
    geom_path( data = shape_rs, aes( x = long, y = lat, group = group), color = "black", size = 0.1) +
    coord_equal() +
    theme( legend.position = "bottom", legend.title = element_text( size = 18, hjust = 0.5),
        legend.text = element_text( size = 14), plot.title = element_text( size = 18, hjust = 0.5)) +
    labs( x = NULL, y = NULL, title = "Physiotherapists/Population Tax in Rio Grande do Sul - Brazil - Source: Crefito and IBGE, 2019.") +
    scale_fill_manual( values = rev(colorRampPalette(brewer.pal(5, "Spectral"))(5)),
        name = "People by Physiotherapists",
        drop = FALSE,
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit( 6, units = "mm"), keywidth = unit( 40, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0.5,
            nrow = 1,
            byrow = T,reverse = T,
            label.position = "bottom"
        )
    )

p2 <- ggplot()+
    geom_polygon(data = shape_rs, aes(fill = cat, group = group, x = long, y = lat)) +
    geom_path(data = shape_rs, aes(x = long, y = lat, group = group), color = "black", size = 0.1) +
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
            byrow = T,reverse = T,
            label.position = "bottom"
        )
    )
```

![Alt text](figures/figure2.png?raw=true "Title")

![Alt text](figures/figure3.png?raw=true "Title")

Of course, we can save the data in .csv format or other to save us in the future from having exceptional effort to organize this data again.
That's it. Thank you guys and share it with your mates !!

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
