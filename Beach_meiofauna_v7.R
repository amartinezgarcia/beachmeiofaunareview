#######################################################
#
#  Beach meiofauna review
#  Martinez, Kohler, Fontaneto, Macher. Beach meiofauna
#  First version 14.12.2022
#  Current version 20.03.2023
#
#######################################################



### Part 0: Load the packages and the data --------------------------------

###### 0.1 Load packages -----------------------------
library(DescTools)
library(igraph)
library(ggplot2)
library(dplyr)


###### 0.1 Load functions -----------------------------

"%likec%" <- function (x, pattern) { 
  grepl(pattern, x, ignore.case=TRUE)}


"%ni%" <- Negate("%in%")


abspres <- function(data, sites.col, sp.col, keep.n = FALSE) {
  stopifnot(
    length(sites.col) == 1,
    length(sp.col) == 1,
    sites.col != sp.col,
    sites.col %in% 1 : ncol(data) | sites.col %in% names(data),
    sp.col %in% 1 : ncol(data) | sp.col %in% names(data),
    is.logical(keep.n)
  )
  presabs <- table(data[ ,c(sites.col, sp.col)])
  presabs <- as.data.frame(unclass(presabs))
  if (!keep.n)  presabs[presabs > 1] <- 1
  presabs <- data.frame(row.names(presabs), presabs)
  names(presabs)[1] <- names(subset(data, select = sites.col))
  rownames(presabs) <- NULL
  return(presabs)
}


###### 0.2 Set working directory --------------------------------------------------------------

setwd("~/Dropbox/_Papers/_Beach_Meiofauna_Jan/0WoS_download")



###### 0.3 Load the reference dataset ----------------------------------------------------------

references <- data.table::fread("allreferences.txt", sep="\t",quote="")
references <- as.data.frame(references)
references <- references[ which (references$PT == "J"),]
references <- references[c("PT","AU","TI","SO","LA","DE","ID",
                           "NR","TC","Z9","U1","U2","J9","PY",
                           "WC","SC","UT","AB")]



### set all to lower case 
references[,c(1:ncol(references))]<-lapply(references[,c(1:ncol(references))],tolower)

### Number of years
range(references$PY)

### Save the beach dataset (Supplementary File 1)

# write.csv2(references,"SupplementaryFile1_allreferences.csv")





#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

### PART 1: Meiofauna versus macrofauna -------------------------------------------------

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 



#### 1.1 Prepare list of keywords for fauna ------------------------------------------

keyw.general <- as.data.frame(cbind(c("meio", "macrofaun","macrobenth"), 
                                    c("meiofauna", "macrofauna","macrofauna"),
                                    rep("general",3),
                                    rep("0",3)))

keyw.meio <- as.data.frame(cbind(c("copepod","nematod","rotifer","halacari","platyhelmint",
                         "annelid","gastrotrich","kinorhynch","loricif","gnathostomulid",
                         "tardigra","polychaet","oligochaet","clitellat",
                         "proseriat","rhabdocoel","turbellari","acoel",
                         "nemertodermatid"), 
                         c("Copepoda", "Nematoda","Rotifera","Halacaridae","Platyhelminthes",
                           "Annelida","Gastrotricha","Kinorhyncha","Loricifera","Gnathostomulida",
                           "Tardigrada","Annelida","Annelida","Annelida",
                           "Platyhelminthes","Platyhelminthes","Platyhelminthes","Acoelomorpha","Acoelomorpha"),
                         rep("meiofauna",19),
                         rep("0",19)))
                       
keyw.macro <- as.data.frame(cbind(c("crab","turtle","seal","bird","wader","fish",
                         "penguin","whale", "talitri","insect"),
                         c("Crab","Turtle","Seals","Birds","Waders","Fish",
                           "Penguins","Whales", "Talitridae","Insects"),
                         rep("macrofauna",10),
                         rep("0",10)))


### Make a common file with all macrofauna and meiofauna keywords
keyw.fauna <- do.call("rbind", list(keyw.general, keyw.macro, keyw.meio))
rm(keyw.general, keyw.macro, keyw.meio)
colnames(keyw.fauna) <- c("keyword","animal.group","category","frequency")



#### 1.2 Annotate the papers and count the frequency of the keyword ----------------

# NOTE: In the MS, the match of meiofaunal and macrofaunal terms are described separately, 
# even we run the search in parallel to simplify the script.


refs.fauna <- data.frame()
for (i in (1:nrow(keyw.fauna))){
  
  ### we match all papers against the i keyword
  papers.i <- references[ which (references$TI %likec% keyw.fauna$keyword[i] |
                                   references$AB %likec% keyw.fauna$keyword[i]),]
  
  ### we collect information if there are matches
  if(nrow(papers.i) > 0) {

  papers.i$word <- keyw.fauna$keyword[i]
  papers.i$cat <- keyw.fauna$category[i]
  papers.i$animal <- keyw.fauna$animal.group[i]
  
  keyw.fauna$frequency[i] <- nrow(papers.i)
  refs.fauna <- rbind(refs.fauna,papers.i)}
 
}

refs.fauna$animal <- as.factor(refs.fauna$animal)
str(refs.fauna)

### Save summary of matches

# write.csv2(keyw.fauna, "TableS1_animalgroups.csv", row.names = FALSE)


#### Production of the beach meiofauna dataset (Supplementary file 2)

refs.meiofauna <- data.frame(refs.fauna[which (refs.fauna$cat == "meiofauna" |
                                                refs.fauna$animal == "meiofauna"),])


nrow(refs.meiofauna[ !duplicated(refs.meiofauna$UT),])

refs.dropmeio <- read.csv2("refs_nomeio.csv") ## manually checked papers not on meio
refs.meiofauna <- refs.meiofauna[ which (refs.meiofauna$UT %ni% refs.dropmeio$UT),] # no macro

### add Jan's and Sören's annotations
annotations <- read.csv2("Jan_annotated_meiofauna.csv")
refs.meiofauna <- merge(refs.meiofauna,annotations, by="UT", all.x = T)
refs.meiofauna <- refs.meiofauna[,c(-27,-26)]


 write.csv2(refs.meiofauna, "SupplementaryFile2_meiofauna.csv", row.names = FALSE)


refs.meiofauna <- refs.meiofauna[ !duplicated(refs.meiofauna$UT),] ## no duplicates 
refs.meiofauna <- refs.meiofauna[,-c(19:21)]




print(paste0("Number of unique meiofaunal references = ", 
      length(unique(refs.meiofauna$UT))))


rm(papers.i)


#### 1.3 Prepare barplot ---------------------------------------------------------

unique(refs.fauna$animal)

refs.fauna <- refs.fauna[ which (refs.fauna$UT %ni% refs.dropmeio$UT),] # no macro

list.animals <- as.data.frame(matrix(ncol = 4, nrow = nlevels(refs.fauna$animal)))
colnames(list.animals) <- c("animal.group","total","overlap","nooverlap")



### Categorize papers dealing with meiofauna, a phylum, or both

for (i in (1:nlevels(refs.fauna$animal))){
  
  animal.i <- levels(refs.fauna$animal)[i]
  
  list.animals$animal.group[i] <- levels(refs.fauna$animal)[i]
  list.animals$total[i] <- length(unique(refs.fauna[which (refs.fauna$animal == animal.i),"UT"]))
  
  list.animals$overlap[i] = length(intersect(refs.fauna[which (refs.fauna$animal == "meiofauna"),"UT"], 
                                    unique(refs.fauna[which (refs.fauna$animal == animal.i),"UT"])))

  list.animals$nooverlap[i] = length(unique(refs.fauna[which (refs.fauna$animal == animal.i),"UT"])) -
                       length(intersect(refs.fauna[which (refs.fauna$animal == "meiofauna"),"UT"], 
                                        unique(refs.fauna[which (refs.fauna$animal == animal.i),"UT"])))
}

list.animals <- unique(merge(list.animals, keyw.fauna[,c(2:3)], by = "animal.group"))



### Re-arragenge the dataframe for the plot

list.animals.plot <- data.frame()

for (i in (1:nrow(list.animals))){
    word.i <- list.animals[i,]
    word.ia <- c(list.animals$animal.group[i],
                 list.animals$category[i],
                 list.animals$overlap[i],
                           "overap")
    word.ib <- c(list.animals$animal.group[i],
                 list.animals$category[i],
                 list.animals$nooverlap[i],
                            "nooverlap")
    words <- as.data.frame(rbind(word.ia,word.ib))
    colnames(words) <- c("category","word","frequency","level")
    
    list.animals.plot <- rbind(list.animals.plot, words)
}

list.animals.plot$level <- as.factor(list.animals.plot$level)
list.animals.plot$frequency <- as.numeric(list.animals.plot$frequency)
str(list.animals.plot)


### Plot both barplots

cb1 <-  ggplot(list.animals.plot[ which (list.animals.plot$category == "meiofauna" |
                                           list.animals.plot$word == "meiofauna"), ],
          aes(x=reorder(category, frequency), y=frequency, fill=level)) +
          geom_bar(alpha = 0.7, stat = "identity",  position = "stack") +
          #scale_fill_manual(values=c("#3c1407")) +
          #geom_bar(stat = "identity", position = "stack", width = 1) +
          scale_fill_manual(values = c("#767676","#3c1407")) +
          ggtitle("A") +
          theme_classic() +
          labs(fill="") + xlab("") + ylab("") +
          coord_flip() +
          theme(legend.position="none")

cb2 <-  ggplot(list.animals[ which (list.animals$category == "macrofauna" |
                                    list.animals$animal.group == "macrofauna" |
                                    list.animals$animal.group == "meiofauna"), ],
              aes(x=reorder(animal.group, total), y=total)) +
              geom_bar(stat = "identity") +
              scale_fill_manual(values = c("#767676","#3c1407")) +
              theme_classic() +
              ggtitle("B") +
              labs(fill="") + xlab("") + ylab("") +
              coord_flip() +
              theme(legend.position="none")


lemon::grid_arrange_shared_legend(cb1, cb2, ncol = 2, nrow = 1, position='right')



### Clean the house
rm(cb1,cb2)
rm(list.animals, list.animals.plot, keyw.fauna, papers.i, word.i, words)
rm(animal.i, word.ia, word.ib, refs.dropmeio)




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

#### PARTE 2: Word clouds -----------------------------------------------------------------------

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 



##### 2.1 Meiofauna beach wordcloud -------------------------------------------------------------------

  
refs.fauna <- refs.fauna[ !duplicated(refs.fauna$UT),]

 
### Clean the keywords in the "meiofauna beach dataset"

keywords <- unique(tolower( unlist(strsplit(refs.meiofauna$DE,"; "))))

      keywords <- gsub(" beaches","",keywords)
      keywords <- gsub("beaches ","",keywords)
      keywords <- gsub("beaches","",keywords)
      keywords <- gsub(" beach","",keywords)
      keywords <- gsub("beach ","",keywords)
      keywords <- gsub("beach","",keywords)
      keywords <- gsub("coastal erosion","erosion",keywords)
      keywords <- gsub("sediment transport","transport",keywords)
      keywords <- gsub("coastal management","management",keywords)
      keywords <- gsub("polychaetes","polychaeta",keywords)
      keywords <- gsub("copepoda","copepod",keywords)
      keywords <- gsub("copepods","copepod",keywords)
      keywords <- gsub("nematodes","nematoda",keywords)
      keywords <- gsub("nematode","nematoda",keywords)
      keywords <- gsub("polychaete","polychaeta",keywords)
      keywords <- gsub("gastrotrich","gastrotricha",keywords)
      keywords <- gsub("gastrotrichs","gastrotricha",keywords)
      keywords <- gsub("gastrotrichaa","gastrotricha",keywords)
      keywords <- gsub("harpacticoid copepod","harpacticoida",keywords)
      keywords <- gsub("new records","new record",keywords)
      keywords <- gsub("free-living marine nematode","free-living marine nematodes",keywords)
      keywords <- unique(keywords)
      
      

meio.b <- tolower( unlist(strsplit(refs.meiofauna$DE,"; ")))
      meio.b <- gsub(" beaches","",meio.b)
      meio.b <- gsub("beaches ","",meio.b)
      meio.b <- gsub("beaches","",meio.b)
      meio.b <- gsub(" beach","",meio.b)
      meio.b <- gsub("beach ","",meio.b)
      meio.b <- gsub("beach","",meio.b)
      meio.b <- gsub("coastal erosion","erosion",meio.b)
      meio.b <- gsub("sediment transport","transport",meio.b)
      meio.b <- gsub("coastal management","management",meio.b)
      meio.b <- gsub("polychaetes","polychaeta",meio.b)
      meio.b <- gsub("copepoda","copepod",meio.b)
      meio.b <- gsub("copepods","copepod",meio.b)
      meio.b <- gsub("nematodes","nematoda",meio.b)
      meio.b <- gsub("nematode","nematoda",meio.b)
      meio.b <- gsub("polychaete","polychaeta",meio.b)
      meio.b <- gsub("gastrotrich","gastrotricha",meio.b)
      meio.b <- gsub("gastrotrichs","gastrotricha",meio.b)
      meio.b <- gsub("gastrotrichaa","gastrotricha",meio.b)
      meio.b <- gsub("new records","new record",meio.b)
      meio.b <- gsub("free-living marine nematode","free-living marine nematodes",meio.b)
      

meio.keywords <- vector()

      for (i in (1:length(keywords))){
        #meio.b <- tolower( unlist(strsplit(refs.meiofauna$DE,"; ")))
        meio.keywords.i <- length(which(keywords[i] == meio.b))
        meio.keywords.i <- as.data.frame(cbind(keywords[i],meio.keywords.i))
        meio.keywords <- rbind(meio.keywords,meio.keywords.i)
      }

colnames(meio.keywords) <- c("keyword","frequency")

meio.keywords$frequency <- as.numeric(meio.keywords$frequency)
meio.keywords <- meio.keywords[order(meio.keywords$frequency, decreasing = TRUE), ]
meio.keywords <- meio.keywords[ which (meio.keywords$frequency > 1), ]

meio.keywords <- meio.keywords [ which (meio.keywords$keyword != "meiofauna"),]

write.csv2(meio.keywords, "TableXX_meiofuana")

wordcloud2::wordcloud2(meio.keywords, size = 0.9)
  
rm(meio.keywords.i,meio.b,keywords)



##### 2.2. Beach dataset wordcloud --------------------------------------------------------------

keywords.all <- unique(tolower( unlist(strsplit(references$DE,"; "))))


## Homogeneize keywords

        keywords.all <- gsub(" beaches","",keywords.all)
        keywords.all <- gsub("beaches ","",keywords.all)
        keywords.all <- gsub("beaches","",keywords.all)
        keywords.all <- gsub(" beach","",keywords.all)
        keywords.all <- gsub("beach ","",keywords.all)
        keywords.all <- gsub("beach","",keywords.all)
        keywords.all <- gsub("coastal erosion","erosion",keywords.all)
        keywords.all <- gsub("sediment transport","transport",keywords.all)
        keywords.all <- gsub("coastal management","management",keywords.all)
        keywords.all <- gsub("polychaetes","polychaeta",keywords.all)
        keywords.all <- gsub("microplastic","microplastics",keywords.all)
        keywords.all <- gsub("surfzone","surf zone",keywords.all)
        keywords.all <- gsub("surf-zone","surf zone",keywords.all)
        keywords.all <- gsub("surf-zones","surf zone",keywords.all)
        keywords.all <- gsub("microplasticss","microplastics",keywords.all)
        keywords.all <- gsub("polychaetes","polychaeta",keywords.all)
        keywords.all <- gsub("copepoda","copepod",keywords.all)
        keywords.all <- gsub("copepods","copepod",keywords.all)
        keywords.all <- gsub("nematodes","nematoda",keywords.all)
        keywords.all <- gsub("nematode","nematoda",keywords.all)
        keywords.all <- gsub("polychaete","polychaeta",keywords.all)
        keywords.all <- gsub("gastrotrich","gastrotricha",keywords.all)
        keywords.all <- gsub("gastrotrichs","gastrotricha",keywords.all)
        keywords.all <- gsub("gastrotrichaa","gastrotricha",keywords.all)
        keywords.all <- gsub("harpacticoid copepod","harpacticoida",keywords.all)
        keywords.all <- gsub("new records","new record",keywords.all)
        keywords.all <- gsub("free-living marine nematode","free-living marine nematodes",keywords.all)
        keywords.all <- gsub("sediments","sediment",keywords.all)
        keywords.all <- unique(keywords.all)

all.keywords <- references[ which (references$DE != ""),]
all.keywords <- tolower( unlist(strsplit(all.keywords$DE,"; ")))

        all.keywords <- gsub(" beaches","",all.keywords)
        all.keywords <- gsub("beaches ","",all.keywords)
        all.keywords <- gsub("beaches","",all.keywords)
        all.keywords <- gsub(" beach","",all.keywords)
        all.keywords <- gsub("beach ","",all.keywords)
        all.keywords <- gsub("beach","",all.keywords)
        all.keywords <- gsub("coastal erosion","erosion",all.keywords)
        all.keywords <- gsub("sediment transport","transport",all.keywords)
        all.keywords <- gsub("coastal management","management",all.keywords)
        all.keywords <- gsub("microplastic","microplastics",all.keywords)
        all.keywords <- gsub("surf-zone","surf zone",all.keywords)
        all.keywords <- gsub("surf-zones","surf zone",all.keywords)
        all.keywords <- gsub("surfzone","surf zone",all.keywords)
        all.keywords <- gsub("microplasticss","microplastics",all.keywords)
        all.keywords <- gsub("polychaetes","polychaeta",all.keywords)
        all.keywords <- gsub("copepoda","copepod",all.keywords)
        all.keywords <- gsub("copepods","copepod",all.keywords)
        all.keywords <- gsub("nematodes","nematoda",all.keywords)
        all.keywords <- gsub("nematode","nematoda",all.keywords)
        all.keywords <- gsub("polychaete","polychaeta",all.keywords)
        all.keywords <- gsub("gastrotrich","gastrotricha",all.keywords)
        all.keywords <- gsub("gastrotrichs","gastrotricha",all.keywords)
        all.keywords <- gsub("gastrotrichaa","gastrotricha",all.keywords)
        all.keywords <- gsub("harpacticoid copepod","harpacticoida",all.keywords)
        all.keywords <- gsub("new records","new record",all.keywords)
        all.keywords <- gsub("free-living marine nematode","free-living marine nematodes",all.keywords)
        all.keywords <- gsub("sediments","sediment",all.keywords)

 
               
complete.keywords <- data.frame() ## ca. 5 mins to complete
      for (i in (1:length(keywords.all))){
        print(paste0(i," of ", length(keywords.all)))
        all.keywords.i <- length(which(keywords.all[i] == all.keywords))
        all.keywords.i <- as.data.frame(cbind(keywords.all[i],all.keywords.i))
        complete.keywords <- rbind(complete.keywords,all.keywords.i)
      }


colnames(complete.keywords) <- c("keyword","frequency")
complete.keywords$frequency <- as.numeric(complete.keywords$frequency)
complete.keywords <- complete.keywords[order(complete.keywords$frequency, decreasing = TRUE), ]
complete.keywords2 <- complete.keywords[which (complete.keywords$frequency > 10), ]

wordcloud2::wordcloud2(complete.keywords2, size=0.5)



## Write supplementary files
write.csv2(meio.keywords, "SupplementaryFile6_Meiokeywords.csv")
write.csv2(complete.keywords, "SupplementaryFile5_Allkeywords.csv")


## clean the house!
rm(all.keywords.i, complete.keywords, complete.keywords2)
rm(word.ib, keywords, keywords.all, all.keywords)






#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

##### PART 3: Citation network ---------------------------------------------------------

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 




#### 3.1. Preparation of the citation vector (takes long time) -------------------------

# citations <- data.table::fread("allreferences.txt", sep="\t")
# citations <- as.data.frame(citations)
# citations <- citations[c("UT","DI","CR")]
# citations <- citations[ which (citations$DI != ""),]
# 
# 
# citas.vector <- data.frame()
# 
# ### LONG, do it before bed!
# for (i in 1:12053){
#   print(paste0(i," of 12053"))
#   paper.w <- citations$UT[i]
#   paper.i <- citations$DI[i]
#   cita.i <- citations[grep(paper.i,citations$CR),]
#   
#   cita.clean.i <- data.frame(rep(paper.w,nrow(cita.i)),cita.i$UT)
#   citas.vector <- rbind(citas.vector,cita.clean.i)
#    }
# 
# colnames(citas.vector) <- c("sources","targets")
# write.csv2(citas.vector, "citas2.csv")
 



#### 3.2. Cleaning the citations vector --------------------------------------------------------------

citas.vector <- read.csv2("citation_network.csv") #calculated as above

#delete rownames
citas.vector <- citas.vector[,-1] 

#delete incomplete
citas.vector <-citas.vector[ citas.vector$sources != 'UT',] 

## delete redundant links
citas.vector <- citas.vector[ which (citas.vector$targets %in% 
                                       citas.vector$sources),]

# turn into factor
citas.vector[,c(1:2)] <- lapply(citas.vector[,c(1:2)],as.factor) 

# lower case
citas.vector[,c(1:2)]<-lapply(citas.vector[,c(1:2)],tolower) 


## Flag "beach meiofauna dataset" papers and colour nodes
# refs.meiofauna = data.frame(refs.fauna[which (refs.fauna$cat == "meiofauna" |
#                                                 refs.fauna$animal == "meiofauna"),])
meiofauna <- data.frame(cbind(refs.meiofauna$UT, rep("blue", nrow(refs.meiofauna))))
colnames(meiofauna) <- c("sources","meiofauna")


citas.vector <- merge(citas.vector,meiofauna, by="sources", all.x = T)
citas.vector$meiofauna[is.na(citas.vector$meiofauna)] <- "grey50"
citas.vector$meiofauna<- as.factor(citas.vector$meiofauna)
citas.vector <- unique(citas.vector)



#### 3.3. Plotting the network --------------------------------------------------------------

network1 <- network::network(citas.vector, loops = T)

levels(citas.vector$meiofauna) <- c("blue","grey50")


network::plot.network(network1, attrname = NULL,
                      #label = network.vertex.names(v1),
                      thresh = 0, 
                      usearrows = TRUE, 
                      mode = "fruchtermanreingold",
                      jitter=T,
                      displayisolates = TRUE, 
                      interactive = FALSE,  ##<------ nice
                      xlab = NULL,
                      ylab = NULL, 
                      xlim = NULL, 
                      ylim = NULL, 
                      pad = 0.2, 
                      label.pad = 0.5,
                      boxed.labels = FALSE, 
                      label.pos = 1,
                      label.bg = "white",
                      vertex.sides = 10, 
                      vertex.rot = 0, vertex.lwd=0.1,
                      arrowhead.cex = 0, 
                      label.cex = 0.1, 
                      loop.cex = 1, 
                      vertex.cex = 0.5, #size
                      edge.col = 1, 
                      label.col = 1, 
                      vertex.col = as.character(citas.vector$meiofauna), 
                      #vertex.col = "blue", 
                      label.border = 1,
                      vertex.border = 0.1, 
                      edge.lty = 0.5, label.lty = NULL, 
                      vertex.lty = 1,
                      edge.lwd = 0.1, 
                      edge.label = NULL, 
                      edge.label.cex = 1,
                      edge.label.col = 1, 
                      label.lwd = par("lwd"), edge.len = 1000,
                      edge.curve = 1, edge.steps = 50, loop.steps = 20,
                      object.scale = 0.01)


  rm(network1,meiofauna)





#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

#### PART 4: Threads and Ecosystem services ----------------------------------------------

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 


## Vector of meiofauna papers

refs.meiofauna$meiofauna <- "yes"





### 4.1 Threats --------------------------------------------------------------------------


# Load the thread keywords

threat.keywords <- read.csv2("threats_clean.csv"); colnames(threat.keywords) <- c("threat.type", "threat")


# Match thread keywords with papers

threat.keywords.sum <- data.frame(threats_type = unique(threat.keywords$threat.type),
                              frequency = rep(NA,length(unique(threat.keywords$threat.type))))

threat.summary <- list()
threat <- data.frame()


for (i in (1:nrow(threat.keywords.sum))){
            
            keywords.i <- threat.keywords[which (threat.keywords$threat.type == threat.keywords.sum[i,1]),2]
            threat.table.i <- data.frame()
            
        for (k in (1:length(keywords.i))){
            
          threat.paper.k <- references[ which (references$TI %likec% keywords.i[k] |
                                                references$AB %likec% keywords.i[k] |
                                                references$DE %likec% keywords.i[k]),]
            
                      threat.table.i <- rbind(threat.table.i, threat.paper.k)
                       
              }
            threat.summary[[i]] <- threat.table.i
            
            threat.i <- data.frame(UT = unique(threat.table.i$UT),
                                       keyword = rep(threat.keywords.sum[i,1],
                                                     length(unique(threat.table.i$UT)))
                              )    
            
            threat <-rbind(threat, threat.i)
            
            threat.keywords.sum$frequency[i] <- length(unique(threat.summary[[i]]$UT))
          }


threat.keywords.sum

print(paste0("Total of papers matching with threats = ",
              length(unique(threat$UT))))

rm(threat.table.i,threat.i,threat.paper.k)




### Flag meiofauna papers

threat <- merge(threat,refs.meiofauna[c("UT","meiofauna")], by="UT", all.x=T)
threat$meiofauna[is.na(threat$meiofauna)] <- "no"

write.csv2(merge(threat,references, by = "UT", all = F),
           "SupplementaryFile9_threatspapers.csv")

### Threats graphical output

# threats macrofauna plot
threat.macro <- threat[ which (threat$meiofauna == 'no'),]

print(paste0("Macrofaunal papers matching with threats = ",
             length(unique(threat.macro$UT))))

threat.macro <- as.data.frame(table(threat.macro$keyword))





threat.macro$fraction = threat.macro$Freq / sum(threat.macro$Freq)
threat.macro$ymax = cumsum(threat.macro$fraction)
threat.macro$ymin = c(0, head(threat.macro$ymax, n=-1))


threat.macro.plot <- ggplot(threat.macro, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect(alpha = 0.7) +
  #coord_polar(theta="y") +
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("black","#422DA8","#DAA314","#FCCD0F",
                               "#F7E0A6","#61615F","#435CD9")) +
  theme_void() + theme(legend.position = "none")



# threats meiofauna plot
threat.meio <- threat[ which (threat$meiofauna == 'yes'),]

print(paste0("Meiofaunal papers matching with threats = ",
             length(unique(threat.meio$UT))))


threat.meio <- as.data.frame(table(threat.meio$keyword))

threat.meio <- merge(threat.macro[c("Var1")],threat.meio,
                      by = "Var1",
                      all.x = T) ## fill missing threats

threat.meio[is.na(threat.meio)] <- 0


threat.meio$fraction = threat.meio$Freq / sum(threat.meio$Freq)
threat.meio$ymax = cumsum(threat.meio$fraction)
threat.meio$ymin = c(0, head(threat.meio$ymax, n=-1))


threat.meio.plot <- ggplot(threat.meio, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect(alpha = 0.7) +
  #coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("black","#422DA8","#DAA314","#FCCD0F",
                               "#F7E0A6","#61615F","#435CD9")) +
  theme_void() + theme(legend.position = "none")




rm(threat.keywords)





####### 4.2 Ecosystem Services ---------------------------------------------------------------------------

### Match all papers about services
service.keywords <- read.csv2("services_clean.csv")
colnames(service.keywords) <- c("services.type", "service.class", "service")


service.keywords.sum <- data.frame(service_type =unique(service.keywords$services.type),
                              frequency =rep(NA,length(unique(service.keywords$services.type))))


service.summary <- list()
service <- data.frame()

for (i in (1:nrow(service.keywords.sum))){
  
    keywords.i <- service.keywords[which (service.keywords$services.type == service.keywords.sum[i,1]),3]
    service.table.i <- data.frame()

                for (k in (1:length(keywords.i))){
                services.paper.k <- references[ which (references$TI %likec% keywords.i[k] |
                                                        references$AB %likec% keywords.i[k] |
                                                        references$DE %likec% keywords.i[k]),]
                
                              service.table.i <- rbind(service.table.i, services.paper.k)
       
                            }
    
          service.summary[[i]] <- service.table.i
          
          service.i <- data.frame(UT = unique(service.table.i$UT),
                                      keyword = rep(service.keywords.sum[i,1],
                                                    length(unique(service.table.i$UT)))
                                     )    
          
          service <-rbind(service, service.i)
          
          service.keywords.sum$frequency[i] <- length(unique(service.summary[[i]]$UT))
          
      }

service.keywords.sum

rm(service.table.i,service.i,services.paper.k)


### Flag service meiofauna papers

service <- merge(service,refs.meiofauna[c("UT","meiofauna")], by="UT", all.x=T)
service$meiofauna[is.na(service$meiofauna)] <- "no"

print(paste0("Total papers matching services = ",
             length(unique(service$UT))))

write.csv2(merge(service,references, by = "UT", all = F),
           "SupplementaryFile10_services.csv")

### Service papers graphical outputs

## service macrofauna selection
service.macro <- service[ which (service$meiofauna == 'no'),]

print(paste0("Macrofaunal papers matching services = ",
             length(unique(service.macro$UT))))

service.macro <- as.data.frame(table(service.macro$keyword))
service.macro$fraction = service.macro$Freq / sum(service.macro$Freq)

service.macro$ymax = cumsum(service.macro$fraction)
service.macro$ymin = c(0, head(service.macro$ymax, n=-1))


## service macrofauna plot
service.macro.plot <- ggplot(service.macro, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect(alpha = 0.8) +
  #coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("black","grey50","grey90")) +
  theme_void() + theme(legend.position = "none")

rm(service.macro,service.meio)


## service meiofauna selection
service.meio <- service[ which (service$meiofauna == 'yes'),]

print(paste0("Meiofaunal papers matching ecosystem services = ",
             length(unique(service.meio$UT))))

service.meio <- as.data.frame(table(service.meio$keyword))
service.meio$fraction = service.meio$Freq / sum(service.meio$Freq)

service.meio$ymax = cumsum(service.meio$fraction)
service.meio$ymin = c(0, head(service.meio$ymax, n=-1))

# service meiofauna plot
service.meio.plot <- ggplot(service.meio, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect(alpha = 0.8) +
  #coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("black","grey50","grey90")) +
  theme_void() + theme(legend.position = "none")



##### 4.3. Total meiofauna services and threats -----------------------------------------------------------------------

gridExtra::grid.arrange(service.meio.plot,
                        service.macro.plot,
                        threat.meio.plot,
                        threat.macro.plot,
                        ncol = 4, nrow=1)


threats <- cbind(threat.macro[,c(1,2)],threat.meio[,2])
threats$category <- "threats"
colnames(threats)[1:3] <- c("Keywords","Total","Meiofauna")

services <- merge(service.macro[,c(1,2)], service.meio[,c(1,2)], by = "Var1", all = T)
services$category <- "services"
colnames(services)[1:3] <- c("Keywords","Total","Meiofauna")

write.csv2(rbind(threats,services), "SupplementaryFile11_threatsservices_frequency.csv")

rm(threat.macro.plot,
   threat.macro,
   threat.meio,
   threat.meio.plot,
   service.macro.plot,
   service.meio.plot,
   service.macro,
   service.meio,
   service.keywords,
   threat.keywords,
   threat,
   threats,
   service,
   service.summary)




##### 4.4. Total meiofauna services and threats -----------------------------------------------------------------------

total.applied <- rbind(service,threat)
print(paste0("Total papers on services and threats = ", 
              nrow(total.applied[unique(total.applied$UT),])))


total.macro <- total.applied[which (total.applied$meiofauna == "no"),]
print(paste0("Macro papers on services and threats = ", 
             nrow(total.macro[unique(total.macro$UT),])))


total.meio <- total.applied[which (total.applied$meiofauna == "yes"),]
print(paste0("Meio papers on services and threats = ", 
             nrow(total.meio[unique(total.meio$UT),])))




### Dataframe connecting services and threats
servicethreats <- merge(service,threat, by="UT")
servicethreats <- servicethreats[,c(1,2,4,5)]

colnames(servicethreats) <- c("UT","service","threat","meiofauna")


### Summarize number of connected services and threats
meio.serv.sum <- abspres(data=servicethreats[which (servicethreats$meiofauna == "yes"),], 
                                        sites.col="service", sp.col="threat", keep.n = T)

macr.serv.sum <- abspres(data=servicethreats[which (servicethreats$meiofauna == "no"),], 
                                   sites.col="service", sp.col="threat",keep.n = T)

nrow(servicethreats[which (servicethreats$meiofauna == "yes"),])
nrow(servicethreats[which (servicethreats$meiofauna == "no"),])


##### 4.5. Chord diagrama connecting both -----------------------------------------------------------------------

rows1 <- meio.serv.sum[,1]
rows2 <- macr.serv.sum[,1]

meio.serv.sum <- meio.serv.sum[,-1]
macr.serv.sum <- macr.serv.sum[,-1]

meio.serv.sum <- apply(meio.serv.sum, 2, as.numeric)
macr.serv.sum <- apply(macr.serv.sum, 2, as.numeric)

row.names(meio.serv.sum) <- rows1
row.names(macr.serv.sum) <- rows2


rownames(macr.serv.sum) <- c("aCultural","aProvisioning","aRegulationandmaintenance")
rownames(meio.serv.sum) <- c("aCultural","aProvisioning","aRegulationandmaintenance")

meio.serv.sum <- as.matrix(meio.serv.sum)



grid.col = c(aCultural = "black",
             aProvisioning = "black",
             aRegulationandmaintenance = "black",
             Biologicalinvasions = "#435CD9",
             Climatechange = "#422DA8",
             Coastaldevelopmentandprotection = "#DAA314",
             Erosion = "#FCCD0F",
             Exploitation = "#F7E0A6",
             Pollution = "#61615F",
             Recreation = "#435CD9")



library(circlize)

## plot meiofauna chord
circos.clear()
circos.par(gap.degree = c(rep(2, nrow(meio.serv.sum)-1), 10, rep(2, ncol(meio.serv.sum)-1), 10))
chordDiagramFromMatrix(meio.serv.sum, transparency = 0.5, grid.col = grid.col, big.gap = 5)


### plot macrofauna chord
circos.clear()
circos.par(gap.degree = c(rep(2, nrow(macr.serv.sum)-1), 10, rep(2, ncol(macr.serv.sum)-1), 10))
circlize::chordDiagramFromMatrix(macr.serv.sum, transparency = 0.7, big.gap = 5,
                                 grid.col = grid.col)




rm(services.macro.sum, services.meio.sum, service.macro, service.meio, service.keywords.sum)
rm(threat.macro, threat.meio, threat.summary, threat.keywords.sum)
rm(total.applied, total.macro, total.meio)


rm(servicethreats)


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

######## PARTE 5: papers on molecular themes ------------------------------------------------------------------------------------

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

molecular <- references[ which (references$TI %likec% "metabarcoding" |
                                  references$TI %likec% "environmental dna" |
                                  references$TI %likec% "dna taxonomy" |
                                  references$TI %likec% "barcoding" |
                                  references$AB %likec% "metabarcoding" |
                                  references$AB %likec% "environmental dna" |
                                  references$AB %likec% "barcoding" |
                                  references$AB %likec% "dna taxonomy" |
                                  references$DE %likec% "metabarcoding" |
                                  references$DE %likec% "environmental dna" |
                                  references$DE %likec% "dna taxonomy" |
                                  references$DE %likec% "barcoding"),]


molecular <- data.frame(UT = unique(molecular$UT),
                           keyword = "molecular")

molecular <- merge(molecular,refs.meiofauna[c("UT","meiofauna")], by="UT", all.x=T)
molecular$meiofauna[is.na(molecular$meiofauna)] <- "no"

write.csv2(merge(molecular,references, by ="UT"),
           "SupplementaryFile12_molecular.csv",
           row.names = F)

rm(molecular,molecular.UT)


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

####### PART 7: Genera mentioned in the studies ----------------------------------------------

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

setwd("~/Dropbox/_Papers/_Beach_Meiofauna_Jan/0WoS_download")# nrow(refs.meiofauna[ which (refs.meiofauna$Genera.mentioned == ""),])
# 
# genera_raw <- as.data.frame(refs.meiofauna$Genera.mentioned)
# 
#   sep <- function(x) strsplit(x, ",")
#   genera_raw <- as.vector(unlist(apply(genera_raw, 1, sep)))
#   genera_raw <- gsub(" ","", genera_raw)
#   genera_raw <- unique(genera_raw)
# 
#   write.csv2(genera_raw, "genera_raw.csv")
#   
#   rm(genera_raw, sep)


 

#### 6.1. Matching postgis with the data of the genera in references --------------------------

setwd("~/Dropbox/_Papers/_Beach_Meiofauna_Jan/0WoS_download")# nrow(refs.meiofauna[ which (refs.meiofauna$Genera.mentioned == ""),])


### list of genera mentioned in the papers
genera <- read.csv2("List_genera_new.csv", sep = ";", dec = ".") 

  
## filter out genera that are not meiofaunal
genera <- genera[ which (genera$include == "YES"), ]  

table(genera$Group) 


## GenBank
  # genes.meio <- DBI::dbReadTable(DBI::dbConnect(RPostgres::Postgres(),
  #                                      dbname="stygofaunamundi_lv1"),
  #                            "Phylo.genes.v2") 
  # 
  # genes.meio <- genes.meio[ !is.na(genes.meio$Tax.name),]
  # 
  # genes.meio <- genes.meio[  !is.na(genes.meio$Gen.ID),]
  # 
  # genes.meio <- genes.meio[ which (genes.meio$Qual.gene %in% 
  #                                  c("18SrRNA",
  #                                    "COI")),]
  # 
  # taxa <- DBI::dbReadTable(DBI::dbConnect(RPostgres::Postgres(),
  #                                               dbname="stygofaunamundi_lv1"),
  #                                "Taxa.MAIN.v2")  
  # 
  # 
  # 
  # genes.meio <- genes.meio[c("Gen.ID","Tax.original","Gen.genbank.ID","Tax.name","Qual.gene")]
  # 
  # genes.meio <- merge(genes.meio,taxa[c("Tax.name","Tax.genus", "Tax.rank")])
  # genes.meio <- genes.meio[ !is.na(genes.meio$Tax.genus),]
  # genes.meio <- genes.meio[ which (genes.meio$Tax.genus != ""),]
  # 
  # saveRDS(genes.meio, "GenBank.meiofauna.R")

  genes.meio <- readRDS("GenBank.meiofauna.R")
  
  ## Merge gbif and genbank
  genes.match <- merge(genes.meio,
                      genera,
                      by.x = "Tax.genus",
                      by.y = "ScientificName_accepted",
                      all = F)


genes.sum <- genes.match %>%
  group_by(Tax.genus) %>%
  dplyr::summarise(n.sequence = n(),
                   n.COI = n_distinct(Gen.genbank.ID[Qual.gene == "COI"]),
                   n.18S = n_distinct(Gen.genbank.ID[Qual.gene == "18SrRNA"]))

genes.sum <- as.data.frame(genes.sum)


genes.no <- as.data.frame(genera[ which (genera$verbatimScientificName %ni% genes.sum$Tax.genus),"verbatimScientificName"])
colnames(genes.no) <- "Tax.genus"
genes.no$n.sequence <- 0
genes.no$n.COI <- 0
genes.no$n.18S <- 0

genes.sum <- rbind(genes.sum, genes.no)

genes.sum <- merge(genes.sum,
                    genera[c("verbatimScientificName","Group")],
                    by.x = "Tax.genus",
                    by.y = "verbatimScientificName")
rm(genes.no)

write.table(genes.sum, "SupplementaryFigure14_genesmatch.csv",
            sep = ";", dec = ".",
            row.names = F)

unique(genes.sum$Group)

rm(genera,genes.meio)



###### 6.2  Tree figure ---------------------------------------------------------------------


tre.meta <- ape::read.tree("genesplot.tre") ## tree with the necessary groups
plot(tre.meta)


plot(tre.meta,type="phylogram",FALSE,node.pos=2)


tre.meta$tip.label %ni% genes.sum$Group

tree.sum <- genes.sum %>%
  group_by(Group) %>%
  dplyr::summarise(total.genera = n(),
                   n.genera.18s = sum(n.18S),
                   n.genera.COI = sum(n.COI),
                   n.genus.genes = n_distinct(Tax.genus[n.COI != 0 | n.18S != 0]),
                   n.genus.nogenes = n_distinct(Tax.genus[n.sequence == 0]),
                   gene.freq = n_distinct(Tax.genus[n.sequence != 0])/n())

write.table(tree.sum, "Table2_sequencespergroup.csv",
            sep = ";", dec = ".",
            row.names = F)

tree.sum <- as.data.frame(tree.sum)

tree.sum <- tree.sum[order(match(tree.sum[,1],tre.meta$tip.label), 
                                   decreasing = TRUE),]

tree.sum.plot <- data.frame()

for (i in (1:nrow(tree.sum))){
  animal.i <- tree.sum[i,]
  animal.ib <- c(tree.sum$group[i],
                 tree.sum$n.genus.nogenes[i],
                 "nogenes")
  animal.ia <- c(tree.sum$group[i],
                 tree.sum$n.genus.genes[i],
                 "genes")
  animals <- as.data.frame(rbind(animal.ia,animal.ib))
  animals$group <- animal.i[,1]
  colnames(animals) <- c("count","level","Group")
  
  tree.sum.plot <- rbind(tree.sum.plot, animals)
}

tree.sum.plot$count <- as.integer(tree.sum.plot$count)
tree.sum.plot <- merge(tree.sum.plot, tree.sum[,c(1:2,5)], by="Group")

tree.sum.plot <- tree.sum.plot[order(match(tree.sum.plot[,1],tre.meta$tip.label), 
                           decreasing = TRUE),]

tree.sum.plot %>%
  #arrange(order) %>%
  ggplot( aes( x=Group, y=count, fill=level) ) +
  geom_bar(alpha = 0.7, stat = "identity",  position = "stack") +
  scale_fill_manual(values = c("#3c1407", "#767676")) +
  theme_classic() +
  labs(fill="") + xlab("") + ylab("") +
  coord_flip()




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

####### PART 8: Map ----------------------------------------------

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 



countries <- read.csv2("countries_1807.csv") ## Manually extracted by Jan and Sören

countries <- table(countries$country)
countries <- as.data.frame(countries)

colnames(countries) <- c("country","studies")

#Match country names
data_map <- joinCountryData2Map(countries, joinCode = "NAME", nameJoinColumn = "country", verbose = TRUE)

#Color palette
custom_palette <- c("1-5" = "#abdbe3",   # Light Green
                    "6-10" = "#f5dca4",  # Light Blue
                    "11-15" = "#eab676", # Light Red
                    "16-20" = "#e28743", # Light Yellow
                    "21+" = "#e66927")   # Light Purple

custom_palette <- c("1-5" = "#f5dca4",   # Light Green
                    "6-10" = "#eab676",  # Light Blue
                    "11-15" = "#937A62", # Light Red
                    "16-20" = "#492000", # Light Yellow
                    "21+" = "#362312")   # Light Purple


#Categories
data_map@data$study_category <- cut(data_map@data$studies,
                                    breaks = c(-Inf, 5, 10, 15,20, Inf),
                                    labels = c("1-5", "6-10", "11-15", "16-20", "20+"))

#Plot
map <- mapCountryData(data_map, nameColumnToPlot = "study_category", mapTitle = "Research Papers Published per Country",
                      catMethod = "categorical", colourPalette = custom_palette, addLegend = TRUE)

#Print map
print(map)