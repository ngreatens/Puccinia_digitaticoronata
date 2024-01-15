### SET WORKING DIRECTORY
setwd("PATH_TO_FILES")

#load packages. install if necessary: e.g. install.packages("example")
library(ggtree)
library(readxl)
library(glue)
library(dplyr)
library(tidyr)

#read in tree
tree <- read.tree("ITS2.nwk")

#read in data tables
df <- read_excel("loci.xlsx")
df <- df[df$ITS_tree == 'YES',]
df<- df[1:52,]


######################################################
####SOME REFORMATTING OF EXCEL FILE BEFORE TREE ######
######################################################

#move ITS column to front since the tip labels are named for ITS seqs
df<-df[,c(which(colnames(df)=="ITS"), which(colnames(df)!="ITS"))]
#add ".1" to all the genbank numbers in ITS column since that's the format they are in the tree
for (i in 1:nrow(df)){df$ITS[i]<-paste0(df$ITS[i], ".1", collapse = ",")}

#everything in the table with spaces must be put in quotes or eventually italics
#else it screws up the parse function
for (i in 1:nrow(df)){df$ID[i]<-paste0("'", df$ID[i], "'", collapse = ",")}
for (i in 1:nrow(df)){df$Origin[i]<-paste0("'", df$Origin[i], "'", collapse = ",")}
for (i in 1:nrow(df)){df$Year[i]<-paste0("'", df$Year[i], "'", collapse = ",")}

#separate species into genus, species, subsp, etc.
df<- separate(data = df, col = Host, into = c("Genus", "Species", "ssp", "ssp_name"), sep = " ")
#look at table to see if everything is right. in this case some species names arent given
#if ' "sp." move value to col ssp since that way it won't be italicized later
for (i in 1: nrow(df)){
  if (df$Species[i] == "sp."){df$Species[i]<- NA; df$ssp[i] <- "sp."}
}

#for this tree we need to find which organisms are only represented once
#you can't use the clade label feature to label single tips
un <- table(df$Organism)
nonun <- un[un!=1]
un <- un[un ==1]
df$Organism_uni <- NA
for (i in 1:nrow(df)){
  if(df$Organism[i] %in% names(un) == TRUE)
  {df$Organism_uni[i]<-df$Organism[i]}
}

#now separate into genus species
df<- separate(data = df, col = Organism_uni, into = c("Org_genus", "Org_species"), sep = " ")
#make field for names with italics. then delete the names for everything that doesnt need a label
df <- dplyr::mutate(df, OrgTipLabel = glue("~~italic({Org_genus})~italic({Org_species})"))
for(i in 1:nrow(df)){if(is.na(df$Org_genus[i])){df$OrgTipLabel[i]<-NA}}

#manual corrections for taxa with sub species, varieties
df$OrgTipLabel[2]<-"~~italic(P.)~italic(coronata)~italic(s.)~italic(l.)~var.~italic(gibberosa)"
df$OrgTipLabel[3]<-"~~italic(P.)~italic(coronata)~italic(s.)~italic(l.)~var.~italic(himalensis)"

#add asterisk to tip labels for taxa present in North America. 
for ( i in c(2, 22:25, 28, 44, 46, 48, 50)){df$OrgTipLabel[i]<-paste0(df$OrgTipLabel[i], '~\"*\"', collapse = ",")}

#next add column to code for color if host genus is poa
df$color <- NA
for (i in 1: nrow(df)){
  if(df$Genus[i] == "Poa") {df$color[i] <- "b"}
  else {df$color[i] <- "a"}
}

#use new table for other cladelabels
cladelabels <- data.frame(matrix(nrow = length(nonun), ncol = 2))
for ( i in 1: length(nonun))
{cladelabels[i,1] <- names(nonun)[i]}
colnames(cladelabels) <- c("Taxon", "Label")
cladelabels <- separate(data = cladelabels, col = Taxon, into = c("A", "B", "C", "D", "E", "F", "G"), sep = " ")
for ( i in 1: nrow(cladelabels))
{
  cladelabels[i,8]<-paste0("~italic(", cladelabels[i,1], ")", "~italic(", cladelabels[i,2], ")", collapse = ",");
  if ( !is.na(cladelabels[i,3])){cladelabels[i,8]<- paste0(cladelabels[i,8], "~", cladelabels[i,3], "~italic(", cladelabels[i,4], ")", collapse = ",")}
  if ( !is.na(cladelabels[i,5])){cladelabels[i,8]<- paste0(cladelabels[i,8], "~", cladelabels[i,5], "~", cladelabels[i,6], "~italic(", cladelabels[i,7], ")", collapse = ",")}
}
cladelabels$node <- NA
cladelabels$geom <- NA
cladelabels$color <- NA
cladelabels<-cladelabels[,8:10]

## add asterisks to taxa present in north america
for (i in c(1:4)){
  paste0(cladelabels$Label[i]<-paste0(cladelabels$Label[i], '~\"*\"',  collapse = ","))
}

#make geoms for clade labels. unfortunately geom_cladelabel doesn't accept mapping
#and geom_cladelab doesn't accept parsing as far as I can tell, so here we we are 
#preparing to copy paste into the row below.

nodes <- c(99, 73, 74, 77)
for ( i in 1: nrow(cladelabels)){cladelabels$node[i] <- nodes[i]}
#for (i in 1: nrow(cladelabels)){cat(cladelabels$geom[i], "\n")}

for (i in 1: nrow(cladelabels)){
  cladelabels$geom[i] <-  paste0("geom_cladelabel(node =", cladelabels$node[i], ", label = '", cladelabels$Label[i], 
                                 "', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset", collapse = ",")
} 
cladelabels$color <- NA
for (i in c(1, 4)){cladelabels$color[i] <- "textcolor"}
for (i in 1: nrow(cladelabels))
  {
  if(!is.na(cladelabels$color[i]))
    {cladelabels$geom[i] <- paste0(cladelabels$geom[i], ", color = ", cladelabels$color[i], ")", collapse = ",")}
  else {cladelabels$geom[i] <- paste0(cladelabels$geom[i], ")", collapse = ",")}
}
#find which nodes you need for which clade labels. Add geom text with nodes labeled temporarily
#and make note of which nodes need labeling
#ggtree(tree) %<+% df + geom_tiplab(aes(label = Organism), size = 2) + geom_text2(aes(label = node, subset =! isTip))
#nodes <- c(86, 84, 82, 61)
#for ( i in 1: nrow(cladelabels)){cladelabels$node[i] <- nodes[i]}
#for (i in 1: nrow(cladelabels)){cat(cladelabels$geom[i], "\n")}

#make new field for each tip
df <- dplyr::mutate(df, lab = glue("{ID}~{Origin}~{Year}~italic({Genus})~italic({Species})~{ssp}~italic({ssp_name})"))

#filter bootstrap values for >80
q <- ggtree(tree)
d <- q$data
#select internal nodes
d <- d[!d$isTip,]
#convert to numeric
d$label <- as.numeric(d$label)
#select labels more than some value
d80 <- d[d$label>0.8,]
#change to percent and round to nearest integer
d80$label70 <- round(d80$label*100, digits = 0)

# unfortunately some values will overlap with the tree and the data must be subset and manually adjusted
# to have a nice looking figure
d80b <- d80[7, ]
d80b$label80b <- d80b$label80
d80c <- d80[4, ]
d80c$label80c <- d80c$label80
d80<- d80[c(2:3, 5,6, 8:9),]

#add textsize, textcolor, and cladeoffset variables for ease of change

textsize <- 2.2
cladeoffset <- 1.12
textcolor <- "green4"
  
############################
########FORMAT TREE#########
############################

#tree
p<- ggtree(tree) %<+% df +
  geom_tiplab(
    aes(label=OrgTipLabel, color = color),
    linesize = .15, 
    offset = (cladeoffset + .005), ##add a little to offset bc of bar in clades
    size = textsize,
    parse = TRUE,
    align = TRUE,
    show.legend = FALSE
  ) +
### all the clade labels. probably there's a better way to do this, but this works
geom_cladelabel(node =59, label = '~italic(P.)~italic(coronata)~var.~italic(avenae)', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset, color = textcolor) + 
geom_cladelabel(node =73, label = '~italic(P.)~italic(coronata)~var.~italic(avenae)', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
geom_cladelabel(node =74, label = '~italic(P.)~italic(coronata)~var.~italic(coronata)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
geom_cladelabel(node =77, label = '~italic(P.)~italic(digitaticoronata)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset, color = textcolor) +
geom_cladelabel(node =59, label = '~~~~~~~~~f.~sp.~italic(avenae)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset, color = textcolor, vjust = 1.5) + 
geom_cladelabel(node =73, label = '~~~~~~~~~f.~sp.~italic(graminicola)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset,  vjust = 1.5) +
geom_cladelabel(node =101, label = '~Outgroup', fontsize = textsize, parse = TRUE , offset = cladeoffset*1.675, barsize = .5)  +
geom_cladelabel(node =101, label = '~~taxa', fontsize = textsize, parse = TRUE , offset = cladeoffset*1.675, barsize = .5,  vjust = 1.75)  +

#Put black dots over green dots using empty tip labels 
  geom_tiplab(
    aes(label = ""),
    linesize = .15, 
    offset = (cladeoffset + .005),
    size = textsize,
    align = TRUE,
  ) +
  
  #tip labels. Put text on label with no border or padding to cover up dots from first tip label (2nd "clade" label)
  geom_tiplab(
    aes(label = lab, color = color), #format excel with a Color column
    linesize = .15,
    geom = "label",
    label.size = 0,
    label.padding = unit(0, "lines"),
    parse = TRUE,
    align = TRUE,
    size = textsize,
    show.legend = FALSE,
  )+
  
  
#add tips again but with empty quotes as labels to make the dots black
  geom_tiplab(
    aes(label = ""),
    linesize = .15,
    align = TRUE,
    size = textsize,
  )+
#add colors
  scale_color_manual(values=c("black", textcolor)) + 
  
#size of plot
  xlim(NA, 2.75) + ##adjust right value to widen (smaller value) or narrow (bigger value) tree
  
#scale bar
  geom_treescale(fontsize = textsize*.8, x = 0, y = 0) 

#now rotate a few branches so outgroup taxa are on bottom and Pca is on top
p<- flip (p, 49, 101)

### add "strips", or clade labels for polyphyletic groups (at least per this ITS2 tree. P. series Coronata is monophyletic)
p<- p + geom_striplab('MW404745.1', 'GU598100.1', label = ' Puccinia', fontsize = textsize, color = "black", offset=cladeoffset*1.675, fontface = 3, barsize = .5)
p<- p + geom_striplab('MW404745.1', 'GU598100.1', label = '  ser.', fontsize = textsize, color = "black", offset=cladeoffset*1.675, vjust = 1.9, barsize = .5)
p<- p + geom_striplab('MW404745.1', 'GU598100.1', label = ' Coronata', fontsize = textsize,  color = "black", offset=cladeoffset*1.675, vjust = 3.3, fontface = 3, barsize = .5)

### add final bootstrap labels that were originally subset. Must be done after branch rotation
p <- p %<+%d70 +geom_text2(aes(label = label70), size = textsize*.7, hjust = 1.5, vjust = -.5)
p <- p %<+% d70b + geom_text2(aes(label = label70b), size = textsize*.7, hjust = 1.2, vjust = -.5)
p <- p %<+% d70c + geom_text2(aes(label = label70c), size = textsize*.7, hjust = -.2)


#save to tif image file.
tiff(file = "PoaITStreeGIT.tif", units ="in", height = 5, width = 7.5, res = 1200)
p
dev.off()

