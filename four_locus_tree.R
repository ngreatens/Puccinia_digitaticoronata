  ### set working directory
  setwd("PATH/TO/DIR") ####SET DIR########
  
  #load packages
  library(ggtree)
  library(readxl)
  library(glue)
  library(treeio)
  
  #read in tree
  tree <- read.mrbayes("four_locus.tre")
  
  #read in data tables
  df <- read_excel("Four_locus_supporting_data.xlsx")
  
  #make new field for each tip
  df <- dplyr::mutate(df, lab = glue("{ID}~{Origin}~{Year}~italic({Host_genus})~italic({Host_species})~{nonitalic}~italic({ssp})"))
  
  #filter posterior values .95
  q <- ggtree(tree)
  d <- q$data
  #select internal nodes
  d <- d[!d$isTip,]
  #convert to numeric
  d$prob_percent <- as.numeric(d$prob_percent)
  #select labels more than some value
  d95 <- d[d$prob_percent>95,]
  
  #subset nodes whose labels appeared under tree.
  d48 <- d95[d95$node == 48,]
  d95 <- d95[d95$node != 48,]
  d49 <- d95[d95$node == 49,]
  d95 <- d95[d95$node != 49,]
  d50 <- d95[d95$node == 50,]
  d95 <- d95[d95$node != 50,]
  d58 <- d95[d95$node == 58,]
  d95 <- d95[d95$node != 58,]
  
  #add textsize, textcolor, and cladeoffset variables for ease of change
  
  textsize <- 2.2
  cladeoffset <- .385
  textcolor <- "green4"
    
  #tree
  p <- ggtree(tree) %<+% df +
    
  #Clade labels. Can't get geom_cladelab to parse italics when reading in
    ##an excel. geom_cladelabel can, but doesn't accepting mapping. idk.
    ##so entered manually.used an excel formula to make formatting easier
    ## record nodes manually. e.g. run below or similar and compare with your own tree
    ## ggtree(tree) +geom_text(aes(label = node))
      geom_cladelabel(node =48, label ='~italic(P.)~italic(coronata)~var.~italic(coronata)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =46, label ='~italic(P.)~italic(coronata)~var.~italic(avenae)~f.~sp.~italic(avenae)', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset, color = textcolor) +
      geom_cladelabel(node =49, label ='~italic(P.)~italic(digitaticoronata)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset, color = textcolor) +
      geom_cladelabel(node =57, label ='~italic(P.)~italic(coronati-brevispora)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =51, label ='~italic(P.)~italic(coronati-agrostidis)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =58, label ='~italic(P.)~italic(coronati-longispora)', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =52, label ='~italic(P.)~italic(coronati-japonica)', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =59, label ='~italic(P.)~italic(coronati-hordei)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =62, label ='~italic(P.)~italic(coronati-calamagrostidis)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =61, label =' ~italic(P.)~italic(pseudodigitata)~"*"', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset) +
      geom_cladelabel(node =38, label ='~italic(Puccinia)', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset*1.75) +
      geom_cladelabel(node =38, label ='~~"ser."', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset*1.75, vjust = 2.3) +
      geom_cladelabel(node =38, label ='~italic(Coronata)', fontsize = textsize, align = TRUE, parse = TRUE, offset = cladeoffset*1.75, vjust = 3) +
    
    #more clade labels since single nodes can't be labelled with geom_cladelabel
  geom_tiplab(
    aes(label=Secondary_clade_label, color = Clade_color),
    linesize = .15,
    offset = (cladeoffset + .005), ##add a little to offset bc of bar in clades
    size = textsize,
    parse = TRUE,
    align = TRUE,
    show.legend = FALSE
  )+
    #and once more with empty labels to make dots black instead of colored
    #for clades in other colors
    geom_tiplab(
      aes(label = ""),
      linesize = .15,
      offset = (cladeoffset + .005),
      size = textsize,
      align = TRUE,
    ) +

  #tip labels. Put text on label with no border or padding to cover up dots from first tip label (2nd "clade" label)
    geom_tiplab(
      aes(label = lab, color = Color), #format excel with a Color column
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
    xlim(NA, .95) + ##adjust right value to widen or narrow tree
    
  #scale bar
    geom_treescale(fontsize = textsize*.8, x = 0, y = -.4) +
    
  #bootstrap values
    geom_text(
      data = d95, 
      aes(label = prob_percent),
      size = textsize*.6,
      hjust = 1.3, #adjust hjust and vjust to put bootstrap values in desired place
      vjust = -.5) +
  # #other node label that appeared under tree
     geom_text(data = d48, aes(label = prob_percent), size = textsize*.6, hjust = - .1, vjust = .3) +
     geom_text(data = d49, aes(label = prob_percent), size = textsize*.6, hjust = 1.1, vjust = -.9) +
     geom_text(data = d50, aes(label = prob_percent), size = textsize*.6, hjust = 1.3, vjust = 1.5) +
     geom_text(data = d58, aes(label = prob_percent), size = textsize*.6, hjust = -.1, vjust = .3) +
  #add strip to notate outgroup taxa. not all from single node so cant use clade label
  geom_striplab('a1', 'a35', label = ' Outgroup', fontsize = textsize, color = "black", offset=cladeoffset*1.75, barsize = .5) +
  geom_striplab('a1', 'a35', label = '  taxa', fontsize = textsize, color = "black", offset=cladeoffset*1.75, barsize = .5, vjust=2)
  
  #save to tif image file.
  tiff(file = "Four_locus.tif", units ="in", height = 3.25, width = 7.5, res = 1200)
  p
  dev.off()
