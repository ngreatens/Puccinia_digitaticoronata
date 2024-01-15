## README

R code and files for creating phylogenetic trees in "Host specificity of Puccinia digitaticoronata, a new crown rust fungus of Kentucky bluegrass in North America"

Figures were created with ggtree using outputs from mrbayes and MEGA. Hopefully it will be some use to others creating their own phylogenetic trees with the excellent [ggtree](https://guangchuangyu.github.io/software/ggtree/) package.

In particular, I think I found some tricks to make the tree look very nice. They are not of my invention but anyway took a lot of trial and error to get right: 
* dots to connect tree tips to species labels with labels lined up.
* a combination of tip labels, clade labels and strip labels to accurately and cleanly notate taxa and polyphyletic groupings
* subsetting node labels and moving them to make the tree look cleaner

See original ggtree documentation for questions of usage, but I hope this code is commented well enough to be of some use as an example. For the ITS tree, produced second, much of the supporting data is reformatted using R as well. If you want to go from treefile + supporting data (e.g. name, collection year, place) to nicely formatted tree, this may be of some help. Else follow the second half, or the code for the four-locus tree, where much of the formatting is done manually in excel instead.

MAKE SURE TO DOWNLOAD ALL FILES INTO A DIRECTORY AND SET YOUR WORKING DIRECTORY TO THE PROPER LOCATION. 
Then everything should work (or at least it did as of Jan 15 2024).


