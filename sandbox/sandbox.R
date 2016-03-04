#### draw alleles from loci list ####
load("./sandbox/MonkSealAllelesOLD.RData")

titi <- lapply(titi, FUN = function(x) {
  x[!grepl(pattern = "009|819", x = x)]
})
m <- drawLoci(x = titi)
ggsave("./sandbox/allele_loc_list.pdf", width = 10, height = 5)

