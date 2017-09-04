devtools::load_all(".")
roxygen2::roxygenize()

xy <- read.table("./sandbox/gis_data_sim.csv", sep = ";", header = TRUE)
xy$id <- as.numeric(xy$sample)
head(xy)
write.table(xy, file = "./sandbox/test_genotipi_id.csv", sep = ";", col.names = TRUE,
            row.names = FALSE, quote = FALSE)

# Rscript do_allele_match.R --profile --input=gis_demo_gene.csv --fig=mojaslika.png
# Rscript do_allele_match.R --input=test_genotipi_id.csv --output=testout.csv --alleleMismatch=3
