pkgname <- "zvau"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('zvau')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("read.clumpp")
### * read.clumpp

flush(stderr()); flush(stdout())

### Name: read.clumpp
### Title: Read Clumpp file
### Aliases: read.clumpp

### ** Examples

x <- "1        1   (0)      2 :  0.0403 0.9597
2        2   (0)      2 :  0.0209 0.9791
3        3   (0)      2 :  0.0051 0.9949
4        4   (0)      2 :  0.0115 0.9885
5        5   (0)      3 :  0.0502 0.9498
6        6   (0)      3 :  0.0425 0.9575
7        7   (0)      3 :  0.0239 0.9761"

write(x, "temp.clumpp.txt")
read.clumpp("temp.clumpp.txt")
unlink("temp.clumpp.txt")



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
