### Version 0.28 (4.9.2017)
Removed `makeNGSfilter` because it was causing problems.

### Version 0.27 (4.3.2016)
Added function `readColony` which extracts data from a Colony project.

### Version 0.25 (11.12.2015)
Expanded the functionality of `readClumpp` function to read consecutive individual number and population designation.

### Version 0.25 (2.12.2015)
Fixed bug in window size, genindSlidingWindow now also prints cohort dates. Printing can be disabled through argument `verbose`.

### Version 0.24 (6.11.2015)
Added function `writeCoancestry` and `goSlide`.

### Version 0.23 (5.11.2015)
Added function `genindSlidingWindow`.

### Version 0.22 (28.10.2015)
Enhanced writeGenPop function. It now accepts lists. Each list element (genind object) is written as its own population.
Added function for calculating Garza-Williamson M-ratio.

### Version 0.21 (13.10.2015)
Added function writeGenPop. This function differs from writeGenePop in that it uses strata/pop functionality of genind objects.

### Version 0.2 (17.6.2015)
Added functions subsetGenData, writeINEST, drawLoci and findIntegerInterval.

### Version 0.1 (31.12.2014)
Added functions `readClumpp`, `writeStructure` and `writeGenePop`.