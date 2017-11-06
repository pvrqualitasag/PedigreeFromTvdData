###
###
###
###   Purpose:   Build Pedigree from TVD-DAta
###   started:   2017/11/02 (skn and pvr)
###
### ######################################### ###


sInputFile <- "inst/extdata/KLDAT_20170524_20.txt"
# check that input file exists
if (!file.exists(sInputFile))
  stop("Cannot find input file: ",sInputFile )

#require(PedigreeFromTvdData)
lResultPedigree <- PedigreeFromTvdData::read_tvd_input(psInputFile = sInputFile)

str(lResultPedigree)

### # further consistency checks, e.g. is ?
### #   - every parent also an animal
### #   - validation of tvd-number i.e. first two chars == country, after twelve chars numeric
### #   - consistency of birthdates of animals and parents
### #   - consistency of sex of parents
### # ...
#
lCheckedPedigree <- PedigreeFromTvdData::check_parent_as_animal(plPedigree = lResultPedigree)
lCheckedPedigree2 <- PedigreeFromTvdData::check_tvdid(plPedigree = lCheckedPedigree)
lCheckedPedigree3 <- PedigreeFromTvdData::check_tvdid(plPedigree = lCheckedPedigree2)


### # unique ids for all animals

### # generate output
