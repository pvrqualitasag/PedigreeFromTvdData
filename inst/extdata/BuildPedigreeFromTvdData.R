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

### # read TVD-File
#require(PedigreeFromTvdData)
lResultPedigree <- PedigreeFromTvdData::read_tvd_input(psInputFile = sInputFile)
#str(lResultPedigree)

### # further consistency checks, e.g. is ?
### #   - every parent also an animal with function check_parent_as_animal
#lCheckedPedigree <- PedigreeFromTvdData::check_parent_as_animal(plPedigree = lResultPedigree)
### #   - validation of tvd-number i.e. first two chars == country, after twelve chars numeric
lCheckedPedigree2 <- PedigreeFromTvdData::check_tvdid(plPedigree = lCheckedPedigree)
### #   - consistency of birthdates of animals and parents
lCheckedPedigree3 <- PedigreeFromTvdData::check_birthdate(plPedigree = lCheckedPedigree2)
### #   - consistency of sex of parents
lCheckedPedigree4 <- PedigreeFromTvdData::check_sex(plPedigree = lCheckedPedigree3)
### # ...


### # unique ids for all animals
#lProcessedPedigree <- PedigreeFromTvdData::unique_id(plPedigree = lCheckedPedigree4)

### # generate output
PedigreeFromTvdData::output(plPedigree = lProcessedPedigree)
