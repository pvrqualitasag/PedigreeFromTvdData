###
###
###
###   Purpose:   Build Pedigree from TVD-DAta
###   started:   2017/11/02 (skn and pvr)
###
### ######################################### ###


### # argument parsing from commandline
args<-commandArgs(trailingOnly = TRUE)
print(args)
if(length(args)!=2) stop("didn't receive 2 arguments")
sInputFile <-args[1]
sOutputFile <-args[2]
if(!file.exists(sInputFile)) stop("1st argument isn't an existing file")
#sInputFile <- "inst/extdata/KLDAT_20170524_20.txt"
#sOutputFile <- "inst/extdata/PedigreeFromTvdData.txt"



# check that input file exists
if (!file.exists(sInputFile))
  stop("Cannot find input file: ",sInputFile )

### # read TVD-File
#require(PedigreeFromTvdData)
lResultPedigree <- PedigreeFromTvdData::read_tvd_input(psInputFile = sInputFile)
#str(lResultPedigree)

### # further consistency checks, e.g. is ?
### #   - validation of tvd-number i.e. first two chars == country, after twelve chars numeric
lCheckedPedigree2 <- PedigreeFromTvdData::check_tvdid(plPedigree = lResultPedigree)
### #   - consistency of birthdates of animals and parents
lCheckedPedigree3 <- PedigreeFromTvdData::check_birthdate(plPedigree = lCheckedPedigree2)
### #   - consistency of sex of parents
lCheckedPedigree4 <- PedigreeFromTvdData::check_sex(plPedigree = lCheckedPedigree3)


### # generate output
PedigreeFromTvdData::output(plPedigree = lCheckedPedigree4, psOutfile = sOutputFile)

