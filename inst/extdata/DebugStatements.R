wrongTvdNr <- "C0120003365547"
!PedigreeFromTvdData:::is.letter(pId = substr(wrongTvdNr,
                                 start = lFormatBorder$TVDCountry$lower,
                                 stop  = lFormatBorder$TVDCountry$upper))



### #
plPedigree <- lResultPedigree
lFormatBorder = PedigreeFromTvdData::getBirthdateBorder()
lLimitValue = PedigreeFromTvdData::getBirthdayConsistencyLimit()
lCheckedPedigree3 <- plPedigree

idxPed <- 1

lCurrentAni <- lCheckedPedigree3[[idxPed]]

# test
as.numeric(substr(lCurrentAni$Geburtsdatum,
                  start = lFormatBorder$Year$lower,
                  stop  = lFormatBorder$Year$upper)) < lLimitValue$cLowestLimitYear

### # debug
plPedigree = lCheckedPedigree2
lCheckedPedigree3 <- plPedigree
idxPed <- 19
lCurrentAni <- lCheckedPedigree3[[idxPed]]



require(PedigreeFromTvdData)
lFormatBorder = getBirthdateBorder()

### # directly running local changes in package source
devtools::load_all()



### # debug sex consistency
sInputFile <- "inst/extdata/KLDAT_20170524_20.txt"
# check that input file exists
if (!file.exists(sInputFile))
  stop("Cannot find input file: ",sInputFile )

#require(PedigreeFromTvdData)
lResultPedigree <- PedigreeFromTvdData::read_tvd_input(psInputFile = sInputFile)


### # further consistency checks, e.g. is ?
### #   - every parent also an animal
### #   - validation of tvd-number i.e. first two chars == country, after twelve chars numeric
### #   - consistency of birthdates of animals and parents
### #   - consistency of sex of parents
### # ...
#
lCheckedPedigree <- PedigreeFromTvdData::check_parent_as_animal(plPedigree = lResultPedigree)
lCheckedPedigree2 <- PedigreeFromTvdData::check_tvdid(plPedigree = lCheckedPedigree)
lCheckedPedigree3 <- PedigreeFromTvdData::check_birthdate(plPedigree = lCheckedPedigree2, pbOut = TRUE)
#lCheckedPedigree4 <- PedigreeFromTvdData::check_sex(plPedigree = lCheckedPedigree3)


plPedigree <- lCheckedPedigree3
plPedigree <- lResultPedigree
lCheckedPedigree4 <- plPedigree

idxPed <- 21


lCurrentAni <- lCheckedPedigree4[[idxPed]]
if(is.element(lCurrentAni$MutterId, vecAnimals)){
  if(as.numeric(lCurrentAni$Sex) != as.numeric(cWeiblich)){
    lCurrentAni$Sex <- NA
  }
}
if(is.element(lCurrentAni$VaterId, vecAnimals)){
  if(as.numeric(lCurrentAni$Sex) != as.numeric(cMaennlich)){
    lCurrentAni$Sex <- NA
  }
}


is.element("CH710373055041", vecAnimals)


is.element("CH120001814627", vecAnimals)



plPedigree <- lResultPedigree


####################################################

cat("original RC: >", lProcessedPedigree[[idxPed]]$MutterRassecode, "<\n",
    "replaced RC: >", gsub(pattern = "[[:blank:]+]",
                          replacement = "",
                          x=lProcessedPedigree[[idxPed]]$MutterRassecode), "<\n", sep="")

gsub(pattern = "[[:blank:]+]",
     replacement = "",
     x=lProcessedPedigree[[idxPed]]$MutterRassecode)

cat("original RC: >", lProcessedPedigree[[idxPed]]$TierRassecode, "<\n",
    "replaced RC: >", gsub(pattern = "[[:blank:]+]",
                           replacement = "",
                           x=lProcessedPedigree[[idxPed]]$TierRassecode), "<\n", sep="")

cat("original ID: >", lProcessedPedigree[[idxPed]]$MutterId, "<\n",
    "replaced ID: >", gsub(pattern = "[[:blank:]+]",
                           replacement = "",
                           x=lProcessedPedigree[[idxPed]]$MutterId), "<\n", sep="")

cat("original ID: >", lProcessedPedigree[[idxPed]]$TierId, "<\n",
    "replaced ID: >", gsub(pattern = "[[:blank:]+]",
                           replacement = "",
                           x=lProcessedPedigree[[idxPed]]$TierId), "<\n", sep="")

is.na(gsub(pattern = "[[:blank:]+]",
           replacement = "",
           x=lProcessedPedigree[[idxPed]]$MutterId))

is.null(gsub(pattern = "[[:blank:]+]",
           replacement = "",
           x=lProcessedPedigree[[idxPed]]$MutterId))

identical(gsub(pattern = "[[:blank:]+]",
             replacement = "",
             x=lProcessedPedigree[[idxPed]]$MutterId), "")

gsub(pattern = "[[:blank:]+]",
               replacement = "",
               x=lProcessedPedigree[[idxPed]]$MutterId) == ""

