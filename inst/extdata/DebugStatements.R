###
###
###
###   Purpose:   birthdate consistency
###   started:   2017-12-06 (pvr)


### # testing with large data set on castor
### # directory
# zws@castor:/qualstorzws01/data_projekte/projekte/muku_ZWS_SourceCodeAnpassungen/20170904_FLP_Fettabdeckung/VKS/data$




### # reading format from xlsx
sFormatDsch <- system.file(file.path("extdata", "RindviehCH_Schnittstelle_4_13_D.xlsx"),
                           package = "PedigreeFromTvdData")
tbl_dsch_k11 <- PedigreeFromTvdData::getK11ColPositionFromDsch(psFormatDschFile = sFormatDsch)
tbl_dsch_k11

grep(pattern = "Kalbedatum", tbl_dsch_k11[[2]])

### # reading the pedigree
sDataFileName <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
                             package = "PedigreeFromTvdData")

vec_format_new <- PedigreeFromTvdData::getK11ColPositionVecFromDsch(psFormatDschFile = sFormatDsch)
tbl_ped_new <- PedigreeFromTvdData::laf_open_fwf_tvd_input(psInputFile     = sDataFileName,
                                                           pvecColPosition = vec_format_new)

(n_bd_col <- PedigreeFromTvdData::getBirthdateColIdxDsch(psFormatDschFile = sFormatDsch))
tbl_ped_new[1,n_bd_col]
tbl_ped_new[[n_bd_col]][1]

is.character(tbl_ped_new[[n_bd_col]][1])


if (is.character(tbl_ped_new[[n_bd_col]])){
  tbl_ped_new[[n_bd_col]] <- as.numeric(tbl_ped_new[[n_bd_col]])
}
###
###
###
###   Purpose:   Debug wrong column positions
###   started:   2017-12-04 (pvr)
###
### ########################################### ###

### # define input file
sDataFileName <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
                             package = "PedigreeFromTvdData")


### # read a few lines to verify positions
nr_lines_read <- 50
con_ped <- file(description = sDataFileName)
vec_ped <- readLines(con = con_ped, n = nr_lines_read)
close(con_ped)

head(vec_ped)

### # element 4 has known father
vec_ped[4]


### # reading format from xlsx
sFormatDsch <- system.file(file.path("extdata", "RindviehCH_Schnittstelle_4_13_D.xlsx"),
                           package = "PedigreeFromTvdData")

tbl_dsch_format <- getDschFormatTblDf(psFormatDschFile = sFormatDsch)


### # K11 start row
pnTagCol   = 2
psStartTag = "Satzart K11"
psEndTag   = NA
(n_k11_start_row <- which(tbl_dsch_format[,pnTagCol] == psStartTag))
### # we should only have one starting tag
if (length(n_k11_start_row) != 1){
  stop("*** No unique starting position found:")
  print(n_k11_start_row)
}
### # maching positions of end tags
if (is.na(psEndTag)){
  (vecEndPos <- which(is.na(tbl_dsch_format[,pnTagCol])))
} else {
  (vecEndPos <- which(tbl_dsch_format[,pnTagCol] == psEndTag))
}

if (length(vecEndPos) < 1)
  stop(" *** Cannot find position of end tag: ", psEndTag)

### # in case more than one ending positions were found, take
### #  the first after the starting position
if(length(vecEndPos) > 1){
  n_k11_end_row <- vecEndPos[vecEndPos > n_k11_start_row][1]
}


if (n_k11_end_row < n_k11_start_row)
  stop(" End-row of K11 smaller than start-row")

tbl_dsch_k11 <- tbl_dsch_format[n_k11_start_row:(n_k11_end_row-1),c(1,2,5)]




### # ################################################################## ###
### # consistency checks
### # id
any(PedigreeFromTvdData::is.notletter(substr(head(tbl_ped[[6]]), 1, 2)))

### # country codes
any(PedigreeFromTvdData::is.notletter(substr(tbl_ped[[6]], 1, 2)))

### # numbers
any(PedigreeFromTvdData::is.notnumber(substr(tbl_ped[[6]], 3, 14)))


### # mother
PedigreeFromTvdData::is.notletter(substr(head(tbl_ped[[2]]), 1, 2))
PedigreeFromTvdData::is.notnumber(substr(head(tbl_ped[[2]]), 3, 14))


sDataFileName <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
                             package = "PedigreeFromTvdData")
tbl_ped <- PedigreeFromTvdData::readr_fwf_tvd_input(psInputFile = sDataFileName,
                                                    pbOut = TRUE)
tbl_ped_checked <- PedigreeFromTvdData::check_tvd_id2(ptblPedigree = tbl_ped)


### # checking structure of LaF
sDataFileName <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
                             package = "PedigreeFromTvdData")
vecFormat <- c(22,14,3,31,8,14,3,1,14,3)
laf_ped <- LaF::laf_open_fwf(filename = sDataFileName,
                             column_types = rep("character", length(vecFormat)),
                             column_widths = vecFormat)



!all(sapply(c("readr", "iotools", "LaF", "microbenchmark"),
            function(x) x  %in% installed.packages(), USE.NAMES = FALSE))



############################################# ###
### # old statements before 2017-11-08
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

