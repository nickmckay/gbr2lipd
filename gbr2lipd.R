library(googlesheets4)
library(lipdR)
library(tidyverse)
library(googledrive)
library(lubridate)
#get the data we need

metadata <- read_sheet("1sQ80-25GAWPNFGvhdJG3RqRPF1jYzD-ugVftOtUB-ZA")


translator <- read_sheet("12lj30-Sx-7pioREuuDTIi3wC-wQRJUcZSfKoZZ_v7dw")

variableMetadata <- read_sheet("1juWns4XSBdQ8okMg59miiOJ-nhLi337kgAtuq8HKw5I")
#googledrive::drive_auth()

allDataFiles <-   googledrive::drive_ls(path = as_id("https://drive.google.com/drive/folders/1lAPmdjddqYWpIbEYHYVaDBzD0wEhx2ir"))

allDataFiles <- allDataFiles %>%
  mutate(dataSetName = str_remove(name,".csv"))

#modify metadata file to LiPD standard
#this will omit some fields without matches
mnames <- names(metadata)
lnames <- c()
for(n in 1:length(mnames)){
  newName <- translator$`LiPD TS_name`[translator$`GBR Name` == mnames[n]]
  if(length(newName) == 0){
    newName <- mnames[n]
  }
  if(is.na(newName)){
    newName <- mnames[n]
  }
  lnames[n] <- newName
}

#which columns have lipd names
lvar <- which(lnames %in% translator$`LiPD TS_name`)

lipdMeta <- metadata[,lvar]
names(lipdMeta) <- lnames[lvar]

nonLipdMeta <- metadata[,-lvar]
names(nonLipdMeta) <- lnames[-lvar]
nonLipdMeta$dataSetName <- lipdMeta$dataSetName
#create files one by one
udsn <- unique(lipdMeta$dataSetName)


#one dataset


tlmeta <- filter(lipdMeta,dataSetName == tdsn)
tnlmeta <- filter(nonLipdMeta,dataSetName == tdsn)


tdataDl <- googledrive::drive_download(file = allDataFiles$drive_resource[allDataFiles$dataSetName == tdsn],
                                     path = file.path("data",paste0(tdsn,".csv")),overwrite = TRUE)

#load the data!
tdata <- read_csv(tdataDl$local_path)

#expected variables
vars <- str_split(tnlmeta$meths_primaryVariablesList[[1]],pattern = "; ",simplify = TRUE)

nvars <- ncol(tdata) #don't include year here

#initialize ts

ts <- tlmeta

#replicate the rows by the number variables
while(nvars > nrow(ts)){
  ts <- bind_rows(ts,tlmeta)
}

#add in empty conditional rows
toAdd <- unique(translator$`needs transformation`) %>% na.omit()

for(ta in toAdd){
  ts[ta] <- NA
}

#add in empty year and values
ts$paleoData_values <- NA
ts$paleoData_longName <- NA
ts$paleoData_units <- NA
ts$paleoData_description <- NA

#start building the ts
ts$paleoData_variableName <- names(tdata)

#fix geo elevation
ts$geo_elevation <- as.numeric(ts$geo_elevation)

#deal with the conditional assignments
for(v in 1:length(ts$paleoData_variableName)){
  thisVar <- ts$paleoData_variableName[v]

  rtr <- which(translator$if_var == thisVar)
  if(length(rtr) > 0){#plug the name
   trtr <- translator$`needs transformation`[rtr]
   gbrVar <- translator$`GBR Name`[rtr]
   for(g in 1:length(rtr)){
     ts[[trtr[g]]][v] <- tnlmeta[[gbrVar[g]]][[1]]
   }
  }

  #assign number
  ts$paleoData_number <- v

   #assign data values
  ts$paleoData_values[v] <- list(as.matrix(tdata[,names(tdata) == thisVar]))

  #add in variable metadata
  tvm <- filter(variableMetadata,gbrName == thisVar)

  if(nrow(tvm) == 1){
  ts$paleoData_longName[v] <- tvm$longName[[1]]
  ts$paleoData_units[v] <- tvm$lipdUnits[[1]]
  ts$paleoData_variableName[v] <- tvm$lipdName[[1]]
  ts$paleoData_description[v] <- tvm$paleoData_description[[1]]
  }

}

##deal with specials


#is anomaly
#is this true for all datasets?

#alt measurement metadata

#assign TSid
ts$paleoData_TSid <- paste0("gbr2lipd",ts$dataSetName,ts$paleoData_variableName) #make sure this is unique, but it makes it reproducible

if(any(duplicated(ts$paleoData_TSid))){
  stop("duplicated TSids. Fix this.")
}

ts$datasetId <- ts$dataSetName


L <- purrr::transpose(ts) %>%
  collapseTs(force = TRUE)

L$archiveType <- "Coral"
L$lipdVersion <- 1.2
L$createdBy <- "https://github.com/nickmckay/gbr2lipd"
L <- initializeChangelog(L)

validLipd(L)

