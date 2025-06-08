#####Code to create social network matrices####
####Date: Jan 14, 2021
####Code written by: Anita Montero
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we calculate both the affiliative and agonistic matrices of social interactions at each social group 

#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/')
library(igraph)
library(stats)
library(XLConnect)
library(XLConnectJars)


####Prepare Data File####
#Read in csv file
socint_0216 <- read.csv("~/Desktop/tb_socint.csv", header = TRUE, na.strings=c("NA",""))
dim(socint_0216) 


####Agonistic Social Matrix####
socint_0216_agr <- socint_0216
socint_0216_agr$interac_type <- as.character(socint_0216_agr$interac_type) #coerce variable interaction type to character type
split <- strsplit(socint_0216_agr$interac_type,'_') #split the variable interaction type using "_"
type <- rep(NA, length(socint_0216_agr$interac_type))  #create a vector to hold the new variable

# create a loop to fill the vector that will hold the new variable
for (ii in 1:length(split)) { 
  type[ii] <- split[[ii]][1]
} 

# joint the vector type to the data frame
socint_0216_agr$type <- as.factor(type)
head(socint_0216_agr, 10); summary(socint_0216_agr)
socint_0216_agr$year_col <- paste(socint_0216_agr$year, socint_0216_agr$col_area, sep="_")
year_col <- as.character(unique(socint_0216_agr$year_col))

#All Interactions in table
# pull out the individuals of each of the groups
dyad_all <- na.omit(subset(socint_0216_agr, select=c(uid_ini, uid_rec, col_area))) # delete all NAs
summary(dyad_all); dim(dyad_all) 

# create a vector with the names of the social groups
groupsID <- as.character(unique(socint_0216_agr$year_col))

# create a list with the unique group ID
grouplist <-  vector("list", length(groupsID))

# name the elements in the list. To do that, ask for the names of the groups
groupsID
names(grouplist) <- groupsID

# create different objects for each group ID
for (ii in 1:(length(groupsID))){
  groupinter <- subset(socint_0216_agr, socint_0216_agr$year_col == groupsID[ii], select= c(uid_ini, uid_rec))
  grouplist[[ii]] <- groupinter     
  temp.name <-  groupsID[ii]
  names(grouplist)[ii] <- temp.name
}

#check we have the data in the list
grouplist

##Make into matrices##
# create a list with the unique year.col
matrixlist <- vector("list", length(groupsID))
names(matrixlist) <- groupsID

#create a loop to read the list
for (ii in 1:(length(groupsID))) {
  el<-as.matrix(grouplist[[ii]]) #read the data that is stored in a list
  el[,1]<-as.character(el[,1]) #Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems.
  el[,2]<-as.character(el[,2])
  g=graph.edgelist(el)
  
  a=as.matrix(get.adjacency(g,sparse=TRUE, type ="both"))
  rownames(a) <- colnames(a)
  
  matrixlist[[ii]] <- a
  temp.name <-  (groupsID[ii])
  names(matrixlist)[ii] <- temp.name
}

summary(matrixlist)

#Save to excel

matrices <- loadWorkbook("socint_0216_agr.xlsx", create = TRUE) #create workbook to store the results
createSheet(matrices, names(matrixlist)) # create a worksheet name as the social group contained in the matrixlist object
writeWorksheet(matrices, matrixlist, names(matrixlist), header = TRUE) # Write built-in dataset matrixlist names created above
saveWorkbook(matrices) # Save workbook - this actusocint_ad writes the output file to disk/computer

####Affiliative Social Matrix####
socint_0216_aff <- socint_0216 
socint_0216_aff$interac_type <- as.character(socint_0216_aff$interac_type) #coerce variable interaction type to character type
split <- strsplit(socint_0216_aff$interac_type,'_') #split the variable interaction type using "_"
type <- rep(NA, length(socint_0216_aff$interac_type))  #create a vector to hold the new variable

# create a loop to fill the vector that will hold the new variable
for (ii in 1:length(split)) { 
  type[ii] <- split[[ii]][1]
} 

# joint the vector type to the data frame
soc_obs$type <- as.factor(type)
head(soc_obs, 10); summary(soc_obs)

sum(length(which(soc_obs$type == "disp")), length(which(soc_obs$type == "agr")), length(which(soc_obs$type == "mount")), 
    length(which(soc_obs$type == "sex"))) 

#take out all the non affiliative interactions
soc_obs_aff<- subset(soc_obs[!(soc_obs$type %in% c("agr", "disp" ,"mount","sex")),]) 
head(soc_obs_aff, 10); summary(soc_obs_aff); dim(soc_obs_aff) 

# joint the vector type to the data frame
socint_0216_aff$type <- as.factor(type)
head(socint_0216_aff, 10); summary(socint_0216_aff)
socint_0216_aff$year_col <- paste(socint_0216_aff$year, socint_0216_aff$col_area, sep="_")
year_col <- as.character(unique(socint_0216_aff$year_col))

#All Interactions in table
# pull out the individuals of each of the groups
dyad_all <- na.omit(subset(socint_0216_aff, select=c(uid_ini, uid_rec, col_area))) # delete all NAs
summary(dyad_all); dim(dyad_all)

# create a vector with the names of the social groups
groupsID <- as.character(unique(socint_0216_aff$year_col))

# create a list with the unique group ID
grouplist <-  vector("list", length(groupsID))

# name the elements in the list. To do that, ask for the names of the groups
groupsID
names(grouplist) <- groupsID

# create different objects for each group ID
for (ii in 1:(length(groupsID))){
  groupinter <- subset(socint_0216_aff, socint_0216_aff$year_col == groupsID[ii], select= c(uid_ini, uid_rec))
  grouplist[[ii]] <- groupinter     
  temp.name <-  groupsID[ii]
  names(grouplist)[ii] <- temp.name
}

#check we have the data in the list
grouplist

#Make into matrices

# create a list with the unique year.col
matrixlist <- vector("list", length(groupsID))
names(matrixlist) <- groupsID

#create a loop to read the list
for (ii in 1:(length(groupsID))) {
  el<-as.matrix(grouplist[[ii]]) #read the data that is stored in a list
  el[,1]<-as.character(el[,1]) #Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems.
  el[,2]<-as.character(el[,2])
  g=graph.edgelist(el)
  
  a=as.matrix(get.adjacency(g,sparse=TRUE, type ="both"))
  rownames(a) <- colnames(a)
  
  matrixlist[[ii]] <- a
  temp.name <-  (groupsID[ii])
  names(matrixlist)[ii] <- temp.name
}

summary(matrixlist)

#Save to excel

matrices <- loadWorkbook("socint_0216_aff.xlsx", create = TRUE) #create workbook to store the results
createSheet(matrices, names(matrixlist)) # create a worksheet name as the social group contained in the matrixlist object
writeWorksheet(matrices, matrixlist, names(matrixlist), header = TRUE) # Write built-in dataset matrixlist names created above
saveWorkbook(matrices) # Save workbook - this actusocint_ad writes the output file to disk/computer
