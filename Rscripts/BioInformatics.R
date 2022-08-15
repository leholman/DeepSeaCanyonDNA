#Analysing Caludios Canyons
#Luke Holman 13.05.2019


#Lets read in packages
library(metabarTOAD)
library(dada2)
library("lulu")

#Set up folders
Folders()

#Now we copy the raw data into the back up folder

#Lets look at the files
list.files("1.rawreads/")


#Lets unzip them
Unzip()

#now lets count up the raw reads
files <- list.files("1.rawreads",pattern=".fastq",full.names = TRUE)
rawreadcount <- sapply(files,FastqCount)
mean(rawreadcount)
sd(rawreadcount)

#Now we merge the seqs
MergeReads(usearchdest = "/Users/Luke/Bioinformatics/PATH/usearch")
#and count them up 
mergedreadcount <- sapply(list.files("2.mergedreads",pattern=".fastq",full.names = TRUE),FastqCount)

#we then trim the primers
PrimerStrip(UsePrimerFile = TRUE,cutadaptdest = "/Users/Luke/Bioinformatics/PATH/cutadapt", ncores=7)
#count them up!
strippedreadcount <- sapply(list.files("3.strippedreads",pattern=".fastq",full.names = TRUE),FastqCount)



#Now we dereplicate, filter and pool

PoolNFilterReads(vsearchdest="/Users/Luke/Bioinformatics/PATH/vsearch",UsePrimerFile = TRUE)

#Now we do some real work! Cluster 0.97 first

OTUCluster(usearchdest = "/Users/Luke/Bioinformatics/PATH/usearch",UsePrimerFile = TRUE)

#Unoise time

DenoiseUNOISE3(usearchdest = "/Users/Luke/Bioinformatics/PATH/usearch",UsePrimerFile = TRUE)

#LULU
ApplyLulu("5.OTUs/Leray.unoise3.OTUs.fasta",table = "6.mappings/OTUtabs/Leray.raw.unoise3.csv",output = "8.LULU/Leraylulu.unoise3.2.csv")
ApplyLulu("5.OTUs/Zhan.unoise3.OTUs.fasta",table = "6.mappings/OTUtabs/Zhan.raw.unoise3.csv",output = "8.LULU/Zhanlulu.unoise3.2.csv")




