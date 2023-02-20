# Extraction of Phylum using Kraken2 output files

direct=choose.dir() # Select the folder with all output files (.kreport2)

total_tax=NULL
level="P" # P for "Phylum"

for(pieces in 1:length(list.files(direct))) # Extraction of all names of the group
{
  files_read=read.delim(header = F,paste(direct,list.files(direct)[pieces],sep="\\"))
  
  for(i in 1:length(files_read[,1]))
  {
    if(files_read[i,6]=="  Bacteria")
    {
      start_bacteria=i  
    }else if (files_read[i,6]=="  Eukaryota")
    {
      end_bacteria=i
    }
  }
  files_read=files_read[start_bacteria:end_bacteria-1,]
  
  files_filo = files_read[files_read[,4]==level,] 
  
  total_tax = c(total_tax,files_filo[,6]) 
}

total_tax=unique(gsub(" ", "", total_tax, fixed = TRUE))

Output=data.frame(matrix(nrow = length(total_tax)))

rownames(Output)=total_tax ##Copiamos los nombres a Output

for(pieces in 1:length(list.files(direct)))
{
  files_read=read.delim(header = F,paste(direct,list.files(direct)[pieces],sep="\\"))
  
  for(i in 1:length(files_read[,1]))
  {
    if(files_read[i,6]=="  Bacteria")
    {
      start_bacteria=i  
    }else if (files_read[i,6]=="  Eukaryota")
    {
      end_bacteria=i
    }
  }
  files_read=files_read[start_bacteria:end_bacteria-1,]

  
  
  files_filo=files_read[files_read[,4]==level,] 
  
  files_filo[,6]=gsub(" ", "", files_filo[,6], fixed = TRUE)
  
  agg = aggregate(files_filo$V2,by = list(files_filo$V6),FUN = sum)
  
  Output[match(agg[,1],row.names(Output)),pieces]=agg[,2]/sum(agg[,2]) 
  
  colnames(Output)[pieces]=list.files(direct)[pieces] 
}


colSums(Output,na.rm = T)

write.table(Output, file="Phylum.txt")




