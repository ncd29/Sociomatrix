# reads in data on WOM Researchers from the top 30 Business Schools
# returns a sociomatrix, with a 1 if Professors have worked 
# together and a 0 otherwise
# includes all professors and researchers that professors 
# at the top 30 Business Schools have collaborated with

data <- read.csv("WOMResearchers.csv", header = T)
attach(data)
ns <- paste(First.Name,Last.Name)
ns2 <- ns[grep("  ", as.character(ns))]
namesVector <- vector("character",length = 66)
i = 1
# remove extra spaces in the middle
# Why do they exist?
for (name in ns) {
  if (name%in%ns2) {
    newName <- sub(" ","",name)
    name <- newName
  }
  namesVector[i] <- name
  i = i + 1
}
ns <- namesVector
names <- list(ns)
Collaborators <- as.character(Collaborators)

l <- list()
for (n in 1:66) {
  if (!is.na(Collaborators[n])) (Collaborators[[n]] <- strsplit(as.character(Collaborators[n]),", "))
  v <- vector("numeric",length = 66)
  for (m in 1:66) {
    if (ns[m]%in%Collaborators[[n]][[1]]) {
      v[m] = 1
    }
  }
  l[[n]] <- v
}
sociomatrix <- matrix(unlist(l),nrow = length(ns), ncol = length(ns), dimnames = names)
colnames(sociomatrix) <- ns
print(sociomatrix)
