# reads in data on WOM Researchers from the top 30 Business Schools
# returns a sociomatrix, with a 1 if Professors have worked 
# together and a 0 otherwise
# includes all professors and researchers that professors 
# at the top 30 Business Schools have collaborated with

wom <- read.csv("WOM.csv", header = T, strip.white = T) # remove extra spaces in the middle with strip.white
ns <- paste(as.character(wom$First.Name),as.character(wom$Last.Name))
names <- list(ns)
Collaborators <- as.character(wom$Collaborators)

# for all researchers, create a vector of all zeros, and change to a 1
# if they have collaborated with a researcher in the corresponding column
l <- list()
for (n in 1:66) {
  if (!is.na(Collaborators[n])) (Collaborators[[n]] <- strsplit(as.character(Collaborators[n]),", "))
  v <- vector("numeric",length = length(ns))
  for (m in 1:66) {
    if (ns[m]%in%Collaborators[[n]][[1]]) { #if the column researcher is a collaborator of the row researcher
      v[m] = 1
    }
  }
  l[[n]] <- v
}

# turn the list into a matrix
sociomatrix <- matrix(unlist(l),nrow = length(ns), ncol = length(ns), dimnames = names)
colnames(sociomatrix) <- ns

print(sociomatrix)

