datasheet <- read.csv('/home/deepak/Documents/projects/Term-Project2/Datasheet1.csv', header = TRUE, sep = ",", na.strings = "?")
data1 <- na.omit(datasheet)
write.csv(data1, file = "Data1.csv")

data <- read.csv('/home/deepak/Documents/projects/Term-Project2/Data1.csv', header = TRUE, sep = ",")
View(data)

