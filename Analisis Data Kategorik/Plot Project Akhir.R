## VISUALISASI ##
titanic <- read.csv(file.choose(), header = TRUE, sep = ";")
# Memberi Nama Data
titanic$Survived <- sapply(as.character(titanic$Survived),switch,
                           "0" = "Not Survived",
                           "1" = "Survived")
titanic$Embarked <- sapply(as.character(titanic$Embarked),switch,
                           "0" = "Southampton",
                           "1" = "Cherbourg",
                           "2" = "Queenstown")
titanic$Sex <- sapply(as.character(titanic$Sex),switch,
                           "0" = "Female",
                           "1" = "Male")

# Analisa Data
#hubungan jenis kelamin dan bertahan hidup
library(ggplot2)
ggplot(data = titanic, mapping = aes(x = Sex,
                                     y = Survived)) +
  geom_count(aes(color = Sex))
#hubungan embarkasi dengan bertahan hidup
titanic.survived <- titanic[titanic$Survived == "Survived",]
ggplot(data = titanic.survived, mapping = aes(x = Embarked,
                                              y = Survived)) +
  geom_col(aes(fill = Embarked), show.legend = F) +
  labs(title = "Embarkasi Penumpang yang Bertahan Hidup",
       x = "Embarkasi",
       y = "Bertahan Hidup")
#hubungan umur, embarkasi dan bertahan hidup
ggplot(data = titanic.survived, mapping = aes(x = Embarked,
                                              y = Survived)) +
  geom_col(aes(fill = Sex), show.legend = T) +
  labs(title = "Embarkasi Penumpang yang Bertahan Hidup",
       x = "Embarkasi Penumpang",
       y = "Bertahan Hidup")
#hubungan jumlah orangtua dengan bertahan hidup
ggplot(data = titanic, mapping = aes(x = Parch,
                                     y = Survived)) +
  geom_count(aes(color = Parch))   
