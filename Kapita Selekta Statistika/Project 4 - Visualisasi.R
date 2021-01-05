library(readxl)
cereal <- read_excel("D:/I/VII/Kapita Selekta Statistika/cereal.xlsx")
View(cereal)
cereal <- cereal+1  

#mengecek missing value
sapply(cereal, function(x) sum(is.na(x))) 
#cek data
str(cereal)
#menghapus beberapa kolom yg tidak dipakai
cereal <- cereal[,-c(1,11)]
#mengubah bentuk data
cereal$manufacturer <- as.factor(cereal$manufacturer)
cereal$...11 <- as.factor(cereal$...11)
cereal$calories <- as.integer(cereal$calories)
cereal$proteins <- as.integer(cereal$proteins)
cereal$fat <- as.integer(cereal$fat)
cereal$sodium <- as.integer(cereal$sodium)
cereal$fiber <- as.integer(cereal$fiber)
cereal$carbohydrates <- as.integer(cereal$carbohydrates)
cereal$sugar <- as.integer(cereal$sugar)
cereal$potassium <- as.integer(cereal$potassium)

#cek ulang beberapa datanya 
str(cereal)
head(cereal)

## VISUALISASI ##
library(ggplot2)

# Memberi Nama Data
cereal$...11 <- sapply(as.character(cereal$...11),switch,
                           "1" = "General Mills",
                           "2" = "Kellogs",
                           "3" = "Quaker")
# Pie Chart
g <- sum(cereal$manufacturer == "General Mills")
k <- sum(cereal$manufacturer == "Kellogs")
q <- sum(cereal$manufacturer == "Quaker")
slices <- c(g,k,q)
lbls <- c("General Mills", "Kellogs", "Quaker")
lbls <- paste(lbls, slices)
lbls <- paste(lbls)
pie(slices, labels = lbls)

ggplot(data = cereal) +
  geom_bar(mapping = aes(x = ...11, fill = potassium)) +
  labs(x = "Manufaktur",
       y = "Pottasium")

ggplot(data = cereal) +
  geom_bar(aes(fill = calories), stat = "identity", position = position_dodge(0.9)) +
  facet_wrap(~manufacturer)