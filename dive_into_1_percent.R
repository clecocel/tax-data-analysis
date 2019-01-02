library(stringr)

setwd("/Users/clement/Documents/") # Replace this with the actual path to the data file
impots = read.csv("impots-france.csv")

# Data cleaning
space = substr(impots$Seuil.de.revenu.brut.annuel[1], 2,2)

clean_num <- function(str_vector) {
  str_vector <- str_replace_all(str_vector, "€", "")
  str_vector <- str_replace_all(str_vector, "%", "")
  str_vector <- str_replace_all(str_vector, ",", "")
  str_vector <- str_replace_all(str_vector, space, "")
  str_vector <- as.numeric(str_vector)
  return(str_vector)
}

impots$seuil_trimmed <- clean_num(impots$Seuil.de.revenu.brut.annuel)

impots$revenu_moy_trimmed <- clean_num(impots$Revenu.brut.annuel.moyen)

impots$Nombre.d.individus <- clean_num(impots$Nombre.d.individus)

impots$revenu_total = impots$revenu_moy_trimmed * impots$Nombre.d.individus

impots$Impôt.total <- clean_num(impots$Impôt.total)

impots$IRPP <- clean_num(impots$IRPP)

impots$PPE <- clean_num(impots$PPE)

revenu_total_sum = sum(impots$revenu_total)

# Understanding exponential
impots$seuil_trimmed <- impots$seuil_trimmed / 1000
max_rev = impots$seuil_trimmed[127]
ylims = c(0, max_rev)
plot(impots$Quantile, impots$seuil_trimmed, 'l', ylim=ylims, main="Whole population", ylab="Revenue (in k€)", xlab="Percentile")
plot(impots$Quantile[91:127], impots$seuil_trimmed[91:127], 'l', main="Top 10%", ylab="Revenue (in k€)", xlab="Percentile", ylim=ylims)
plot(impots$Quantile[100:127], impots$seuil_trimmed[100:127], 'l', main="Top 1%", ylab="Revenue (in k€)", xlab="Percentile", ylim=ylims)
plot(impots$Quantile[109:127], impots$seuil_trimmed[109:127], 'l', main="Top 0.1%", ylab="Revenue (in k€)", xlab="Percentile", ylim=ylims)
plot(impots$Quantile[118:127], impots$seuil_trimmed[118:127], 'l', main="Top 0.01%", ylab="Revenue (in k€)", xlab="Percentile", ylim=ylims)
plot(impots$Quantile[1:100], impots$seuil_trimmed[1:100], 'l', main="Percentile 0-99", ylab="Revenue (in k€)", xlab="Percentile")
