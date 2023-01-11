# 11/01 Lezione con Chiarucci, Arianna ed Elisa. Analisi dati R di biodiversità.
# Riprende concetti base di R, stiamo usando R studio non R classico

# Riprendiamo le matrici di comunità

############################ PRIMO SCRIPT

### COMMUNITY MATRIX: vegan::dune CO-OCCURENCE DATA
###########################
# 
library(vegan)
library(tidyverse) # manipolazione del dato
?vegan

# load community and environmental data (vegan package)
data(dune)
data(dune.env)

View(dune)
View(dune.env)

# dimensions (rows, columns, cells)
# in R: data[row(s), column(s)]
dim(dune) # two dimensions for data.frame objects
class(dune)
rownames(dune)
colnames(dune)
dune[5:7, 5:9]

head(dune) # mi fa vedere le prime 6 righe di tutte le colonne
tail(dune) # stessa cosa ma con le ultime 6 righe
dune[3:5, 3:7] > 2
dune[1:5, 1:5] > 0

# we apply a condition, which can be true or false
rowSums(dune[1:5, 1:5] > 0) # fa la somma della riga se ci sono elementi maggiori di 0, mi dà il conto di quanti T ci sono per riga: quante volte per riga la mia condizione viene rispettata 

# species richness: summing all occuring species (cover > 0) per observations
rich <- rowSums(dune > 0)
rich
class(rich)

# tidyverse library is used for the data management
# code's grammar is a bit different from base R code
library(tidyverse)
# ctrl + shift + M the shortcut for the pipe operator of tidyverse: %>% 
# %>% pipe per concatenare le funzioni

# Using a pipe-line of tidyverse
a <- rich %>% # tutte le funzioni vanno a lavorare su rich, devo prima scrivere il dataset che devo andare a usare 
  sort() %>% # ordinami il dataset
  as.data.frame() %>%  # fallo diventare un data frame
  rename(richness = ".") # rinomina la colonna. il punto è il nome della colonna, è una roba di default, niente che devo trovare. è semplicemente un elemento che poi io vado a rinominare. non si sa se funziona per più di una colonna
a

# Devo cercare es %>% è la pipe di tidyverse (credo), però funziona anche la pipeline che uso io |>



############################# SECONDO SCRIPT

### COMMUNITY MATRIX: vegan::dune CO-OCCURENCE DATA
###########################
library(vegan)
?vegan
# library(tidyverse)

data(dune)
data(dune.env)

# dimensions (rows, columns, cells)
# in R: data[row(s), column(s)]
dim(dune)

# co-occurrence species data
# each record/observation/vegetation plot (rows) corresponds
# to cover values of all the species, from 0 to max cover (9 in our case)
dune
dune[1:10, 1:10] 

# from co-occurence data to occurrence data using tidyverse
# each record/observation (rows) of occurrence data corresponds
# to cover values of a single species in a locality

library(tidyverse)
# ctrl + shift + M the shortcut for the pipe operator of tidyverse: %>% 

occurrence_dune <- dune %>% 
  rownames_to_column("id") %>% # create a new column using names of rows
  pivot_longer(!id, names_to = "sp", values_to = "cover") %>% # pivot table with species (present and absent) as single record for each plot
  filter(cover > 0) # exclude all absent species

occurrence_summary <- occurrence_dune %>% 
  group_by(sp) %>% 
  summarise(mean_cover = mean(cover),
            n = n())

### MULTIVARIATE ANALYSES: CLUSTERING AND ORDINATION
##########################

vegan::specnumber(dune) # built-in function of vegan library to calculate species richness. :: serve perchè in diversi pacchetti ci sono funzioni che hanno lo stesso nome, se chiamo prima il pacchetto e poi la funzione non rischio di sbagliare funzione

rowSums(dune) # somma riga
max(dune) # plot con l'osservazione con il valore più alto

cover_level <- dune %>% # psrto da dune
  rownames_to_column("id") %>% # deve spostarmi il nome delle righe e farla diventare una colonna a sè con nome ID
  pivot_longer(!id, names_to = "sp", values_to = "cover") %>% # raggruppa in base alla copertura le specie
  filter(cover > 0) %>% # filtro le specie che hanno copertura >0 perchè se devo fare analisi le faccio con le specie presenti nella comunità
  pull(cover) %>% # dei tibble mi serve solo la colonna cover
  unique() %>% # dimmi quali sono i valori unici
  sort() # mettili in ordine

cover_level

# presence-absence matrix, su scale micro uso le abbondanze, se ho tanti dati e studio i gradienti su larga scala converto in pa
dune_pa <- decostand(dune, method = "pa") # decostand standardizza in base a quello che chiedo io, in questo caso trasformo il dataframe in presenza-assenza
dune_pa[1:5, 1:7]

hist(colSums(dune_pa)) # ...many rare species. istogramma della presenza delle specie: è una specie comune o una rara? uso la frequenza
# poche comunità con molte specie

dune_pa %>% 
  colSums() %>% 
  hist()

dune_pa %>% 
  colSums() %>% 
  sort(decreasing = TRUE) #riordina in mono decrescente


# data transformation of species cover values. trasformiamo dataframe in matrice
hist(as.matrix((dune))) # double-zero issue
hist(as.matrix((dune[dune > 0]))) # right skewed cover data. toglie le specie che hanno 0

dune_log <- log1p(dune) #rendo gaussiano il mio dato. trasformazione del dato per renderlo conforme agli assunti delle statistiche che dobbiamo fare
dune_sqrt <- sqrt(dune) 

par(mfrow = c(2, 3))
hist(as.matrix((dune[dune > 0])),
     main = "Ordinal cover data",
     xlab = "Species cover values",
     ylab = "Frequency") # right skewed cover data
hist(as.matrix((dune_sqrt[dune_sqrt > 0])), 
     main = "Log-transformed cover data",
     xlab = "Species cover values",
     ylab = "Frequency")
hist(as.matrix((dune_log[dune_log > 0])), 
     main = "Square root-transformed cover data",
     xlab = "Species cover values",
     ylab = "Frequency")
car::qqPlot(dune[dune > 0])
car::qqPlot(dune_sqrt[dune_sqrt > 0])
car::qqPlot(dune_log[dune_log > 0])



### Ordination
# Principal component analysis
### See detail on vegan::rda() at: 
# https://cran.r-project.org/web/packages/vegan/vegan.pdf


# PCA dà overview del dato, si vede in 2D quello che non lo è. 
pca <- rda(dune_log) # funzione rda fatta sulla curva log trasformata. dà coordinate agli elementi
summary(pca)

# ordination biplots. plottiamo la pca
ordiplot(pca, display = "si") # plot sui siti
ordiplot(pca, display = "sp", type = "text") # plot sulle specie. vedo come sono simili nella risposta alla nicchia ecologica in base alla distanza

str(pca) # structure of pca object

pca$tot.chi
(pca$CA$eig[1] / pca$tot.chi) * 100 # explained variance by first axis

((pca$CA$eig[1] + pca$CA$eig[2]) / pca$tot.chi) * 100 # explained variance by first and second axes

summary(pca)


### Clustering
# plot partition according their specis composition, using K-means clustering
set.seed(1221) # fix the random factor
clustering <- kmeans(dune_log, 3)
clustering
?kmeans

groups <- clustering$cluster # obtain group assignment

groups %>% 
  table()

# plot groups in the  multivariate space of the pca
ordiplot(pca, display = "si", type = "n")
for(i in 1:3) ordihull(pca, groups = groups, show.groups = i, col = i)
for(i in 1:3) ordispider(pca, groups = groups, show.groups = i, col = i)

# apply pca on environmental variables of our plot observations
head(dune.env)
str(dune.env)

par(mfrow = c(1, 3))

# Plot soil thickness
plot(pca, display = "sites", type = "n",
     main = "Soil thickness") 
with(dune.env, points(pca, disp = "si", cex = as.numeric(A1)))
with(dune.env, text(pca, disp = "si", labels = as.numeric(A1)))
ordisurf (pca, dune.env[, 'A1'], add = T, col = 'red', )

# Plot Moisture
plot(pca, display = "sites", type = "n", 
     main = "Soil Moisture")
with(dune.env, points(pca, disp = "si", pch = as.numeric(Moisture)))
with(dune.env, legend("topleft", levels(Moisture), pch = 1:4,
                      title = "Moisture"))
with(dune.env, ordispider(pca, Moisture, label = TRUE))

ordiplot(pca, display = "si", type = "n",
         main = "Species composition")
for(i in 1:3) ordihull(pca, groups = groups, show.groups = i, col = i)
for(i in 1:3) ordispider(pca, groups = groups, show.groups = i, col = i, label = T)

# Boxplot of species richness. di nuovo dato di presenza-assenza, ho riunito i gruppi, ho calcolato la ricchezza di specie per ogni gruppo e poi l'ho plottato.
boxplot_data <- dune %>% 
  decostand(., method = "pa") %>% 
  rowSums() %>% 
  as.data.frame() %>% 
  cbind(groups) %>% 
  rename(rich = ".") %>%
  mutate(groups = as.factor(groups)) 

boxplot(boxplot_data$rich ~ boxplot_data$groups)



### INDICATOR SPECIES ANALYSIS
### Statistical Test for species co-occurrence patterns
###########################

# Specie indicatrici: specie presenti in luoghi che hanno caratteristiche particolari. Specie che hanno occorrenze simili spesso hanno caratteristiche simili quindi arrivo a capire che tipo di ambiente trovo
# Before to run the test, we take a look to species frequencies among groups
groups
groups == 1 # apply a condition

dune_pa <- decostand(dune, method = "pa")
group_1 <- dune_pa[groups == 1, ] # apply a condition to a dataset, on rows
# colSums(group_1) > 0 # new condition
group_1 <- group_1[, colSums(group_1) > 0] # apply a new condition to the dataset, on columns
# t(group_1) # transpose 
group_1 <- as.data.frame(t(group_1)) # transform the transpose of a matrix, to dataframe
group_1 <- sort(rowSums(group_1), decreasing = TRUE) # I sort species to be identify faster most frequent species
group_1 <- data.frame(row.names = c(1:length(group_1)), sp = names(group_1), fr = group_1) # I define a new dataframe with meaningful field and names

# Apply the same procedure to the other two groups (this could be done using a for() loop)
comm_pa <- decostand(dune, method = "pa")
group_2 <- comm_pa[groups == 2, ]
group_2 <- group_2[, colSums(group_2) > 0]
group_2 <- as.data.frame(t(group_2))
group_2 <- sort(rowSums(group_2), decreasing = TRUE) 
group_2 <- data.frame(row.names = c(1:length(group_2)), sp = names(group_2), fr = group_2)

# gr3...
comm_pa <- decostand(dune, method = "pa")
group_3 <- comm_pa[groups == 3, ]
group_3 <- group_3[, colSums(group_3) > 0]
group_3 <- as.data.frame(t(group_3))
group_3 <- sort(rowSums(group_3), decreasing = TRUE)
group_3 <- data.frame(row.names = c(1:length(group_3)), sp = names(group_3), fr = group_3)

### Merge the three dataframe using the "sp" field as link between dataframes
tot_species <- merge(x = group_1, y = group_2, by = "sp", all = TRUE)
tot_species <- merge(x = tot_species, y = group_3, by = "sp", all = TRUE)
tot_species[is.na(tot_species)] <- 0
names(tot_species) <- c("sp", "gr_1", "gr_2", "gr_3")


# install.packages("indicspecies")
library(indicspecies)

indi <- multipatt(dune_pa, groups) # see default parameters to be sure on the test you are using
# See tutorial at
# https://cran.r-project.org/web/packages/indicspecies/vignettes/indicspeciesTutorial.pdf

summary(indi)

### EXERCISE
# clustering (kmeans) of plots according the site chemical compositionn
# ordination plot (first and second pca axes) with site and "species"
# indicator species for each group

# Data set
data(varespec)
data(varechem)

# Main functions
kmeans()
rda()
multipatt()



############################### TERZO SCRIPT
#b additiva è numero di specie mancanti da campione rispetto al totale. la moltiplicativa non si misura sulla totalità ma cercatela. 
# la beta diversità non è il turnover, tutt'altra cosa. la beta è più una differenza tra i diversi plot. 
# beta moltiplicativa: posso creare le curve di accumulo perchè aumenta con il numero di unità che vengono campionate.



install.packages(c("vegan", "Hmisc", "betapart"))


library(vegan)
library(Hmisc)
library(betapart)

# upload a dataset from a *.csv
dat <- read.csv("C:/Users/Letizia Rattighieri/OneDrive/Desktop/script/data_biomac_2.csv")
dim(dat) # ci sono sia plot che specie, non è stato separato in scheda ambientale e specie ma è tutto insieme
names(dat)
head(names(dat), n = 10)
dat[1:5, 1:10]

# Hierarchical sampling design
# unique() to look at the different classes for each hierarchical category
unique(dat$Habitat)
unique(dat$Region)
unique(dat$Dataset)
unique(dat$Management)

comm <- dat[, -(1:5)] # elimino le prime 5 colonne del dataset vecchio perchè non mi servono le variabili ambientali per calcolare la beta
comm_pa <- decostand(comm, "pa")

### Additive partitioning ###
?adipart
adipart(comm_pa) # funzione che crea alfa diversità a livello di plot, paragona i plot a livello di gamma su tutto il dataset e poi estraggo la beta diversità.

# i modelli nulli sono modelli randomizzati, sono più teorici, rappresenta la regola generale. randomizza specie e plot per capire se il valore è in un range di valore casuale, serve per dare misura statistica.
# poi mi paragona l'osservato con il teorico. 
# Alpha: local diversity, mean species richness
# Gamma: regional diversity, total diversity
# Beta: missing species from the species pool, dark diversity

adi_mod <- adipart(comm_pa, dat[, c("Plot", "Habitat", "Region")]) #(alpha beta gamma)

adi_mod
# plot richness higher than expected
# missing species from the region: lower than expected

str(adi_mod)
adi_mod$statistic
obs <- adi_mod$statistic[c("alpha.1", "beta.1")]
expc <- adi_mod$oecosimu$means[c(1, 4)]
adi_mat <- matrix(data = c(obs, expc), ncol = 2)

barplot(adi_mat,
        space = 1,
        xlim = c(1, 5),
        ylab = "Number of species",
        names.arg = c("Observed", "Expected"),
        legend.text = c("Alpha plot", "Beta plot"),
        args.legend = list(x = "topright"))

# alpha è più alta rispetto all'attesa. la differenza nel plot è la beta diversità. prendo gli individui (pa) e li ributto a caso nei plot, il numero verrebbe più basso. 
# ...we go further across the hierarchical scheme
adipart(comm_pa, dat[, c(1, 5, 4)]) # plots (alpha1), management (alpha2) and dataset (gamma)
adipart(comm_pa, dat[, 1:4]) # plots (alpha1), habitat (alpha2), region (alpha3) and gamma (gamma)


# beta moltiplicativa 
### Multiplicative partitioning ###
?multipart
multipart(comm_pa, scales = 0)
multipart(comm_pa, dat[, c("Plot", "Habitat", "Region")], scales = 0)
multipart(comm_pa, dat[, c("Plot", "Habitat", "Region", "Dataset")], scales = 0)

mlt_mod <- multipart(comm_pa, dat[, 1:4])
str(mlt_mod)
obs <- mlt_mod$statistic[c(5, 6, 7)]
expc <- mlt_mod$oecosimu$means[c(5, 6, 7)]
multi_mat <- matrix(data = c(obs, expc), ncol = 2)

barplot(multi_mat,
        space = 1,
        xlim = c(1, 5),
        ylab = "Number of species",
        names.arg = c("Observed", "Expected"),
        legend.text = c("Alpha plot", "Beta plot"),
        args.legend = list(x = "topright"))

### Beta partitioning (Baselga framework) ###
### Load the dataset "Barrio Colorado Islands"

data(BCI)

### And check the structure

str(BCI)

### Exercise ###
### Calculate and plot species richness at plot-level for the entire dataset and
### convert matrix to presence/absence

sr <- specnumber(BCI)
hist(sr,
     breaks = 20,
     xlab = "Species Richness",
     ylab = "Number of plots",
     main = "")

BCI <- decostand(BCI, "pa") # transform the abundance matrix into a presence/absence matrix

### Calculate distance and similarity metrics among pairs of plots and multiple plots

vegdist(BCI[1:2, ], method = "jaccard") # Similarity index 
1 - vegdist(BCI[1:2, ], method = "jaccard") # Distance index
vegdist(BCI[2:3, ], method = "jaccard")
vegdist(BCI[1:3, ], method = "jaccard")
vegdist(BCI, method = "jaccard")
hist(vegdist(BCI, method = "jaccard"),
     xlab = "Jaccard's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")

hist(vegdist(BCI, method = "bray"),
     xlab = "Sorensen's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")

par(mfrow = c(1, 2))
hist(vegdist(BCI, method = "jaccard"),
     xlab = "Jaccard's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")
hist(vegdist(BCI, method = "bray"),
     xlab = "Sorensen's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")
par(mfrow = c(1, 1))

### Partitioning beta diversity

?beta.multi
beta.multi(BCI)
beta.multi(BCI[1:2, ])
beta.multi(BCI[1:3, ])

?beta.pair
beta.pair(BCI[1:3, ])
beta.pair(BCI)
boxplot(beta.pair(BCI))
boxplot(beta.pair(BCI),
        names = c("Turnover", "Nestedness", "Total beta"))

par(mfrow = c(1, 2))
boxplot(beta.pair(BCI),
        names = c("Turnover", "Nestedness", "Total beta"),
        main = "Sorensen")
boxplot(beta.pair(BCI, "jaccard"),
        names = c("Turnover", "Nestedness", "Total beta"),
        main = "Jaccard")
par(mfrow = c(1, 1))

### Distance decay

data(BCI.env)

spat_dist <- dist(BCI.env[, 1:2])
dissim_BCI <- beta.pair(BCI_pa)$beta.sor

plot(spat_dist, dissim_BCI$beta.sim, ylim = c(0.1, 0.6), xlim = c(0, max(spat_dist)))
pr <- decay.model(dissim_BCI$beta.sor, spat_dist, 
                  model.type = "pow")
summary(pr$model)
summary(pr$data.y)
plot(pr)
bci_decay <- glm(dissim_BCI$beta.sor ~ spat_dist, family = "binomial")
plot(bci_decay)

summary(bci_decay)
bci_decay
xs <- seq(0, 1000, 10)
ys <- predict(bci_decay, list(spat_dist = xs), type = "response")
lines(xs, ys, col = "red", lwd = 3)



### EXERCISE
data(mite)
data(mite.xy)

mite_dist <- dist(mite.xy)

mite <- decostand(mite, "pa")
mite_dissim <- beta.pair(mite)$beta.sor

plot(mite_dist, mite_dissim, xlim = c(0, max(mite_dist)), las = 1,
     xlab = "Spatial distance (m.)", ylab = "Sorensen's dissimilarity")

mite_decay <- glm(mite_dissim ~ mite_dist, family = "binomial")
mite_decay
summary(mite_decay)

xs <- seq(0, 10, 0.1)
ys <- predict(mite_decay, list(mite_dist = xs), type = "response")

plot(mite_dist, mite_dissim, xlim = c(0, max(mite_dist)), las = 1,
     xlab = "Spatial distance (m.)", ylab = "Sorensen's dissimilarity")
lines(xs, ys, col = "red", lwd = 3)


forest <- readRDS("data/probabilistic_sample.rds")

comm_pa <- vegan::decostand(forest$plot[,c(9:ncol(forest$plot))], method = "pa")
eco_dist <- beta.pair(comm_pa)$turnover
hist(as.matrix(eco_dist))

plot(dist_for, eco_dist,  xlab = "Spatial distance (m.)", ylab = "Sorensen's dissimilarity")

forest$plot[, c(1:8)]

library(tidyverse)
sam <- forest$plot %>% dim()
group_by(site) %>% 
  sample_n(1)
comm <- forest$plot[, 9:ncol(forest$plot)]
colSums(comm)

dist_for <- dist(forest$plot[, c(2:3)])
comm_c <- comm[, colSums(comm) > 0]
comm_pa <- vegan::decostand(comm_c, method = "pa")
eco_dist <- beta.pair(comm_pa)$beta.sor

plot(dist_for, eco_dist, 
     xlab = "Spatial distance (m.)", 
     ylab = "Sorensen's dissimilarity")

mite_decay <- glm(eco_dist ~ dist_for, family = "binomial")
mite_decay
summary(mite_decay)

xs <- seq(0, 10, 0.1)
ys <- predict(mite_decay, 
              list(dist_for = xs), type = "response")

plot(dist_for, eco_dist, 
     xlab = "Spatial distance (m.)", ylab = "Sorensen's dissimilarity")
lines(xs, ys, col = "red", lwd = 3)

sam
comm_pa <- forest$plot[, 9:ncol(forest$plot)] %>% 
  decostand(., method = "pa")
comm_pa <- comm_pa[, colSums(comm_pa) > 0]

adi_mod <- adipart(comm_pa, 
                   forest$plot[, c("id", "site", "veg")])
%>% 
  ungroup())

ss <- read.csv("data/data_biomac_2.csv")
str(ss)
str(sam)
env <- forest$env








