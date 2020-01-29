#script utilizzato per analizzare il dataset
library(DataExplorer)
library(ggplot2)

#caricamento dataset
dataset= read.csv("bank.csv", header = TRUE, sep=",", row.names=NULL)

dataset$X = NULL

#Analisi introduttiva libreria DataExplorer
introduce(dataset)
#Creazione grafica analisi dataset
plot_intro(dataset)

#fare deviazione ognuno

dataset$y = factor(dataset$y)


table(dataset$y)/nrow(dataset)

barplot(table(dataset$y)/nrow(dataset),
        main = 'Distribuzione della classe',
        ylab = 'Percentuale di campioni',
        xlab = 'Risultato della campagna',
        names = c('Non sottoscritto', 'Sottoscritto'),
        ylim = c(0, 1),
        col = c('blue'))

#Barplot distribuzione label da predirre
barplot(table(dataset$y), 
        main = "Numero di clienti che hanno accettatto la campagna proposta", 
        names = c("False", "True"),
        ylim = c(0, 5000),
        col = c('blue'))

#job in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.job = as.vector(table(df.yes$job)) / 521
df.yes.job
as.vector(table(dataset$y))

df.no = dataset[dataset$y == 'no', ]
df.no.job = as.vector(table(df.no$job)) / 4000
df.no.job

rows = row.names(table(dataset$job))
df.job = data.frame(job_no = df.no.job, job_yes = df.yes.job)
row.names(df.job) = rows

par(mar=c(7,5,2,2)+.1)
barplot(t(df.job), 
        main = "Distribuzione del risultato della campagna in base all\'occupazione",
        col = c('blue', 'green'),
        ylim = c(0, 0.3),
        ylab = 'Proporzione di campioni',
        beside = TRUE,
        las=2)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

par(mar=c(7,5,2,2)+.1)
barplot(table(dataset$job), 
        main = "Distribuzione impieghi dei clienti",
        ylim = c(0, 1000),
        las = 2,
        col = c('blue'))

#marital in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.marital = as.vector(table(df.yes$marital)) / 521
df.yes.marital
as.vector(table(dataset$y))

df.no = dataset[dataset$y == 'no', ]
df.no.marital = as.vector(table(df.no$marital)) / 4000
df.no.marital

rows = row.names(table(dataset$marital))
df.marital = data.frame(job_no = df.no.marital, job_yes = df.yes.marital)
row.names(df.marital) = rows

par(mar=c(4,5,2,2)+.1)
barplot(t(df.marital), 
        main = "Distribuzione del risultato della campagna in base allo stato coniugale",
        col = c('blue', 'green'),
        ylim = c(0, 0.7),
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

par(mar=c(2,2,2,2)+.1)
barplot(table(dataset$marital), 
        main = "Distribuzione stato coniugale",
        col = c('blue'),
        ylim = c(0, 3000))

#education in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.education = as.vector(table(df.yes$education)) / 521
df.yes.education
as.vector(table(dataset$y))

df.no = dataset[dataset$y == 'no', ]
df.no.education = as.vector(table(df.no$education)) / 4000
df.no.education

rows = row.names(table(dataset$education))
df.education = data.frame(no = df.no.education, yes = df.yes.education)
row.names(df.education) = rows

par(mar=c(4,5,2,2)+.1)
barplot(t(df.education), 
        main = "Distribuzione del risultato della campagna in base al titolo di studio",
        col = c('blue', 'green'),
        ylim = c(0, 0.6),
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

par(mar=c(2,2,2,2)+.1)
barplot(table(dataset$marital), 
        main = "Distribuzione stato coniugale",
        col = c('blue'),
        ylim = c(0, 3000))

barplot(table(dataset$education), 
        main = "Distribuzione del tipo di istruzione dei clienti",
        col = c('blue'),
        ylim = c(0, 2500))

barplot(table(dataset$default), 
        main = "Distribuzione clienti con/senza credito in difetto",
        col = c('blue'),
        ylim = c(0, 5000))

df.default.yes = dataset[dataset$default == 'yes', ]
df.default.yes.y = as.vector(table(df.default.yes$y))
df.default.yes.y

df.default.no = dataset[dataset$default == 'no', ]
df.default.no.y = as.vector(table(df.default.no$y))
df.default.no.y

rows = c('Non sottoscritto', 'Sottoscritto')
df.default = data.frame(def_no = df.default.no.y, def_yes = df.default.yes.y)
row.names(df.default) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.default), 
        main = "Distribuzione clienti con/senza credito in difetto",
        col = c('blue', 'green'),
        ylim = c(0, 4000),
        xlab = 'Risultato campagna',
        ylab = 'Numero di campioni',
        beside = TRUE)

legend('topright',
      c('Senza credito in difetto', 'Con credito in difetto'),
      fill = c('blue', 'green'))

barplot(table(dataset$balance), 
        main = "Distribuzione dei bilancio annuale medio",
        col = c('blue'))

par(mar=c(5,5,2,2)+.1)
mean(dataset$balance)
sd(dataset$balance)
max(dataset$balance)
min(dataset$balance)
median(dataset$balance)
quantile(dataset$balance, 0.25)
quantile(dataset$balance, 0.75)

range = 3

quantile(dataset$balance, 0.75) + (IQR(dataset$balance) * range)
quantile(dataset$balance, 0.25) - (IQR(dataset$balance) * range)

#analizzo risultato campagna in base al balance
df.balance.min = dataset[dataset$balance >= 0 & dataset$balance <= 2000, ]
df.balance.min.y = as.vector(table(df.balance.min$y))
df.balance.min.y

df.balance.max = dataset[dataset$balance > 2000, ]
df.balance.max.y = as.vector(table(df.balance.max$y))
df.balance.max.y

df.balance.neg = dataset[dataset$balance < 0, ]
df.balance.neg.y = as.vector(table(df.balance.neg$y))
df.balance.neg.y

rows = c('Non sottoscritto', 'Sottoscritto')
df.balance = data.frame(ban_beg = df.balance.neg.y, ban_min = df.balance.min.y, ban_max = df.balance.max.y)
row.names(df.balance) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.balance), 
        main = "Distribuzione clienti con/senza credito in difetto",
        col = c('blue', 'green', 'lightblue'),
        ylim = c(0, 3000),
        xlab = 'Risultato campagna',
        ylab = 'Numero di campioni',
        beside = TRUE)

legend('topright',
       c('Saldo negativo', 'Saldo tra 0 e 2000', 'Saldo maggiore di 2000'),
       fill = c('blue', 'green', 'lightblue'))
#boxplot
boxplot(dataset$balance,
        range = 3,
        outline = FALSE,
        ylim = c(-4000, 6000),
        ylab = "Bilancio annuo medio")

hist(table(dataset$balance), 
     main = "Distribuzione dei bilancio annuale medio",
     ylab = 'Numero di clienti',
     xlab = 'Bilancio annuale',
     col = c('blue'),
     breaks = c(min(dataset$balance), 0, 500, max(dataset$balance)))

#housing in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.housing = as.vector(table(df.yes$housing)) / 521
df.yes.housing
as.vector(table(dataset$y))

df.no = dataset[dataset$y == 'no', ]
df.no.housing = as.vector(table(df.no$housing)) / 4000
df.no.housing

rows = row.names(table(dataset$housing))
df.housing = data.frame(job_no = df.no.housing, job_yes = df.yes.housing)
row.names(df.housing) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.housing), 
        main = "Distribuzione del risultato della campagna in base al prestito immobiliare",
        col = c('blue', 'green'),
        ylim = c(0, 0.7),
        xlab = 'Prestito immobliare',
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))


par(mar=c(2,2,2,2)+.1)
barplot(table(dataset$housing), 
        main = "Distribuzione clienti con/senza prestito immobiliare",
        col = c('blue'),
        ylim= c(0, 3000))

#loan in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.loan = as.vector(table(df.yes$loan)) / 521
df.yes.loan
as.vector(table(dataset$y))

df.no = dataset[dataset$y == 'no', ]
df.no.loan = as.vector(table(df.no$loan)) / 4000
df.no.loan

rows = row.names(table(dataset$loan))
df.loan = data.frame(job_no = df.no.loan, job_yes = df.yes.loan)
row.names(df.loan) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.loan), 
        main = "Distribuzione del risultato della campagna in base al prestito personale",
        col = c('blue', 'green'),
        ylim = c(0, 1),
        xlab = 'Prestito personale',
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

barplot(table(dataset$loan), 
        main = "Distribuzione clienti con/senza prestito personale",
        col = c('blue'),
        ylim = c(0, 40000))

#plot ultima campagna 
#contact in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.contact = as.vector(table(df.yes$contact)) / 521
df.yes.contact

df.no = dataset[dataset$y == 'no', ]
df.no.contact = as.vector(table(df.no$contact)) / 4000
df.no.contact

rows = row.names(table(dataset$contact))
df.contact = data.frame(job_no = df.no.contact, job_yes = df.yes.contact)
row.names(df.contact) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.contact), 
        main = "Distribuzione tipo di contatto in base al risultato della campagna",
        col = c('blue', 'green'),
        ylim = c(0, 1),
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

barplot(table(dataset$contact),
        main = "Distribuzione metodo di comunicazione durante la campagna",
        col = c('blue'),
        ylim = c(0, 30000))

#day in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.day = as.vector(table(df.yes$day)) / 521
df.yes.day

df.no = dataset[dataset$y == 'no', ]
df.no.day = as.vector(table(df.no$day)) / 4000
df.no.day

rows = row.names(table(dataset$day))
df.day = data.frame(job_no = df.no.day, job_yes = df.yes.day)
row.names(df.day) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.day), 
        main = "Distribuzione del giorno in base al risultato della campagna",
        col = c('blue', 'green'),
        ylim = c(0, 0.08),
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

barplot(table(dataset$day), 
        main = "Distribuzione giorni in cui è avvenuta l'ultima comunicazione",
        col = c('blue'),
        ylim = c(0, 3000),
        xlab = 'Giorni della settimana')

#month in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.month = as.vector(table(df.yes$month)) / 521
df.yes.month

df.no = dataset[dataset$y == 'no', ]
df.no.month = as.vector(table(df.no$month)) / 4000
df.no.month

rows = row.names(table(dataset$month))
df.month = data.frame(job_no = df.no.month, job_yes = df.yes.month)
row.names(df.month) = rows

par(mar=c(5,5,2,5)+.1)
barplot(t(df.month), 
        main = "Distribuzione del risultato della campagna in base al mese dell'ultimo contatto",
        col = c('blue', 'green'),
        ylim = c(0, 0.4),
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

barplot(table(dataset$month), 
        main = "Distribuzione mese in cui è avvenuta l'ultima comunicazione",
        col = c('blue'),
        ylim = c(0, 16000))

barplot(table(dataset$duration), 
        main = "Distribuzione durata l'ultima comunicazione (secondi)",
        col = c('blue'),
        ylim = c(0, 200))

par(mar=c(5,5,2,2)+.1)
hist(dataset$duration, 
     main = "Distribuzione durata l'ultima comunicazione", 
     col = c('blue'),
     xlab = 'Durata (secondi)',
     ylab = 'Numero di clienti')

max(dataset$duration)

boxplot(dataset$duration,
        range = 3,
        outline = FALSE,
        ylim = c(-100, 1000),
        ylab = "Numero di clienti")

#campaign in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.campaign = as.vector(df.yes$campaign)
df.yes.campaign 

df.no = dataset[dataset$y == 'no', ]
df.no.campaign = as.vector(df.no$campaign)
df.no.campaign

df.campaign.max = c(max(df.no.campaign), max(df.yes.campaign))

df.campaign.mean = c(mean(df.no.campaign), mean(df.yes.campaign))
df.campaign.mean

df.campaign.sd = c(sd(df.no.campaign), sd(df.yes.campaign))
df.campaign.sd

names(df.campaign.mean) = c('Non sottoscritto', 'Sottoscritto')
barplot(df.campaign.mean,
        main = 'Numero di contatti medi in base al risultato',
        col = c('blue'),
        ylab = 'Media numero di contatti',
        ylim = c(0, 3))

names(df.campaign.max) = c('Non sottoscritto', 'Sottoscritto')
df.campaign.max
barplot(df.campaign.max,
        main = 'Numero massimo di contatti in base al risultato',
        col = c('blue'),
        ylab = 'Massimo numero di contatti',
        ylim = c(0, 60))

df.campaign.stats = data.frame(mean = df.campaign.mean, max = df.campaign.max)
df.campaign.stats

rows = row.names(table(dataset$campaign))
df.campaign = data.frame(job_no = df.no.campaign, job_yes = df.yes.campaign)
row.names(df.campaign) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.campaign), 
        main = "Distribuzione del numero di contatti in base al risultato della campagna",
        col = c('blue', 'green'),
        ylim = c(0, 0.4),
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

barplot(table(dataset$campaign), 
        main = "Distribuzione numero di contatti durante la campagna",
        col = c('blue'),
        ylim = c(0, 20000),
        xlab = 'Numero di contatti',
        ylab = 'Numero di clienti')

boxplot(dataset$campaign,
        range = 3,
        outline = FALSE,
        ylim = c(0, 10),
        ylab = "Numero di clienti")

#plot altri
#-1 = non contattato
barplot(table(dataset$pdays), 
        main = "Distribuzione numero di giorni trascorsi dopo il contattato per una campagna precedente",
        col = c('blue'))

hist(dataset$pdays, 
     main = "Distribuzione numero di giorni trascorsi dopo il contattato per una campagna precedente", 
     col = c('blue'),
     ylim = c(0, 40000),
     xlim = c(-200, 1000),
     xlab = 'Numero di giorni',
     ylab = 'Numero di clienti')

mean(dataset$pdays)
sd(dataset$pdays)
max(dataset$pdays)
min(dataset$pdays)

boxplot(dataset$pdays,
       range = 3,
       outline = FALSE,
       ylab = "Numero di clienti")

barplot(table(dataset$previous), 
        main = "Distribuzione numero di comunicazioni con il cliente prima dell'attuale campagna",
        col = c('blue'))

hist(dataset$previous, 
     main = "Distribuzione numero di comunicazioni con il cliente prima dell'attuale campagna", 
     xlab="NUmero di comunicazioni",
     ylab = 'Numero di client',
     col = c('blue'),
     ylim = c(0, 50000),
     xlim = c(0, 300))

boxplot(dataset$previous,
        range = 3,
        ylab = "Numero di clienti")

#poutcome in base alla classe
df.yes = dataset[dataset$y == 'yes', ]
df.yes.poutcome = as.vector(table(df.yes$poutcome)) / 521
df.yes.poutcome

df.no = dataset[dataset$y == 'no', ]
df.no.poutcome = as.vector(table(df.no$poutcome)) / 4000
df.no.poutcome

rows = row.names(table(dataset$poutcome))
df.poutcome = data.frame(job_no = df.no.poutcome, job_yes = df.yes.poutcome)
row.names(df.poutcome) = rows

par(mar=c(5,5,2,2)+.1)
barplot(t(df.poutcome), 
        main = "Distribuzione del risultato della campagna precedente",
        col = c('blue', 'green'),
        ylim = c(0, 1),
        ylab = 'Proporzione di campioni',
        beside = TRUE)

legend('topright',
       c('Non sottoscritto', 'Sottoscritto'),
       fill = c('blue', 'green'))

barplot(table(dataset$poutcome), 
        main = "Distribuzione risultati precedente campagna",
        col = c('blue'),
        ylim = c(0, 40000),
        xlab = "Risultati",
        ylab = "Numero di clienti")
