###################################
# Projet de séries temporelles
# Matthieu Bricaire - Yseult Masson
###################################


###################################
# Chargement des librairies utiles
###################################

library(tseries)
library(forecast)
library(fUnitRoots)

###########################
# Partie 1 : Les données
###########################

# 1.1 : Série choisie

#Remarque : sauvegarder le code et les données dans un même dossier, 
#et spécifier le 'working directory' de la session sur 'Source file location'.

#On charge les données, et on nomme les colonnes du Dataframe
data = read.csv("valeurs_mensuelles.csv", sep=";", col.names = c('Dates', 'Indice', 'Codes'))

#On enlève les trois premières lignes, qui ne sont pas pertinentes
#On enlève également la troisième colonne, qui n'est pas utile
data = data[-(1:3), 1:2 ]

#On réinitialise l'index du DataFrame
rownames(data) = NULL

#On transforme la colonne 'Dates' de sorte que les dates soient bien 
#reconnues comme telles
data$Dates <- as.Date(paste(data$Dates,1,sep="-"), format="%Y-%m-%d")

#On créé deux nouvelles colonnes, contenant les années et les mois
#associés aux dates de la série
annee = as.numeric(format(data$Dates, format = "%Y")) 
mois = format(data$Dates, format = '%m')

#On ajoute les nouvelles colonnes au Dataframe initial
data = cbind(data, annee, mois)

#On convertit les valeurs de l'indice en données numériques
data$Indice <- as.numeric(data$Indice)

#On créé la série temporelle associée aux valeurs prises par l'indice
#de production
Xt.ts <- ts(data$Indice, start=c(1990, 1), end=c(2022, 2), frequency=12)

#On trace la série, et on sauvegarde le graphique obtenu
png('Serie_initiale.png', width=600, height=450)
plot.ts(Xt.ts, xlab="Années", ylab="Indice")
dev.off()

# 1.2 : transformation de la série

#Régression linéaire de l'indice sur les dates
regLinIndice=lm(Xt.ts ~ Dates,data=data)
summary(regLinIndice)

#La régression précédente met en évidence une tendance linéaire 
#croissante de la série (coefficient significativement positif)
#A priori, la série ne semble donc pas stationnaire

#On vérifie la non stationnarité de la série à l'aide de tests ADF et KPSS

adf.test(Xt.ts)
#La p-value associée à ce test vaut 0.70
#L'hypothèse nulle (non stationnarité de la série) n'est rejetée
#à aucun niveau usuel

kpss.test(Xt.ts)
#La p-value associée à ce test est inférieure à 0.01
#L'hypothèse nulle (stationnarité de la série) est rejetée
#à tous les niveaux usuels

#Conclusion : la série n'est pas stationnaire, on va la différencier
#pour tenter de la rendre stationnaire

#Différenciation de la série, ajout d'un NA pour la valeur
#de "l'accroissement 0", qui n'est pas défini
diff_indice.ts <- ts(c(NA,diff(data$Indice,1)), start=c(1990, 1), end=c(2022, 2), frequency=12)

#On vérifie la stationnarité de la série différenciée à l'aide de tests ADF et KPSS

adf.test(na.omit(diff_indice.ts))
#La p-value associée à ce test est inférieure à 0.01
#L'hypothèse nulle (non stationnarité de la série) est rejetée
#à tous les niveaux usuels

kpss.test(na.omit(diff_indice.ts))
#La p-value associée à ce test est supérieure à 0.1
#L'hypothèse nulle (stationnarité de la série) n'est rejetée
#à aucun niveau usuel 


#Conclusion : on peut considérer la série différenciée comme stationnaire

#On trace la série différenciée, et on sauvegarde le graphique obtenu
png('Serie_differenciee.png', width=600, height=450)
plot.ts(diff_indice.ts, xlab="Années", ylab="Accroissements")
dev.off()

###########################
# Partie 2 : Modèles ARMA
###########################

# 2.1 : choix d'un modèle ARMA(p,q) pour la série différenciée

#On trace l'autocorrélogramme de la série différenciée
png('Autocorrélogramme_serie_differenciee.png', width=400, height=300)
acf(diff_indice.ts,na.action=na.omit)
dev.off()

#L'autocorrélogramme semble ne plus présenter de "pics" significatifs au delà de l'ordre 2
#De plus, nous choisissons d'ignorer les pics pour des retards supérieurs à 6
#Nous allons chercher 0 <= q <= 2 (q_max=2)

#On trace l'autocorrélogramme partiel de la série différenciée
png('Autocorrélogramme_partiel_serie_differenciee.png', width=400, height=300)
pacf(diff_indice.ts,na.action=na.omit)
dev.off()

#L'autocorrélogramme partiel semble ne plus présenter de "pics" significatifs au delà de l'ordre 3
#De plus, nous choisissons d'ignorer les pics pour des retards supérieurs à 6
#Nous allons chercher 0 <= p <= 3 (p_max=3)

#Création de la grille de paramètres
grille=expand.grid(p=seq(0,3),q=seq(0,2)) 

#On supprime l'ARMA(0,0), que l'on ne teste pas
grille=grille[-c(1),] 

#On créé un Dataframe qui regroupe les différentes combinaisons 
#de paramètres à tester
tableau_modeles=data.frame("p"=grille$p,"q"=grille$q)

#On enlève le NA au début de la série différenciée
diff_indice_2.ts = na.omit(diff_indice.ts)

#On teste toutes les combinaisons (p,q) de la grille
#On ajoute les scores BIC et AIC de chaque modèle au Dataframe tableau_modeles
for (i in (1:nrow(grille))){
  modTemp=try(arima(diff_indice_2.ts,order=c(tableau_modeles$p[i],0,tableau_modeles$q[i]),include.mean = T))
  tableau_modeles$AIC[i]=if (class(modTemp)=="try-error") NA else modTemp$aic
  tableau_modeles$BIC[i]=if (class(modTemp)=="try-error") NA else BIC(modTemp)
}

#On sélectionne l'ARMA qui minimise le critère AIC
minAIC=which.min(tableau_modeles$AIC)
tableau_modeles[minAIC,]

#La minimisation du critère AIC conduit à sélectionner le modèle MA(2)
#L'AIC de ce modèle vaut 1334.319

#On sélectionne l'ARMA qui minimise le critère BIC
minBIC=which.min(tableau_modeles$BIC)
tableau_modeles[minBIC,]

#La minimisation du critère BIC conduit à sélectionner le modèle MA(2)
#Le BIC de ce modèle vaut 1350.132

#On appelle le modèle retenu : il s'agit d'un MA(2) pour la série différenciée
selected_model=arima(diff_indice_2.ts,order=c(tableau_modeles$p[minBIC],0,tableau_modeles$q[minBIC]),include.mean = T)
selected_model

#On va tester la significativité du modèle

#On commence par récupérer les statistiques de test associées
#aux coefficients estimés
t=selected_model$coef/sqrt(diag(selected_model$var.coef))

#On calcule les p-valeurs associées aux statistiques de test (formule pour un test bilatéral)
pval=(1-pnorm(abs(t)))*2

#On regroupe les coefficients, les écarts-types, les statistiques 
#de test et les p-valeurs dans un DataFrame
results=rbind(coef=selected_model$coef,se=sqrt(diag(selected_model$var.coef)),t,pval)

#Conclusion : les coefficients du modèle MA(2) sont significatifs à 
#tous les niveaux usuels (notamment le coefficient ma2).

#Pour que le modèle soit valide, il faut également que les résidus se 
#comportent comme un bruit blanc

#On représente graphiquement les résidus du modèle, on sauvegarde 
#le graphique ainsi obtenu
png('Residus_modele_MA2.png', width=400, height=300)
plot(selected_model$residuals)
dev.off()

#On représente graphiquement les autocorrélations des résidus, on 
#sauvegarde le graphique ainsi obtenu
png('Autocorrélations_modele_MA2.png', width=400, height=300)
plot(acf(selected_model$residuals))
dev.off()

#Les deux graphiques précédents semblent montrer que les résidus du
#modèle se comportent bien comme un bruit blanc

#On teste l'absence d'autocorrélation des résidus avec des tests de Box-Pierce
#On commence à tester l'absence d'autocorrélation à partir de p+q+1=3
#On va jusqu'à 22, de sorte à avoir 22-2=20 autocorrélations testées
bpTest=lapply(seq(3,22),Box.test,x=selected_model$residuals,type="Box-Pierce",fitdf=2)

#Le test de Box-Pierce ne rejette jamais l'hypothèse nulle (absence d'autocorrélation des résidus)
#Il semble donc ne pas y avoir d'autocorrélation des résidus

#On confirme cette impression avec des tests de Ljung-Box, plus performants que les tests de Box-Pierce 
lbTest=lapply(seq(3,22),Box.test,x=selected_model$residuals,type="Ljung-Box",fitdf=2)

#Le test de Ljung-Box ne rejette jamais l'hypothèse nulle (absence d'autocorrélation des résidus)

#Les résultats des test précédents permettent d'affirmer qu'il
#n'y a pas d'autocorrélation au sein des résidus

#Conclusion : les coefficients du modèle MA(2) sont significatifs
#à tous les niveaux usuels, et les résidus du modèle se comportent
#comme un bruit blanc. Le modèle MA(2) est donc valide.

###########################
# Partie 3 : Prévision
###########################

#On prévoit les valeurs de la série différenciée
#aux horizons T+1 et T+2 (mars et avril 2022), et les régions de confiance à 95% associées
prevision = forecast(selected_model, h=2, level=0.95)

#On trace les prévisions précédemment calculées, et on sauvegarde le graphique obtenu
#Pour plus de lisibilité, on se restreint à l'affichage 
#de l'intervalle de temps 2020-2022

png('Prévisions_modele_MA(2).png', width=400, height=300)
plot(prevision, xlim=c(2020, 2022.2), ylim=c(-3, 3.9), xlab='Années', ylab='Accroissements')
dev.off()

