# Projet de séries temporelles : étude de l'indice de production industrielle (IPI) de la fabrication de produits laitiers en France

Matthieu Bricaire - Yseult Masson

Projet mené dans le cadre de la deuxième année du cycle ingénieur de l'ENSAE Paris.

Données : série temporelle mensuelle de l'IPI de la fabrication de prduits laitiers en France, de janvier 1990 à février 2022

# 1. Les données

1.1 Mise en forme de la série

-Suppression d'éléments non pertinents

-Création de nouvelles variables utiles

-Visualisation de la série mise en forme

1.2 Transformation de la série
-Régression linéaire (mise en évidence de l'existence d'une tendance linéaire croissante)
-Tests ADF (Augmented-Dickey-Fuller) et KPSS (Kwiatkowski-Phillips-Schmidt-Shin) pour mettre en évidence la non stationnarité de la série
-Différenciation
-Tests ADF et KPSS pour mettre en évidence la stationnarité de la série différenciée
-Visualisation de la série différenciée

# 2. Modèles ARMA

2.1 Choix d'un modèle ARMA(p,q) pour la série différenciée
-Tracé et étude de l'autocorrélogramme de la série différenciée
-Tracé et étude de l'autocorrélogramme partiel de la série différenciée
-Comparaison de plusieurs modèles : choix du modèle MA(2), qui minimise les critères AIC et BIC

2.2 Etude de la validité du modèle retenu
-Mise en évidence de la significativité des coefficients du modèle (tests de Student)
-Visualisation des résidus du modèle
-Visualisation de l'autocorrélogramme des résidus du modèle
-Mise en évidence de l'absence d'autocorrélation des résidus du modèle (tests de Box-Pierce et de Ljung-Box)

# 3. Prévisions

-Prévisions du modèle pour les mois de mars et avril 2022
-Visualisation des prévisions, et des régions de confiance à 95%
