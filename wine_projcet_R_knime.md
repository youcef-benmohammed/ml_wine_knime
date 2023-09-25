---
title: "Analyse comparative des modèles de classification pour la prédiction des cépages de vin italien"
author: "Youcef BEN MOHAMMED"
date: "`r Sys.Date()`"
output: 
  rmdformats::readthedown:
    self_contained: true
    thumbnails: true
    lightbox: true
    gallery: false
    highlight: tango
    df_print: paged
    toc_depth: 6
    number_sections: true
    collapsed: false
    fig_caption: true
    fig_align: "center"
---




Options Knitr
```{r}
knitr::opts_chunk$set(cache=FALSE)
knitr::opts_chunk$set(fig.width=10) # for rstudio
knitr::opts_chunk$set(fig.width=6, fig.height=6) # for rstudio
knitr::opts_chunk$set(fig.width=14, fig.height=14) # for html
options(width = 100)



```

Loading libraries
```{r}
library(tidyverse)
library(magrittr) # syntaxe, notamment affectation %<>%
library(GGally)   # plot pairs better than default plot
library(plotly)   # plots interactifs
library(factoextra)
library(ggplot2)
```
# Introduction 

<div style="text-align: justify">
Le présent rapport a pour objectif d'analyser un jeu de données portant sur l'analyse chimique de vins italiens provenant de trois cépages différents. La tâche consiste à appliquer des méthodes de classification pour prédire le cépage d'un échantillon en utilisant les résultats de l'analyse chimique

Le jeu de données que nous allons utiliser est issu d'une analyse chimique de vins produits dans la même région d'Italie, mais provenant de trois cépages différents. Les caractéristiques des vins ont été mesurées pour 13 constituants différents. Le jeu de données comprend un total de 178 exemples, répartis en trois classes, correspondant aux trois cépages.

Dans la première partie de ce rapport, nous présenterons une analyse exploratoire des données, qui comprendra une description synthétique du contenu et de la pertinence des variables par rapport aux classes à prédire. Nous utiliserons le langage de programmation R pour effectuer cette analyse.

Dans la deuxième partie, nous présenterons les différents modèles de classification mis en œuvre, ainsi que les méthodes d'évaluation et de comparaison des résultats obtenus. Nous utiliserons le logiciel KNIME pour construire des workflows de classification et évaluer les performances des modèles.

Enfin, nous conclurons ce rapport en résumant les résultats obtenus et en discutant de leur pertinence pour l'application pratique de la classification des vins.
</div>

# Exploration du jeu de données à disposition et analyses préliminaires à l'apprentissage automatique

## Chargement de données
<div style="text-align: justify">
Le jeu de données contient 178 observations pour trois cépages de vin. Les cépages sont répartis en trois classes, la première contenant 59 observations, la deuxième 71 observations et la troisième 48 observations. Treize variables ont été mesurées pour les trois cépages.

```{r}
wine = read_csv('https://gitlab.com/rbarriot/datamining/-/raw/master/data/wine.data.csv') %>%
   mutate(cultivar=as.factor( c('C','M','V'))[ cultivar ] )
head(wine)
```


La description des variables dans ce jeu de données est la suivante :

    cultivar : un facteur indiquant le cultivar de vin
    alcohol : la quantité d'alcool dans le vin
    malic-acid : la quantité d'acide malique dans le vin
    ash : la quantité de cendre dans le vin
    alcalinity-of-ash : l'alcalinité des cendres dans le vin
    magnesium : la quantité de magnésium dans le vin
    total-phenols : la quantité de phénols totaux dans le vin
    flavonoids : la quantité de flavonoïdes dans le vin
    nonflavonoid-phenols : la quantité de phénols non flavonoïdes dans le vin
    proanthocyanins : la quantité de proanthocyanidines dans le vin
    color-intensity : l'intensité de la couleur dans le vin
    hue : la teinte du vin
    od280-od315-of-diluted-wines : l'absorbance de la lumière à 280 nm et 315 nm pour le vin dilué
    proline : la quantité de proline (un acide aminé) dans le vin

</div>

## Analyse descriptive 
<div style="text-align: justify">
La première étape consiste à calculer les statistiques descriptives pour chaque variable, y compris la moyenne, l'écart-type, la médiane, le minimum et le maximum, et visualiser les distributions des variables. 

```{r}

summary(wine)
```

Les resultats de la fonction summry() sur notre jeu de données, permet de voir que les variables mesurées ont des plages de valeurs différentes, par exemple, alcohol varie de 11.03 à 14.83, tandis que color-intensity varie de 1.28 à 13.
La moyenne de alcohol est d'environ 13.00, ce qui indique que la plupart des vins ont une teneur en alcool autour de 13%.
La médiane de alcalinity-of-ash est de 19.50, indiquant que la moitié des vins ont une alcalinité des cendres inférieure à 19,50.
La plage de valeurs de proline est très large, allant de 278 à 1680, avec une moyenne de 746,9 et une médiane de 673,5, indiquant que les vins de cette base de données varient beaucoup en termes de teneur en proline.
En résumé cela montre que les variables ont été mesurées avec différents échelles.
et comme indiqué dans la description de jeu de données fournit, aucune observation manquante n'est signalée pour aucune des variables mesurées.



```{r}
wine %>%
  ggplot(aes(x=cultivar, fill=cultivar)) + # aes pour aesthetic
  geom_bar(stat = 'count') + # https://ggplot2.tidyverse.org/reference/geom_bar.html
  geom_text(stat='count', aes(label=..count..), vjust=-1) # pour afficher les effectifs
```

Cela nous permet de voir le nombre d'échantillons de vin inclus dans le jeu de données pour chacune des trois classes de vin.

Ensuite, dans une seconde étape, on peut afficher les boxplots pour l'ensemble des variables afin de visualiser la distribution de chaque variable pour chaque classe de vin, notamment en termes de tendance centrale, de dispersion et de présence de valeurs aberrantes.

```{r}
# Create boxplot for all variables
boxplot_data <- gather(wine, key = "variable", value = "value", -cultivar)
ggplot(boxplot_data, aes(x = variable, y = value, fill = cultivar)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Variable", y = "Value", title = "Distribution of Variables by Cultivar")

```


Les boxplots montrent des différences de variance entre les variables, notamment avec une forte variance pour la variable "proline". Pour éliminer l'effet de l'échelle et de la variance des variables, il est recommandé de normaliser les données. Pour cela, on peut utiliser la méthode de la z-score qui transforme chaque valeur en la différence entre cette valeur et la moyenne des valeurs, divisée par l'écart-type des valeurs.
</div>

## Normalisation des données avec Z-score

<div style="text-align: justify">
Cette étape consiste à mettre les valeurs de chaque variable dans une plage de valeurs communes afin de les rendre comparables. La normalisation avec Z-score est l'une des méthodes les plus courantes de normalisation. Elle consiste à soustraire la moyenne des valeurs de chaque variable et à diviser le résultat par l'écart type.

```{r}
# Select only the numeric columns
wine_numeric <- wine %>% select_if(is.numeric)

# Normalize the data using scale function
wine_normalized <- scale(wine_numeric)

# Convert the normalized data to a tibble
wine_normalized <- as_tibble(wine_normalized)

# Combine the normalized data with the cultivar column
wine_normalized <- wine %>% select(cultivar) %>% bind_cols(wine_normalized)


head(wine_normalized)
```

Ensuite, on peut refaire une représentation de boxplots sur le jeu de données normalisé, afin de voir si cela a eu un impact sur la distribution et la variance des variables. Cette représentation permet de comparer directement la distribution des variables normalisées entre elles, en éliminant les effets de l'échelle et de la variance.

```{r}
boxplot_data <- gather(wine_normalized, key = "variable", value = "value", -cultivar)
ggplot(boxplot_data, aes(x = variable, y = value, fill = cultivar)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Variable", y = "Value", title = "Distribution of Variables by Cultivar")
```

Après la normalisation, on peut observer que les boxplots sont désormais alignés et que l'effet de l'échelle a été éliminé, ce qui permet de comparer les variables sur une même échelle. Les données normalisées sont ainsi prêtes à être utilisées pour les analyses suivantes, telles que la recherche de corrélations entre les variables ou la réalisation de modèles prédictifs.
</div>

## Analyse en Composantes Principales (ACP)

<div style="text-align: justify">
L'Analyse en Composantes Principales (ACP) est une technique d'analyse multivariée qui permet de réduire la dimensionnalité d'un jeu de données, tout en conservant le maximum d'informations possibles. Elle permet également de visualiser la structure des données, d'identifier les variables les plus discriminantes et de détecter des relations entre les variables. Dans le cas du jeu de données Italian Wines, l'ACP pourrait être utilisée pour identifier les variables les plus importantes dans la différenciation entre les différents types de vins italiens, et pour visualiser la structure sous-jacente des données. 


```{r}
# Appliquer l'ACP
pca.wine <- prcomp(wine_normalized[,2:14], scale = FALSE)  # l'argument "scale = FALSE" indique que les données sont déjà normalisées

# Visualiser les résultats
summary(pca.wine)
```

Dans le tableau généré par la fonction summary(), on peut voir que le PC1 explique 36,2% de la variance totale, le PC2 explique 19,2% et ainsi de suite. Les deux premiers PC expliquent plus de la moitié de la variance totale (55,4%) et les dix premiers PC expliquent plus de 96% de la variance totale.
Pour cette analyse, nous allons retenir que les deux premières PCs.

Histogramme des valeurs propres, exprimé en pourcentage de variance pour les composantes successives

```{r}
fviz_eig(pca.wine, addlabels = T, ylim = c(0,40))

```



</div>

### Contribution des variables

<div style="text-align: justify">
Le cercle de corrélation permet donc de visualiser la structure de corrélation entre les variables et les composantes principales, et de déterminer quelles variables ont la plus grande influence sur chaque composante. Cela peut aider à identifier les variables les plus importantes dans l'analyse et à comprendre comment les différentes variables contribuent à la variation totale des données.

```{r}
# Visualiser le cercle de corrélation
pca.wine %>% 
  fviz_pca_var(col.var="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

```

La longueur de vecteur associé à chaque variable ainsi que sa couleur (allant du bleu au orange) indiquent la force de la contribution de cette variable dans la formation du jeu de données. Cela permet de mieux comprendre les relations entre les variables et les composantes principales, ainsi que de déterminer les variables les plus importantes pour la structure du jeu de données. on peut voir par exemple la variable flavonoids possède une contribution élevée par rapport la variable ash,

En examinant le cercle de corrélation, on peut observer, par exemple, que les variables qui sont corrélées positivement entre elles sont regroupées ensemble. Par exemple, on peut voir que les variables "flavonoids", "total-phenols" et "proanthocyanins" sont fortement corrélées entre elles, et corrlées à l'axe 1, ce qui suggère qu'elles pourraient représenter une caractéristique commune des vins.

De plus, on peut observer que les variables "alcohol" et "proline" sont également fortement corrélées entre elles, ainsi sur l'axe 2, ce qui suggère qu'elles pourraient également représenter une caractéristique commune des vins.

La représentation en couleur permet d'observer que les variables "flavonoids", "od280-od315-of-diluted-wines", "total-phenols", "proline", "alcohol" et "color-intensity" contribuent davantage à la dispersion des données que les variables "proanthocyanins", "magnesium", "ash", "malic-acid", "alcalinity-of-ash" et "nonflavonoid-phenols", comme indiqué par le cercle de corrélation.
</div>

### Projection des individus

<div style="text-align: justify">
```{r}
# Visualisation des individus
fviz_pca_ind(pca.wine,
             geom = "point", # utiliser des points pour représenter les individus
             pointshape = 21, # forme des points
             pointsize = 2, # taille des points
             fill.ind = wine$cultivar, # couleur des points selon le cultivar
             palette = c( "#FC4E07", "#bef574", "#00AFBB"), # couleurs à utiliser
             addEllipses = TRUE, # ajouter des ellipses pour représenter les groupes
             ellipse.level = 0.95, # niveau de confiance des ellipses
             legend.title = "Cultivar") # titre de la légende
```

En examinant la projection des individus sur le plan factoriel, nous constatons une séparation relativement nette des trois groupes (cultivars) dans les deux premières composantes principales. Cela indique que les variables sélectionnées peuvent aider à différencier les cultivars. Toutefois, il est important de noter qu'il y a une certaine superposition entre les groupes, notamment entre les cultivars C et M, ainsi que M et V (un vin classer sur deux cépages différents). Cette superposition peut entraîner une classification erronée de certains individus lors de l'utilisation de toutes les variables.

Afin d'améliorer la classification des cultivars, une approche possible est de sélectionner les variables les plus pertinentes pour la classification des cultivars et d'ignorer celles qui ne contribuent pas significativement à la classification. Cette sélection de variables peut être réalisée en utilisant des méthodes telles que l'analyse discriminante (objectif du prochain TP). En réduisant le nombre de variables, nous pouvons potentiellement améliorer la distinction entre les groupes et ainsi obtenir une classification plus précise des cultivars. Il est également important de garder à l'esprit que d'autres méthodes d'apprentissage automatique, telles que les arbres de décision, les forêts aléatoires ou les réseaux de neurones, peuvent être utilisées pour améliorer la classification des cultivars.
</div>

# Méthodes de classification à mettre en œuvre ainsi que méthodes d'évaluation et de comparaison des résultats obtenus

<div style="text-align: justify">
Dans cette section, nous présentons les méthodes de classification que nous avons étudiées ainsi que les méthodes d'évaluation et de comparaison des résultats obtenus.
</div>

## Méthodes de classification étudiées

<div style="text-align: justify">
Les forêts aléatoires (Random Forest) est un algorithme d'apprentissage supervisé qui combine plusieurs arbres de décision en un modèle de classification robuste. Cet algorithme utilise un échantillonnage aléatoire pour créer des sous-ensembles de données d'entraînement et construit un arbre de décision pour chaque sous-ensemble. Les prédictions finales sont basées sur la moyenne des prédictions de chaque arbre.

L'Arbre de Décision (Decision tree) est également un algorithme d'apprentissage supervisé qui utilise une structure d'arbre pour prendre des décisions de classification. L'arbre est construit en séparant les données en fonction des variables les plus importantes pour la classification et en créant des noeuds pour chaque séparation. Les feuilles de l'arbre correspondent aux classes à prédire.
</div>

## Métriques d'évaluation des performances

<div style="text-align: justify">
Nous avons choisi d'utiliser deux métriques d'évaluation des performances : la matrice de confusion et la l'accuracy.

La matrice de confusion est une table qui permet de visualiser les résultats de la classification en comparant les prédictions du modèle aux valeurs réelles. Elle contient quatre cellules : les vrais positifs (VP), les vrais négatifs (VN), les faux positifs (FP) et les faux négatifs (FN). Cette matrice est utilisée pour calculer plusieurs métriques telles que la sensibilité, la spécificité et le taux d'erreur.

L'accuracy est une mesure de la précision du modèle qui correspond au nombre de prédictions correctes divisé par le nombre total de prédictions. Elle est exprimée en pourcentage et varie de 0 à 1. Plus l'accuracy est proche de 1, plus le modèle est précis.
</div>

## Méthodes de comparaison des modèles

<div style="text-align: justify">
Pour comparer les performances des différentes méthodes de classification, nous avons utilisé deux méthodes : la cross-validation et la partitioning.

La cross-validation est une technique d'évaluation de modèles qui consiste à diviser le jeu de données en plusieurs sous-ensembles et à entraîner le modèle sur l'un de ces sous-ensembles tout en testant sur les autres. Nous avons choisi une cross-validation avec 10 nombres de validation et un échantillonnage aléatoire.

La partitioning consiste à diviser le jeu de données en un ensemble d'apprentissage (2/3 des données) et un ensemble de test (1/3 des données). Nous avons utilisé cette méthode pour évaluer les performances des modèles sur un ensemble de données inédit.

En résumé, nous avons étudié deux méthodes de classification (Random Forest et Decision Tree), utilisé deux métriques d'évaluation des performances (confusion matrix et accuracy) et comparé les performances des modèles selon deux modèles : la cross-validation avec 10 nombre de validations, avec un échantillonnage aléatoire et la méthode de partitionnement 2/3 apprentissage et 1/3 test.
</div>

# Mise en œuvre avec Knime

<div style="text-align: justify">
Nous avons utilisé Knime pour mettre en œuvre les méthodes de classification et les méthodes d'évaluation des performances sur le jeu de données des vins italiens. Nous avons créé un workflow qui contient quatre modèles de classification différents : Random Forest, Decision Tree, Random Forest (avec cross-validation) et Decision Tree (avec cross-validation).

Chacun de ces modèles a été construit en utilisant des nœuds Knime spécifiques pour chaque méthode de classification et d'évaluation des performances. Les données ont été préparées à l'aide de nœuds tels que :

    Le nœud "CSV Reader" pour charger le jeu de données,
    Le nœud "Boxplot" pour visualiser la distribution des variables,
    Le nœud "Normalizer" avec la normalisation z-score pour normaliser les données,
    Le nœud "Partitioning" pour diviser les données en ensembles d'apprentissage et de test,
    Le nœud 'X-Partitioner' et 'X-Aggregator' pour la construction de la validation croisée,
    Le nœud "Learner" pour construire le modèle d'apprentissage et de prédiction, avec le nœud "Predictor",
    Le nœud "Scorer" pour évaluer la qualité des prédictions,
    Le nœud "Decision Tree View" pour visualiser l'arbre de décision du modèle d'apprentissage.

Les paramètres par défaut ont été utilisés sauf pour certains nœuds :

    Pour le nœud "Partitioning", nous avons utilisé une proportion de 66% pour l'ensemble d'apprentissage et de 34% pour l'ensemble de test,
    Pour le nœud "Decision Tree Learner", nous avons utilisé le critère "Gain Ratio" et n'avons pas utilisé la méthode de réduction de l'arbre "Pruning",
    Pour le nœud "Random Forest Learner", nous avons activé l'option "Append overall prediction confidence" pour obtenir une mesure de confiance pour les prédictions,
    Pour le nœud "X-Partitioner", nous avons utilisé une validation croisée à 10 folds avec un échantillonnage aléatoire.

Le workflow a été exécuté et les résultats de chaque modèle ont été enregistrés. Les résultats ont été analysés pour déterminer quelle méthode de classification et quelle méthode d'évaluation des performances fournissent les meilleurs résultats pour la classification des cultivars de vin italien. Les résultats seront présentés dans la section suivante.

Remarque : Bien que l'ajout d'un nœud pour le traitement des données manquantes puisse améliorer les résultats, il n'est pas nécessaire dans ce cas-ci car la description du jeu de données indique qu'il ne contient pas de données manquantes. Par contre une normalisation des données est fortement recomandée pour améliorer la qualité de prédiction.
</div>



<div style="text-align: center">

![Workflow pour la classification des cultivars de vin italien avec Knime](/home/yben/Documents/foufouille/dm.italian.wine.tidyverse.knime.analyses/wine_project.svg)

</div>




# Evaluation des performances et comparaisons des méthodes et/ou de leurs paramètres

## Résultats des matrices obtenues à partir des modèles de prédiction 

<div style="text-align: center">


![Matrice de confusion obtenue pour le modèle d'arbres de décision par la méthode de paratitionement ](/home/yben/Documents/foufouille/dm.italian.wine.tidyverse.knime.analyses/results/confusion_matrix_decision_tree_partitioning.png)

![Matrice de confusion obtenue pour le modèle de random forest par la méthode de paratitionement](/home/yben/Documents/foufouille/dm.italian.wine.tidyverse.knime.analyses/results/confusion_matrix_random_forest_partitioning.png)


![Matrice de confusion obtenue pour le modèle d'arbres de décision par la méthode de cross validation](/home/yben/Documents/foufouille/dm.italian.wine.tidyverse.knime.analyses/results/confusion_matrix_decision_tree_corss_validation.png)


![Matrice de confusion obtenue pour le modèle de random forest par la méthode de cross-validation](/home/yben/Documents/foufouille/dm.italian.wine.tidyverse.knime.analyses/results/confusion_matrix_random_forest_corss_validation.png)

</div>


<div style="text-align: justify">

Pour le modèle d'arbres de décision avec la méthode de partitionnement, 56 observations ont été correctement classées tandis que 5 ont été mal classées, ce qui donne une accuracy de 91,603%. Pour le modèle de forêts aléatoires avec la même méthode, toutes les 61 observations ont été correctement classées, ce qui donne une accuracy de 100%.

En utilisant la méthode de validation croisée, le modèle d'arbres de décision a correctement classé 175 observations avec seulement 3 observations mal classées, ce qui donne une accuracy de 98,315%. Le modèle de forêts aléatoires a correctement classé 165 observations avec 13 observations mal classées, ce qui donne une accuracy de 92,697%.

Ces résultats montrent que les modèles de forêts aléatoires ont tendance à donner de meilleures performances que les arbres de décision, surtout lorsqu'ils sont utilisés avec la méthode de partitionnement. Cependant, le modèle d'arbres de décision avec la méthode de validation croisée a donné des résultats comparables à celui de la forêt aléatoire, ce qui montre que la méthode de validation croisée peut être utile pour améliorer les performances des modèles de classification.

Il convient de noter que ces résultats dépendent également des paramètres utilisés pour chaque modèle, tels que le nombre d'arbres dans la forêt aléatoire ou le critère de partitionnement pour les arbres de décision. Des ajustements de paramètres peuvent donc être nécessaires pour obtenir les meilleures performances possibles.







</div>
