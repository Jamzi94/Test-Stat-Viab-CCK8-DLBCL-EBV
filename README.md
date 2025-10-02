# Analyse Statistique de Viabilité CCK8 - DLBCL-EBV

## 📋 Table des matières
1. [Description du projet](#description-du-projet)
2. [Contexte scientifique](#contexte-scientifique)
3. [Prérequis](#prérequis)
4. [Installation](#installation)
5. [Structure des données](#structure-des-données)
6. [Utilisation](#utilisation)
7. [Workflow détaillé](#workflow-détaillé)
8. [Documentation des fonctions](#documentation-des-fonctions)
9. [Paramètres configurables](#paramètres-configurables)
10. [Résultats et visualisations](#résultats-et-visualisations)
11. [Tests statistiques](#tests-statistiques)
12. [Dépannage](#dépannage)
13. [Exemples](#exemples)
14. [Contribuer](#contribuer)

---

## 📖 Description du projet

Ce projet fournit un pipeline d'analyse automatisé en R pour évaluer l'effet du SAHA (acide subéroylanilide hydroxamique) sur l'activité métabolique mitochondriale de lignées cellulaires de lymphome B diffus à grandes cellules (DLBCL) associé au virus d'Epstein-Barr (EBV).

Le script `CCK8_Final_Auto.R` permet de :
- Importer et transformer automatiquement des données d'absorbance depuis un fichier Excel multi-feuilles
- Détecter et retirer les valeurs aberrantes (outliers) avec plusieurs méthodes au choix
- Calculer les moyennes et écarts-types pour chaque condition expérimentale
- Effectuer des analyses statistiques paramétriques (ANOVA) ou non paramétriques (Scheirer-Ray-Hare)
- Générer des visualisations sous forme de graphiques en barres avec barres d'erreur

---

## 🔬 Contexte scientifique

### Test CCK8 (Cell Counting Kit-8)
Le test CCK8 est un test colorimétrique utilisé pour évaluer la viabilité et la prolifération cellulaire. Il mesure l'activité des enzymes déshydrogénases mitochondriales qui réduisent le réactif WST-8 en un formazan orange détectable par absorbance (450 nm).

### SAHA (Vorinostat)
Le SAHA est un inhibiteur des histones désacétylases (HDAC) utilisé dans le traitement de certains lymphomes. Ce projet évalue son effet dose-dépendant et temps-dépendant sur deux lignées cellulaires :
- **PRI** : Lignée cellulaire de DLBCL-EBV
- **SU-DHL-4** : Lignée cellulaire de DLBCL

### Hypothèses testées
- **H0** : Le SAHA n'a aucun effet significatif (dose, temps ou interaction)
- **H1** : Le SAHA a un effet significatif (dose, temps ou interaction)

---

## 🔧 Prérequis

### Système requis
- **R version** : ≥ 4.0.0
- **RStudio** (recommandé pour une meilleure expérience)
- **Système d'exploitation** : Windows, macOS ou Linux

### Packages R nécessaires

Le script nécessite les packages suivants :

```r
# Manipulation et visualisation des données
library(tidyverse)    # Suite complète de packages (dplyr, ggplot2, tidyr, etc.)
library(dplyr)        # Manipulation de données
library(purrr)        # Programmation fonctionnelle
library(tidyr)        # Organisation des données

# Importation de données
library(readxl)       # Lecture de fichiers Excel

# Statistiques
library(FSA)          # Test de Dunn et analyses statistiques
library(rstatix)      # Tests statistiques dans un format tidy
library(rcompanion)   # Test de Scheirer-Ray-Hare

# Visualisation
library(ggplot2)      # Création de graphiques
library(ggsignif)     # Ajout de significations statistiques aux graphiques
library(scales)       # Mise à l'échelle des graphiques

# Utilitaires
library(glue)         # Interpolation de chaînes de caractères
```

---

## 📥 Installation

### 1. Installation de R et RStudio

1. Téléchargez et installez R depuis [CRAN](https://cran.r-project.org/)
2. Téléchargez et installez RStudio depuis [RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### 2. Installation des packages

Ouvrez R ou RStudio et exécutez les commandes suivantes :

```r
# Liste des packages requis
packages <- c(
  "tidyverse",
  "FSA",
  "ggplot2",
  "rstatix",
  "ggsignif",
  "scales",
  "glue",
  "dplyr",
  "purrr",
  "tidyr",
  "readxl",
  "rcompanion"
)

# Installation des packages manquants
packages_manquants <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(packages_manquants) > 0) {
  install.packages(packages_manquants)
}

# Chargement des packages
lapply(packages, library, character.only = TRUE)
```

### 3. Téléchargement du script

Clonez ce dépôt ou téléchargez le fichier `CCK8_Final_Auto.R` :

```bash
git clone https://github.com/Jamzi94/Test-Stat-Viab-CCK8-DLBCL-EBV.git
cd Test-Stat-Viab-CCK8-DLBCL-EBV
```

---

## 📊 Structure des données

### Format du fichier Excel requis

Le fichier Excel doit contenir **plusieurs feuilles** (une par essai/triplicat) avec la structure suivante :

| Cell_line | Concentration_SAHA | Time | Absorbances |
|-----------|-------------------|------|-------------|
| PRI       | 0                 | 24h  | 1.234       |
| PRI       | 10                | 24h  | 1.156       |
| PRI       | 25                | 24h  | 0.987       |
| SU-DHL-4  | 0                 | 48h  | 1.345       |
| ...       | ...               | ...  | ...         |

### Description des colonnes

- **Cell_line** : Nom de la lignée cellulaire (facteur)
  - Valeurs attendues : `"PRI"`, `"SU-DHL-4"`
  
- **Concentration_SAHA** : Concentration de SAHA en µM (facteur)
  - Valeurs attendues : `0`, `10`, `25`
  
- **Time** : Temps d'incubation (facteur)
  - Valeurs attendues : `"24h"`, `"48h"`
  
- **Absorbances** : Valeur d'absorbance mesurée à 450 nm (numérique)
  - Format : nombre décimal (ex: 1.234)

### Exemple de structure multi-feuilles

```
Fichier Excel : CCK8_uni.xlsx
├── Feuille1 (Essai 1 - 24h)
├── Feuille2 (Essai 2 - 24h)
├── Feuille3 (Essai 1 - 48h)
└── Feuille4 (Essai 2 - 48h)
```

---

## 🚀 Utilisation

### 1. Configuration du chemin du fichier

Ouvrez le fichier `CCK8_Final_Auto.R` et modifiez la ligne 609 pour spécifier le chemin de votre fichier Excel :

```r
# Ligne 609 - Modifiez ce chemin
file_path <- '/Users/jamzi/Downloads/CCK8 uni.xlsx'

# Exemple Windows
file_path <- 'C:/Users/VotreNom/Documents/CCK8_uni.xlsx'

# Exemple macOS/Linux
file_path <- '/home/utilisateur/Documents/CCK8_uni.xlsx'
```

### 2. Exécution du script

#### Option A : Via RStudio
1. Ouvrez `CCK8_Final_Auto.R` dans RStudio
2. Cliquez sur **Source** ou appuyez sur `Ctrl+Shift+S` (Windows/Linux) ou `Cmd+Shift+S` (macOS)

#### Option B : Via ligne de commande R
```r
source("CCK8_Final_Auto.R")
```

#### Option C : Via terminal
```bash
Rscript CCK8_Final_Auto.R
```

### 3. Suivi de l'exécution

Le script affiche des messages de progression :

```
Lecture des données de la feuille : Feuille1
Tableau généré : essai1_24h_normalise
OK 1
...
OK 2
OK 3
OK 4
=== Résultats combinés avec moyennes et écarts-types ===
```

---

## 🔄 Workflow détaillé

Le pipeline d'analyse se décompose en **5 étapes principales** :

### Étape 1 : Traitement des feuilles Excel
```r
results <- process_all_sheets(file_path)
```

**Fonctions impliquées :**
- `process_all_sheets()` : Boucle sur toutes les feuilles
- `transform_data()` : Transformation des données
- `process_outliers()` : Détection et retrait des outliers

**Processus :**
1. Lecture de chaque feuille Excel
2. Conversion des types de données (facteurs et numériques)
3. Détection des valeurs aberrantes par groupe
4. Retrait des outliers identifiés
5. Stockage des données nettoyées

**Sortie :** Liste de tableaux nettoyés (un par feuille)

---

### Étape 2 : Combinaison des résultats
```r
resultats_essai_combine_p <- process_combined_stats(results)
```

**Fonction impliquée :**
- `process_combined_stats()` : Fusion des données

**Processus :**
1. Extraction des tables nettoyées
2. Dégroupement des données groupées
3. Fusion en un seul dataframe
4. Vérification de l'intégrité des données

**Sortie :** Dataframe combiné avec toutes les observations

---

### Étape 3 : Tests statistiques
```r
stat_results <- perform_stat_tests(resultats_essai_combine_p)
resultats_essai_combine_m <- SD_Mean(resultats_essai_combine_p)
```

**Fonctions impliquées :**
- `perform_stat_tests()` : Tests d'hypothèses
- `SD_Mean()` : Calcul des statistiques descriptives

**Processus :**

#### 3.1 Vérification des hypothèses
- **Test de normalité** (Shapiro-Wilk) pour chaque groupe
- **Test d'homogénéité des variances** (Bartlett)

#### 3.2 Choix du test approprié

**Si conditions respectées (normalité + homogénéité) :**
- ✅ **ANOVA à 2 facteurs** (test paramétrique)
- Post-hoc : **Test de Tukey HSD**

**Si conditions non respectées :**
- ✅ **Test de Scheirer-Ray-Hare** (test non paramétrique)
- Post-hoc : **Test de Dunn** avec correction de Benjamini-Hochberg

#### 3.3 Calcul des statistiques descriptives
- Moyennes par groupe
- Écarts-types par groupe
- Combinaison des conditions

**Sortie :** 
- `stat_results` : Résultats des tests statistiques
- `resultats_essai_combine_m` : Moyennes et écarts-types

---

### Étape 4 : Génération des graphiques
```r
create_CCK8_barplot(resultats_essai_combine_m)
```

**Fonction impliquée :**
- `create_CCK8_barplot()` : Visualisation des données

**Processus :**
1. Création d'un graphique pour PRI
2. Création d'un graphique pour SU-DHL-4
3. Ajout des barres d'erreur (± SD)
4. Codage par couleur des concentrations

**Sortie :** 2 graphiques en barres affichés dans la fenêtre de visualisation R

---

### Étape 5 : Affichage des résultats finaux
```r
cat("\n=== Résultats combinés avec moyennes et écarts-types ===\n")
print(stat_results)
```

**Sortie :** Rapport complet dans la console R

---

## 📚 Documentation des fonctions

### Fonctions principales

#### `transform_data(sheet_name, compteur)`
Transforme les données brutes d'une feuille Excel.

**Paramètres :**
- `sheet_name` : Nom de la feuille à traiter (chaîne de caractères)
- `compteur` : Numéro d'essai (entier)

**Retour :**
- Nom du tableau créé dans l'environnement global

**Exemple :**
```r
table_name <- transform_data("Feuille1", 1)
# Retourne : "essai1_24h_normalise"
```

---

#### `detect_outliers(x, group_info)`
Détecte les valeurs aberrantes selon la méthode configurée.

**Paramètres :**
- `x` : Vecteur de valeurs numériques
- `group_info` : Information sur le groupe (pour affichage)

**Retour :**
- Vecteur logique (`TRUE` = outlier, `FALSE` = valeur normale)

**Méthodes disponibles :**

1. **IQR (Inter-Quartile Range)** - Méthode par défaut
   ```
   Outlier si : x < Q1 - k*IQR  OU  x > Q3 + k*IQR
   où k = threshold (défaut: 1)
   ```

2. **Z-score**
   ```
   Outlier si : |Z| > z_thresh
   où Z = (x - moyenne) / écart-type
   ```

3. **MAD (Median Absolute Deviation)**
   ```
   Outlier si : |x - médiane| / MAD > threshold
   ```

4. **Percentile**
   ```
   Outlier si : x < percentile_inf  OU  x > percentile_sup
   ```

---

#### `process_outliers(data, remove_outliers = TRUE)`
Traite et retire les valeurs aberrantes d'un jeu de données.

**Paramètres :**
- `data` : Dataframe contenant les données
- `remove_outliers` : Booléen pour activer/désactiver le retrait (défaut: `TRUE`)

**Retour :**
- Dataframe nettoyé sans outliers

**Exemple :**
```r
data_clean <- process_outliers(raw_data, remove_outliers = TRUE)
```

---

#### `SD_Mean(data)`
Calcule les moyennes et écarts-types par condition.

**Paramètres :**
- `data` : Dataframe avec colonnes Cell_line, Time, Concentration_SAHA, Absorbances

**Retour :**
- Dataframe avec colonnes supplémentaires Mean_Absorbance et SD_Absorbance

**Exemple :**
```r
stats <- SD_Mean(combined_data)
```

---

#### `perform_stat_tests(data)`
Effectue les tests statistiques appropriés.

**Paramètres :**
- `data` : Dataframe combiné

**Retour :**
- Liste contenant :
  - Résultats des tests de normalité
  - Résultats des tests d'homogénéité
  - Résultats ANOVA ou Scheirer-Ray-Hare
  - Résultats des tests post-hoc
  - Interprétations

**Exemple :**
```r
results <- perform_stat_tests(combined_data)
print(results$ANOVA$interpretation)
```

---

#### `create_CCK8_barplot(data_summary)`
Génère les graphiques en barres avec barres d'erreur.

**Paramètres :**
- `data_summary` : Dataframe avec moyennes et écarts-types

**Sortie :**
- 2 graphiques ggplot2 affichés (PRI et SU-DHL-4)

**Personnalisation :**
```r
# Les couleurs sont définies dans la fonction
colors <- c(
  "0" = "#04BF23",   # Vert pour contrôle (0 µM)
  "10" = "#00bbff",  # Bleu pour 10 µM
  "25" = "#ff0000"   # Rouge pour 25 µM
)
```

---

#### `process_all_sheets(file_path)`
Fonction principale pour traiter toutes les feuilles Excel.

**Paramètres :**
- `file_path` : Chemin complet vers le fichier Excel

**Retour :**
- Liste contenant :
  - `tables_cleaned` : Liste de dataframes nettoyés

**Exemple :**
```r
results <- process_all_sheets("/chemin/vers/fichier.xlsx")
```

---

#### `process_combined_stats(results)`
Combine les statistiques de plusieurs essais.

**Paramètres :**
- `results` : Liste retournée par `process_all_sheets()`

**Retour :**
- Dataframe combiné unique

**Exemple :**
```r
combined <- process_combined_stats(results)
```

---

#### `combine_conditions(data)`
Regroupe les données par conditions et combine les lignes.

**Paramètres :**
- `data` : Dataframe avec statistiques

**Retour :**
- Dataframe avec absorbances regroupées par condition

**Exemple :**
```r
grouped <- combine_conditions(data_summary)
```

---

## ⚙️ Paramètres configurables

### Configuration de la détection des outliers

Les paramètres sont définis aux lignes 64-70 du script :

```r
outlier_settings <- list(
  method = "IQR",              # Méthode de détection
  threshold = 1,               # Seuil pour MAD ou IQR
  z_thresh = 1.5,              # Seuil pour Z-score
  lower_percentile = 0.05,     # Limite inférieure pour Percentile
  upper_percentile = 0.95      # Limite supérieure pour Percentile
)
```

### Modification des paramètres

#### Pour utiliser la méthode Z-score :
```r
outlier_settings <- list(
  method = "Z-score",
  z_thresh = 2.5    # Seuil plus conservateur
)
```

#### Pour une détection IQR plus stricte :
```r
outlier_settings <- list(
  method = "IQR",
  threshold = 1.5   # Seuil classique de Tukey
)
```

#### Pour désactiver complètement la détection :
```r
# Dans process_all_sheets(), ligne 502
cleaned_table <- process_outliers(data_transformed, remove_outliers = FALSE)
```

---

## 📈 Résultats et visualisations

### 1. Résultats statistiques dans la console

#### Exemple de sortie - Test de normalité :
```
== Résultats des tests de normalité (Shapiro-Wilk) ==
# A tibble: 12 × 4
   Cell_line Concentration_SAHA Time  p_value
   <fct>     <fct>              <fct>   <dbl>
 1 PRI       0                  24h    0.234
 2 PRI       10                 24h    0.456
 3 PRI       25                 24h    0.678
   ...
```

#### Exemple de sortie - ANOVA :
```
                        Df Sum Sq Mean Sq F value   Pr(>F)    
Concentration_SAHA       2  3.456   1.728  23.45 1.23e-08 ***
Time                     1  0.789   0.789  10.71  0.00156 ** 
Concentration_SAHA:Time  2  0.234   0.117   1.59  0.21234    
Residuals              54  3.978   0.074                     
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

#### Interprétation automatique :
```
Résultat significatif : H0 rejetée pour l'effet dose, temps ou interaction.

Test post-hoc Tukey :
H0 rejetée : différence significative entre 25-0 (p = 0.00001).
H0 rejetée : différence significative entre 10-0 (p = 0.00234).
H0 acceptée : aucune différence significative entre 25-10 (p = 0.12345).
```

---

### 2. Graphiques générés

Le script génère **2 graphiques en barres** (un par lignée cellulaire) :

#### Caractéristiques des graphiques :
- **Axe X** : Conditions (Temps.Concentration)
  - Ordre : 24h.0, 24h.10, 24h.25, 48h.0, 48h.10, 48h.25
  
- **Axe Y** : Absorbance moyenne ± Écart-type

- **Couleurs** :
  - 🟢 Vert : Contrôle (0 µM SAHA)
  - 🔵 Bleu : 10 µM SAHA
  - 🔴 Rouge : 25 µM SAHA

- **Barres d'erreur** : ± 1 écart-type

#### Titre du graphique :
```
"Effet du SAHA sur l'activité métabolique enzymatique des mitochondries pour [Lignée]"
```

#### Sauvegarde des graphiques :
Pour sauvegarder les graphiques, ajoutez après la ligne 415 :

```r
# Sauvegarder le graphique PRI
p1 <- generate_plot_for_cell_line(data_summary, "PRI")
ggsave("CCK8_PRI.png", plot = p1, width = 10, height = 6, dpi = 300)

# Sauvegarder le graphique SU-DHL-4
p2 <- generate_plot_for_cell_line(data_summary, "SU-DHL-4")
ggsave("CCK8_SU-DHL-4.png", plot = p2, width = 10, height = 6, dpi = 300)
```

---

## 📊 Tests statistiques

### Arbre de décision des tests

```
Données combinées
    |
    ├─> Test de Shapiro-Wilk (normalité)
    |   └─> Pour chaque groupe (Cell_line × Concentration × Time)
    |
    ├─> Test de Bartlett (homogénéité des variances)
    |
    └─> Décision
        |
        ├─> Si normalité ET homogénéité respectées
        |   └─> ANOVA à 2 facteurs
        |       └─> Post-hoc : Tukey HSD
        |
        └─> Si conditions NON respectées
            └─> Scheirer-Ray-Hare (équivalent non paramétrique ANOVA)
                └─> Post-hoc : Test de Dunn avec correction BH
```

### Interprétation des p-values

| p-value | Signification | Symbole |
|---------|--------------|---------|
| < 0.001 | Hautement significatif | *** |
| < 0.01  | Très significatif | ** |
| < 0.05  | Significatif | * |
| ≥ 0.05  | Non significatif | ns |

### Hypothèses testées

1. **Effet principal de la Concentration** : Le SAHA a-t-il un effet dose-dépendant ?
2. **Effet principal du Temps** : L'effet du SAHA varie-t-il selon le temps d'exposition ?
3. **Interaction Concentration × Temps** : L'effet du SAHA dépend-il du temps d'exposition ?

---

## 🔧 Dépannage

### Problème 1 : Package manquant
```
Error in library(XXX) : there is no package called 'XXX'
```

**Solution :**
```r
install.packages("XXX")
```

---

### Problème 2 : Fichier Excel introuvable
```
Error: `path` does not exist: '/chemin/vers/fichier.xlsx'
```

**Solution :**
- Vérifiez le chemin du fichier (ligne 609)
- Utilisez des barres obliques `/` au lieu de `\` (même sur Windows)
- Utilisez des chemins absolus complets

**Exemple de chemin correct :**
```r
# Windows
file_path <- 'C:/Users/Nom/Documents/CCK8_uni.xlsx'

# macOS/Linux
file_path <- '/home/utilisateur/Documents/CCK8_uni.xlsx'

# Chemin relatif (si dans le même dossier)
file_path <- './CCK8_uni.xlsx'
```

---

### Problème 3 : Colonnes manquantes
```
Erreur : Les colonnes suivantes sont manquantes : Cell_line, Time
```

**Solution :**
- Vérifiez que votre fichier Excel contient les colonnes : 
  - `Cell_line`
  - `Concentration_SAHA`
  - `Time`
  - `Absorbances`
- Les noms doivent correspondre exactement (sensible à la casse)

---

### Problème 4 : Pas assez de données pour Shapiro-Wilk
```
Error in shapiro.test(Absorbances) : sample size must be between 3 and 5000
```

**Solution :**
- Assurez-vous d'avoir au moins 3 réplicats par condition
- Si vous avez moins de données, utilisez directement un test non paramétrique

**Modification du script :**
```r
# Ligne 255, commentez la vérification de normalité et forcez le test non paramétrique
# Remplacez la condition ligne 299 par :
if (FALSE) {  # Force le test non paramétrique
```

---

### Problème 5 : Erreur de syntaxe à la ligne 67
```
Error: unexpected numeric constant in "z_thresh = 1,5"
```

**Solution :**
Le séparateur décimal en R est le point `.`, pas la virgule `,`

**Correction ligne 67 :**
```r
# INCORRECT
z_thresh = 1,5

# CORRECT
z_thresh = 1.5
```

---

### Problème 6 : Graphiques ne s'affichent pas
```
# Aucun graphique visible
```

**Solution :**
- Dans RStudio : Vérifiez l'onglet "Plots" en bas à droite
- Augmentez la taille de la fenêtre de visualisation
- Sauvegardez manuellement les graphiques :

```r
# Après l'exécution, dans la console :
dev.copy(png, 'graphique.png', width = 800, height = 600)
dev.off()
```

---

### Problème 7 : Mémoire insuffisante
```
Error: cannot allocate vector of size XXX Mb
```

**Solution :**
```r
# Augmenter la limite de mémoire (Windows uniquement)
memory.limit(size = 16000)  # 16 Go

# Nettoyer la mémoire
rm(list = ls())
gc()
```

---

### Problème 8 : Encoding des caractères
```
# Les caractères accentués s'affichent mal
```

**Solution :**
```r
# En début de script, ajoutez :
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")  # Linux/macOS
Sys.setlocale("LC_ALL", "French_France.1252")  # Windows
```

---

## 💡 Exemples

### Exemple 1 : Exécution complète standard

```r
# 1. Définir le chemin du fichier
file_path <- '/Users/jamzi/Downloads/CCK8_uni.xlsx'

# 2. Traiter toutes les feuilles
results <- process_all_sheets(file_path)

# 3. Combiner les résultats
resultats_essai_combine_p <- process_combined_stats(results)

# 4. Effectuer les tests statistiques
stat_results <- perform_stat_tests(resultats_essai_combine_p)
resultats_essai_combine_m <- SD_Mean(resultats_essai_combine_p)

# 5. Générer les graphiques
create_CCK8_barplot(resultats_essai_combine_m)

# 6. Afficher les résultats
print(stat_results)
```

---

### Exemple 2 : Modifier les paramètres de détection des outliers

```r
# Utiliser la méthode Z-score avec un seuil de 3
outlier_settings <- list(
  method = "Z-score",
  z_thresh = 3
)

# Puis exécuter normalement
results <- process_all_sheets(file_path)
# ...
```

---

### Exemple 3 : Traiter une seule feuille

```r
# Définir le chemin du fichier
file_path <- '/Users/jamzi/Downloads/CCK8_uni.xlsx'

# Traiter uniquement la première feuille
table_name <- transform_data(sheet_name = "Feuille1", compteur = 1)

# Récupérer les données
data <- get(table_name)

# Nettoyer les outliers
data_clean <- process_outliers(data, remove_outliers = TRUE)

# Calculer les statistiques
stats <- SD_Mean(data_clean)

print(stats)
```

---

### Exemple 4 : Exporter les résultats en CSV

```r
# Après l'étape 3
write.csv(resultats_essai_combine_m, 
          "resultats_moyennes.csv", 
          row.names = FALSE)

# Exporter les résultats statistiques
sink("resultats_statistiques.txt")
print(stat_results)
sink()
```

---

### Exemple 5 : Personnaliser les graphiques

```r
# Créer la fonction personnalisée
custom_plot <- function(data_summary, cell_line) {
  colors <- c("0" = "#04BF23", "10" = "#00bbff", "25" = "#ff0000")
  
  plot_data <- data_summary %>%
    filter(Cell_line == cell_line) %>%
    mutate(
      x_axis = paste(Time, Concentration_SAHA, sep = "."),
      x_axis = factor(x_axis, levels = c("24h.0", "24h.10", "24h.25", 
                                         "48h.0", "48h.10", "48h.25"))
    )
  
  p <- ggplot(plot_data, aes(x = x_axis, y = Mean_Absorbance, 
                             fill = as.factor(Concentration_SAHA))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, color = "black") +
    geom_errorbar(
      aes(ymin = Mean_Absorbance - SD_Absorbance, 
          ymax = Mean_Absorbance + SD_Absorbance),
      width = 0.2,
      position = position_dodge(width = 0.8)
    ) +
    scale_fill_manual(values = colors, name = "Concentration SAHA (µM)") +
    labs(
      title = paste("Effet du SAHA sur", cell_line),
      subtitle = "Activité métabolique mitochondriale (Test CCK8)",
      x = "Conditions (Temps.Concentration)",
      y = "Absorbance (450 nm) ± SD",
      caption = "Données normalisées et valeurs aberrantes retirées"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40")
    )
  
  return(p)
}

# Utilisation
p1 <- custom_plot(resultats_essai_combine_m, "PRI")
p2 <- custom_plot(resultats_essai_combine_m, "SU-DHL-4")

# Afficher
print(p1)
print(p2)

# Sauvegarder en haute résolution
ggsave("PRI_custom.png", p1, width = 12, height = 8, dpi = 300)
ggsave("SU-DHL-4_custom.png", p2, width = 12, height = 8, dpi = 300)
```

---

## 🤝 Contribuer

Les contributions sont les bienvenues ! Pour contribuer :

1. Forkez le projet
2. Créez une branche pour votre fonctionnalité (`git checkout -b feature/AmazingFeature`)
3. Committez vos changements (`git commit -m 'Add some AmazingFeature'`)
4. Poussez vers la branche (`git push origin feature/AmazingFeature`)
5. Ouvrez une Pull Request

### Améliorations possibles

- [ ] Support de formats de fichiers supplémentaires (CSV, TSV)
- [ ] Interface graphique (Shiny app)
- [ ] Génération automatique de rapports PDF/HTML
- [ ] Tests unitaires automatisés
- [ ] Support de lignées cellulaires supplémentaires
- [ ] Analyse de survie cellulaire (IC50)
- [ ] Comparaison inter-essais avec métadonnées
- [ ] Export des graphiques en format vectoriel (SVG, EPS)

---

## 📝 Licence

Ce projet est fourni à des fins éducatives et de recherche.

---

## 👥 Auteur

**Jamzi94**
- GitHub: [@Jamzi94](https://github.com/Jamzi94)
- Projet: [Test-Stat-Viab-CCK8-DLBCL-EBV](https://github.com/Jamzi94/Test-Stat-Viab-CCK8-DLBCL-EBV)

---

## 📚 Références

### Méthodes statistiques
- Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). *Biometrika*, 52(3/4), 591-611.
- Bartlett, M. S. (1937). Properties of sufficiency and statistical tests. *Proceedings of the Royal Society of London. Series A*, 160(901), 268-282.
- Scheirer, C. J., Ray, W. S., & Hare, N. (1976). The analysis of ranked data derived from completely randomized factorial designs. *Biometrics*, 429-434.
- Dunn, O. J. (1964). Multiple comparisons using rank sums. *Technometrics*, 6(3), 241-252.
- Tukey, J. W. (1949). Comparing individual means in the analysis of variance. *Biometrics*, 99-114.

### Test CCK8
- Ishiyama, M., et al. (1997). A combined assay of cell viability and in vitro cytotoxicity with a highly water-soluble tetrazolium salt, neutral red and crystal violet. *Biological and Pharmaceutical Bulletin*, 20(11), 1203-1206.

### SAHA/Vorinostat
- Mann, B. S., et al. (2007). FDA approval summary: vorinostat for treatment of advanced primary cutaneous T-cell lymphoma. *The Oncologist*, 12(10), 1247-1252.

---

## 📞 Support

Pour toute question ou problème :

1. Consultez la section [Dépannage](#dépannage)
2. Ouvrez une [Issue](https://github.com/Jamzi94/Test-Stat-Viab-CCK8-DLBCL-EBV/issues) sur GitHub
3. Vérifiez les [Issues existantes](https://github.com/Jamzi94/Test-Stat-Viab-CCK8-DLBCL-EBV/issues?q=is%3Aissue) pour voir si votre problème a déjà été résolu

---

## 📅 Historique des versions

### Version actuelle (2024)
- ✅ Pipeline d'analyse complet et automatisé
- ✅ Détection et retrait des valeurs aberrantes (4 méthodes)
- ✅ Tests statistiques paramétriques et non paramétriques
- ✅ Génération de graphiques en barres avec barres d'erreur
- ✅ Support de plusieurs lignées cellulaires
- ✅ Documentation complète

---

**Note** : Ce README a été généré pour faciliter l'utilisation du script d'analyse CCK8. Pour toute suggestion d'amélioration de la documentation, n'hésitez pas à contribuer !
