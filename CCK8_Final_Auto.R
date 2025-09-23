
# Chargement des bibliothèques nécessaires
library(tidyverse)
library(FSA)
library(ggplot2)
library(rstatix)
library(ggsignif)
library(scales)
library(glue)
library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(rcompanion)

"==========================================================="
# Fonction pour transformer les données d'une feuille
"==========================================================="
transform_data <- function(sheet_name, compteur) {
  # Vérifiez que `sheet_name` est bien un seul élément
  if (length(sheet_name) != 1) {
    stop("Erreur : `sheet_name` doit être un seul nom de feuille.")
  }
  
  # Lire les données de la feuille Excel
  cat("Lecture des données de la feuille :", sheet_name, "\n")
  raw_data <- read_excel(file_path, sheet = sheet_name, col_names = TRUE)
  
  # Vérifiez que l'objet 'raw_data' est bien un tableau de données
  if (!is.data.frame(raw_data)) {
    stop("Erreur : La feuille Excel '", sheet_name, "' n'a pas retourné un tableau valide.")
  }
  
  # Transformation des données
  data_transformed <- raw_data %>%
    mutate(
      Cell_line = as.factor(Cell_line),  # Conversion en facteur
      Concentration_SAHA = as.factor(Concentration_SAHA),  # Conversion en facteur
      Time = as.factor(Time),  # Conversion en facteur
      Absorbances = as.numeric(Absorbances)  # Conversion en numérique
    )
  
  # Obtenir la valeur unique de temps
  time_value <- unique(data_transformed$Time)
  time_value <- ifelse(length(time_value) == 1, as.character(time_value), "unknown_time")
  
  # Générer un nom de tableau
  table_name <- paste0("essai", compteur, "_", time_value, "_normalise")
  
  # Sauvegarder les données dans l'environnement global
  assign(table_name, data_transformed, envir = .GlobalEnv)
  
  # Afficher un message de confirmation
  cat("Tableau généré :", table_name, "\n")
  
  # Retourner le nom du tableau
  return(table_name)
}


"==============================================="
"================================================"
# Paramètres modifiable pour la détection des outliers
outlier_settings <- list(
  method = "IQR",   # Méthode par défaut (MAD, IQR, Z-score, Percentile)
  threshold = 1,         # Seuil pour MAD ou IQR
  z_thresh = 1,5,            # Seuil pour Z-score
  lower_percentile = 0.05, # Limite inférieure pour Percentile
  upper_percentile = 0.95  # Limite supérieure pour Percentile
)

"=================================================================="
# Regrouper par les colonnes représentant les conditions et combiner les lignes
"=================================================================="
combine_conditions <- function(data) {
  combined_data <- data %>%
    group_by(Cell_line, Time, Concentration_SAHA, Mean_Absorbance, SD_Absorbance) %>%
    summarise(
      Absorbances = list(Absorbances),  # Stocker toutes les valeurs pour chaque condition
      .groups = "drop"
    )
  
  return(combined_data)
}
"=================================================================="
# Fonction pour définir les méthode de detection outliers
"=================================================================="
detect_outliers <- function(x, group_info) {
  method <- outlier_settings$method
  threshold <- outlier_settings$threshold
  z_thresh <- outlier_settings$z_thresh
  lower_percentile <- outlier_settings$lower_percentile
  upper_percentile <- outlier_settings$upper_percentile
  
  cat("\n")
  print(paste("Groupe en cours : ", group_info))
  print(paste("Données : ", paste(x, collapse = ", ")))
  
  if (method == "IQR") {
    # Méthode IQR
    Q1 <- quantile(x, 0.25, na.rm = FALSE)
    Q3 <- quantile(x, 0.75, na.rm = FALSE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - threshold * IQR
    upper_bound <- Q3 + threshold * IQR
    
    print("Méthode : IQR")
    print(paste("Q1 (1er quartile) =", Q1))
    print(paste("Q3 (3e quartile) =", Q3))
    print(paste("IQR =", IQR))
    print(paste("Limite inférieure =", lower_bound))
    print(paste("Limite supérieure =", upper_bound))
    
    outlier_flag <- (x < lower_bound) | (x > upper_bound)
    print(paste("Outliers détectés :", paste(x[outlier_flag], collapse = ", ")))
    
  } else if (method == "Z-score") {
    # Méthode Z-score
    mean_x <- mean(x, na.rm = FALSE)
    sd_x <- sd(x, na.rm = FALSE)
    z_scores <- (x - mean_x) / sd_x
    
    print("Méthode : Z-score")
    print(paste("Moyenne =", mean_x))
    print(paste("Écart-type =", sd_x))
    print(paste("Scores Z =", paste(round(z_scores, 3), collapse = ", ")))
    print(paste("Seuil Z =", z_thresh))
    
    outlier_flag <- abs(z_scores) > z_thresh
    print(paste("Outliers détectés :", paste(x[outlier_flag], collapse = ", ")))
    
  } else if (method == "Percentile") {
    # Méthode Percentile
    lower_bound <- quantile(x, lower_percentile, na.rm = FALSE)
    upper_bound <- quantile(x, upper_percentile, na.rm = FALSE)
    
    print("Méthode : Percentile")
    print(paste("Limite inférieure (", lower_percentile * 100, "%) =", lower_bound))
    print(paste("Limite supérieure (", upper_percentile * 100, "%) =", upper_bound))
    
    outlier_flag <- (x < lower_bound) | (x > upper_bound)
    print(paste("Outliers détectés :", paste(x[outlier_flag], collapse = ", ")))
    
  } else if (method == "MAD") {
    # Méthode MAD
    med <- median(x, na.rm = FALSE)
    mad_value <- mad(x, na.rm = FALSE)
    modified_z_score <- abs(x - med) / mad_value
    
    print("Méthode : MAD")
    print(paste("Médiane =", med))
    print(paste("MAD =", mad_value))
    print(paste("Scores Z modifiés =", paste(round(modified_z_score, 3), collapse = ", ")))
    print(paste("Seuil =", threshold))
    
    outlier_flag <- modified_z_score > threshold
    print(paste("Outliers détectés :", paste(x[outlier_flag], collapse = ", ")))
    
  } else {
    stop("Méthode de détection des outliers non reconnue.")
  }
  
  return(outlier_flag)
}

"=================================================================="
# Fonction proccesive du retrait des valeurs aberrantes d'un essai
"=================================================================="
process_outliers <- function(data, remove_outliers = TRUE) {
  if (!is.data.frame(data)) {
    stop("Erreur : L'entrée fournie à `process_outliers` n'est pas un tableau valide.")
  }
  
  if (remove_outliers) {
    # Détection et marquage des valeurs aberrantes
    data <- data %>%
      group_by(Cell_line, Concentration_SAHA, Time) %>% 
      mutate(
        Outlier = {
          # Crée une description pour identifier chaque groupe
          group_info <- glue("{unique(Cell_line)}, {unique(Concentration_SAHA)}, {unique(Time)}") # nolint: line_length_linter.
          detect_outliers(Absorbances, group_info = group_info)
        }
      ) 
    
    # Affichage des données aberrantes retirées
    outliers <- data %>% filter(Outlier)
    if (nrow(outliers) > 0) {
      cat("\nValeurs considérées comme aberrantes et retirées :\n")
      print(outliers)
    } else {
      cat("\nAucune valeur aberrante détectée.\n")
    }
    
    # Filtrer les données sans valeurs aberrantes pour les calculs de moyenne
    data_clean <- data %>% filter(!Outlier)
    
    cat("\nTableau après retrait des valeurs aberrantes :\n")
    print(data_clean)
    
    return(data_clean)
  } else {
    # Si les valeurs aberrantes ne doivent pas être retirées, retourner les données d'origine
    return(data)
  }
}


"=================================================================="
# Fonction pour nettoyer les valeurs aberrantes
"=================================================================="
clean_outliers <- function(table_name) {
  if (exists(table_name)) {
    table_data <- get(table_name)
    cleaned_data <- process_outliers(table_data)
    cleaned_table_name <- paste0(table_name, "_clean")
    assign(cleaned_table_name, cleaned_data, envir = .GlobalEnv)
    return(cleaned_table_name)
  }
  return(NULL)
}

"=================================================================="
#Fonction pour le calcul d'écart-type + moyenne
"=================================================================="
SD_Mean <- function(data) {
  # Vérification des données
  if (nrow(data) == 0) {
    stop("Aucune donnée valide pour le calcul des moyennes et écarts-types.")
  }
  
  if (!"Absorbances" %in% names(data)) {
    stop("La colonne 'Absorbance' est manquante dans le jeu de données.")
  }
  
  if (!is.numeric(data$Absorbances)) {
    stop("La colonne 'Absorbance' doit être numérique.")
  }
data_summary <- data %>%
  group_by(.data$Cell_line, .data$Time, .data$Concentration_SAHA) %>%
  reframe(
    Absorbances = num(Absorbances),
    Mean_Absorbance = mean(.data$Absorbances, na.rm = TRUE),
    SD_Absorbance = sd(.data$Absorbances, na.rm = TRUE),
    .groups = "drop"
  )
 
data_f <- combine_conditions(data_summary)
  # Retour des statistiques finales
  return(data_f)  # Supprimer les colonnes temporaires si non nécessaires
}
"=================================================================="
# Fonction procéssive Analyse statistique 
"=================================================================="
perform_stat_tests <- function(data) {
  cat("\nH0 : Le SAHA n'a aucun effet significatif (dose, temps ou interaction).")
  cat("\nH1 : Le SAHA a un effet significatif (dose, temps ou interaction).\n")
  data$Absorbances <- as.numeric(data$Absorbances)
  results <- list()
  
  # Vérification globale des hypothèses
  cat("\n== Test global des hypothèses (normalité et homogénéité) ==\n")
  
  # Tester la normalité pour chaque combinaison de Concentration_SAHA et Time
  cat("\n== Résultats des tests de normalité (Shapiro-Wilk) ==\n")
  shapiro_results <- data %>%
    group_by(Cell_line, Concentration_SAHA, Time) %>%
    reframe(
      p_value = tryCatch({
        shapiro.test(Absorbances)$p.value
      }, error = function(e) {
        cat("Erreur dans Shapiro-Wilk :", e$message, "\n")
        return(NA)
      })
    )
  
  print(shapiro_results)
  cat("\nNote : Une p-value < 0.05 indique que l'hypothèse de normalité est rejetée.\n")
  
  # Tester l'homogénéité des variances (Bartlett)
  cat("\n== Résultat du test de Bartlett (homogénéité des variances) ==\n")
  bartlett_result <- tryCatch({
    bartlett.test(Absorbances ~ interaction(Concentration_SAHA, Time), data = data)
  }, error = function(e) {
    cat("Erreur avec le test de Bartlett :", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(bartlett_result)) {
    print(bartlett_result)
  } else {
    cat("Test de Bartlett non réalisé ou échoué.\n")
  }
  
  # Décision basée sur les hypothèses
  normality_respected <- all(shapiro_results$p_value > 0.05, na.rm = TRUE)
  homogeneity_respected <- !is.null(bartlett_result) && bartlett_result$p.value > 0.05
  
  if (normality_respected && homogeneity_respected) {
    cat("\n== Conditions respectées : Test ANOVA paramétrique sera utilisé. ==\n")
    
    # ANOVA à 2 facteurs
    anova_result <- aov(Absorbances ~ Concentration_SAHA * Time, data = data)
    p_values <- summary(anova_result)[[1]][["Pr(>F)"]]
    
    # Interprétation des résultats ANOVA
    interpretation <- if (any(p_values < 0.05)) {
      "Résultat significatif : H0 rejetée pour l'effet dose, temps ou interaction."
    } else {
      "Résultat non significatif : H0 acceptée. Aucun effet détecté."
    }
    
    # Post-hoc Tukey
    tukey_result <- TukeyHSD(anova_result)
    tukey_interpretation <- tukey_result %>%
      purrr::map(as.data.frame) %>%
      purrr::map(~ .x %>%
                   rownames_to_column("Comparison") %>%
                   mutate(
                     Interpretation = ifelse(
                       `p adj` < 0.05,
                       paste("H0 rejetée : différence significative entre", Comparison, "(p =", round(`p adj`, 5), ")."),
                       paste("H0 acceptée : aucune différence significative entre", Comparison, "(p =", round(`p adj`, 5), ").")
                     )
                   )
      )
    
    results[["ANOVA"]] <- list(
      test = "ANOVA à 2 facteurs",
      result = summary(anova_result),
      posthoc = tukey_interpretation,
      interpretation = interpretation
    )
  } else {
    cat("\n== Conditions non respectées : Test non paramétrique sera utilisé. ==\n")
    
    # Test non paramétrique Scheirer-Ray-Hare
    scheirer_result <- scheirerRayHare(Absorbances ~ Concentration_SAHA * Time, data = data)
    
    # Post-hoc Dunn pour effets non paramétriques
    dunn_result <- FSA::dunnTest(Absorbances ~ interaction(Cell_line, Concentration_SAHA, Time), 
                                 data = data, method = "bh")
    
    # Traitement des résultats post-hoc
    dunn_interpretation <- dunn_result$res %>%
      mutate(
        L1 = sapply(strsplit(Comparison, " - "), `[`, 1),
        L2 = sapply(strsplit(Comparison, " - "), `[`, 2),
        Interpretation = case_when(
          P.adj < 0.001 ~ paste("H0 rejetée : différence **hautement significative** entre", Comparison, "(p =", format(P.adj, scientific = TRUE), ")."),
          P.adj < 0.01 ~ paste("H0 rejetée : différence **très significative** entre", Comparison, "(p =", round(P.adj, 5), ")."),
          P.adj < 0.05 ~ paste("H0 rejetée : différence **significative** entre", Comparison, "(p =", round(P.adj, 5), ")."),
          TRUE ~ paste("H0 acceptée : **aucune différence significative** entre", Comparison, "(p =", round(P.adj, 5), ").")
        )
      )
    
    results[["ScheirerRayHare"]] <- list(
      test = "Scheirer-Ray-Hare",
      result = scheirer_result,
      posthoc = dunn_interpretation,
      interpretation = "Résultat significatif ou non selon le test."
    )
  }
  
  return(results)
}
"=================================================================="
# Fonction pour appliquer les statistiques aux données
"=================================================================="
Calcul_SD_Mean <- function(cleaned_table_name) {
  if (exists(cleaned_table_name)) {
    cleaned_data <- get(cleaned_table_name)
    stats <- SD_Mean(cleaned_data)
    stats$Essai <- gsub("_normalise_clean", "", cleaned_table_name)
    return(stats)
  }
  return(NULL)
}

create_CCK8_barplot <- function(data_summary) {
  # Couleurs pour les concentrations
  colors <- c("0" = "#04BF23", "10" = "#00bbff", "25" = "#ff0000")
  
  # Fonction pour créer un graphique pour une seule lignée
  generate_plot_for_cell_line <- function(data_summary, cell_line) {
    plot_data <- data_summary %>%
      filter(Cell_line == cell_line) %>%
      mutate(
        x_axis = paste(Time, Concentration_SAHA, sep = "."),
        x_axis = factor(x_axis, levels = c("24h.0", "24h.10", "24h.25", "48h.0", "48h.10", "48h.25")) # Ordre 24h -> 48h
      )
    
    ggplot(plot_data, aes(x = x_axis, y = Mean_Absorbance, fill = as.factor(Concentration_SAHA))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
      geom_errorbar(
        aes(ymin = Mean_Absorbance - SD_Absorbance, ymax = Mean_Absorbance + SD_Absorbance),
        width = 0.2,
        position = position_dodge(width = 0.8)
      ) +
      scale_fill_manual(values = colors, name = "Concentration SAHA (µM)") +
      labs(
        title = paste("Effet du SAHA sur l'activité métabolique enzymatique des mitochondrie pour", cell_line),
        x = "Conditions (Temps.Concentration)",
        y = "Absorbance Moyenne (± Écart-Type)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
      )
  }
  
  # Générer les graphiques pour PRI et SU-DHL-4
  print(generate_plot_for_cell_line(data_summary, "PRI"))
  print(generate_plot_for_cell_line(data_summary, "SU-DHL-4"))
}

"=================================================================="
# Fonction pour combiné ,calculer les moyennes et écarts types après nettoyage 
"=================================================================="
process_combined_stats <- function(combined_stats) {
  # Vérification que l'entrée est une liste
  if (!is.list(combined_stats)) {
    stop("Erreur : L'entrée doit être une liste.")
  }
  
  # Dégroupement et traitement des colonnes nécessaires pour chaque tibble
  cleaned_list <- lapply(combined_stats, function(tbl) {
    # Vérification si l'élément est un tibble ou une liste contenant un tibble
    if (!inherits(tbl, "tbl_df") && is.list(tbl)) {
      tbl <- tbl[[1]]
    }
    
    # Vérification des colonnes nécessaires
    required_columns <- c("Cell_line", "Concentration_SAHA", "Time", "Absorbances")
    missing_cols <- setdiff(required_columns, colnames(tbl))
    if (length(missing_cols) > 0) {
      stop(glue::glue("Erreur : Les colonnes suivantes sont manquantes : {paste(missing_cols, collapse = ', ')}"))
    }
    
    # Transformation des données (conversion des types)
    tbl <- tbl %>%
      mutate(
        Cell_line = as.factor(Cell_line),  # Conversion en facteur
        Concentration_SAHA = as.factor(Concentration_SAHA),  # Conversion en facteur
        Time = as.factor(Time),  # Conversion en facteur
        Absorbances = as.numeric(Absorbances)  # Conversion en numérique
      )
    
    # Supprimer les valeurs aberrantes si elles sont marquées
    if ("Outlier" %in% colnames(tbl)) {
      tbl <- tbl %>% filter(!Outlier)  # Retirer les valeurs avec `Outlier == TRUE`
    }
    
    return(tbl)
  })
  
  # Combiner tous les tibbles en un seul dataframe
  combined_df <- bind_rows(cleaned_list, .id = "Source")
  
  # Vérification et affichage des colonnes du tableau combiné
  cat("Colonnes de combined_df après combinaison :", paste(colnames(combined_df), collapse = ", "), "\n")
  
  # Afficher un aperçu des données combinées
  cat("Aperçu des données combinées :\n")
  print(head(combined_df))
  
  # Retourner le tableau combiné
  return(combined_df)
}
"================================================================="
# Fonction pour traiter mes triplicat (optionnel selon expérimentation)
"=================================================================="
process_all_sheets <- function(file_path) {
  # Obtenir les noms des feuilles
  sheet_names <- excel_sheets(file_path)
  print("Noms des feuilles disponibles :")
  print(sheet_names)
  
  if (length(sheet_names) == 0) {
    stop("Le fichier Excel ne contient aucune feuille.")
  }
  
  # Initialisation des listes pour stocker les résultats
  tables_generées <- list()
  tables_cleaned <- list()
  
  compteur <- 1
  for (sheet in sheet_names) {
    tryCatch({
      cat("\nTraitement de la feuille :", sheet, "\n")
      
      # Appel de `transform_data`
      table_name <- transform_data(sheet_name = sheet, compteur = compteur)
      
      # Récupérer le tableau transformé
      data_transformed <- get(table_name)
      print("Données transformées :")
      print(data_transformed)
      
      # Nettoyage des valeurs aberrantes
      cleaned_table <- process_outliers(data_transformed, remove_outliers = TRUE)
      print("Données nettoyées :")
      print(cleaned_table)
      
      # Ajouter aux listes
      tables_cleaned[[table_name]] <- cleaned_table
      
    }, error = function(e) {
      cat("Erreur lors du traitement de la feuille :", sheet, "\n", e$message, "\n")
    })
    compteur <- compteur + 1
  }
  
  return(list(
    tables_cleaned = tables_cleaned
  ))
}


"===================== # Fonction principale pour traiter toutes les feuilles ========================"
"==========================================================="

process_combined_stats <- function(results) {
  # Vérifier que l'objet results contient bien la clé "tables_cleaned"
  if (!is.list(results) || !("tables_cleaned" %in% names(results))) {
    stop("Erreur : L'entrée ne contient pas la clé 'tables_cleaned'.")
  }
  
  # Extraire la liste des tables dans "tables_cleaned"
  combined_stats <- results$tables_cleaned
  
  # Dégroupement et combinaison des dataframes
  combined_df <- lapply(names(combined_stats), function(tbl_name) {
    tbl <- combined_stats[[tbl_name]]
    
    # Vérifier si c'est un tibble groupé et dégroupé si nécessaire
    if ("grouped_df" %in% class(tbl)) {
      tbl <- tbl %>% ungroup()
    }
    tbl <- tbl %>%
      mutate(Source = tbl_name)
    
    return(tbl)
  }) %>%
    bind_rows() 
  
  print("Colonnes de combined_df après combinaison :")
  print(colnames(combined_df))
  print("Aperçu des données combinées :")
  print(combined_df)
  print(combined_df$Absorbances)
  
  return(combined_df)
  print("process ok")
}

"================================================================="
# Fonction pour traiter mes triplicat (optionnel selon expérimentation)
"=================================================================="
process_all_sheets <- function(file_path) {
  # Obtenir les noms des feuilles
  sheet_names <- excel_sheets(file_path)
  print("Noms des feuilles disponibles :")
  print(sheet_names)
  
  if (length(sheet_names) == 0) {
    stop("Le fichier Excel ne contient aucune feuille.")
  }
  
  # Initialisation des listes pour stocker les résultats
  tables_generées <- list()
  tables_cleaned <- list()
  
  compteur <- 1
  for (sheet in sheet_names) {
    tryCatch({
      cat("\nTraitement de la feuille :", sheet, "\n")
      
      # Appel de `transform_data`
      table_name <- transform_data(sheet_name = sheet, compteur = compteur)
      
      # Récupérer le tableau transformé
      data_transformed <- get(table_name)
      print("Données transformées :")
      print(data_transformed)
      
      # Nettoyage des valeurs aberrantes
      cleaned_table <- process_outliers(data_transformed, remove_outliers = TRUE)
      print("Données nettoyées :")
      print(cleaned_table)
      
      # Ajouter aux listes
      tables_cleaned[[table_name]] <- cleaned_table
      
    }, error = function(e) {
      cat("Erreur lors du traitement de la feuille :", sheet, "\n", e$message, "\n")
    })
    compteur <- compteur + 1
  }
  
  return(list(
    tables_cleaned = tables_cleaned
  ))
}
"===================== # Fonction principale pour traiter toutes les feuilles ========================"
"============================================================================================================"
# Chemin vers votre fichier Excel
file_path <- '/Users/jamzi/Downloads/CCK8 uni.xlsx'

# Étape 1 : Traitement des feuilles Excel
results <- process_all_sheets(file_path)
print(results)
cat("OK 1\n")

# Étape 2 : Combinaison des résultats
resultats_essai_combine_p <- process_combined_stats(results)
cat("OK 2\n")

# Étape 3 : Tests statistiques
stat_results <- perform_stat_tests(resultats_essai_combine_p)
resultats_essai_combine_m <- SD_Mean(resultats_essai_combine_p)
cat("OK 3\n")

# Étape 4 : Génération des graphiques
create_CCK8_barplot(resultats_essai_combine_m)
cat("OK 4\n")

# Étape 5 : Affichage des résultats finaux
cat("\n=== Résultats combinés avec moyennes et écarts-types ===\n")
print(stat_results)
