#====================================
# Instaliuojami reikalingi paketai
#====================================
source("required_libraries.R")
source("all_function.R")
library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)
library(forcats)
library(RColorBrewer)
library(corrplot)

#====================================
# PISA duomenų įkėlimas
#====================================

myroot <- "C:/R/"
ILSA <- "PISA"
year <- 2022 
myvariables  <- c("cnt", "escs","pv1math", "math", 
                  "belong", "st034q01ta", "st034q02ta", "st034q03ta","st034q04ta", "st034q05ta", "st034q06ta",
                  "relatst", "st267q01ja", "st267q02ja", "st267q03ja", "st267q04ja", "st267q05ja",
                  "st267q06ja", "st267q07ja", "st267q08ja")
countries <- c("LTU", "LVA", "EST", "POL")
ltu_data <- try(data_form(myroot, ILSA, year, "LTU", myvariables))
est_data <- try(data_form(myroot, ILSA, year, "EST", myvariables))
lva_data <- try(data_form(myroot, ILSA, year, "LVA", myvariables))
pol_data <- try(data_form(myroot, ILSA, year, "POL", myvariables))

#====================================
# Indeksų klausimų lentelės
#====================================

#------------------------------------RELATST------------------------------------#
relatst_variables <- c("st267q01ja", "st267q02ja", "st267q03ja", "st267q04ja", "st267q05ja",
                       "st267q06ja", "st267q07ja", "st267q08ja")


relatst_klausimai_table <- data.frame(
    Klausimo_Indeksas = relatst_variables,
    Klausimas = c(
        "Mokytojai mano mokykloje mane gerbia.",
        "Jei ateičiau į pamoką nusiminusi, mokytojai dėl manęs susirūpintų.",
        "Jei po 3 metų grįžčiau į savo mokyklą, mokytojai džiaugtųsi mane pamatę.",
        "Mokytojai mano mokykloje man kelia baimę.",
        "Kai mokytojai klausia, kaip man sekasi, jiems nuoširdžiai rūpi mano atsakymas.",
        "Mokytojai mano mokykloje yra draugiški su manimi.",
        "Mokytojai domisi mokinių gerove.",
        "Mokytojai mano mokykloje yra pikti su manimi."
    ),
    stringsAsFactors = FALSE
)


print(relatst_klausimai_table)


#------------------------------------BELONG------------------------------------#
belong_variables <- c("st034q01ta", "st034q02ta", "st034q03ta", "st034q04ta", "st034q05ta", "st034q06ta")


belong_klausimai_table <- data.frame(
    Klausimo_Indeksas = belong_variables,
    Klausimas = c(
        "Jaučiuosi kaip svetimas/pašalinis (arba esu atstumtas) mokykloje.",
        "Mokykloje lengvai susirandu draugų.",
        "Jaučiuosi, kad priklausau mokyklai.",
        "Mokykloje jaučiuosi nepatogiai ir svetimas.",
        "Panašu, kad kiti mokiniai mane mėgsta.",
        "Mokykloje jaučiuosi vienišas."
    ),
    stringsAsFactors = FALSE
)


print(belong_klausimai_table)

#===================================================
# Skaitinių duomenų aprašomųjų statistikų lentelės
#===================================================

all_data <- rbind(ltu_data, lva_data, est_data, pol_data)

dsc_variables1 <- c("escs", "math", "belong", "relatst")

data_by_country <- split(all_data, all_data$cnt)

country_names_lt <- c(
    "LITHUANIA" = "Lietuva",
    "LATVIA" = "Latvija",
    "ESTONIA" = "Estija",
    "POLAND" = "Lenkija"
)

create_table_for_variable <- function(variable) {
    tables_list <- lapply(names(data_by_country), function(country) {
        df <- data_by_country[[country]]
        freq_obj <- frequency_table(df, variable)
        tbl <- bind_rows(Filter(is.data.frame, freq_obj))
        tbl$Šalis <- country_names_lt[country]
        
        return(tbl)
    })
    
    table <- bind_rows(tables_list)
    
    table <- table[, c("Šalis", setdiff(names(table), "Šalis"))]
    
    table <- table %>%
        rename(
            Kintamasis = Variable,
            `Stebėjimų sk.` = N,
            Svertinis_N = `Weighted N`,
            `Praleistų reikšmių sk.` = `Na's`,
            Vidurkis = Mean,
            SE = SE,
            `Standartinis nuokrypis` = SD,
            Min = Min,
            Q1 = `1st Qu.`,
            Mediana = Median,
            Q3 = `3rd Qu.`,
            Max = Max
        ) %>%
        mutate(
            IQR = as.numeric(Q3) - as.numeric(Q1)
        ) %>%
        select(-SE, -Kintamasis, -Svertinis_N)
    
    numeric_cols <- setdiff(names(table), "Šalis")
    table[numeric_cols] <- lapply(table[numeric_cols], function(x) round(as.numeric(x), 2))
    
    return(table)
}



escs_table <- create_table_for_variable("escs")
math_table <- create_table_for_variable("math")
belong_table <- create_table_for_variable("belong")
relatst_table <- create_table_for_variable("relatst")

#============================
# Stulpelinės diagramos
#============================

create_plot <- function(data, variable, title_text) {
    data$Šalis <- factor(data$Šalis, levels = c("Lietuva", "Latvija", "Estija", "Lenkija"))
    
    ggplot(data, aes(x = Šalis, y = Vidurkis, fill = Šalis)) +
        geom_col(width = 0.6, alpha = 0.8) +
        geom_text(aes(label = round(Vidurkis, 2)), vjust = -0.5, size = 5) +
        labs(
            title = title_text,
            x = "",
            y = paste(" ", variable, "Vidurkis")  
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position = "none"
        ) +
        scale_fill_manual(values = c(
            "Lietuva" = "#fc8d62", 
            "Latvija" = "#b7b2e1",  
            "Estija" = "#fdca40",  
            "Lenkija" = "#00ccbb"   
        ))
}

create_plot(math_table, "", "")

#============================
# Stačiakampės diagramos
#============================

data_list <- list(ltu_data, lva_data, est_data, pol_data)

all_data <- bind_rows(
    mutate(data_list[[1]], country = "LTU"),
    mutate(data_list[[2]], country = "LVA"),
    mutate(data_list[[3]], country = "EST"),
    mutate(data_list[[4]], country = "POL")
)


y_min <- min(c(all_data$escs, all_data$belong, all_data$relatst), na.rm = TRUE)
y_max <- max(c(all_data$escs, all_data$belong, all_data$relatst), na.rm = TRUE)


plot_boxplot <- function(data_list, country_names, variable_text, variable, title_text, y_limits = NULL, show_y_label = TRUE) {
    
    all_data <- bind_rows(
        mutate(data_list[[1]], country = "LTU"),
        mutate(data_list[[2]], country = "LVA"),
        mutate(data_list[[3]], country = "EST"),
        mutate(data_list[[4]], country = "POL")
    )
    
    all_data <- all_data %>%
        mutate(country = factor(country, levels = c("LTU", "LVA", "EST", "POL")))
    
    data_subset <- all_data[, c("country", variable)]
    
    data_long <- pivot_longer(
        data_subset, 
        cols = variable, 
        names_to = "kintamasis", 
        values_to = "reiksme"
    )
    
    p <- ggplot(data_long, aes(x = country, y = reiksme, fill = country)) +
        geom_boxplot(outlier.colour = "grey20", outlier.shape = 16, outlier.size = 2, alpha = 0.9, na.rm = TRUE) +
        scale_fill_manual(values = c(
            "LTU" = "#fc8d62",
            "LVA" = "#b7b2e1",
            "EST" = "#fdca40",
            "POL" = "#00ccbb"
        )) +
        labs(
            title = title_text,
            x = "",
            y = if (show_y_label) "Reikšmė" else NULL
        ) +
        scale_x_discrete(labels = country_names) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 17, face = "plain", hjust = 0.5),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 18),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            panel.grid = element_line(color = "gray90"),
            legend.position = "none"
        )
    

    if (!is.null(y_limits)) {
        p <- p + ylim(y_limits)
    }
    
    return(p)
}


p1 <- plot_boxplot(data_list, country_names_lt, "SEK", "escs", "SEK indeksas", y_limits = c(y_min, y_max), show_y_label = TRUE)
p2 <- plot_boxplot(data_list, country_names_lt, "BELONG", "belong", "BELONG indeksas", y_limits = c(y_min, y_max), show_y_label = FALSE)
p3 <- plot_boxplot(data_list, country_names_lt, "RELATST", "relatst", "RELATST indeksas", y_limits = c(y_min, y_max), show_y_label = FALSE)


(p1 + p2 + p3) +
    plot_annotation(
        theme = theme(
            plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
        )
    )

#====================================
# Indeksų klausimų užpildymo lentelės
#====================================

#------------------------------------BELONG------------------------------------#
calculate_answer_percentage <- function(data, columns, country_name) {
    result <- sapply(data[columns], function(x) {
        mean(x %in% c("AGREE", "DISAGREE", "STRONGLY AGREE", "STRONGLY DISAGREE"), na.rm = TRUE) * 100
    }) 
    
    data.frame(
        Klausimo_Indeksas = names(result),  
        Percentage = as.numeric(result),
        Country = country_name,
        stringsAsFactors = FALSE
    )
}


belong_variables <- c("st034q01ta", "st034q02ta", "st034q03ta", "st034q04ta", "st034q05ta", "st034q06ta")


country_data <- list(
    list(data = ltu_data, name = "Lietuva"),
    list(data = est_data, name = "Estija"),
    list(data = pol_data, name = "Lenkija"),
    list(data = lva_data, name = "Latvija")
)


combined_table <- bind_rows(lapply(country_data, function(x) {
    calculate_answer_percentage(x$data, belong_variables, x$name)
}))


belong_klausimu_table <- combined_table %>%
    pivot_wider(
        id_cols = Klausimo_Indeksas,  
        names_from = Country,
        values_from = Percentage,
        names_prefix = ""
    ) %>%
    
    mutate(Klausimas = case_when(
        Klausimo_Indeksas == "st034q01ta" ~ "Jaučiuosi kaip svetimas/pašalinis (arba esu atstumtas) mokykloje.",
        Klausimo_Indeksas == "st034q02ta" ~ "Mokykloje lengvai susirandu draugų.",
        Klausimo_Indeksas == "st034q03ta" ~ "Jaučiuosi, kad priklausau mokyklai.",
        Klausimo_Indeksas == "st034q04ta" ~ "Mokykloje jaučiuosi nepatogiai ir svetimas.",
        Klausimo_Indeksas == "st034q05ta" ~ "Panašu, kad kiti mokiniai mane mėgsta.",
        Klausimo_Indeksas == "st034q06ta" ~ "Mokykloje jaučiuosi vienišas.",
        TRUE ~ Klausimo_Indeksas
    )) %>%
    select(Klausimo_Indeksas, Klausimas, everything())

colnames(belong_klausimu_table) <- gsub("_", " ", colnames(belong_klausimu_table))


belong_klausimu_table <- belong_klausimu_table %>%
    mutate(across(c(Lietuva, Estija, Lenkija, Latvija), ~ paste0(round(., 1), "%")))


print(belong_klausimu_table)


#------------------------------------RELATST------------------------------------#
relatst_variables <- c("st267q01ja", "st267q02ja", "st267q03ja", "st267q04ja", "st267q05ja",
                       "st267q06ja", "st267q07ja", "st267q08ja")

combined_table <- bind_rows(lapply(country_data, function(x) {
    calculate_answer_percentage(x$data, relatst_variables, x$name)
}))


relatst_klausimu_table <- combined_table %>%
    pivot_wider(
        id_cols = Klausimo_Indeksas,  
        names_from = Country,
        values_from = Percentage,
        names_prefix = ""
    ) %>%
    
    mutate(Klausimas = case_when(
        Klausimo_Indeksas == "st267q01ja" ~ "Mokytojai mano mokykloje mane gerbia.",
        Klausimo_Indeksas == "st267q02ja" ~ "Jei ateičiau į pamoką nusiminusi, mokytojai dėl manęs susirūpintų.",
        Klausimo_Indeksas == "st267q03ja" ~ "Jei po 3 metų grįžčiau į savo mokyklą, mokytojai džiaugtųsi mane pamatę.",
        Klausimo_Indeksas == "st267q04ja" ~ "Mokytojai mano mokykloje man kelia baimę.",
        Klausimo_Indeksas == "st267q05ja" ~ "Kai mokytojai klausia, kaip man sekasi, jiems nuoširdžiai rūpi mano atsakymas.",
        Klausimo_Indeksas == "st267q06ja" ~ "Mokytojai mano mokykloje yra draugiški su manimi.",
        Klausimo_Indeksas == "st267q07ja" ~ "Mokytojai domisi mokinių gerove.",
        Klausimo_Indeksas == "st267q08ja" ~ "Mokytojai mano mokykloje yra pikti su manimi.",
        TRUE ~ Klausimo_Indeksas
    )) %>%
    select(Klausimo_Indeksas, Klausimas, everything())


colnames(relatst_klausimu_table) <- gsub("_", " ", colnames(relatst_klausimu_table))


relatst_klausimu_table <- relatst_klausimu_table %>%
    mutate(across(c(Lietuva, Estija, Lenkija, Latvija), ~ paste0(round(., 1), "%")))


print(relatst_klausimu_table)

#=======================================
# Indeksų klausimų užpildymo grafikai
#=======================================

#------------------------------------RELATST------------------------------------#

color_palette <- brewer.pal(4, "PiYG")  


relatst_questions <- c(
    "st267q01ja" = "Mokyklos mokytojai \nmane gerbia",
    "st267q02ja" = "Jeigu ateičiau į pamokas \nnusiminęs (-usi), mokytojai \nsusirūpintų dėl manęs.",
    "st267q03ja" = "Jeigu po trejų metų vėl \napsilankyčiau mokykloje, \nmokytojai apsidžiaugtų mane \npamatę.",
    "st267q04ja" = "Mano mokyklos mokytojai \nman kelia nerimą.",
    "st267q05ja" = "Kai mano mokytojai klausia,\n kaip man sekasi, jiems iš \ntikrųjų rūpi, ką atsakysiu.",
    "st267q06ja" = "Mano mokyklos mokytojai su \nmanimi bendrauja draugiškai.",
    "st267q07ja" = "Mano mokyklos mokytojams \nrūpi mokinių gerovė.",
    "st267q08ja" = "Mano mokyklos mokytojai \nyra nusiteikę prieš mane."
)


atsakymas_lt <- c(   
    "STRONGLY DISAGREE" = "Visiškai nesutinku" ,
    "DISAGREE" = "Nesutinku",   
    "AGREE" = "Sutinku",
    "STRONGLY AGREE" = "Visiškai sutinku"
    
    
)


prepare_country_plot_data <- function(data, country_name, questions) {
    data %>%
        select(all_of(names(questions))) %>%
        pivot_longer(cols = everything(), names_to = "Klausimo_ID", values_to = "Atsakymas") %>%
        filter(Atsakymas %in% c("AGREE", "DISAGREE", "STRONGLY AGREE", "STRONGLY DISAGREE")) %>%
        group_by(Klausimo_ID, Atsakymas) %>%
        summarise(Skaicius = n(), .groups = "drop") %>%
        group_by(Klausimo_ID) %>%
        mutate(Procentas = Skaicius / sum(Skaicius, na.rm = TRUE) * 100,  
               Salis = country_name) %>%
        ungroup()
}


plot_data <- bind_rows(
    prepare_country_plot_data(ltu_data, "Lietuva", relatst_questions),
    prepare_country_plot_data(lva_data, "Latvija", relatst_questions),
    prepare_country_plot_data(est_data, "Estija", relatst_questions),
    prepare_country_plot_data(pol_data, "Lenkija", relatst_questions)
) %>%
    mutate(
        Klausimas = relatst_questions[Klausimo_ID],
        Atsakymas = factor(
            atsakymas_lt[Atsakymas],
            levels = c("Visiškai nesutinku", "Nesutinku", "Sutinku", "Visiškai sutinku")
        ),
        Salis = factor(
            Salis,
            levels = c("Lietuva", "Latvija", "Estija", "Lenkija")  # Pakeista tvarka
        )
    )


klausimai <- unique(plot_data$Klausimas)


plot_question <- function(q_name) {
    ggplot(filter(plot_data, Klausimas == q_name), aes(x = Salis, y = Procentas, fill = Atsakymas)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c(
            "Visiškai sutinku" = color_palette[4],
            "Sutinku" = color_palette[3],
            "Nesutinku" = color_palette[2],
            "Visiškai nesutinku" = color_palette[1]
        )) +
        labs(
            title = q_name,  
            x = NULL, 
            y = "Procentas",  
            fill = "Atsakymas"
        ) +
        theme_minimal(base_size = 15) +
        theme(
            plot.title = element_text(
                size = 14,          
                face = "plain",     
                hjust = 0.5,
                margin = margin(b = 3)  
            )
        )
}





plot_list <- lapply(klausimai, function(q) {
    plot_question(q) + theme(legend.position = "none")
})


combined_plots <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]]) /
    (plot_list[[4]] | plot_list[[5]] | plot_list[[6]]) /
    (plot_list[[7]] | plot_list[[8]])


final_plot <- combined_plots +
    plot_annotation(
        theme = theme(
            plot.title = element_text(
                size = 21,          
                face = "bold",      
                hjust = 0.5,
                margin = margin(b = 15)
            )
        )
    ) +
    plot_layout(guides = "collect") &  
    theme(legend.position = "bottom")  

final_plot

#------------------------------------BELONG------------------------------------#

belong_questions <- c(
    
    "st034q01ta" = "Mokykloje jaučiuosi kaip pašalinis (-ė)\n (arba atstumtasis (-oji)).",
    
    "st034q02ta" = "Mokykloje lengvai susirandu draugų.",
    
    "st034q03ta" = "Jaučiuosi priklausantis (-i) \nmokyklai.",
    
    "st034q04ta" = "Mokykloje jaučiuosi nejaukiai \nir ne vietoje.",
    
    "st034q05ta" = "Atrodo, kad kitiems mokiniams \naš patinku.",
    
    "st034q06ta" = "Mokykloje jaučiuosi vienišas (-a)."
    
)



atsakymas_lt <- c(  
    "STRONGLY AGREE" = "Visiškai sutinku",  
    "AGREE" = "Sutinku",
    "DISAGREE" = "Nesutinku",   
    "STRONGLY DISAGREE" = "Visiškai nesutinku"
    
)

atsakymu_tvarka <- c("Visiškai nesutinku", "Nesutinku", "Sutinku", "Visiškai sutinku")



plot_data <- bind_rows(
    prepare_country_plot_data(ltu_data, "Lietuva", belong_questions),
    prepare_country_plot_data(lva_data, "Latvija", belong_questions),
    prepare_country_plot_data(est_data, "Estija", belong_questions),
    prepare_country_plot_data(pol_data, "Lenkija", belong_questions)
) %>%
    mutate(
        Klausimas = belong_questions[Klausimo_ID],
        Atsakymas = factor(atsakymas_lt[Atsakymas], levels = atsakymu_tvarka),
        Salis = factor(Salis, levels = c("Lietuva", "Latvija", "Estija", "Lenkija"))  # <- ČIA svarbu
    )



klausimai <- unique(plot_data$Klausimas)


plot_question <- function(q_name) {
    ggplot(filter(plot_data, Klausimas == q_name), aes(x = Salis, y = Procentas, fill = Atsakymas)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(
            values = setNames(color_palette, atsakymu_tvarka)
        ) +
        labs(
            title = q_name,
            x = NULL,
            y = "Procentas",
            fill = "Atsakymas"
        ) +
        theme_minimal(base_size = 15) +
        theme(
            plot.title = element_text(
                size = 14,
                face = "plain",
                hjust = 0.5,
                margin = margin(b = 3)
            )
        )
}



plot_list <- lapply(klausimai, function(q) {
    plot_question(q) + theme(legend.position = "none")
})

combined_plots <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]]) /
    (plot_list[[4]] | plot_list[[5]] | plot_list[[6]]) 


final_plot <- combined_plots +
    plot_annotation(
        theme = theme(
            plot.title = element_text(
                size = 21,         
                face = "bold",     
                hjust = 0.5,
                margin = margin(b = 15)
            )
        )
    ) +
    plot_layout(guides = "collect") &  
    theme(legend.position = "bottom") 

final_plot

#=======================================
# Koreliacijos matricos
#=======================================

library(RColorBrewer)


plot_correlation <- function(mydata, myvariables, variables, method = "Spearman", title_text) {
    
    if (length(variables) == 1) {
        variables <- unlist(variables)
    }

    cor_results <- tryCatch({
        correlation(mydata, myvariables, method = method, variables)
    }, error = function(e) {
        message("Error in correlation(): ", e$message)
        return(NULL)
    })
    
    if (is.null(cor_results)) return()
    
    cor_matrix <- tryCatch({
        cor_results[["Correlation matrix"]][[1]]
    }, error = function(e) {
        message("Error extracting correlation matrix: ", e$message)
        return(NULL)
    })
    
    if (is.null(cor_matrix)) return()

    custom_palette <- colorRampPalette(c("lightcoral", "white", "lightblue"))(200)

    corrplot(cor_matrix,
             method = "color",
             type = "upper",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             tl.cex = 0.8,
             cl.pos = "r",
             number.cex = 0.8,
             mar = c(0, 0, 3, 0),
             cex.main = 1.5,
             cex.lab = 1.2,
             title = title_text,
             col = custom_palette)
}

plot_correlation(ltu_data, myvariables, c("escs", "math", "belong", "relatst"), method = "Spearman", title_text = "Lietuva")
plot_correlation(lva_data, myvariables, c("escs", "math", "belong", "relatst"), method = "Spearman", title_text = "Latvija")
plot_correlation(est_data, myvariables, c("escs", "math", "belong", "relatst"), method = "Spearman", title_text = "Estija")
plot_correlation(pol_data, myvariables, c("escs", "math", "belong", "relatst"), method = "Spearman", title_text = "Lenkija")

#=======================================
# Tiesinė regresija
#=======================================

#------------------------------------RELATST------------------------------------#

countries_data <- list(
    "Lietuva" = ltu_data,
    "Latvija" = lva_data,
    "Estija" = est_data,
    "Lenkija" = pol_data
)

create_regression_summary_table_relatst <- function(countries_data, depended = "math", independed_with_escs = c("relatst", "escs"), independed_without_escs = "relatst", package = 2) {
    format_number <- function(x) {
        if (is.na(x)) return(NA)
        if (abs(x) < 0.001) return(formatC(x, format = "e", digits = 2))
        return(formatC(x, format = "f", digits = 3))
    }
    
    results <- data.frame(
        Šalis = character(),
        Modelis = character(),
        RELATST = character(),
        SEK = character(),
        R2 = character(),
        Paklaidų.normalumas = character(),
        stringsAsFactors = FALSE
    )
    
    for (country_name in names(countries_data)) {
        country_data <- countries_data[[country_name]]
        
        reg_with_escs <- tryCatch(line_regression(country_data, myvariables, depended, independed_with_escs, package), error = function(e) NULL)
        reg_without_escs <- tryCatch(line_regression(country_data, myvariables, depended, independed_without_escs, package), error = function(e) NULL)
        
        if (is.null(reg_with_escs) || is.null(reg_without_escs)) next
        
        coefs_with <- reg_with_escs$coefmat
        coefs_without <- reg_without_escs$coefmat
        if (nrow(coefs_with) == 0 || nrow(coefs_without) == 0) next
        
        R2_with <- summary(reg_with_escs)$r.squared
        R2_without <- summary(reg_without_escs)$r.squared
        
        residuals_with <- reg_with_escs$residuals
        residuals_without <- reg_without_escs$residuals
        

        pval_normal_with <- tryCatch(nortest::cvm.test(residuals_with)$p.value, error = function(e) NA)
        pval_normal_without <- tryCatch(nortest::cvm.test(residuals_without)$p.value, error = function(e) NA)
        
        normal_with <- ifelse(!is.na(pval_normal_with) && pval_normal_with > 0.05, "Taip", "Ne")
        normal_without <- ifelse(!is.na(pval_normal_without) && pval_normal_without > 0.05, "Taip", "Ne")
        
        country_results <- rbind(
            data.frame(
                Šalis = country_name,
                Modelis = "Be SEK",
                RELATST = format_number(coefs_without["relatst", "coef"]),
                SEK = NA,
                R2 = format_number(R2_without),
                Paklaidų.normalumas = normal_without,
                stringsAsFactors = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                RELATST = format_number(coefs_without["relatst", "Pr(>|t|)"]),
                SEK = NA,
                R2 = NA,
                Paklaidų.normalumas = format_number(pval_normal_without),
                stringsAsFactors = FALSE
            ),
            data.frame(
                Šalis = country_name,
                Modelis = "Su SEK",
                RELATST = format_number(coefs_with["relatst", "coef"]),
                SEK = format_number(coefs_with["escs", "coef"]),
                R2 = format_number(R2_with),
                Paklaidų.normalumas = normal_with,
                stringsAsFactors = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                RELATST = format_number(coefs_with["relatst", "Pr(>|t|)"]),
                SEK = format_number(coefs_with["escs", "Pr(>|t|)"]),
                R2 = NA,
                Paklaidų.normalumas = format_number(pval_normal_with),
                stringsAsFactors = FALSE
            )
        )
        
        results <- rbind(results, country_results)
    }
    
    return(results)
}


results_relatst <- create_regression_summary_table_relatst(countries_data)

print(results_relatst)

#------------------------------------RELATST KLAUSIMAI------------------------------------#

format_p_value <- function(p) {
    if (is.na(p)) return(NA)
    if (p < 2.2e-16) return("<2.2e-16")
    if (p < 0.001) return(formatC(p, format = "e", digits = 2))  # eksponentinis tik labai mažoms
    return(formatC(p, format = "f", digits = 3))  # įprastas formatas su 3 skaičiais po kablelio
}

create_multiple_regression_tables <- function(countries_data, questions, dependent_var = "math", package = 2) {
    results_list <- list()
    
    for (q in questions) {
        cat("Apdorojamas klausimas:", q, "\n")
        table_result <- create_regression_summary_table(countries_data, q, dependent_var, package)
        rownames(table_result) <- NULL 
        results_list[[q]] <- table_result
    }
    
    return(results_list)
}

create_regression_summary_table <- function(countries_data, question, dependent_var = "math", package = 2) {
    format_number <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x)) return(NA)
        if (abs(x) < 0.001) return(formatC(x, format = "e", digits = 3))
        return(formatC(x, format = "f", digits = 3))
    }
    
    format_coefficient <- function(x) {
        if (is.na(x)) return(NA)
        formatC(x, format = "f", digits = 3)
    }
    
    adjusted_r2_manual <- function(r2, n, k) {
        if (is.na(r2) || is.na(n) || is.na(k)) return(NA)
        return(1 - ((1 - r2) * (n - 1)) / (n - k - 1))
    }
    
    results <- data.frame()
    
    for (country_name in names(countries_data)) {
        country_data <- countries_data[[country_name]]
        

        vars_no_escs <- c(question)
        model_no_escs <- line_regression(country_data, myvariables, dependent_var, vars_no_escs, package)
        coefs_no_escs <- model_no_escs$coefmat
        

        vars_with_escs <- c(question, "escs")
        model_with_escs <- line_regression(country_data, myvariables, dependent_var, vars_with_escs, package)
        coefs_with_escs <- model_with_escs$coefmat
        
        identify_category <- function(lvl) {
            case_when(
                grepl("STRONGLY DISAGREE$", lvl, ignore.case = TRUE) ~ "Visiškai_nesutinku",
                grepl("DISAGREE$", lvl, ignore.case = TRUE) & !grepl("STRONGLY", lvl) ~ "Nesutinku",
                grepl("AGREE$", lvl, ignore.case = TRUE) & !grepl("STRONGLY", lvl) ~ "Sutinku",
                grepl("STRONGLY AGREE$", lvl, ignore.case = TRUE) ~ "Visiškai_sutinku",
                TRUE ~ NA_character_
            )
        }
        
        extract_values <- function(coefs) {
            factor_levels <- grep(paste0("^", question), rownames(coefs), value = TRUE)
            coef_vals <- setNames(rep(NA, 4), c("Visiškai_nesutinku", "Nesutinku", "Sutinku", "Visiškai_sutinku"))
            p_vals <- coef_vals
            
            if ("(Intercept)" %in% rownames(coefs)) {
                coef_vals["Visiškai_nesutinku"] <- as.numeric(format_coefficient(coefs["(Intercept)", "coef"]))
                p_vals["Visiškai_nesutinku"] <- coefs["(Intercept)", "Pr(>|t|)"]
            }
            
            for (lvl in factor_levels) {
                col_name <- identify_category(lvl)
                if (!is.na(col_name) && lvl %in% rownames(coefs)) {
                    coef_vals[col_name] <- as.numeric(format_coefficient(coefs[lvl, "coef"]))
                    p_vals[col_name] <- coefs[lvl, "Pr(>|t|)"]
                }
            }
            list(coef = coef_vals, p = p_vals)
        }
        
        vals_no_escs <- extract_values(coefs_no_escs)
        vals_with_escs <- extract_values(coefs_with_escs)
        

        pval_normal_with <- tryCatch(nortest::cvm.test(model_with_escs$residuals)$p.value, error = function(e) NA)
        pval_normal_without <- tryCatch(nortest::cvm.test(model_no_escs$residuals)$p.value, error = function(e) NA)
        
        normal_with <- ifelse(!is.na(pval_normal_with) && pval_normal_with > 0.05, "Taip", "Ne")
        normal_without <- ifelse(!is.na(pval_normal_without) && pval_normal_without > 0.05, "Taip", "Ne")
        

        r2_no_escs <- model_no_escs$r.squared
        r2_with_escs <- model_with_escs$r.squared
        
        n_no_escs <- nrow(country_data)
        k_no_escs <- length(vars_no_escs)
        adj_r2_no_escs <- adjusted_r2_manual(r2_no_escs, n_no_escs, k_no_escs)
        
        n_with_escs <- nrow(country_data)
        k_with_escs <- length(vars_with_escs)
        adj_r2_with_escs <- adjusted_r2_manual(r2_with_escs, n_with_escs, k_with_escs)
        
        results <- rbind(
            results,
            data.frame(
                Šalis = country_name,
                Modelis = "Be SEK",
                `Visiškai nesutinku` = vals_no_escs$coef["Visiškai_nesutinku"],
                `Nesutinku` = vals_no_escs$coef["Nesutinku"],
                `Sutinku` = vals_no_escs$coef["Sutinku"],
                `Visiškai sutinku` = vals_no_escs$coef["Visiškai_sutinku"],
                R2 = format_number(adj_r2_no_escs),
                `Paklaidų normalumas` = normal_without,
                stringsAsFactors = FALSE,
                check.names = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                `Visiškai nesutinku` = format_p_value(vals_no_escs$p["Visiškai_nesutinku"]),
                `Nesutinku` = format_p_value(vals_no_escs$p["Nesutinku"]),
                `Sutinku` = format_p_value(vals_no_escs$p["Sutinku"]),
                `Visiškai sutinku` = format_p_value(vals_no_escs$p["Visiškai_sutinku"]),
                R2 = NA,
                `Paklaidų normalumas` = format_number(pval_normal_without),
                stringsAsFactors = FALSE,
                check.names = FALSE
            ),
            data.frame(
                Šalis = country_name,
                Modelis = "Su SEK",
                `Visiškai nesutinku` = vals_with_escs$coef["Visiškai_nesutinku"],
                `Nesutinku` = vals_with_escs$coef["Nesutinku"],
                `Sutinku` = vals_with_escs$coef["Sutinku"],
                `Visiškai sutinku` = vals_with_escs$coef["Visiškai_sutinku"],
                R2 = format_number(adj_r2_with_escs),
                `Paklaidų normalumas` = normal_with,
                stringsAsFactors = FALSE,
                check.names = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                `Visiškai nesutinku` = format_p_value(vals_with_escs$p["Visiškai_nesutinku"]),
                `Nesutinku` = format_p_value(vals_with_escs$p["Nesutinku"]),
                `Sutinku` = format_p_value(vals_with_escs$p["Sutinku"]),
                `Visiškai sutinku` = format_p_value(vals_with_escs$p["Visiškai_sutinku"]),
                R2 = NA,
                `Paklaidų normalumas` = format_number(pval_normal_with),
                stringsAsFactors = FALSE,
                check.names = FALSE
            )
        )
    }
    return(results)
}



questions_relatst <- c("st267q01ja", "st267q02ja", "st267q03ja", "st267q04ja", "st267q05ja", "st267q06ja", "st267q07ja", "st267q08ja")


results_relatst <- create_multiple_regression_tables(countries_data, questions_relatst)


results_relatst <- lapply(results_relatst, function(x) {
    colnames(x) <- gsub("\\.", " ", colnames(x))
    return(x)
})


#------------------------------------BELONG------------------------------------#

create_regression_summary_table_relatst <- function(countries_data, depended = "math", independed_with_escs = c("belong", "escs"), independed_without_escs = "belong", package = 2) {
    format_number <- function(x) {
        if (is.na(x)) return(NA)
        if (abs(x) < 0.001) return(formatC(x, format = "e", digits = 2))
        return(formatC(x, format = "f", digits = 3))
    }
    results <- data.frame(
        Šalis = character(),
        Modelis = character(),
        RELATST = character(),
        SEK = character(),
        R2 = character(),
        Paklaidų.normalumas = character(),
        stringsAsFactors = FALSE
    )
    for (country_name in names(data_by_country)) {
        country_data <- data_by_country[[country_name]]
        reg_with_escs <- tryCatch(line_regression(country_data, myvariables, depended, independed_with_escs, package), error = function(e) NULL)
        reg_without_escs <- tryCatch(line_regression(country_data, myvariables, depended, independed_without_escs, package), error = function(e) NULL)
        if (is.null(reg_with_escs) || is.null(reg_without_escs)) next
        coefs_with <- reg_with_escs$coefmat
        coefs_without <- reg_without_escs$coefmat
        if (nrow(coefs_with) == 0 || nrow(coefs_without) == 0) next
        R2_with <- summary(reg_with_escs)$r.squared
        R2_without <- summary(reg_without_escs)$r.squared
        residuals_with <- reg_with_escs$residuals
        residuals_without <- reg_without_escs$residuals
        pval_normal_with <- tryCatch(nortest::cvm.test(residuals_with)$p.value, error = function(e) NA)
        pval_normal_without <- tryCatch(nortest::cvm.test(residuals_without)$p.value, error = function(e) NA)
        normal_with <- ifelse(!is.na(pval_normal_with) && pval_normal_with > 0.05, "Taip", "Ne")
        normal_without <- ifelse(!is.na(pval_normal_without) && pval_normal_without > 0.05, "Taip", "Ne")
        country_results <- rbind(
            data.frame(
                Šalis = country_name,
                Modelis = "Be SEK",
                BELONG = format_number(coefs_without["belong", "coef"]),
                SEK = NA,
                R2 = format_number(R2_without),
                Paklaidų.normalumas = normal_without,
                stringsAsFactors = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                BELONG = format_number(coefs_without["belong", "Pr(>|t|)"]),
                SEK = NA,
                R2 = NA,
                Paklaidų.normalumas = format_number(pval_normal_without),
                stringsAsFactors = FALSE
            ),
            data.frame(
                Šalis = country_name,
                Modelis = "Su SEK",
                BELONG = format_number(coefs_with["belong", "coef"]),
                SEK = format_number(coefs_with["escs", "coef"]),
                R2 = format_number(R2_with),
                Paklaidų.normalumas = normal_with,
                stringsAsFactors = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                BELONG = format_number(coefs_with["belong", "Pr(>|t|)"]),
                SEK = format_number(coefs_with["escs", "Pr(>|t|)"]),
                R2 = NA,
                Paklaidų.normalumas = format_number(pval_normal_with),
                stringsAsFactors = FALSE
            )
        )
        results <- rbind(results, country_results)
    }
    return(results)
}

results_belong <- create_regression_summary_table_relatst(countries_data)

print(results_belong)


#------------------------------------BELONG KLAUSIMAI------------------------------------#


create_regression_summary_table <- function(countries_data, question, dependent_var = "math", package = 2) {
    format_number <- function(x) {
        if (is.na(x)) return(NA)
        if (abs(x) < 0.001) return(formatC(x, format = "e", digits = 3))
        return(formatC(x, format = "f", digits = 3))
    }
    format_coefficient <- function(x) {
        if (is.na(x)) return(NA)
        formatC(x, format = "f", digits = 3)
    }
    results <- data.frame()
    for (country_name in names(countries_data)) {
        country_data <- countries_data[[country_name]]
        vars_no_escs <- c(question)
        model_no_escs <- line_regression(country_data, myvariables, dependent_var, vars_no_escs, package)
        coefs_no_escs <- model_no_escs$coefmat
        vars_with_escs <- c(question, "escs")
        model_with_escs <- line_regression(country_data, myvariables, dependent_var, vars_with_escs, package)
        coefs_with_escs <- model_with_escs$coefmat
        identify_category <- function(lvl) {
            case_when(
                grepl("STRONGLY DISAGREE$", lvl, ignore.case = TRUE) ~ "Visiškai_nesutinku",
                grepl("DISAGREE$", lvl, ignore.case = TRUE) & !grepl("STRONGLY", lvl) ~ "Nesutinku",
                grepl("AGREE$", lvl, ignore.case = TRUE) & !grepl("STRONGLY", lvl) ~ "Sutinku",
                grepl("STRONGLY AGREE$", lvl, ignore.case = TRUE) ~ "Visiškai_sutinku",
                TRUE ~ NA_character_
            )
        }

        extract_values <- function(coefs) {
            factor_levels <- grep(paste0("^", question), rownames(coefs), value = TRUE)

            coef_vals <- setNames(rep(NA, 4), c("Visiškai_nesutinku", "Nesutinku", "Sutinku", "Visiškai_sutinku"))
            p_vals <- coef_vals

            if ("(Intercept)" %in% rownames(coefs)) {
                coef_vals["Visiškai_nesutinku"] <- as.numeric(format_coefficient(coefs["(Intercept)", "coef"]))
                p_vals["Visiškai_nesutinku"] <- coefs["(Intercept)", "Pr(>|t|)"]
            }

            for (lvl in factor_levels) {
                col_name <- identify_category(lvl)
                if (!is.na(col_name) && lvl %in% rownames(coefs)) {
                    coef_vals[col_name] <- as.numeric(format_coefficient(coefs[lvl, "coef"]))
                    p_vals[col_name] <- coefs[lvl, "Pr(>|t|)"]
                }
            }
            list(coef = coef_vals, p = p_vals)
        }
        vals_no_escs <- extract_values(coefs_no_escs)
        vals_with_escs <- extract_values(coefs_with_escs)
        pval_normal_with <- tryCatch(nortest::cvm.test(model_with_escs$residuals)$p.value, error = function(e) NA)
        pval_normal_without <- tryCatch(nortest::cvm.test(model_no_escs$residuals)$p.value, error = function(e) NA)
        normal_with <- ifelse(!is.na(pval_normal_with) && pval_normal_with > 0.05, "Taip", "Ne")
        normal_without <- ifelse(!is.na(pval_normal_without) && pval_normal_without > 0.05, "Taip", "Ne")
        r2_no_escs <- as.numeric(format_number(model_no_escs$r.squared))
        r2_with_escs <- as.numeric(format_number(model_with_escs$r.squared))
        results <- rbind(
            results,
            data.frame(
                Šalis = country_name,
                Modelis = "Be SEK",
                `Visiškai nesutinku` = vals_no_escs$coef["Visiškai_nesutinku"],
                `Nesutinku` = vals_no_escs$coef["Nesutinku"],
                `Sutinku` = vals_no_escs$coef["Sutinku"],
                `Visiškai sutinku` = vals_no_escs$coef["Visiškai_sutinku"],
                R2 = r2_no_escs,
                `Paklaidų normalumas` = normal_without,
                stringsAsFactors = FALSE,
                check.names = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                `Visiškai nesutinku` = format_p_value(vals_no_escs$p["Visiškai_nesutinku"]),
                `Nesutinku` = format_p_value(vals_no_escs$p["Nesutinku"]),
                `Sutinku` = format_p_value(vals_no_escs$p["Sutinku"]),
                `Visiškai sutinku` = format_p_value(vals_no_escs$p["Visiškai_sutinku"]),
                R2 = NA,
                `Paklaidų normalumas` = format_number(pval_normal_without),
                stringsAsFactors = FALSE,
                check.names = FALSE
            ),
            data.frame(
                Šalis = country_name,
                Modelis = "Su SEK",
                `Visiškai nesutinku` = vals_with_escs$coef["Visiškai_nesutinku"],
                `Nesutinku` = vals_with_escs$coef["Nesutinku"],
                `Sutinku` = vals_with_escs$coef["Sutinku"],
                `Visiškai sutinku` = vals_with_escs$coef["Visiškai_sutinku"],
                R2 = r2_with_escs,
                `Paklaidų normalumas` = normal_with,
                stringsAsFactors = FALSE,
                check.names = FALSE
            ),
            data.frame(
                Šalis = "",
                Modelis = "p-reikšmė",
                `Visiškai nesutinku` = format_p_value(vals_with_escs$p["Visiškai_nesutinku"]),
                `Nesutinku` = format_p_value(vals_with_escs$p["Nesutinku"]),
                `Sutinku` = format_p_value(vals_with_escs$p["Sutinku"]),
                `Visiškai sutinku` = format_p_value(vals_with_escs$p["Visiškai_sutinku"]),
                R2 = NA,
                `Paklaidų normalumas` = format_number(pval_normal_with),
                stringsAsFactors = FALSE,
                check.names = FALSE
            )
        )
    }
    return(results)
}



questions_belong <- c("st034q01ta", "st034q02ta", "st034q03ta", "st034q04ta", "st034q05ta", "st034q06ta")

countries_data <- list(
    "Lietuva" = ltu_data,
    "Latvija" = lva_data,
    "Estija" = est_data,
    "Lenkija" = pol_data
)

reorder_question_levels <- function(data, questions) {
    for (q in questions) {
        if (q %in% names(data)) {
            data[[q]] <- factor(data[[q]],
                                levels = c("STRONGLY DISAGREE", "DISAGREE", "AGREE", "STRONGLY AGREE"))
        }
    }
    return(data)
}

countries_data <- lapply(countries_data, reorder_question_levels, questions = questions_belong)

results_belong <- create_multiple_regression_tables(countries_data, questions_belong)

results_belong <- lapply(results_belong, function(x) {
    colnames(x) <- gsub("\\.", " ", colnames(x))
    return(x)
})

print(results_belong)

#==================================================================
# Atsakiusiųjų ir neatsakiusiųjų rezultatų pasiskirstymo analizė
#==================================================================

#------------------------------------RELATST KLAUSIMAI------------------------------------#

analyze_country <- function(country_data) {
    library(ggplot2)
    library(patchwork)
    library(dplyr)
    library(grid)
    library(purrr)
    questions <- c("st267q01ja", "st267q02ja", "st267q03ja", "st267q04ja", "st267q05ja", "st267q06ja", "st267q07ja", "st267q08ja")

    pv_math_cols <- grep("^pv[0-9]+math$", names(country_data), value = TRUE)

    for (q in questions) {
        country_data[[paste0(q, "_answered")]] <- ifelse(
            country_data[[q]] %in% c("AGREE", "STRONGLY AGREE", "DISAGREE", "STRONGLY DISAGREE"),
            "Atsakyta", "Neatsakyta"
        )
    }

    all_pv_values <- unlist(country_data[, pv_math_cols])
    breaks_global <- pretty(all_pv_values, n = 25)
    x_limits <- range(breaks_global)
    
    calculate_avg_histograms <- function(q) {
        histograms <- map(pv_math_cols, function(pv) {

            answered_rows <- country_data[[paste0(q, "_answered")]] == "Atsakyta"
            answered <- country_data[answered_rows, pv]
            weights_answered <- country_data[answered_rows, "w_fstuwt"]

            not_answered_rows <- country_data[[paste0(q, "_answered")]] == "Neatsakyta"
            not_answered <- country_data[not_answered_rows, pv]
            weights_not_answered <- country_data[not_answered_rows, "w_fstuwt"]

            h_answered <- hist(answered, breaks = breaks_global, weights = weights_answered, plot = FALSE)
            h_not_answered <- hist(not_answered, breaks = breaks_global, weights = weights_not_answered, plot = FALSE)
            list(
                answered = h_answered$counts,
                not_answered = h_not_answered$counts,
                breaks = h_answered$breaks
            )
        })
        avg_counts_answered <- reduce(map(histograms, "answered"), `+`) / length(pv_math_cols)
        avg_counts_not_answered <- reduce(map(histograms, "not_answered"), `+`) / length(pv_math_cols)
        list(
            answered = avg_counts_answered,
            not_answered = avg_counts_not_answered,
            breaks = histograms[[1]]$breaks
        )
    }
    
    

    max_counts <- map_dbl(questions, function(q) {
        hists <- calculate_avg_histograms(q)
        max(hists$answered, hists$not_answered)
    })
    max_count <- max(max_counts, na.rm = TRUE)

    question_labels <- setNames(questions, questions)

    plot_pairs <- map(questions, function(q) {
        hists <- calculate_avg_histograms(q)

        plot_data <- data.frame(
            x = head(hists$breaks, -1) + diff(hists$breaks)/2,
            answered = hists$answered,
            not_answered = hists$not_answered
        )

        plot_data$width <- diff(hists$breaks)
        

        p_answered <- ggplot(plot_data) +
            geom_rect(aes(xmin = x - width/2, xmax = x + width/2, ymin = 0, ymax = answered),
                      fill = "#3498db", color = "white", alpha = 0.8) +
            labs(x = "Matematikos rezultatai", y = "Mokinių skaičius", subtitle = "Atsakyta") +
            theme_minimal(base_size = 14) +
            theme(
                plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 14),
                axis.title = element_text(size = 13),
                panel.grid.minor = element_blank(),
                plot.margin = margin(5, 5, 5, 5)
            ) +
            scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 3)) +
            scale_y_continuous(limits = c(0, max_count * 1.1), expand = c(0, 0))
        

        p_not_answered <- ggplot(plot_data) +
            geom_rect(aes(xmin = x - width/2, xmax = x + width/2, ymin = 0, ymax = not_answered),
                      fill = "#e67e22", color = "white", alpha = 0.8) +
            labs(x = "Matematikos rezultatai", y = "", subtitle = "Neatsakyta") +
            theme_minimal(base_size = 14) +
            theme(
                plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 14),
                axis.title = element_text(size = 13),
                panel.grid.minor = element_blank(),
                plot.margin = margin(5, 5, 5, 5)
            ) +
            scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 3)) +
            scale_y_continuous(limits = c(0, max_count * 1.1), expand = c(0, 0))
        

        wrap_elements(full =
                          patchwork::plot_spacer() /
                          grid::textGrob(question_labels[q], gp = grid::gpar(fontsize = 15, fontface = "bold")) /
                          (p_answered | p_not_answered) +
                          plot_layout(heights = c(0.1, 0.1, 1)))
    })

    panel <- wrap_plots(plot_pairs, ncol = 2) & 
        theme(plot.margin = margin(10, 10, 20, 10))

    ks_results <- map_dfr(questions, function(q) {
        p_values <- map_dbl(pv_math_cols, function(pv) {
            answered <- country_data[country_data[[paste0(q, "_answered")]] == "Atsakyta", pv]
            not_answered <- country_data[country_data[[paste0(q, "_answered")]] == "Neatsakyta", pv]
            if (length(answered) > 0 && length(not_answered) > 0) {
                ks.test(answered, not_answered)$p.value
            } else {
                NA
            }
        })
        data.frame(
            question = q,
            p_value = mean(p_values, na.rm = TRUE)
        )
    })

    ks_results$p_value <- sapply(ks_results$p_value, function(p) {
        if (is.na(p)) {
            return(NA)
        } else if (p >= 0.001) {
            return(formatC(p, format = "f", digits = 3))
        } else {
            return(formatC(p, format = "e", digits = 2))
        }
    })
    

    ks_results$`Vienodi skirstiniai` <- sapply(ks_results$`p_value`, function(p) {
        if (is.na(p)) {
            return(NA)
        } else if (as.numeric(p) >= 0.05) {
            return("Taip")
        } else {
            return("Ne")
        }
    })
    

    colnames(ks_results) <- c("Klausimas", "p-reikšmė", "Vienodi skirstiniai")
    return(list(
        plot = panel,
        ks_results = ks_results
    ))
    
}


ltu_result <- analyze_country(ltu_data)
print(ltu_result$plot)
print(ltu_result$ks_results)

lva_result <- analyze_country(lva_data)
print(lva_result$plot)
print(lva_result$ks_results)

est_result <- analyze_country(est_data)
print(est_result$plot)
print(est_result$ks_results)

pol_result <- analyze_country(pol_data)
print(pol_result$plot)
print(pol_result$ks_results)

#------------------------------------BELONG KLAUSIMAI------------------------------------#



analyze_country <- function(country_data) {
    library(ggplot2)
    library(patchwork)
    library(dplyr)
    library(grid)
    library(purrr)
    
    questions <- c("st034q01ta", "st034q02ta", "st034q03ta", "st034q04ta", "st034q05ta", "st034q06ta")

    pv_math_cols <- grep("^pv[0-9]+math$", names(country_data), value = TRUE)
    for (q in questions) {
        country_data[[paste0(q, "_answered")]] <- ifelse(
            country_data[[q]] %in% c("AGREE", "STRONGLY AGREE", "DISAGREE", "STRONGLY DISAGREE"),
            "Atsakyta", "Neatsakyta"
        )
    }

    all_pv_values <- unlist(country_data[, pv_math_cols])
    breaks_global <- pretty(all_pv_values, n = 25)
    x_limits <- range(breaks_global)
    calculate_avg_histograms <- function(q) {
        histograms <- map(pv_math_cols, function(pv) {
            # Atsakiusių mokinių duomenys
            answered_rows <- country_data[[paste0(q, "_answered")]] == "Atsakyta"
            answered <- country_data[answered_rows, pv]
            weights_answered <- country_data[answered_rows, "w_fstuwt"]
            # Neatsakiusių mokinių duomenys
            not_answered_rows <- country_data[[paste0(q, "_answered")]] == "Neatsakyta"
            not_answered <- country_data[not_answered_rows, pv]
            weights_not_answered <- country_data[not_answered_rows, "w_fstuwt"]
            # Svertinės histogramų skaičiavimas
            h_answered <- hist(answered, breaks = breaks_global, weights = weights_answered, plot = FALSE)
            h_not_answered <- hist(not_answered, breaks = breaks_global, weights = weights_not_answered, plot = FALSE)
            list(
                answered = h_answered$counts,
                not_answered = h_not_answered$counts,
                breaks = h_answered$breaks
            )
        })
        avg_counts_answered <- reduce(map(histograms, "answered"), `+`) / length(pv_math_cols)
        avg_counts_not_answered <- reduce(map(histograms, "not_answered"), `+`) / length(pv_math_cols)
        list(
            answered = avg_counts_answered,
            not_answered = avg_counts_not_answered,
            breaks = histograms[[1]]$breaks
        )
    }
    
    max_counts <- map_dbl(questions, function(q) {
        hists <- calculate_avg_histograms(q)
        max(hists$answered, hists$not_answered)
    })
    max_count <- max(max_counts, na.rm = TRUE)
    question_labels <- setNames(questions, questions)
    plot_pairs <- map(questions, function(q) {
        hists <- calculate_avg_histograms(q)
        plot_data <- data.frame(
            x = head(hists$breaks, -1) + diff(hists$breaks)/2,
            answered = hists$answered,
            not_answered = hists$not_answered
        )
        plot_data$width <- diff(hists$breaks)
        p_answered <- ggplot(plot_data) +
            geom_rect(aes(xmin = x - width/2, xmax = x + width/2, ymin = 0, ymax = answered),
                      fill = "#3498db", color = "white", alpha = 0.8) +
            labs(x = "Matematikos rezultatai", y = "Mokinių skaičius", subtitle = "Atsakyta") +
            theme_minimal(base_size = 14) +
            theme(
                plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 14),
                axis.title = element_text(size = 13),
                panel.grid.minor = element_blank(),
                plot.margin = margin(5, 5, 5, 5)
            ) +
            scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 3)) +
            scale_y_continuous(limits = c(0, max_count * 1.1), expand = c(0, 0))
        p_not_answered <- ggplot(plot_data) +
            geom_rect(aes(xmin = x - width/2, xmax = x + width/2, ymin = 0, ymax = not_answered),
                      fill = "#e67e22", color = "white", alpha = 0.8) +
            labs(x = "Matematikos rezultatai", y = "", subtitle = "Neatsakyta") +
            theme_minimal(base_size = 14) +
            theme(
                plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 14),
                axis.title = element_text(size = 13),
                panel.grid.minor = element_blank(),
                plot.margin = margin(5, 5, 5, 5)
            ) +
            scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 3)) +
            scale_y_continuous(limits = c(0, max_count * 1.1), expand = c(0, 0))
        wrap_elements(full =
                          patchwork::plot_spacer() /
                          grid::textGrob(question_labels[q], gp = grid::gpar(fontsize = 16, fontface = "bold")) /
                          (p_answered | p_not_answered) +
                          plot_layout(heights = c(0.1, 0.1, 1)))
    })
    panel <- wrap_plots(plot_pairs, ncol = 2) & 
        theme(plot.margin = margin(10, 10, 20, 10))
    ks_results <- map_dfr(questions, function(q) {
        p_values <- map_dbl(pv_math_cols, function(pv) {
            answered <- country_data[country_data[[paste0(q, "_answered")]] == "Atsakyta", pv]
            not_answered <- country_data[country_data[[paste0(q, "_answered")]] == "Neatsakyta", pv]
            if (length(answered) > 0 && length(not_answered) > 0) {
                ks.test(answered, not_answered)$p.value
            } else {
                NA
            }
        })
        data.frame(
            question = q,
            p_value = mean(p_values, na.rm = TRUE)
        )
    })
    ks_results$p_value <- sapply(ks_results$p_value, function(p) {
        if (is.na(p)) {
            return(NA)
        } else if (p >= 0.001) {
            return(formatC(p, format = "f", digits = 3))
        } else {
            return(formatC(p, format = "e", digits = 2))
        }
    })
    ks_results$`Vienodi skirstiniai` <- sapply(ks_results$`p_value`, function(p) {
        if (is.na(p)) {
            return(NA)
        } else if (as.numeric(p) >= 0.05) {
            return("Taip")
        } else {
            return("Ne")
        }
    })
    colnames(ks_results) <- c("Klausimas", "p-reikšmė", "Skirstiniai vienodi")
    return(list(
        plot = panel,
        ks_results = ks_results
    ))
}


ltu_result <- analyze_country(ltu_data)
print(ltu_result$plot)
ks_ltu <- (ltu_result$ks_results)

lva_result <- analyze_country(lva_data)
print(lva_result$plot)
print(lva_result$ks_results)

est_result <- analyze_country(est_data)
print(est_result$plot)
print(est_result$ks_results)

pol_result <- analyze_country(pol_data)
print(pol_result$plot)
print(pol_result$ks_results)


#==================================================================
# Atsakiusiųjų ir neatsakiusiųjų statistikų lenteles
#==================================================================

#------------------------------------BELONG------------------------------------#


round_numeric_columns <- function(df, digits = 3) {
    df[] <- lapply(df, function(x) {
        if (is.numeric(x)) round(x, digits = digits) else x
    })
    return(df)
}

add_sheet_with_format <- function(wb, sheet_name, df) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df)
    numeric_cols <- which(sapply(df, is.numeric))
    for (col in numeric_cols) {
        addStyle(wb, sheet = sheet_name,
                 style = createStyle(numFmt = "0.000"),
                 cols = col, rows = 2:(nrow(df) + 1),
                 gridExpand = TRUE)
    }
}

process_country_data <- function(data, country_name) {
    questions <- c("st034q01ta", "st034q02ta", "st034q03ta", "st034q04ta", "st034q05ta", "st034q06ta")
    atsakyta_list <- list()
    neatsakyta_list <- list()
    for (q in questions) {

        data$answered <- ifelse(data[[q]] %in% c("AGREE", "STRONGLY AGREE", "DISAGREE", "STRONGLY DISAGREE"),
                                "Atsakyta", "Neatsakyta")

        answered_count <- sum(data$answered == "Atsakyta", na.rm = TRUE)
        percent_answered <- (answered_count / nrow(data)) * 100

        answered_data <- data[data$answered == "Atsakyta", ]
        tbl_ans <- frequency_table(answered_data, "math")
        sum_ans <- summary(tbl_ans)[[1]]
        iqr_ans <- sum_ans$`3rd Qu.` - sum_ans$`1st Qu.`
        row_ans <- data.frame(
            `Klausimo indeksas` = q,
            Vidurkis = sum_ans$Mean,
            Mediana = sum_ans$Median,
            SN = sum_ans$SD,
            IQR = iqr_ans,
            Procentas = round(percent_answered, 3)
        )
        atsakyta_list[[length(atsakyta_list) + 1]] <- row_ans
        # Neatsakiusiųjų grupė
        not_answered_data <- data[data$answered == "Neatsakyta", ]
        tbl_not <- frequency_table(not_answered_data, "math")
        sum_not <- summary(tbl_not)[[1]]
        iqr_not <- sum_not$`3rd Qu.` - sum_not$`1st Qu.`
        row_not <- data.frame(
            `Klausimo indeksas` = q,
            Vidurkis = sum_not$Mean,
            Mediana = sum_not$Median,
            SN = sum_not$SD,
            IQR = iqr_not,
            Procentas = round(100 - percent_answered, 3)
        )
        neatsakyta_list[[length(neatsakyta_list) + 1]] <- row_not
    }

    atsakyta_table <- do.call(rbind, atsakyta_list)
    neatsakyta_table <- do.call(rbind, neatsakyta_list)
    rownames(atsakyta_table) <- NULL
    rownames(neatsakyta_table) <- NULL
    atsakyta_table_rounded <- round_numeric_columns(atsakyta_table, 3)
    neatsakyta_table_rounded <- round_numeric_columns(neatsakyta_table, 3)
    
    return(list(Atsakyta = atsakyta_table_rounded, Neatsakyta = neatsakyta_table_rounded))
}


results_ltu <- process_country_data(ltu_data, "ltu")
results_ltu
results_lva <- process_country_data(lva_data, "lva")
results_lva
results_est <- process_country_data(est_data, "est")
results_est
results_pol <- process_country_data(pol_data, "pol")
results_pol


library(openxlsx)
wb <- createWorkbook()

countries_results <- list(
    LTU = results_ltu,
    LVA = results_lva,
    EST = results_est,
    POL = results_pol
)


for (country in names(countries_results)) {
    atsakyta_df <- countries_results[[country]]$Atsakyta
    neatsakyta_df <- countries_results[[country]]$Neatsakyta
    

    sheet_ans <- paste0(country, "_Atsakyta")
    sheet_not <- paste0(country, "_Neatsakyta")
    
    add_sheet_with_format(wb, sheet_ans, atsakyta_df)
    add_sheet_with_format(wb, sheet_not, neatsakyta_df)
}


saveWorkbook(wb, file = "st034_saliu_duomenys.xlsx", overwrite = TRUE)

#------------------------------------RELATST------------------------------------#


round_numeric_columns <- function(df, digits = 3) {
    df[] <- lapply(df, function(x) {
        if (is.numeric(x)) round(x, digits = digits) else x
    })
    return(df)
}


add_sheet_with_format <- function(wb, sheet_name, df) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df)
    
    numeric_cols <- which(sapply(df, is.numeric))
    for (col in numeric_cols) {
        addStyle(wb, sheet = sheet_name,
                 style = createStyle(numFmt = "0.000"),
                 cols = col, rows = 2:(nrow(df) + 1),
                 gridExpand = TRUE)
    }
}


process_country_data <- function(data, country_name) {
    questions <- c("st267q01ja", "st267q02ja", "st267q03ja", "st267q04ja",
                   "st267q05ja", "st267q06ja", "st267q07ja", "st267q08ja")
    
    atsakyta_list <- list()
    neatsakyta_list <- list()
    
    for (q in questions) {

        data$answered <- ifelse(data[[q]] %in% c("AGREE", "STRONGLY AGREE", "DISAGREE", "STRONGLY DISAGREE"),
                                "Atsakyta", "Neatsakyta")
        

        answered_count <- sum(data$answered == "Atsakyta", na.rm = TRUE)
        percent_answered <- (answered_count / nrow(data)) * 100
        

        answered_data <- data[data$answered == "Atsakyta", ]
        tbl_ans <- frequency_table(answered_data, "math")
        sum_ans <- summary(tbl_ans)[[1]]
        iqr_ans <- sum_ans$`3rd Qu.` - sum_ans$`1st Qu.`
        row_ans <- data.frame(
            `Klausimo indeksas` = q,
            Vidurkis = sum_ans$Mean,
            Mediana = sum_ans$Median,
            SN = sum_ans$SD,
            IQR = iqr_ans,
            Procentas = round(percent_answered, 3)
        )
        atsakyta_list[[length(atsakyta_list) + 1]] <- row_ans

        not_answered_data <- data[data$answered == "Neatsakyta", ]
        tbl_not <- frequency_table(not_answered_data, "math")
        sum_not <- summary(tbl_not)[[1]]
        iqr_not <- sum_not$`3rd Qu.` - sum_not$`1st Qu.`
        row_not <- data.frame(
            `Klausimo indeksas` = q,
            Vidurkis = sum_not$Mean,
            Mediana = sum_not$Median,
            SN = sum_not$SD,
            IQR = iqr_not,
            Procentas = round(100 - percent_answered, 3)
        )
        neatsakyta_list[[length(neatsakyta_list) + 1]] <- row_not
    }
    

    atsakyta_table <- do.call(rbind, atsakyta_list)
    neatsakyta_table <- do.call(rbind, neatsakyta_list)
    
    rownames(atsakyta_table) <- NULL
    rownames(neatsakyta_table) <- NULL
    
    atsakyta_table_rounded <- round_numeric_columns(atsakyta_table, 3)
    neatsakyta_table_rounded <- round_numeric_columns(neatsakyta_table, 3)
    
    
    return(list(Atsakyta = atsakyta_table_rounded, Neatsakyta = neatsakyta_table_rounded))
}


results_ltu <- process_country_data(ltu_data, "ltu")
results_ltu
results_est <- process_country_data(est_data, "est")
results_est
results_pol <- process_country_data(pol_data, "pol")
results_pol
results_lva <- process_country_data(lva_data, "lva")
results_lva


