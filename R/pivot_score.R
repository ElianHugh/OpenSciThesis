# Pivots the journal policy dataframe
# to allow for subsequent graphing
pivot_score <- function(df) {
    graphdf <- df %>%
        dplyr::filter(Top10Perc == TRUE) %>%
        dplyr::filter(GroupedDisc != "Other") %>%
        pivot_longer(
            cols = c(
                DataCitation,
                DataTransparency,
                AnalysisTransparency,
                MaterialsTransparency,
                DesignAnalysis,
                Preregistration,
                AnalysisPreReg,
                Replication,
                RegRepPubBias,
                Badges,
                Submitted,
                Accepted,
                Published
            ),
            names_to = "Open Science Practice",
            values_to = "value"
        ) %>%
        ungroup() %>%
        group_by(`Open Science Practice`, GroupedDisc) %>%
        dplyr::filter(!is.na(value)) %>%
        summarise(
            p.est = sum(value / length(value)),
            variance = (p.est * (1 - p.est)) / length(value),
            std.dev = sqrt(variance),
            discN = paste0(GroupedDisc, ", n = ", n)
        ) %>%
        distinct()

    graphdf$`Open Science Practice` <- factor(graphdf$`Open Science Practice`,
        levels = c(
            "DataCitation",
            "DataTransparency",
            "AnalysisTransparency",
            "MaterialsTransparency",
            "DesignAnalysis",
            "Preregistration",
            "AnalysisPreReg",
            "Replication",
            "RegRepPubBias",
            "Badges",
            "Submitted",
            "Accepted",
            "Published"
        )
    )
    return(graphdf)
}