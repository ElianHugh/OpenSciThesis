aggregate_score <- function(df) {


    # TODO either include open access in the calculated score, or explain why not

    df$OSS <- df %>%
        select(DataCitation:Badges, Submitted, Accepted, Published) %>%
        rowSums(na.rm = TRUE)

    df %<>% mutate(
        ScoreGrade = quantcut(df$OSS, q = 5)
    )


    levels(df$ScoreGrade) <- c(
        "None", "Low",
        "Medium", "High", "Very High"
    )

    df %<>%
        distinct(Title, .keep_all = TRUE)
    return(df)
}