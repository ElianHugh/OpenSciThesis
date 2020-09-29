fetch_sherpa <- function(combinedCite, key){

# * TODO Distinguish between types of OA better, e.g. OA fees vs none

##############################
#  Helper Functions          #
##############################

#' Create new progress bar
#' Shortcut for creating a new progress bar
#' @param count. The number of iterations that the progress bar goes for
#' @usage new_bar(count)
#' @examples
#' count <- 5
#' new_bar(count)
new_bar <- function(count) {
    pb <- txtProgressBar(
        min = 0,
        max = count,
        style = 3
    )
    pb
}

#' Fetch a nested JSON from Sherpa
#' MUST OPEN AND CLOSE CORE CLUSTERS PRIOR TO AND FOLLOWING THIS FUNCTION
#' @param opts.
#' @param count.
#' @param nextJournals.
#' @param key.
#' @param is_issn
#' @examples
#' out <- fetch_json(opts, count, nextJournals, key)
fetch_json <- function(opts, count, pb, nextJournals, key, is_issn = FALSE, is_publisher = FALSE) {
    if (is_issn == TRUE) {
        urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=%s&format=Json&filter=[["issn","equals","%s"]]'
    } else if (is_publisher == TRUE) {
        urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publisher&api-key=%s&format=Json&filter=[["name","equals","%s"]]'
    } else {
        urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=%s&format=Json&filter=[["title","equals","%s"]]'
    }

    temp <- foreach(i = 1:count, .options.snow = opts, .errorhandling = "remove") %dopar% {
        journalArg <- nextJournals[i, ]
        jsonURL <-
            sprintf(
                urlArg,
                key,
                toString(journalArg)
            )
        jsonURL <- URLencode(jsonURL)
        loadedJSON <- jsonlite::fromJSON(jsonURL)

        if (length(loadedJSON[["items"]]) > 0) {
            loadedJSON
        } else {
            next
        }
    }

    temp
}

explore_json <- function(API) {
    x <- API %>%
        flatten() %>%
        list.filter(!is.null(title)) %>%
        list.select(
            Titles = flatten(title),
            ISSNS = flatten(issns),
            PublisherPolicy = flatten(publisher_policy)
        ) %>%
        list.filter(!(is.null(PublisherPolicy$permitted_oa))) %>%
        list.select(
            Title = Titles$title,
            ISSN = ISSNS$issn,
            PermittedOA = flatten(PublisherPolicy$permitted_oa)
        ) %>%
        list.select(Title,
            ISSN,
            Submitted = "submitted" %in% PermittedOA$article_version,
            Accepted = "accepted" %in% PermittedOA$article_version,
            Published = "published" %in% PermittedOA$article_version
        ) %>%
        list.rbind() %>%
        as_tibble()

    y <- API %>%
        flatten() %>%
        list.filter(!is.null(title)) %>%
        list.select(
            Titles = flatten(title),
            ISSNS = flatten(issns),
            PublisherPolicy = flatten(publisher_policy)
        ) %>%
        list.filter((is.null(PublisherPolicy$permitted_oa))) %>%
        list.select(
            Title = Titles$title,
            ISSN = ISSNS$issn,
            Submitted = FALSE,
            Accepted = FALSE,
            Published = FALSE
        ) %>%
        list.rbind() %>%
        as_tibble()

    if (nrow(x) > 0 && nrow(y) > 0) {
        z <- full_join(x, y) %>%
            unnest_longer(Title) %>%
            unnest_longer(ISSN) %>%
            unnest_longer(Submitted) %>%
            unnest_longer(Accepted) %>%
            unnest_longer(Published)
    } else if (!nrow(x) > 0) {
        z <- y %>%
            unnest_longer(Title) %>%
            unnest_longer(ISSN) %>%
            unnest_longer(Submitted) %>%
            unnest_longer(Accepted) %>%
            unnest_longer(Published)
    } else {
        z <- x %>%
            unnest_longer(Title) %>%
            unnest_longer(ISSN) %>%
            unnest_longer(Submitted) %>%
            unnest_longer(Accepted) %>%
            unnest_longer(Published)
    }
    return(z)
}


##############################
#  Data Setup                #
##############################

journals <- combinedCite %>%
    ungroup()
journals$ISSN <- gsub("[[:space:]]", "", journals$ISSN)
journals$ISSN <- gsub("^(.{4})(.*)$", "\\1-\\2", journals$ISSN)


nextJournals <- journals %>%
    select(Title) %>%
    distinct()

# * Here we set up the variables and objects required for parallel processing the data
cores <- detectCores()
message(c(("\n* SETUP: computer cores used: "), cores, " *\n"))
cat("\nInitial journals : ", as.character((nrow(nextJournals)), "\n"))
opts <- list(progress = (function(n) setTxtProgressBar(pb, n)))

##############################
#  Parse 1                   #
##############################

message("\n* PARSE 1 OF 5 * Finding journals by title\n")

# * Initiate API fetching
count <- nrow(journals)
pb <- new_bar(count)
cl <- snow::makeCluster(cores, type = "SOCK")
registerDoSNOW(cl)
API <- fetch_json(opts, count, pb, nextJournals, key)
close(pb)

if (length(API) > 0) {
    parse1 <- explore_json(API)
} else {
    message("\nDid not find any journals at parse 1.")
}

##############################
#  Prep                      #
##############################

if (exists("parse1")) {
    foundJournals <- parse1 %>%
        distinct()

    leftoverJournals <- anti_join(combinedCite,
        foundJournals,
        by = "Title"
    )

    nextJournals <- leftoverJournals %>%
        ungroup() %>%
        select(MatchTitle) %>%
        distinct()

    cat("\nJournals left : ", as.character((nrow(nextJournals))))
}

##############################
#  Parse 2                   #
##############################

message("\n* PARSE 2 OF 5 * Finding journals by matchtitle\n")

# * Initiate API fetching
count <- nrow(nextJournals)
pb <- new_bar(count)
registerDoSNOW(cl)
API <- fetch_json(opts, count, pb, nextJournals, key)
close(pb)

if (length(API) > 0) {
    parse2 <- explore_json(API)
} else {
    message("\nDid not find any journals at parse 2.")
}

##############################
#  Prep                      #
##############################

if (exists("parse2")) {
    foundJournals <- parse2 %>%
        select(Title) %>%
        rename(MatchTitle = Title) %>%
        distinct()

    leftoverJournals <- anti_join(leftoverJournals,
        foundJournals,
        by = "MatchTitle"
    )

    nextJournals <- leftoverJournals %>%
        ungroup() %>%
        select(ISSN) %>%
        distinct()

    nextJournals$ISSN <- gsub("[[:space:]]", "", nextJournals$ISSN)
    nextJournals$ISSN <- gsub("^(.{4})(.*)$", "\\1-\\2", nextJournals$ISSN)
    cat("\nISSNs left : ", as.character((nrow(nextJournals))))
}

##############################
#  Parse 3                   #
##############################

message("\n* PARSE 3 OF 5 * Finding journals by ISSN\n")

# * Initiate API fetching
count <- nrow(nextJournals)
pb <- new_bar(count)
registerDoSNOW(cl)
API <- fetch_json(opts, count, pb, nextJournals, key, is_issn = TRUE)
close(pb)

if (length(API) > 0) {
    parse3 <- explore_json(API)
} else {
    message("\nDid not find any journals at parse 3.")
}

##############################
#  Prep                      #
##############################

if (exists("parse3")) {
    foundJournals <- parse3 %>%
        distinct()

    leftoverJournals <- anti_join(leftoverJournals, foundJournals, by = "Title")

    nextJournals <- leftoverJournals %>%
        ungroup() %>%
        select(Title) %>%
        distinct()
}

nextJournals$Title <- str_replace(nextJournals$Title, " and ", " & ")

##############################
#  Parse 4                   #
##############################

message("\n* PARSE 4 OF 5 * Finding journals by modified title\n")

# * Initiate API fetching
count <- nrow(nextJournals)
pb <- new_bar(count)
registerDoSNOW(cl)
API <- fetch_json(opts, count, pb, nextJournals, key)
close(pb)
count <- length(API)

if (length(API) > 0) {
    parse4 <- explore_json(API)
} else {
    message("\nDid not find any journals at parse 4.")
}

##############################
#  Prep                      #
##############################

if (exists("parse4")) {
    foundJournals <- parse4 %>%
        distinct()

    leftoverJournals <- anti_join(combinedCite,
        foundJournals,
        by = "Title"
    )

    nextJournals <- leftoverJournals %>%
        ungroup() %>%
        select(MatchTitle) %>%
        distinct()

    cat("\nJournals left : ", as.character((nrow(nextJournals))))
}

nextPublishers <- leftoverJournals %>%
    ungroup() %>%
    select(Publisher) %>%
    distinct()

##############################
#  Parse 5                   #
##############################

message("\n* PARSE 5 OF 6 * Finding journals through publisher matching \n")

# * Initiate API fetching
count <- nrow(nextPublishers)
pb <- new_bar(count)
registerDoSNOW(cl)
API <- fetch_json(opts, count, pb, nextPublishers, key, is_publisher = TRUE)
close(pb)
count <- length(API)

# * Prep for explore
if (count > 0) {
    explore_publisher <- function(API, i) {
        temp <- purrr::flatten(API)
        if ("publications" %in% names(temp)) {
            temp %>%
                tibble::as_tibble(.name_repair = "minimal") %>%
                select(publications) %>%
                unnest(cols = (publications)) %>%
                select(title) %>%
                unnest(cols = (title)) %>%
                select(title)
        }
        else {
            temp <- NULL
        }
    }
}

pl <- map2(API, seq_along(API), .f = explore_publisher)
pl <- unlist(pl, use.names = FALSE)
cat("\n Journals found: ", length(pl))

tempdf <- as.data.frame(pl) %>%
    rename(Title = pl)
tempdf <- anti_join(tempdf, combinedCite)


if (exists("parse1")) tempdf <- anti_join(tempdf, parse1, by = "Title")
if (exists("parse2")) tempdf <- anti_join(tempdf, parse2, by = "Title")
if (exists("parse3")) tempdf <- anti_join(tempdf, parse3, by = "Title")
if (exists("parse4")) tempdf <- anti_join(tempdf, parse4, by = "ISSN")


# ! Look for matches of title

fuzzydf <- fuzzyjoin::stringdist_left_join(nextJournals, tempdf, by = "Title", distance_col = "Distance", max_dist = 10)

listA <- fuzzydf$Title.x[!is.na(fuzzydf$Title.x)]
listB <- fuzzydf$Title.y[!is.na(fuzzydf$Title.y)]

matchJournals <- sapply(listA, function(y) sapply(listB, function(x) grepl(y, x)))

matchJournals %<>%
    as.data.frame() %>%
    rownames_to_column("id") %>%
    tidyr::gather(key = "key", value = "value", -id) %>%
    dplyr::filter(value == TRUE)


# ! There is definitely a better way to do this but...
matchJournals$id <- str_replace_all(matchJournals$id, "[^[:alnum:]]", " ")
matchJournals$id <- trimws(matchJournals$id, which = "both")
matchJournals$key <- str_replace_all(matchJournals$key, "[^[:alnum:]]", " ")
matchJournals$key <- str_replace_all(matchJournals$key, "[0-9]+", " ")
matchJournals$key <- trimws(matchJournals$key, which = "both")


matchJournals %<>%
    distinct()

cat("\nJournals matched: ", nrow(matchJournals))

nextJournals <- matchJournals %>%
    select(id)

##############################
# Parse 6                    #
##############################

message("\n* PARSE 6 OF 6 * Matching journals \n")

# * Initiate API fetching
count <- nrow(nextJournals)
pb <- new_bar(count)
registerDoSNOW(cl)
API <- fetch_json(opts, count, pb, nextJournals, key)
close(pb)

if (length(API) > 0) {
    parse6 <- explore_json(API)
}

##############################
# Synthesis                  #
##############################

combinedPolicies <- parse1
if (exists("parse2")) {
    combinedPolicies <- add_case(combinedPolicies, parse2)
}
if (exists("parse3")) {
    combinedPolicies <- add_case(combinedPolicies, parse3)
}
if (exists("parse4")) {
    combinedPolicies <- add_case(combinedPolicies, parse4)
}
if (exists("parse6")) {
    combinedPolicies <- add_case(combinedPolicies, parse6)
}

combinedPolicies$ISSN <- gsub("-", "", combinedPolicies$ISSN)

##############################
#  Cleanup                   #
##############################

snow::stopCluster(cl)
registerDoSEQ()

return(combinedPolicies)

}
