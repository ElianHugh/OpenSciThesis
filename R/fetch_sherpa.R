fetch_sherpa <- function(combinedCite, key){

# ! Take in topFactor ISSNs and check them
# ! change omit to omit only issns
journals <- combinedCite %>%
    ungroup()
journals$ISSN <- gsub("[[:space:]]", "", journals$ISSN)
journals$ISSN <- gsub("^(.{4})(.*)$", "\\1-\\2", journals$ISSN)
count <- nrow(journals)

urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=%s&format=Json&filter=[["issn","equals","%s"]]'

# * Preallocation * #

journalPolicy <-
  tibble(
    issn = character(),
    title = character(),
    article_version = character()
  )

df_JSON <-
    tibble(
        publishers = character(),
        issns = character(),
        url = character(),
        id = character(),
        listed_in_doaj_phrases = character(),
        type_phrases = character(),
        type = character(),
        system_metadata = character(),
        listed_in_doaj = character(),
        publisher_policy = character(),
        title = character()
    )

jList <- vector(mode = "list", length = 100)
API <- vector(mode = "list", length = 100)
API_P2 <- vector(mode = "list", length = 100)


# * Loop Preparation * #

cores <- detectCores()
message(c(("\n* SETUP: computer cores used: "), cores, " *\n"))
cl <- snow::makeCluster(cores)
registerDoSNOW(cl)

pb <- txtProgressBar(
  min = 0,
  max = count,
  style = 3
)

opts <- list(progress = (function(n) setTxtProgressBar(pb, n)))

# ! Parse 1
message("\n* PARSE 1 OF 5 * Loading ISSNs from API, this may take some time\n")

API <- foreach(i = 1:count, .options.snow = opts, .errorhandling = "pass") %dopar% {
  journalArg <- journals[i,][3]
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
    journalArg
  }
}

close(pb)
snow::stopCluster(cl)

# ! Parse 2
message("\n* PARSE 2 OF 5 *\n")
count <- length(API)

for (i in 1:count)
{
  tryCatch(
    expr = {
      # Progress bar
      flush.console()
      setTxtProgressBar(pb, i)

      # We flatten, and then convert the JSON into a tibble for easier manipulation
      df_JSON <- API[[i]] %>%
        flatten() %>%
        purrr::map_if(is.data.frame, list) %>%
        tibble::as_tibble(.name_repair = "minimal")


      # Makes sure that journals not listed in SHERPA/Romeo are not iterated over
      if (!(is_tibble(API[[i]][1]))) {
        # Obtain the ISSNs from the JSON
        a <- df_JSON %>%
          select(issns) %>%
          unnest(cols = (issns)) %>%
          select(issn)

        # Obtain the title from the JSON
        b <- df_JSON %>%
          select(title) %>%
          unnest(cols = (title)) %>%
          select(title)

        # Obtain the policies from the JSON
            c <- df_JSON %>%
            dplyr::select(publisher_policy) %>%
            unnest(cols = (publisher_policy))
        if ("permitted_oa" %in% colnames(c)) {
            c %<>%
            select(permitted_oa) %>%
                unnest(cols = (permitted_oa)) %>%
                unnest(article_version) %>%
                select(article_version)
        } else {
            # c %<>%
            # dplyr::select(open_access_prohibited)
            next
        }

        # Merge the parts together
        d <- merge(a, b, all = TRUE)
        d <- merge(d, c, all = TRUE)
        journalPolicy <-
          tibble::add_case(
            journalPolicy,
            issn = d$issn,
            title = d$title,
            `article_version` = d$article_version
          )

        # Output the unusued ISSNs for re-checking
      } else {
     
      }
    },
    error = function(e) {
      message(paste("\t * Caught an error on itertion ", i))
      message(paste(e))
      message("\t *ISSN: ", API[[i]][["items"]][["issns"]][[1]][["issn"]])
    }
  )
}


##############################
# 2 - Analysis (3/5)         #
##############################

message("\n* PARSE 3 OF 5 * Converting leftover ISSNs to Journal Titles\n")

# Anti join the found journals from the original dataframe, which gives us
# the left over journals
foundJournals <- journalPolicy %>%
    select(issn) %>%
    rename(ISSN = issn) %>%
    distinct()
issn <- foundJournals %>% pull(1)
issn <- gsub("-", "", issn)
foundJournals[1] <- issn

nextJournals <- anti_join(combinedCite, foundJournals) %>%
    ungroup() %>%
    select(Title) %>%
    distinct()

##############################
# 2 - Analysis (4/5)         #
##############################

message("\n* PARSE 4 OF 5 * Loading ISSNs from API, this may take some time\n")

count <- nrow(nextJournals)
pb <- txtProgressBar(
  min = 0,
  max = count,
  style = 3
)
cl <- snow::makeCluster(cores)
registerDoSNOW(cl)

urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=%s&format=Json&filter=[["title","equals","%s"]]'

API_P2 <- foreach(i = 1:count, .options.snow = opts, .errorhandling = "pass") %dopar% {
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
    journalArg
  }
}

close(pb)
snow::stopCluster(cl)


##############################
# 2 - Analysis (5/5)         #
##############################


message("\n* PARSE 5 OF 5 * \n")
count <- length(API_P2)

for (i in 1:count)
{
  tryCatch(
    expr = {
      # Progress bar
      flush.console()
      setTxtProgressBar(pb, i)

      # We flatten, and then convert the JSON into a tibble for easier manipulation
      if (is_list(API_P2[[i]])) {
        df_JSON <- API_P2[[i]] %>%
          flatten() %>%
          purrr::map_if(is.data.frame, list) %>%
          tibble::as_tibble(.name_repair = "minimal")
      }

      # Makes sure that journals not listed in SHERPA/Romeo are not iterated over
      if (!(is_tibble(API_P2[[i]][1])) && !(is_character(API_P2[[i]][1]))) {
        # Obtain the ISSNs from the JSON
        a <- df_JSON %>%
          select(issns) %>%
          unnest(cols = (issns)) %>%
          select(issn)

        # Obtain the title from the JSON
        b <- df_JSON %>%
          select(title) %>%
          unnest(cols = (title)) %>%
          select(title)

        # Obtain the policies from the JSON
        c <- df_JSON %>%
            dplyr::select(publisher_policy) %>%
            unnest(cols = (publisher_policy))
        if ("permitted_oa" %in% colnames(c)) {
            c %<>%
            select(permitted_oa) %>%
                unnest(cols = (permitted_oa)) %>%
                unnest(article_version) %>%
                select(article_version)
        } else {
           
            next
        }

        # Merge the parts together

        d <- merge(a, b, all = TRUE)
        d <- merge(d, c, all = TRUE)

        journalPolicy <-
          tibble::add_case(
            journalPolicy,
            issn = d$issn,
            title = d$title,
            article_version = d$article_version
          )

        
      } else {
        
      }
    },
    error = function(e) {
      message(paste("\t * Caught an error on itertion ", i))
      message(paste(e))
      message("\t *ISSN: ", API_P2[[i]][["items"]][["issns"]][[1]][["issn"]])
    }
  )
}

##############################
# 3 - Aggregate Findings     #
##############################

# Tidy up journal policy by combining duplicates and spreading data
journalPolicy %<>%
    distinct() %>%
    pivot_wider(id_cols = c(issn, title), values_from = article_version, names_from = article_version) %>%
    mutate(submitted == "submitted", published == "published", accepted == "accepted") %>%
    select(-submitted, -accepted, -published) %>%
    mutate_all(list(~ ifelse(is.na(.), FALSE, .))) %>%
    rename(Title = title, Submitted = 'submitted == "submitted"', Published = 'published == "published"', Accepted = 'accepted == "accepted"', ISSN = issn)
journalPolicy$ISSN <- gsub("-", "", journalPolicy$ISSN)

return(journalPolicy)


}
