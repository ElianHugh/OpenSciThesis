#' title
#' @param param
#' @return return
#' @author author
#' @export
fetch_sherpa <- function(masterList, journalSJR, key) {

# Pre-Allocation... because it might speed up iteration?
journalPolicy <-
  tibble(
    issn = character(),
    title = character(),
    article_version = character()
  )

df_JSON <-
  tibble(
 publishers = character(),
   issns =character(),
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

error_log <- NULL
URLArg <-
  'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=%s&format=Json&filter=[["issn","equals","%s"]]'




# Load JSON Conversion
masterList <- na.omit(masterList)
maxCount <- nrow(masterList)


# Set up workers for loop
cores <- detectCores()
message(c(("\n* SETUP: computer cores used: "), cores, " *\n"))
cl <- snow::makeCluster(cores)
registerDoSNOW(cl)


# Percentage bar for tracking progress
pb <- txtProgressBar(
  min = 0,
  max = maxCount,
  style = 3
)

opts <- list(progress = (function(n) setTxtProgressBar(pb, n)))


##############################
# 2 - Analysis (1/5)         #
##############################

# Loop over API
message("\n* PARSE 1 OF 5 * Loading ISSNs from API, this may take some time\n")
API <- foreach(i = 1:maxCount, .options.snow = opts, .errorhandling = "pass") %dopar% {
  journalArg <- masterList[i, ][5]
  jsonURL <-
    sprintf(
      URLArg,
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
# 2 - Analysis (2/5)         #
##############################

message("\n* PARSE 2 OF 5 *\n")

# Loop over imported data
maxCount <- length(API)
for (i in 1:maxCount)
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
          unnest(cols = (publisher_policy)) %>%
          select(permitted_oa) %>%
          unnest(cols = (permitted_oa)) %>%
          unnest(article_version) %>%
          select(article_version)

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
        jList <- c(jList, as.character(API[[i]][1]))
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

jList %<>%
  enframe(name = NULL, value = "ISSN")


# Remove dashes, SJR uses long string of number instead
issn <- jList %>% pull(1)
issn <- gsub("-", "", issn)
jList[1] <- issn

a <- journalSJR %>%
  separate(col = ISSN, into = c("issn", "issn2"), sep = ", ") %>%
  pivot_longer(c("issn", "issn2"))
colnames(a)[8] <- "ISSN"

jList <- left_join(jList, a)
jList <- as_tibble(jList$Title)
jList <- unique(jList)


# Loop over API v# Set up workers for loop

maxCount <- nrow(jList)
pb <- txtProgressBar(
  min = 0,
  max = maxCount,
  style = 3
)
cl <- snow::makeCluster(cores)
registerDoSNOW(cl)
URLArg <-
  'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=A4A2C39C-756A-11EA-84A4-BF0151E46997&format=Json&filter=[["title","equals","%s"]]'

##############################
# 2 - Analysis (4/5)         #
##############################

message("\n* PARSE 4 OF 5 * Loading ISSNs from API, this may take some time\n")
API_P2 <- foreach(i = 1:maxCount, .options.snow = opts, .errorhandling = "pass") %dopar% {
  journalArg <- jList[i, ]
  jsonURL <-
    sprintf(
      URLArg,
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

# Loop over imported data
maxCount <- length(API_P2)
for (i in 1:maxCount)
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
          unnest(cols = (publisher_policy)) %>%
          select(permitted_oa) %>%
          unnest(cols = (permitted_oa)) %>%
          unnest(article_version) %>%
          select(article_version)

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

        # Output the unusued ISSNs for re-checking
      } else {
        jList <- c(jList, as.character(API_P2[[i]][1]))
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
  mutate_all(list(~ ifelse(is.na(.), FALSE, .)))
colnames(journalPolicy)[2] <- "Title"


# Add leftover journals to journalPolicy (no policies found on Sherpa)
jList %<>%
  unlist() %>%
  enframe(name = NULL)
colnames(jList)[1] <- "Title"
journalPolicy <- full_join(journalPolicy, jList)
}