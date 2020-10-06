analyse_survey <- function(df) {

  # Convert barriers to long
  df %<>%
    select(c(
      ParticipantNumber,
      FORcode_1,
      CareerLevel,
      OSBarriersList_1:OSBarriersList_21
    )) %>%
    select(-OSBarriersList_12_TEXT) %>%
    tidyr::gather(
      key = "Barrier_Type",
      value = "value",
      OSBarriersList_1:OSBarriersList_21
    ) %>%
    mutate(
      value = case_when(
        Barrier_Type == "OSBarriersList_1" &
          value == 1 ~ "OAFunding",
        Barrier_Type == "OSBarriersList_2" &
          value == 1 ~ "Credit",
        Barrier_Type == "OSBarriersList_3" &
          value == 1 ~ "Mandate",
        Barrier_Type == "OSBarriersList_4" &
          value == 1 ~ "PracticeInfo",
        Barrier_Type == "OSBarriersList_5" &
          value == 1 ~ "PracticeInfra",
        Barrier_Type == "OSBarriersList_6" &
          value == 1 ~ "PracticeTime",
        Barrier_Type == "OSBarriersList_8" &
          value == 1 ~ "ResDiscourage",
        Barrier_Type == "OSBarriersList_9" &
          value == 1 ~ "OSInterest",
        Barrier_Type == "OSBarriersList_10" &
          value == 1 ~ "OB_None",
        Barrier_Type == "OSBarriersList_11" &
          value == 1 ~ "OSCIntim",
        Barrier_Type == "OSBarriersList_12" & value == 1 ~ "Other",
        Barrier_Type == "OSBarriersList_13" &
          value == 1 ~ "StudentDiscourage",
        Barrier_Type == "OSBarriersList_15" &
          value == 1 ~ "PracticeTraining",
        Barrier_Type == "OSBarriersList_16" &
          value == 1 ~ "ResControl",
        Barrier_Type == "OSBarriersList_17" &
          value == 1 ~ "PracticeRecog",
        Barrier_Type == "OSBarriersList_18" &
          value == 1 ~ "Staff",
        Barrier_Type == "OSBarriersList_19" &
          value == 1 ~ "ResFunding",
        Barrier_Type == "OSBarriersList_20" &
          value == 1 ~ "PracticeLearn",
        Barrier_Type == "OSBarriersList_21" &
          value == 1 ~ "PracticeExpertise",
        TRUE ~ NA_character_
      )
    )
  df$value <- factor(df$value)
  df$Barrier_Type <- NULL
  colnames(df)[4] <- "Barrier"

  # Combine FORcode groups into related disciplines

  df %<>%
    mutate(
      Discipline = case_when(
        # Physical Sciences
        FORcode_1 == "02" | # Chemical Science
        FORcode_1 == "03" | # Physical Sciences
        FORcode_1 == "04" ~ "Physical Sciences", # Earth Sciences

        # Life Sciences
        FORcode_1 == "05" | # Environmental Science
        FORcode_1 == "06" | # Biological Science
        FORcode_1 == "07" ~ "Life Sciences", # Agricultural and Veterinary Sciences

        # Mathematics & Comp. Sciences
        FORcode_1 == "01" | # Mathematics
        FORcode_1 == "08"  | # Information and Computer Sciences
        FORcode_1 == "10" ~ "Math & Comp Sciences", # Technology

        # Business, Economics, and Law
        FORcode_1 == "14" | # Economics
        FORcode_1 == "15" | # Commerce
        FORcode_1 == "18" ~ "Business, & Law", # Law and Legal Studies

        # Psych. & Cog. Sciences
        FORcode_1 == "17" ~ "Psych & Cog. Sciences", # Psych. & Cog. Sciences

        # ASSH
        FORcode_1 == "13" | # Education
        FORcode_1 == "16" | # Studies in Human Society
        FORcode_1 == "19" | # Studies in Creative Arts and Writing
        FORcode_1 == "20" | # Language, Communication, and Culture
        FORcode_1 == "21" | # History and Archaeology
        FORcode_1 == "22" ~ "ASSH", # Philosophy and Religious Studies

        # Engineering
        FORcode_1 == "09" ~ "Engineering", # Engineering

        # Medical & Health Sciences 
        FORcode_1 == "11" ~ "Medical & Health Sciences", # Medical & Health Sciences

        TRUE ~ "Other"
      )
    )
  return(df)
}