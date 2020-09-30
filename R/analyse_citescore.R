analyse_citescore <- function(topFactor, citeScore) {
    citeScore %<>%
        select(Title, Publisher, CiteScore, SJR, `Top 10% (CiteScore Percentile)`, `Scopus Sub-Subject Area`, `Print ISSN`, `E-ISSN`) %>%
        rename(ISSN = `Print ISSN`, ISSN2 = `E-ISSN`) %>%
        pivot_longer(c("ISSN", "ISSN2"), names_repair = "unique") %>%
        rename(ISSN = value, Top10Perc = `Top 10% (CiteScore Percentile)`)
    citeScore$name <- NULL

    # For some reason SCOPUS has messed up the ISSNs...
    # ! Have to add the leading zeros

    citeScore$ISSN <- with_options(
        c(scipen = 999),
        str_pad(citeScore$ISSN, 8, pad = "0")
    )

    df <- left_join(topFactor, citeScore)

    # Retry
    df <- topFactor %>%
        coalesce_join(df, by = "ISSN")

    # Take out the garbage
    message("Retry 1")
    x <- df[is.na(df$`Scopus Sub-Subject Area`), ] %>%
        select(
            Title,
            ISSN,
            Publisher,
            DataTransparency,
            AnalysisTransparency,
            MaterialsTransparency,
            DesignAnalysis,
            Preregistration,
            Replication,
            AnalysisPreReg,
            RegRepPubBias,
            DataTransparency,
            DataCitation,
            Badges
        )
    df <- anti_join(df, x, by = "Title")
    y <- fuzzyjoin::stringdist_left_join(x, citeScore, by = "Title", max_dist = 3, distance_col = "Distance")

    closest <- y %>%
        group_by(Title.x) %>%
        top_n(1, desc(Distance)) %>%
        ungroup() %>%
        rename(
            Title = Title.x,
            MatchTitle = Title.y,
            ISSN = ISSN.x,
            Publisher = Publisher.x,
        ) %>%
        select(-c(Publisher.y, ISSN.y, Distance))

    df <- coalesce_join(df, closest, by = "Title") %>%
        select(Title, MatchTitle, everything())

    # Take out the garbage (again)
    print("Retry 2")
    x <- y[is.na(y$`Title.y`), ] %>%
        select(
            Title.x,
            ISSN.x,
            Publisher.x,
            DataTransparency,
            AnalysisTransparency,
            MaterialsTransparency,
            DesignAnalysis,
            Preregistration,
            Replication,
            AnalysisPreReg,
            RegRepPubBias,
            DataTransparency,
            DataCitation,
            Badges
        ) %>%
        rename(Title = Title.x, ISSN = ISSN.x, Publisher = Publisher.x)

    # ! Fuzzy match again
    y <- fuzzyjoin::stringdist_left_join(x, citeScore, by = "Title", max_dist = 4, distance_col = "Distance")
    x <- y[is.na(y$`Title.y`), ]
    y <- anti_join(y, x)

    # ? We combine two columns into a list for iteration
    # ! Examine if there are NO spaces in BOTH Title.x and Title.y
    # ! If true, this means there has been a mismatch

    y %<>%
        rowwise() %>%
        mutate(Keep = checkmatch(Title.x, Title.y)) %>%
        dplyr::filter(Keep == TRUE)

    # Manual checks
    y %<>%
        dplyr::filter(Title.x != "Motivation Science") %>%
        dplyr::filter(Title.x != "Stigma and Health") %>%
        dplyr::filter(Title.x != "Journal of Cognition") %>%
        dplyr::filter(Title.x != "Social Psychological Bulletin") %>%
        dplyr::filter(Title.x != "Brain Communications") %>%
        dplyr::filter(Title.x != "Journal Of Comparative Psychology") %>%
        dplyr::filter(Title.x != "Meta-Psychology") %>%
        rename(
            Title = Title.x,
            MatchTitle = Title.y,
            ISSN = ISSN.x,
            Publisher = Publisher.x,
        ) %>%
        select(-c(Publisher.y, ISSN.y, Distance, Keep))
    df <- coalesce_join(df, y, by = "Title")



    df %<>%
        mutate_at(vars(DataCitation:Badges), ~ case_when(
            . == 3 ~ 1,
            . == 2 ~ 1,
            . == 1 ~ 1,
            . == 0 ~ 0
        )) %>%
        # !
        rename(SubjectArea = `Scopus Sub-Subject Area`) %>%
        mutate(Discipline = case_when(
            SubjectArea == "Agricultural and Biological Sciences (miscellaneous)" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Agronomy and Crop Science" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Animal Science and Zoology" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Aquatic Science" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Ecology, Evolution, Behavior and Systematics" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Food Science" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Forestry" ~ "Agricultural and Biological Sciences",
            SubjectArea == "General Agricultural and Biological Sciences" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Horticulture" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Insect Science" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Plant Science" ~ "Agricultural and Biological Sciences",
            SubjectArea == "Soil Science" ~ "Agricultural and Biological Sciences",

            SubjectArea == "Arts and Humanities" ~ "Arts and Humanities",
            SubjectArea == "Archeology (arts and humanities)" ~ "Arts and Humanities",
            SubjectArea == "Arts and Humanities (miscellaneous)" ~ "Arts and Humanities",
            SubjectArea == "Classics" ~ "Arts and Humanities",
            SubjectArea == "Conservation" ~ "Arts and Humanities",
            SubjectArea == "General Arts and Humanities" ~ "Arts and Humanities",
            SubjectArea == "History" ~ "Arts and Humanities",
            SubjectArea == "History and Philosophy of Science" ~ "Arts and Humanities",
            SubjectArea == "Language and Linguistics" ~ "Arts and Humanities",
            SubjectArea == "Literature and Literary Theory" ~ "Arts and Humanities",
            SubjectArea == "Museology" ~ "Arts and Humanities",
            SubjectArea == "Music" ~ "Arts and Humanities",
            SubjectArea == "Philosophy" ~ "Arts and Humanities",
            SubjectArea == "Religious Studies" ~ "Arts and Humanities",
            SubjectArea == "Visual Arts and Performing Arts" ~ "Arts and Humanities",

            SubjectArea == "Biochemistry, Genetics and Molecular Biology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Aging" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Biochemistry" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Biochemistry, Genetics and Molecular Biology (miscellaneous)" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Biophysics" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Biotechnology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Cancer Research" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Cell Biology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Clinical Biochemistry" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Developmental Biology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Endocrinology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "General Biochemistry, Genetics and Molecular Biology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Genetics" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Molecular Biology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Molecular Medicine" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Physiology" ~ "Biochemistry, Genetics and Molecular Biology",
            SubjectArea == "Structural Biology" ~ "Biochemistry, Genetics and Molecular Biology",

            SubjectArea == "Business, Management and Accounting" ~ "Business, Management and Accounting",
            SubjectArea == "Accounting" ~ "Business, Management and Accounting",
            SubjectArea == "Business and International Management" ~ "Business, Management and Accounting",
            SubjectArea == "Business, Management and Accounting (miscellaneous)" ~ "Business, Management and Accounting",
            SubjectArea == "General Business, Management and Accounting" ~ "Business, Management and Accounting",
            SubjectArea == "Industrial Relations" ~ "Business, Management and Accounting",
            SubjectArea == "Management Information Systems" ~ "Business, Management and Accounting",
            SubjectArea == "Management of Technology and Innovation" ~ "Business, Management and Accounting",
            SubjectArea == "Marketing" ~ "Business, Management and Accounting",
            SubjectArea == "Organizational Behavior and Human Resource Management" ~ "Business, Management and Accounting",
            SubjectArea == "Strategy and Management" ~ "Business, Management and Accounting",
            SubjectArea == "Tourism, Leisure and Hospitality Management" ~ "Business, Management and Accounting",

            SubjectArea == "Chemical Engineering" ~ "Chemical Engineering",
            SubjectArea == "Bioengineering" ~ "Chemical Engineering",
            SubjectArea == "Catalysis" ~ "Chemical Engineering",
            SubjectArea == "Chemical Engineering (miscellaneous)" ~ "Chemical Engineering",
            SubjectArea == "Chemical Health and Safety" ~ "Chemical Engineering",
            SubjectArea == "Colloid and Surface Chemistry" ~ "Chemical Engineering",
            SubjectArea == "Filtration and Separation" ~ "Chemical Engineering",
            SubjectArea == "Fluid Flow and Transfer Processes" ~ "Chemical Engineering",
            SubjectArea == "General Chemical Engineering" ~ "Chemical Engineering",
            SubjectArea == "Process Chemistry and Technology" ~ "Chemical Engineering",

            SubjectArea == "Chemistry" ~ "Chemistry",
            SubjectArea == "Analytical Chemistry" ~ "Chemistry",
            SubjectArea == "Chemistry (miscellaneous)" ~ "Chemistry",
            SubjectArea == "Electrochemistry" ~ "Chemistry",
            SubjectArea == "General Chemistry" ~ "Chemistry",
            SubjectArea == "Inorganic Chemistry" ~ "Chemistry",
            SubjectArea == "Organic Chemistry" ~ "Chemistry",
            SubjectArea == "Physical and Theoretical Chemistry" ~ "Chemistry",
            SubjectArea == "Spectroscopy" ~ "Chemistry",

            SubjectArea == "Computer Science" ~ "Computer Science",
            SubjectArea == "Artificial Intelligence" ~ "Computer Science",
            SubjectArea == "Computational Theory and Mathematics" ~ "Computer Science",
            SubjectArea == "Computer Graphics and Computer-Aided Design" ~ "Computer Science",
            SubjectArea == "Computer Networks and Communications" ~ "Computer Science",
            SubjectArea == "Computer Science (miscellaneous)" ~ "Computer Science",
            SubjectArea == "Computer Science Applications" ~ "Computer Science",
            SubjectArea == "Computer Vision and Pattern Recognition" ~ "Computer Science",
            SubjectArea == "General Computer Science" ~ "Computer Science",
            SubjectArea == "Hardware and Architecture" ~ "Computer Science",
            SubjectArea == "Human-Computer Interaction" ~ "Computer Science",
            SubjectArea == "Information Systems" ~ "Computer Science",
            SubjectArea == "Signal Processing" ~ "Computer Science",
            SubjectArea == "Software" ~ "Computer Science",

            # * TODO examine here
            SubjectArea == "Decision Sciences" ~ "Decision Sciences",
            SubjectArea == "Decision Sciences (miscellaneous)" ~ "Decision Sciences",
            SubjectArea == "General Decision Sciences" ~ "Decision Sciences",
            SubjectArea == "Information Systems and Management" ~ "Decision Sciences",
            SubjectArea == "Management Science and Operations Research" ~ "Decision Sciences",
            SubjectArea == "Statistics, Probability and Uncertainty" ~ "Decision Sciences",

            SubjectArea == "Dentistry" ~ "Dentistry",
            SubjectArea == "Dental Assisting" ~ "Dentistry",
            SubjectArea == "Dental Hygiene" ~ "Dentistry",
            SubjectArea == "Dentistry (miscellaneous)" ~ "Dentistry",
            SubjectArea == "General Dentistry" ~ "Dentistry",
            SubjectArea == "Oral Surgery" ~ "Dentistry",
            SubjectArea == "Orthodontics" ~ "Dentistry",
            SubjectArea == "Periodontics" ~ "Dentistry",

            SubjectArea == "Earth and Planetary Sciences" ~ "Earth and Planetary Sciences",
            SubjectArea == "Atmospheric Science" ~ "Earth and Planetary Sciences",
            SubjectArea == "Computers in Earth Sciences" ~ "Earth and Planetary Sciences",
            SubjectArea == "Earth and Planetary Sciences (miscellaneous)" ~ "Earth and Planetary Sciences",
            SubjectArea == "Earth-Surface Processes" ~ "Earth and Planetary Sciences",
            SubjectArea == "Economic Geology" ~ "Earth and Planetary Sciences",
            SubjectArea == "General Earth and Planetary Sciences" ~ "Earth and Planetary Sciences",
            SubjectArea == "Geochemistry and Petrology" ~ "Earth and Planetary Sciences",
            SubjectArea == "Geology" ~ "Earth and Planetary Sciences",
            SubjectArea == "Geophysics" ~ "Earth and Planetary Sciences",
            SubjectArea == "Geotechnical Engineering and Engineering Geology" ~ "Earth and Planetary Sciences",
            SubjectArea == "Oceanography" ~ "Earth and Planetary Sciences",
            SubjectArea == "Paleontology" ~ "Earth and Planetary Sciences",
            SubjectArea == "Space and Planetary Science" ~ "Earth and Planetary Sciences",
            SubjectArea == "Stratigraphy" ~ "Earth and Planetary Sciences",

            SubjectArea == "Economics, Econometrics and Finance" ~ "Economics, Econometrics and Finance",
            SubjectArea == "Economics and Econometrics" ~ "Economics, Econometrics and Finance",
            SubjectArea == "Economics, Econometrics and Finance (miscellaneous)" ~ "Economics, Econometrics and Finance",
            SubjectArea == "Finance" ~ "Economics, Econometrics and Finance",
            SubjectArea == "General Economics, Econometrics and Finance" ~ "Economics, Econometrics and Finance",


            # ! Split this field up...?
            SubjectArea == "Energy" ~ "Energy",
            SubjectArea == "Energy (miscellaneous)" ~ "Energy",
            SubjectArea == "Energy Engineering and Power Technology" ~ "Engineering",
            SubjectArea == "Fuel Technology" ~ "Energy",
            SubjectArea == "General Energy" ~ "Energy",
            SubjectArea == "Nuclear Energy and Engineering" ~ "Engineering",
            SubjectArea == "Renewable Energy, Sustainability and the Environment" ~ "Environmental Science",

            SubjectArea == "Engineering" ~ "Engineering",
            SubjectArea == "Aerospace Engineering" ~ "Engineering",
            SubjectArea == "Architecture" ~ "Engineering",
            SubjectArea == "Automotive Engineering" ~ "Engineering",
            SubjectArea == "Biomedical Engineering" ~ "Engineering",
            SubjectArea == "Building and Construction" ~ "Engineering",
            SubjectArea == "Civil and Structural Engineering" ~ "Engineering",
            SubjectArea == "Computational Mechanics" ~ "Engineering",
            SubjectArea == "Control and Systems Engineering" ~ "Engineering",
            SubjectArea == "Electrical and Electronic Engineering" ~ "Engineering",
            SubjectArea == "Engineering (miscellaneous)" ~ "Engineering",
            SubjectArea == "General Engineering" ~ "Engineering",
            SubjectArea == "Industrial and Manufacturing Engineering" ~ "Engineering",
            SubjectArea == "Mechanical Engineering" ~ "Engineering",
            SubjectArea == "Mechanics of Materials" ~ "Engineering",
            SubjectArea == "Media Technology" ~ "Engineering",
            SubjectArea == "Ocean Engineering" ~ "Engineering",
            SubjectArea == "Safety, Risk, Reliability and Quality" ~ "Engineering",

            SubjectArea == "Environmental Science" ~ "Environmental Science",
            SubjectArea == "Ecological Modeling" ~ "Environmental Science",
            SubjectArea == "Ecology" ~ "Environmental Science",
            SubjectArea == "Environmental Chemistry" ~ "Environmental Science",
            SubjectArea == "Environmental Engineering" ~ "Environmental Science",
            SubjectArea == "Environmental Science (miscellaneous)" ~ "Environmental Science",
            SubjectArea == "General Environmental Science" ~ "Environmental Science",
            SubjectArea == "Global and Planetary Change" ~ "Environmental Science",
            SubjectArea == "Health, Toxicology and Mutagenesis" ~ "Environmental Science",
            SubjectArea == "Management, Monitoring, Policy and Law" ~ "Environmental Science",
            SubjectArea == "Nature and Landscape Conservation" ~ "Environmental Science",
            SubjectArea == "Pollution" ~ "Environmental Science",
            SubjectArea == "Waste Management and Disposal" ~ "Environmental Science",
            SubjectArea == "Water Science and Technology" ~ "Environmental Science",

            SubjectArea == "Health Professions" ~ "Health Professions",
            SubjectArea == "Chiropractics" ~ "Health Professions",
            SubjectArea == "Complementary and Manual Therapy" ~ "Health Professions",
            SubjectArea == "Emergency Medical Services" ~ "Health Professions",
            SubjectArea == "General Health Professions" ~ "Health Professions",
            SubjectArea == "Health Information Management" ~ "Health Professions",
            SubjectArea == "Health Professions (miscellaneous)" ~ "Health Professions",
            SubjectArea == "Medical Assisting and Transcription" ~ "Health Professions",
            SubjectArea == "Medical Laboratory Technology" ~ "Health Professions",
            SubjectArea == "Medical Terminology" ~ "Health Professions",
            SubjectArea == "Occupational Therapy" ~ "Health Professions",
            SubjectArea == "Optometry" ~ "Health Professions",
            SubjectArea == "Pharmacy" ~ "Health Professions",
            SubjectArea == "Physical Therapy, Sports Therapy and Rehabilitation" ~ "Health Professions",
            SubjectArea == "Podiatry" ~ "Health Professions",
            SubjectArea == "Radiological and Ultrasound Technology" ~ "Health Professions",
            SubjectArea == "Respiratory Care" ~ "Health Professions",
            SubjectArea == "Speech and Hearing" ~ "Health Professions",

            SubjectArea == "Immunology and Microbiology" ~ "Immunology and Microbiology",
            SubjectArea == "Applied Microbiology and Biotechnology" ~ "Immunology and Microbiology",
            SubjectArea == "General Immunology and Microbiology" ~ "Immunology and Microbiology",
            SubjectArea == "Immunology" ~ "Immunology and Microbiology",
            SubjectArea == "Immunology and Microbiology (miscellaneous) " ~ "Immunology and Microbiology",
            SubjectArea == "Microbiology" ~ "Immunology and Microbiology",
            SubjectArea == "Parasitology" ~ "Immunology and Microbiology",
            SubjectArea == "Virology" ~ "Immunology and Microbiology",

            SubjectArea == "Materials Science" ~ "Materials Science",
            SubjectArea == "Biomaterials" ~ "Materials Science",
            SubjectArea == "Ceramics and Composites" ~ "Materials Science",
            SubjectArea == "Electronic, Optical and Magnetic Materials" ~ "Materials Science",
            SubjectArea == "General Materials Science" ~ "Materials Science",
            SubjectArea == "Materials Chemistry" ~ "Materials Science",
            SubjectArea == "Materials Science (miscellaneous)" ~ "Materials Science",
            SubjectArea == "Metals and Alloys" ~ "Materials Science",
            SubjectArea == "Polymers and Plastics" ~ "Materials Science",
            SubjectArea == "Surfaces, Coatings and Films" ~ "Materials Science",

            SubjectArea == "Mathematics" ~ "Mathematics",
            SubjectArea == "Algebra and Number Theory" ~ "Mathematics",
            SubjectArea == "Analysis" ~ "Mathematics",
            SubjectArea == "Applied Mathematics" ~ "Mathematics",
            SubjectArea == "Computational Mathematics" ~ "Mathematics",
            SubjectArea == "Control and Optimization" ~ "Mathematics",
            SubjectArea == "Discrete Mathematics and Combinatorics" ~ "Mathematics",
            SubjectArea == "General Mathematics" ~ "Mathematics",
            SubjectArea == "Geometry and Topology" ~ "Mathematics",
            SubjectArea == "Logic" ~ "Mathematics",
            SubjectArea == "Mathematical Physics" ~ "Mathematics",
            SubjectArea == "Mathematics (miscellaneous)" ~ "Mathematics",
            SubjectArea == "Modeling and Simulation" ~ "Mathematics",
            SubjectArea == "Numerical Analysis" ~ "Mathematics",
            SubjectArea == "Statistics and Probability" ~ "Mathematics",
            SubjectArea == "Theoretical Computer Science" ~ "Mathematics",

            SubjectArea == "Medicine" ~ "Medicine",
            SubjectArea == "Anatomy" ~ "Medicine",
            SubjectArea == "Anesthesiology and Pain Medicine" ~ "Medicine",
            SubjectArea == "Biochemistry (medical)" ~ "Medicine",
            SubjectArea == "Cardiology and Cardiovascular Medicine" ~ "Medicine",
            SubjectArea == "Complementary and Alternative Medicine" ~ "Medicine",
            SubjectArea == "Critical Care and Intensive Care Medicine" ~ "Medicine",
            SubjectArea == "Dermatology" ~ "Medicine",
            SubjectArea == "Drug Guides" ~ "Medicine",
            SubjectArea == "Embryology" ~ "Medicine",
            SubjectArea == "Emergency Medicine" ~ "Medicine",
            SubjectArea == "Endocrinology, Diabetes and Metabolism" ~ "Medicine",
            SubjectArea == "Epidemiology" ~ "Medicine",
            SubjectArea == "Family Practice" ~ "Medicine",
            SubjectArea == "Gastroenterology" ~ "Medicine",
            SubjectArea == "General Medicine" ~ "Medicine",
            SubjectArea == "Genetics (clinical)" ~ "Medicine",
            SubjectArea == "Geriatrics and Gerontology" ~ "Medicine",
            SubjectArea == "Health Informatics" ~ "Medicine",
            SubjectArea == "Health Policy" ~ "Medicine",
            SubjectArea == "Hematology" ~ "Medicine",
            SubjectArea == "Hepatology" ~ "Medicine",
            SubjectArea == "Histology" ~ "Medicine",
            SubjectArea == "Immunology and Allergy" ~ "Medicine",
            SubjectArea == "Infectious Diseases" ~ "Medicine",
            SubjectArea == "Internal Medicine" ~ "Medicine",
            SubjectArea == "Medicine (miscellaneous)" ~ "Medicine",
            SubjectArea == "Microbiology (medical)" ~ "Medicine",
            SubjectArea == "Nephrology" ~ "Medicine",
            SubjectArea == "Neurology (clinical)" ~ "Medicine",
            SubjectArea == "Obstetrics and Gynecology" ~ "Medicine",
            SubjectArea == "Oncology" ~ "Medicine",
            SubjectArea == "Ophthalmology" ~ "Medicine",
            SubjectArea == "Orthopedics and Sports Medicine" ~ "Medicine",
            SubjectArea == "Otorhinolaryngology" ~ "Medicine",
            SubjectArea == "Pathology and Forensic Medicine" ~ "Medicine",
            SubjectArea == "Pediatrics, Perinatology and Child Health" ~ "Medicine",
            SubjectArea == "Pharmacology (medical)" ~ "Medicine",
            SubjectArea == "Physiology (medical)" ~ "Medicine",
            SubjectArea == "Psychiatry and Mental Health" ~ "Medicine",
            SubjectArea == "Public Health, Environmental and Occupational Health" ~ "Medicine",
            SubjectArea == "Pulmonary and Respiratory Medicine" ~ "Medicine",
            SubjectArea == "Radiology, Nuclear Medicine and Imaging" ~ "Medicine",
            SubjectArea == "Rehabilitation" ~ "Medicine",
            SubjectArea == "Reproductive Medicine" ~ "Medicine",
            SubjectArea == "Reviews and References (medical)" ~ "Medicine",
            SubjectArea == "Rheumatology" ~ "Medicine",
            SubjectArea == "Surgery" ~ "Medicine",
            SubjectArea == "Transplantation" ~ "Medicine",
            SubjectArea == "Urology" ~ "Medicine",

            SubjectArea == "" ~ "Neuroscience",
            SubjectArea == "Behavioral Neuroscience" ~ "Neuroscience",
            SubjectArea == "Biological Psychiatry" ~ "Neuroscience",
            SubjectArea == "Cellular and Molecular Neuroscience" ~ "Neuroscience",
            SubjectArea == "Cognitive Neuroscience" ~ "Neuroscience",
            SubjectArea == "Developmental Neuroscience" ~ "Neuroscience",
            SubjectArea == "Endocrine and Autonomic Systems" ~ "Neuroscience",
            SubjectArea == "General Neuroscience" ~ "Neuroscience",
            SubjectArea == "Neurology" ~ "Neuroscience",
            SubjectArea == "Neuroscience (miscellaneous)" ~ "Neuroscience",
            SubjectArea == "Sensory Systems" ~ "Neuroscience",

            SubjectArea == "Nursing" ~ "Nursing",
            SubjectArea == "Advanced and Specialized Nursing" ~ "Nursing",
            SubjectArea == "Assessment and Diagnosis" ~ "Nursing",
            SubjectArea == "Care Planning" ~ "Nursing",
            SubjectArea == "Community and Home Care" ~ "Nursing",
            SubjectArea == "Critical Care Nursing" ~ "Nursing",
            SubjectArea == "Emergency Nursing" ~ "Nursing",
            SubjectArea == "Fundamentals and Skills" ~ "Nursing",
            SubjectArea == "General Nursing" ~ "Nursing",
            SubjectArea == "Gerontology" ~ "Nursing",
            SubjectArea == "Issues, Ethics and Legal Aspects" ~ "Nursing",
            SubjectArea == "LPN and LVN" ~ "Nursing",
            SubjectArea == "Leadership and Management" ~ "Nursing",
            SubjectArea == "Maternity and Midwifery" ~ "Nursing",
            SubjectArea == "Medical and Surgical Nursing" ~ "Nursing",
            SubjectArea == "Nurse Assisting" ~ "Nursing",
            SubjectArea == "Nursing (miscellaneous)" ~ "Nursing",
            SubjectArea == "Nutrition and Dietetics" ~ "Nursing",
            SubjectArea == "Oncology (nursing)" ~ "Nursing",
            SubjectArea == "Pathophysiology" ~ "Nursing",
            SubjectArea == "Pediatrics" ~ "Nursing",
            SubjectArea == "Pharmacology (nursing)" ~ "Nursing",
            SubjectArea == "Psychiatric Mental Health" ~ "Nursing",
            SubjectArea == "Research and Theory" ~ "Nursing",
            SubjectArea == "Review and Exam Preparation" ~ "Nursing",

            SubjectArea == "Pharmacology, Toxicology and Pharmaceutics" ~ "Pharmacology, Toxicology and Pharmaceutics",
            SubjectArea == "Drug Discovery" ~ "Pharmacology, Toxicology and Pharmaceutics",
            SubjectArea == "General Pharmacology, Toxicology and Pharmaceutics" ~ "Pharmacology, Toxicology and Pharmaceutics",
            SubjectArea == "Pharmaceutical Science" ~ "Pharmacology, Toxicology and Pharmaceutics",
            SubjectArea == "Pharmacology" ~ "Pharmacology, Toxicology and Pharmaceutics",
            SubjectArea == "Pharmacology, Toxicology and Pharmaceutics (miscellaneous)" ~ "Pharmacology, Toxicology and Pharmaceutics",
            SubjectArea == "Toxicology" ~ "Pharmacology, Toxicology and Pharmaceutics",

            SubjectArea == "Physics and Astronomy" ~ "Physics and Astronomy",
            SubjectArea == "Acoustics and Ultrasonics" ~ "Physics and Astronomy",
            SubjectArea == "Astronomy and Astrophysics" ~ "Physics and Astronomy",
            SubjectArea == "Atomic and Molecular Physics, and Optics" ~ "Physics and Astronomy",
            SubjectArea == "Condensed Matter Physics" ~ "Physics and Astronomy",
            SubjectArea == "General Physics and Astronomy" ~ "Physics and Astronomy",
            SubjectArea == "Instrumentation" ~ "Physics and Astronomy",
            SubjectArea == "Nuclear and High Energy Physics" ~ "Physics and Astronomy",
            SubjectArea == "Physics and Astronomy (miscellaneous)" ~ "Physics and Astronomy",
            SubjectArea == "Radiation" ~ "Physics and Astronomy",
            SubjectArea == "Statistical and Nonlinear Physics" ~ "Physics and Astronomy",
            SubjectArea == "Surfaces and Interfaces" ~ "Physics and Astronomy",

            SubjectArea == "Psychology" ~ "Psychology",
            SubjectArea == "Applied Psychology" ~ "Psychology",
            SubjectArea == "Clinical Psychology" ~ "Psychology",
            SubjectArea == "Developmental and Educational Psychology" ~ "Psychology",
            SubjectArea == "Experimental and Cognitive Psychology" ~ "Psychology",
            SubjectArea == "General Psychology" ~ "Psychology",
            SubjectArea == "Neuropsychology and Physiological Psychology" ~ "Psychology",
            SubjectArea == "Psychology (miscellaneous)" ~ "Psychology",
            SubjectArea == "Social Psychology" ~ "Psychology",

            SubjectArea == "Social Sciences" ~ "Social Sciences",
            SubjectArea == "Anthropology" ~ "Social Sciences",
            SubjectArea == "Archeology" ~ "Social Sciences",
            SubjectArea == "Communication" ~ "Social Sciences",
            SubjectArea == "Cultural Studies" ~ "Social Sciences",
            SubjectArea == "Demography" ~ "Social Sciences",
            SubjectArea == "Development" ~ "Social Sciences",
            SubjectArea == "Education" ~ "Social Sciences",
            SubjectArea == "Gender Studies" ~ "Social Sciences",
            SubjectArea == "General Social Sciences" ~ "Social Sciences",
            SubjectArea == "Geography, Planning and Development" ~ "Social Sciences",
            SubjectArea == "Health (social science)" ~ "Social Sciences",
            SubjectArea == "Human Factors and Ergonomics" ~ "Social Sciences",

            # ! originally was Law -> Social
            SubjectArea == "Law" ~ "Law",

            SubjectArea == "Library and Information Sciences" ~ "Social Sciences",
            SubjectArea == "Life-span and Life-course Studies" ~ "Social Sciences",
            SubjectArea == "Linguistics and Language" ~ "Social Sciences",
            SubjectArea == "Political Science and International Relations" ~ "Social Sciences",
            SubjectArea == "Public Administration" ~ "Social Sciences",
            SubjectArea == "Safety Research" ~ "Social Sciences",
            SubjectArea == "Social Sciences (miscellaneous)" ~ "Social Sciences",
            SubjectArea == "Sociology and Political Science" ~ "Social Sciences",
            SubjectArea == "Transportation" ~ "Social Sciences",
            SubjectArea == "Urban Studies" ~ "Social Sciences",

            SubjectArea == "Veterinary" ~ "Veterinary",
            SubjectArea == "Equine" ~ "Veterinary",
            SubjectArea == "Food Animals" ~ "Veterinary",
            SubjectArea == "General Veterinary" ~ "Veterinary",
            SubjectArea == "Small Animals" ~ "Veterinary",
            SubjectArea == "Veterinary (miscellaneous)" ~ "Veterinary",

            SubjectArea == "General Business,Management and Accounting" ~ "Business, Management and Accounting",
            SubjectArea == "General Biochemistry,Genetics and Molecular Biology" ~ "Biochemistry,Genetics and Molecular Biology",
            TRUE ~ "Other"
        )) %>%
        mutate(GroupedDisc = case_when(
            # Math, Chem, Enviro, & Bio Sciences
            Discipline == "Chemistry" ~ "Math, Chem, Enviro, & Bio Sciences",
            Discipline == "Agricultural and Biological Sciences" ~ "Math, Chem, Enviro, & Bio Sciences",
            Discipline == "Biochemistry, Genetics and Molecular Biology" ~ "Math, Chem, Enviro, & Bio Sciences",
            Discipline == "Biochemistry,Genetics and Molecular Biology" ~ "Math, Chem, Enviro, & Bio Sciences",
            Discipline == "Environmental Science" ~ "Math, Chem, Enviro, & Bio Sciences",
            Discipline == "Mathematics" ~ "Math, Chem, Enviro, & Bio Sciences",
            Discipline == "Immunology and Microbiology" ~ "Math, Chem, Enviro, & Bio Sciences",

            # Physical Sciences
            Discipline == "Earth and Planetary Sciences" ~ "Physical Sciences",
            Discipline == "Physics and Astronomy" ~ "Physical Sciences",

            # Tech & Comp Sciences
            Discipline == "Computer Science" ~ "Tech & Comp. Sciences",

            # Engineering
            Discipline == "Engineering" ~ "Engineering",
            Discipline == "Chemical Engineering" ~ "Engineering",

            # Medical & Health Sciences
            Discipline == "Medicine" ~ "Medical & Health Sciences",
            Discipline == "Health Professions" ~ "Medical & Health Sciences",
            Discipline == "Nursing" ~ "Medical & Health Sciences",
            Discipline == "Pharmacology, Toxicology and Pharmaceutics" ~ "Medical & Health Sciences",
            Discipline == "Veterinary" ~ "Medical & Health Sciences",

            # ASSH
            Discipline == "Arts and Humanities" ~ "ASSH",
            Discipline == "Social Sciences" ~ "ASSH",

            # Business & Law
            Discipline == "Business, Management and Accounting" ~ "Business & Economics",
            Discipline == "Economics, Econometrics and Finance" ~ "Business & Economics",
            Discipline == "Law" ~ "Law",

            # Psych & Cog Sciences
            Discipline == "Psychology" ~ "Psych. & Cog. Sciences",
            Discipline == "Neuroscience" ~ "Psych. & Cog. Sciences",
            TRUE ~ "Other"
        ))
    df$OSS <- df %>%
        select(DataCitation:Badges) %>%
        rowSums(na.rm = TRUE)

    df %<>% mutate(
        ScoreGrade = quantcut(df$OSS, q = 5)
    )

    df %<>%
        group_by(GroupedDisc) %>%
        distinct(Title, .keep_all = TRUE) %>%
        add_tally()
    levels(df$ScoreGrade) <- c(
        "None", "Low",
        "Medium", "High", "Very High"
    )


    # ! I don't why this doesn't work above but...
    df %<>%
        dplyr::filter(Title != "Meta-Psychology")

    return(df)
}