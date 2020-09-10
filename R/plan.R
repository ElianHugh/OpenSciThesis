# file.exists("data/scimagojr 2019.csv")
# file.exists("data/top-factor.csv")

the_plan <-
  drake_plan(

    # Import data
    topFactor = get_topFactor(),
    journalSJR = get_journalSJR(),
    openSci = get_openSci(),
    meta = get_metadata(),
    key = get_key(),
    
    # Analysis
    barrierAnalysis = analyse_survey(openSci),
    journalList = analyse_journals(journalSJR, topFactor),
    journalPolicy = fetch_sherpa(journalList, journalSJR, key),
    combinedJournals = combine_journals(journalList, topFactor, journalPolicy),

    # Describe
    statsCareer = describe_career(barrierAnalysis),
    statsDiscipline = describe_discipline(barrierAnalysis),
    statsJournal = describe_journal(combinedJournals),

    # Graphs
    journalGraph = graph_journals(combinedJournals),
    careerGraph = graph_careers(barrierAnalysis),
    disciplineGraph = graph_disciplines(barrierAnalysis),

    Report = target(
      command = {
        rmarkdown::render(knitr_in("doc/EmpiricalReport.Rmd"))
        file_out("doc/EmpiricalReport.pdf")
      }
    )
  )