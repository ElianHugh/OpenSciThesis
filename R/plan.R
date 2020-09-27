# file.exists("data/scimagojr 2019.csv")
# file.exists("data/top-factor.csv")
source(file_in("R/load_functions.R"))

the_plan <-
  drake_plan(

    # Import data
    topFactor = get_topFactor(),
    openSci = get_openSci(),
    meta = get_metadata(),
    key = get_key(),
    citeScore = get_citescore(),
    
    
    # Analysis
    barrierAnalysis = analyse_survey(openSci),
    combinedCite = analyse_citescore(topFactor, citeScore),

    fetchPolicies = fetch_sherpa(combinedCite, key),
    save_sherpa = store_sherpa(fetchPolicies),
    journalPolicies = get_sherpa(save_sherpa),
    combinedPolicies = combine_journals(combinedCite, journalPolicies),

    # Describe
    statsCareer = describe_career(barrierAnalysis),
    statsDiscipline = describe_discipline(barrierAnalysis),
    statsFlow = describe_flow(topFactor, combinedCite, citeScore, combinedPolicies),
    

    # Graphs
  
    careerGraph = graph_careers(barrierAnalysis),
    disciplineGraph = graph_disciplines(barrierAnalysis),
    
    openRidge = graph_openridge(combinedPolicies),
    citeRidge = graph_citeridge(combinedPolicies),
    journalBar = graph_journalbar(combinedPolicies),
    flowGraph = graph_flow(statsFlow),
    


    Report = target(
      command = {
        rmarkdown::render(knitr_in("doc/EmpiricalReport.Rmd"))
        file_out("doc/EmpiricalReport.pdf")
      }
    )
  )