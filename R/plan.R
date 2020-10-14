# Details the workflow for the thesis
# Each function is sourced from a separate
# file for cleanliness, and is processed according to the
# to the plan below.
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
    aggregatePolicies = aggregate_score(combinedPolicies),
    sampleSim = analyse_similarity(aggregatePolicies, citeScore),
    pivotedPolicies = pivot_score(combinedPolicies),

    # Describe
    statsCareer = describe_career(barrierAnalysis),
    statsDiscipline = describe_discipline(barrierAnalysis),
    statsFlow = describe_flow(
      topFactor,
      combinedCite,
      citeScore,
      combinedPolicies
    ),

    # Graphs
    careerGraph = graph_careers(barrierAnalysis, openSci),
    disciplineGraph = graph_disciplines(barrierAnalysis, meta),
    citeRidge = graph_citeridge(aggregatePolicies),
    journalBar = graph_journalbar(pivotedPolicies),
    flowGraph = graph_flow(statsFlow),

    # Markdown
    Report = target(
      command = {
        rmarkdown::render(knitr_in("doc/EmpiricalReport.Rmd"))
        file_out("doc/EmpiricalReport.pdf")
      }
    )
  )