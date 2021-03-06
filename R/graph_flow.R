# Graphs the process of data extraction,
# aggregation, processiong, and synthesis
# Similar to a PRISMA diagram
graph_flow <- function(statsFlow){

topfacN     <- statsFlow$topfacN
citeN       <- statsFlow$citeN
differenceN <- statsFlow$differenceN
combinedN   <- statsFlow$combinedN
filteredN   <- statsFlow$filteredN
finalN      <- statsFlow$finalN
sherpadiffN <- statsFlow$sherpadiffN

par(mar = c(1,4,1,4))
openplotmat()
pos <- coordinates(c(1,3,3,3,3,3,3,3,3,3,3))
straightarrow(from = pos[2, ], to = pos[3, ], arr.pos=NULL)
straightarrow(from = pos[4, ], to = pos[3, ], arr.pos=NULL)
straightarrow(from = pos[3, ], to = pos[6, ], arr.pos=NULL)
straightarrow(from = pos[9, ], to = pos[10, ], endhead = TRUE)
straightarrow(from = pos[6, ], to = pos[9, ], endhead = TRUE)
straightarrow(from = pos[9, ], to = pos[15, ], endhead = TRUE)
straightarrow(from = pos[15, ], to = pos[21, ], endhead = TRUE) 
straightarrow(from = pos[21, ], to = pos[22, ], endhead = TRUE)  
straightarrow(from = pos[21, ], to = pos[27, ], endhead = TRUE) 


my_label <- c(1, 2, 3, 4, 7, 6, 5, 10, 9, 8, 13, 12, 11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)
my_text_size <- 1.3
my_edge_length <- 0.09
for (i in 1:length(my_label)) {
      if(i == 2) 
      {
        textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length, lab = paste0("Step 1. Journals \nobtained from TOP \nfactor database \n(n = ", topfacN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      
      } else if (i == 4) {
             textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length*.9, lab = paste0("Step 1. Journals \nobtained from Scopus \ndatabase (n = ", citeN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      } else if (i ==10) {
           textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length*.8, lab = paste0("Step 2. Journals \ndiscarded \n (n = ", differenceN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      } else if (i == 9) {
             textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length*.9, lab = paste0("Step 2. Journals after \nsynthesis \n (n = ", combinedN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      } else if (i == 15) {
             textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length*.8, lab = paste0("Step 3. Journals after \nfilter \n (n = ", filteredN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      } else if (i==22){
              textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length, lab = paste0("Step 4. Journals \nwithout SHERPA \nmatches (n = ", sherpadiffN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      } else if (i==21){
              textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length/2, lab = paste0("Step 4. Journals after \nSHERPA (n = ",finalN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      } else if (i==27){
              textrect(mid = pos[i, ], radx = my_edge_length, rady = my_edge_length/2, lab = paste0("Final  \n (n = ", finalN, ")"), cex = my_text_size, box.col = "#FFFFFF")
      }
       else {
            next
      }
}



}
