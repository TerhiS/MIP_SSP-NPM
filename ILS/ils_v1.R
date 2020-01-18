##------ Wed Mar 06 11:08:01 2019 ------##

# =======================================
# Title: ILS heuristic for the SSP-NPM
# =======================================

# Description: Computes total flowtime / makespan / number of tool switches for the job sequencing and tool switching problm
# param data.list holds the instances
# param m,ma Machines
# param t Tools
# param j,job Jobs
# param c Tool Capacities of the Machines
# param sw Tool Switching Time of the Machines
# param bks Best Known Solution for specific objective
# @bks = {flowtime, makespan, switches}
# param heur Type of Heuristic
# @heur = {GI // "tool switches objective",SPT // "makespan & flowtime objective}

# =======================================
# file and package preparation
# =======================================
setwd("C:/Users/Dorothea/Dropbox/Math_Model")
setwd("C:/Users/Dorothea/Dropbox/Math_Model/greedy")
setwd("C:/Users/Administrator/Dropbox/Math_Model/greedy")

setwd("E:/Dorothea/EURO/Results-II")
# install required packages if not installed
list.of.packages <- c("naturalsort", "tictoc","R.utils","matrixStats","foreach","parallel","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load the required packages
library(matrixStats)
library(R.utils)
library(tictoc)
library("naturalsort")
library(foreach)
library(parallel)
library(doParallel)


# required function that includes sample of length one
resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 

numCores <- detectCores()
registerDoParallel(numCores)

### !!!Compute this before running the process!!! ###
csv_files <- list.files(pattern = 'ins*',all.files = T,full.names = T)
csv_files <- naturalsort(csv_files)
data.list <- lapply(csv_files,read.table,sep=";",header=F)

# ===========================================
################## Start  ######################
# ===========================================

# for all number of perturbations
#for(pert_iter in 1:100){

  # for all instances
for (ins in 1
     #length(data.list)
     ){
    # for all objectives
    for (obj in c(#"flowtime"#,
                  "makespan"
            #      ,"switches"
                  )){
      bs_pert <- matrix(ncol=4,nrow=19)
      colnames(bs_pert) <- c("bks_ts","bks_tft","bks_fmax","time")
      row.names(bs_pert) <- c(seq(from = 1, to = 10, by =1),seq(from=20, to =100, by=10)
                              #,seq(from=200,to=1000,by=100)
                              )

      # =====================================
      ########## Read Input Data  ###########
      # =====================================
      
      #### Part 1 - Parameters ####
  
      if(obj == "flowtime"){
        heur <- "IEACT"
      }
      
      if(obj == "makespan"){
        heur <- "IEACT"
      }
      
      # if(obj == "switches"){
      #   heur <- "IGI"
      # }
      
      #### retrieve input parameters from data frame
      df <- as.data.frame(data.list[[ins]])
      # Columns
      colnames(df) <- c(1:length(df[1,]))
      # Rows
      row.names(df) <- c(1:length(df[,1]))
      
      #### Initialization of parameters & sets (jobs, machines, tools, req_tools, capacity)
      ### Number of jobs
      max_j <- df[1,2]
      
      ### Number of machines
      max_m <- df[1,1]
      
      ### Tools
      # Number of tools
      max_t <- df[1,3]
      # Set of tools required for job j
       req_t <- list()
       for (j in 1:max_j){
         req_t[[j]] <- which(df[((4+max_m):(4+max_m+max_t-1)),j]==1)
         #if(length(t_req_m[[m]]) < cap_m[[m]]){
          #   t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
         #}
       }
      remove(j)
      # job tool matrix (required for tool switches)
      matrix <- df[((4+max_m):length(df[,1])),(1:max_j)]
      row.names(matrix) <- c(1:max_t)
      colnames(matrix) <- c(1:max_j)
      
      pert_iter=100
        #ceiling((max_j*max_m)/2)
      
      ### Capacity cap_m[[m]] of machine m
      cap_m <- list()
      for (m in 1:max_m){
        cap_m[[m]] <- (df[2,m])
      }
      remove(m)
      
      ### Processing Times p_m[[m]] of the jobs on machine m
      p_m <- list()
      for(m in 1:max_m){
        p_m[[m]] <- df[(3+m),(1:max_j)]
      }
      remove(m)
      
      ### Switching Times sw_m[m] of machine m
      sw_m <- array()
      sw_m[1:max_m] <- unlist(df[3,1:max_m])
      
      # Initial completion time f[m] on machine m
      f <- array()
      f[1:max_m] <- 0
      
      ##############################################
    
      #### Part 2 - Solution ####
      
      #### retrieve initial solution and objective values from construction heuristic 
      solution <- read.csv(paste(heur,ins,".csv",sep = ""),quote = "",header = F,sep = '"',colClasses = "character")
      solution <- as.matrix(solution[,-1])
      
      ### number of tool switches switch
      switch <- as.integer(gsub(solution[1,2],pattern = ",",replacement = ""))
      ### total flow time tft
      tft <- as.integer(gsub(solution[2,2],pattern = ",",replacement = ""))
      ### makespan fmax
      fmax <- as.integer(gsub(solution[3,2],pattern = ",",replacement = ""))
      ### job assignment seq[[m]]
      seq <- strsplit(gsub(solution[5,2],pattern = ",",replacement = ""),"c")
      seq <- strsplit(seq[[1]][-1],'"' )
      seq <- strsplit(gsub("[[:punct:]]", "", seq)," ")
      for (m in 1:max_m){
        seq[[m]] <- as.integer(seq[[m]])
      }
      remove(m)
      
      
      ####################################
      
      # ========================================
      # Tool Loading and keep tool needed soones (KTNS)
      # ========================================
      
      # sequence of unique tools that are still required
      t_req_m <- list()
      for (m in 1:max_m) {
        t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[seq[[m]][-1]]))))
        if(length(t_req_m[[m]]) < cap_m[[m]]){
          t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
        }
      }
      remove(m)
      
      # sequence of all tools that are still required
      all_t_req_m <- list()
      for (m in 1:max_m) {
        all_t_req_m[[m]] <- unlist(req_t[seq[[m]][-1]])
      }
      remove(m)
      
      ### auxiliary parameters for the loading
      load_t <- req_t
      aux_req <- t_req_m
      aux_all_req <- all_t_req_m
      
      ### completion time of job j
      ct <- list()
      # number of tools that are required and not in the magazine
      ts_j <- array(dim <- max_j)
      
      ### initial loading (first jobs)
      for (m in 1:max_m) {
        active_j <- seq[[m]][1]
        ts_j[active_j] <- 0
        # if no free slot
        if (length(req_t[[active_j]]) == cap_m[[m]]) {
          ct[active_j] <- p_m[[m]][active_j]
          # if free slot available
        } else if (length((req_t[[active_j]])) < cap_m[[m]]) {
          hu <- match(req_t[[active_j]],aux_req[[m]])
          hu <- hu[!is.na(hu)]
          if (sum(hu) > 0) {
            aux_req[[m]] <- aux_req[[m]][-hu]
          }
          for (free_slot in (length(req_t[[active_j]])+1):cap_m[[m]]) {
            # put next tool in free slot that is required earliest 
            # but only if it is not in use already by first job
            load_t[[active_j]][free_slot] <- aux_req[[m]][1]
            # remove not inserted
            aux_req[[m]] <- aux_req[[m]][-1]
            ct[active_j] <- p_m[[m]][active_j]
          }
        }
      }
      remove(hu)
      remove(m)
      remove(active_j)
      remove(free_slot)
      
      ### tool optimization 
      for (m in 1:max_m) {
        if (length(seq[[m]]) == 1) {
          active_j <- seq[[m]][1]
          ct[active_j] <- p_m[[m]][active_j]
        } else {
          for (active_j_pos in 2:length(seq[[m]])) {
            active_j <- seq[[m]][active_j_pos]
            previous_j <- seq[[m]][active_j_pos-1]
            not_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==F)
            yes_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==T)
            ts_j[active_j] <- length(not_match)
            # if no tools are to be replaced because required tools are already loaded
            if (length(yes_match) == length(req_t[[active_j]])) {
              load_t[[active_j]] <- load_t[[previous_j]]
              aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
              ct[active_j] <- ct[previous_j] + p_m[[m]][active_j]
            }
            # if tools need to be replaced
            else if (length(yes_match) < length(req_t[[active_j]])) {
              # if all tools need to be replaced 
              if (ts_j[active_j] == cap_m[[m]]) {
                load_t[[active_j]] <- req_t[[active_j]]
                aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                ct[active_j] <- ct[previous_j]+p_m[[m]][active_j]+ts_j[active_j]*sw_m[[m]][1]
              }
              # if any tools need to be replaced (less than magazine capacity)
              else if (ts_j[active_j] < cap_m[[m]]) {
                load_t[[active_j]] <- req_t[[active_j]]
                aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                # which tools are already loaded and later required
                inta <- intersect(load_t[[previous_j]],aux_all_req[[m]])
                # which tools can theoretically be kept
                inta <- setdiff(inta,load_t[[active_j]])
                
                # if there are more free slots than tools required in the future and not in magazine
                if ((cap_m[[m]]-length(req_t[[active_j]])) > length(inta)) {
                  # replace the free slots with tools needed soonest (and not in the magazin) 
                  load_t[[active_j]] <- append(load_t[[active_j]],inta)
                  # randomly keep old tools until magazin is full
                  old_t <- setdiff(load_t[[previous_j]],union(load_t[[active_j]],aux_all_req[[m]]))
                  load_t[[active_j]] <- append(load_t[[active_j]],old_t[1:(cap_m[[m]]-length(load_t[[active_j]]))])
                  ct[active_j] <- ct[previous_j]+p_m[[m]][active_j] + ts_j[active_j]*sw_m[[m]][1]
                  remove(old_t)
                }
                # if job needs exactly cap tool
                else if ((cap_m[[m]]-length(req_t[[active_j]])) == 0 ) {
                  load_t[[active_j]] = req_t[[active_j]]
                  ct[active_j] = ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                  
                } 
                # if there are less free slots than tools required in future and not in magazine 
                else if ((cap_m[[m]]-length(req_t[[active_j]])) <= length(inta) & (cap_m[[m]]-length(req_t[[active_j]])) > 0) {
                  # replace the free slots with tools needed soonest (and not in the magazin)
                  load_t[[active_j]] <- append(load_t[[active_j]],inta[1:(cap_m[[m]] - length(req_t[[active_j]]))])
                  ct[active_j] <- ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                }
              }
            }
          }
        }
      }
      remove(m)
      remove(active_j_pos)
      
      # ========================================
      
      #### Part 3 - best known results ####
      ct_opt <- ct
      ts_opt <- ts_j
      loads_opt <- array()
      for(j in 1:max_j){
        loads_opt[j] <- load_t[j]
      }
      remove(j)
      seq_opt <- seq
      switch_opt <- switch
      fmax_opt <- fmax
      tft_opt <- tft
      
      #### Set initial solution as best known solution bks
      #### for different objectives
      if (obj == "flowtime"){
        bks <- tft
      }
      if (obj == "makespan"){
        bks <- fmax
      }
      # if (obj == "switches"){
      #   bks <- switch
      # }
      
      ##############################################
    
      # =======================================
      # Local Search (1) around initial solution
      # =======================================
    
      ### Start of computation time counter
      tic("ILS")
      tic("100")
      tic("90")
      tic("80")
      tic("70")
      tic("60")
      tic("50")
      tic("40")
      tic("30")
      tic("20")
      tic("10")
      tic("9")
      tic("8")
      tic("7")
      tic("6")
      tic("5")
      tic("4")
      tic("3")
      tic("2")
      tic("1")

      bksR <- bks
      seqR <- seq
      loadsR <- array()
    
      #### Set improvement flag as TRUE
      foreach(ma=1:max_m) %do% {
        
        improved <- T
        
        while (improved == T){
        
          improved <- F
          # new sequence is similar to old sequence
          seq2 <- seqR
          # retain improved best known solution
          bks <- bksR
          # for each job on that machine
          for(j1_pos in 1:(length(seqR[[ma]])-1)){
            # and any other job on that machine
            for(j2_pos in (j1_pos+1):length(seqR[[ma]])){
              
              seq2[[ma]][j1_pos] <- seqR[[ma]][j2_pos]
              seq2[[ma]][j2_pos] <- seqR[[ma]][j1_pos]
              
              # ========================================
              # Tool Loading and keep tool needed soones (KTNS)
              # ========================================
              # sequence of unique tools that are still required
              t_req_m=list()
              for (m in 1:max_m) {
                t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[seq2[[m]][-1]]))))
                if(length(t_req_m[[m]]) < cap_m[[m]]){
                  t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
                }
              }
              remove(m)
              
              # sequence of all tools that are still required
              all_t_req_m <- list()
              for (m in 1:max_m) {
                all_t_req_m[[m]] <- unlist(req_t[seq2[[m]][-1]])
              }
              remove(m)
              
              ### auxiliary parameters for the loading
              load_t <- req_t
              aux_req <- t_req_m
              aux_all_req <- all_t_req_m
              
              ### completion time of job j
              ct <- list()
              # number of tools that are required and not in the magazine
              ts_j <- array(dim <- max_j)
              
              ### initial loading (first jobs)
              for (m in 1:max_m) {
                active_j <- seq2[[m]][1]
                ts_j[active_j] <- 0
                # if no free slot
                if (length(req_t[[active_j]]) == cap_m[[m]]) {
                  ct[active_j] <- p_m[[m]][active_j]
                  # if free slot available
                } else if (length((req_t[[active_j]])) < cap_m[[m]]) {
                  hu <- match(req_t[[active_j]],aux_req[[m]])
                  hu <- hu[!is.na(hu)]
                  if (sum(hu) > 0) {
                    aux_req[[m]] <- aux_req[[m]][-hu]
                  }
                  for (free_slot in (length(req_t[[active_j]])+1):cap_m[[m]]) {
                    # put next tool in free slot that is required earliest 
                    # but only if it is not in use already by first job
                    load_t[[active_j]][free_slot] <- aux_req[[m]][1]
                    # remove not inserted
                    aux_req[[m]] <- aux_req[[m]][-1]
                    ct[active_j] <- p_m[[m]][active_j]
                  }
                }
              }
              remove(hu)
              remove(m)
              remove(active_j)
              remove(free_slot)
              
              ### tool optimization 
              for (m in 1:max_m) {
                if (length(seq2[[m]]) == 1) {
                  active_j <- seq2[[m]][1]
                  ct[active_j] <- p_m[[m]][active_j]
                } else {
                  for (active_j_pos in 2:length(seq2[[m]])) {
                    active_j <- seq2[[m]][active_j_pos]
                    previous_j <- seq2[[m]][active_j_pos-1]
                    not_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==F)
                    yes_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==T)
                    ts_j[active_j] <- length(not_match)
                    # if no tools are to be replaced because required tools are already loaded
                    if (length(yes_match) == length(req_t[[active_j]])) {
                      load_t[[active_j]] <- load_t[[previous_j]]
                      aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                      ct[active_j] <- ct[previous_j] + p_m[[m]][active_j]
                    }
                    # if tools need to be replaced
                    else if (length(yes_match) < length(req_t[[active_j]])) {
                      # if all tools need to be replaced 
                      if (ts_j[active_j] == cap_m[[m]]) {
                        load_t[[active_j]] <- req_t[[active_j]]
                        aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                        ct[active_j] <- ct[previous_j]+p_m[[m]][active_j]+ts_j[active_j]*sw_m[[m]][1]
                      }
                      # if any tools need to be replaced (less than magazine capacity)
                      else if (ts_j[active_j] < cap_m[[m]]) {
                        load_t[[active_j]] <- req_t[[active_j]]
                        aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                        inta <- intersect(load_t[[previous_j]],aux_all_req[[m]])
                        hu <- which(req_t[[active_j]] %in% intersect(load_t[[previous_j]],aux_all_req[[m]]))
                        if (sum(hu) > 0) {
                          inta <- inta[-hu]
                        }
                        remove(hu)
                        # if there are more free slots than tools required in the future and not in magazine
                        if ((cap_m[[m]]-length(req_t[[active_j]])) > length(inta)) {
                          # replace the free slots with tools needed soonest (and not in the magazin) 
                          load_t[[active_j]] <- append(load_t[[active_j]],inta)
                          # randomly keep old tools until magazin is full
                          old_t <- setdiff(load_t[[previous_j]],union(load_t[[active_j]],aux_all_req[[m]]))
                          load_t[[active_j]] <- append(load_t[[active_j]],old_t[1:(cap_m[[m]]-length(load_t[[active_j]]))])
                          ct[active_j] <- ct[previous_j]+p_m[[m]][active_j] + ts_j[active_j]*sw_m[[m]][1]
                          remove(old_t)
                        }
                        # if job needs exactly cap tool
                        else if ((cap_m[[m]]-length(req_t[[active_j]])) == 0 ) {
                          load_t[[active_j]] = req_t[[active_j]]
                          ct[active_j] = ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                          
                        } 
                        # if there are less free slots than tools required in future and not in magazine 
                        else if ((cap_m[[m]]-length(req_t[[active_j]])) <= length(inta) & (cap_m[[m]]-length(req_t[[active_j]])) > 0) {
                          # replace the free slots with tools needed soonest (and not in the magazin)
                          load_t[[active_j]] <- append(load_t[[active_j]],inta[1:(cap_m[[m]] - length(req_t[[active_j]]))])
                          ct[active_j] <- ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                        }
                      }
                    }
                  }
                }
              }
              remove(m)
              remove(active_j_pos)
              
              # ========================================
              
              ####################################
              
              if (obj == "flowtime"){
                obj_new <- Reduce("+",ct)
              }
              if (obj == "makespan"){
                obj_new <- Reduce(max,ct)
              }
              if (obj == "switches"){
                obj_new <- sum(ts_j[!is.na(ts_j)])
              }
              # ========================================
              # Acceptance Criterion
              # ========================================
              # if the solution is better than the best known solution
              if (obj_new < bks){
                # save new bks
                # bks is only updated after all loops break
                bksR <- obj_new
                # use new sequence
                seqR <- seq2
                ### overwrite (current) best solutions
                ct_opt <- ct
                ts_opt <- ts_j
                loads_opt <- array()
                for(j in 1:max_j){
                  loads_opt[j] <- load_t[j]
                }
                remove(j)
                seq_opt <- seq2
                switch_opt=sum(ts_j[!is.na(ts_j)])
                fmax_opt=Reduce(max,ct)
                tft_opt=Reduce("+",ct)
                
                # set improved to TRUE
                improved <- T
                
                
                break
              } else {
                ### otherwise use old sequence and old solution
                seq2 <- seqR
              }
              #if (obj_new < bks) break
            }
            if (obj_new < bks) break
          }
          remove(j1_pos,j2_pos)
          #if(obj_new < bks) break
        }
        
      }
      
      rm(j1_pos,j2_pos,active_j)
  
      ##############################################
      ### Result Local Search
      ##############################################
      ### sequence
      seq_ls <- seq_opt
      ### number of tool switches
      switch_ls <- switch_opt
      ### makespan
      fmax_ls <- fmax_opt
      ### total flow time
      tft_ls <- tft_opt
      ### completion times
      ct_ls <- ct_opt
      ### tool switches per job
      ts_ls <- ts_opt

      # =====================================
      ########## Perturbation  ###########
      # =====================================
    
      ### required variables
      bksR <- bks
      seqR <- seq_ls
  
      tsR <- ts_ls
      ctR <- ct_ls
    
      pert_again <- T
      pert_count <- 0
      
      while (pert_again == T) {
        
        pert_again <- F
        pert_count <- pert_count+1
        
        # seq_pert is similar to seqR but not
        seq_pert <- seqR
        if(obj == "flowtime" | obj=="makespan") {
          
          ### bottleneck machine with highest makespan (same makespan?) of starting solution
          ct_sums=list()
          for(m in 1:max_m){
            ct_sums[[m]] <- sum2(unlist(ctR), seqR[[m]]) 
          }
          remove(m)
          
          # bottleneck_m <- which(unlist(ct_sums) == max(unlist(ct_sums)))
          # if more than one machine
          # bottleneck_m <- resamp(bottleneck_m,1)
          bm_order <- order(unlist(ct_sums),decreasing = T,na.last = NA)
          
          assign_m <- F
          while(assign_m==F){
            assign_m <- T
            for (machine in bm_order){
              ### sum of main processing and setup
              bottleneck_j <- seqR[[machine]]
              p_sum <- list()
              for(j in bottleneck_j){
                p_sum[j] <- (tsR[j]*sw_m[[machine]])+p_m[[machine]][j]
              }
              remove(j)
              p_sum[which(p_sum =='NULL')] <- NA
              p_sum_order <- order(unlist(p_sum),decreasing = T,na.last = NA)
              other_m <- c(1:max_m)[c(1:max_m != machine)]
              # Select the worst job
              poss=F
              while (poss == F) {
                poss <- T
                 for(n in 1:length(p_sum_order)) {
                   select_j <- p_sum_order[[n]]
                   for(m in other_m) {
                     if(length(req_t[[select_j]]) <= cap_m[m]) {
                       j_insert <- select_j
                       poss <- T
                       assign_m <- T
                       break
                     } else {
                       n <- n+1
                     }
                     if(n == length(p_sum_order)) break
                   }
                   if(length(req_t[[select_j]]) <= cap_m[m]) break
                   if(n == length(p_sum_order)) break
                 }
              }
              if(length(req_t[[select_j]]) <= cap_m[m]) break
               remove(n)
               remove(m)
            }
          }
          bottleneck_m <- machine
          remove(machine)
         
         #### Generate new sequence
         # Calculate all possible Phi Values 
         phi <- data.frame(matrix(ncol=max_j,nrow=max_m))
         poss_insert_m <- which(cap_m >= length(req_t[[j_insert]]))
         poss_insert_m <- poss_insert_m[poss_insert_m != bottleneck_m]
      
         for (m in poss_insert_m) {
           l <- length(seqR[[m]])+1
           for (pos in 1:l){
             if(pos == 1){
               ts_after <- length(setdiff(req_t[[seqR[[m]][pos]]],req_t[[j_insert]]))
               pt_pert <- (p_m[[m]][j_insert]+(sw_m[m]*ts_after))
               phi[m,pos] <- pt_pert
      
             }
             if(pos == l) {
               ts_before  <- length(setdiff(req_t[[j_insert]],req_t[[seqR[[m]][length(seqR[[m]])]]]))
               phi[m,pos] <- p_m[[m]][j_insert]+(sw_m[m] * ts_before)
             }
             else if (pos > 1 && pos < l) {
               ts_before <- length(setdiff(req_t[[j_insert]],req_t[[seqR[[m]][(pos-1)]]]))
               ts_after <- length(setdiff(req_t[[seqR[[m]][pos]]],req_t[[j_insert]]))
               phi[m,pos] <- p_m[[m]][j_insert] + (sw_m[m] * ts_before) + (sw_m[m] * ts_after)
             }
           }
         }
         remove(m)
         remove(pos)
         
         # position with the lowest processing time and approximated tool switching times 
         phi_mins <- which(phi == min(phi,na.rm = T),arr.ind = T)
         phi_min <- resamp(1:nrow(phi_mins),1)
          
         ins_machine <- phi_mins[phi_min,1]
         ins_pos <- phi_mins[phi_min,2]
          
         ### New sequence after perturbation
         # Remove job j_insert from old sequence on machine bottleneck_m
         seq_pert[[bottleneck_m]] = seq_pert[[bottleneck_m]][seq_pert[[bottleneck_m]] != j_insert]
         # Insert the job j_insert on ins_machine in position ins_pos
         seq_pert[[ins_machine]] = insert(seq_pert[[ins_machine]],ats=ins_pos,values=j_insert)
        }

        # ========================================
        # Tool Loading and keep tool needed soones (KTNS)
        # ========================================
        
        # sequence of unique tools that are still required
        # !!!! Achtung hier war fehler wenn length(seqpert==1)
        t_req_m <- list()
        for (m in 1:max_m) {
          if(length(seq_pert[[m]])==1){
            t_req_m[[m]] <- unlist(req_t[seq_pert[[m]][1]])
            t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
          }
          else{
            t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[seq_pert[[m]][-1]]))))
            if(length(t_req_m[[m]]) < cap_m[[m]]){
              t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
            }
          }
        }
        remove(m)

        # sequence of all tools that are still required
        all_t_req_m <- list()
        for (m in 1:max_m) {
          all_t_req_m[[m]] <- unlist(req_t[seq_pert[[m]][-1]])
        }
        remove(m)
        
        ### auxiliary parameters for the loading
        load_t <- req_t
        aux_req <- t_req_m
        aux_all_req <- all_t_req_m
        
        ### completion time of job j
        ct <- list()
        # number of tools that are required and not in the magazine
        ts_j <- array(dim <- max_j)
        
        ### initial loading (first jobs)
        for (m in 1:max_m) {
          active_j <- seq_pert[[m]][1]
          ts_j[active_j] <- 0
          # if no free slot
          if (length(req_t[[active_j]]) == cap_m[[m]]) {
            ct[active_j] <- p_m[[m]][active_j]
            # if free slot available
          } else if (length((req_t[[active_j]])) < cap_m[[m]]) {
            hu <- match(req_t[[active_j]],aux_req[[m]])
            hu <- hu[!is.na(hu)]
            if (sum(hu) > 0) {
              aux_req[[m]] <- aux_req[[m]][-hu]
            }
            for (free_slot in (length(req_t[[active_j]])+1):cap_m[[m]]) {
              # put next tool in free slot that is required earliest 
              # but only if it is not in use already by first job
              load_t[[active_j]][free_slot] <- aux_req[[m]][1]
              # remove not inserted
              aux_req[[m]] <- aux_req[[m]][-1]
              ct[active_j] <- p_m[[m]][active_j]
            }
          }
        }
        remove(hu)
        remove(m)
        remove(active_j)
        remove(free_slot)
        
        ### tool optimization 
        for (m in 1:max_m) {
          if (length(seq_pert[[m]]) == 1) {
            active_j <- seq_pert[[m]][1]
            ct[active_j] <- p_m[[m]][active_j]
          } else {
            for (active_j_pos in 2:length(seq_pert[[m]])) {
              active_j <- seq_pert[[m]][active_j_pos]
              previous_j <- seq_pert[[m]][active_j_pos-1]
              not_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==F)
              yes_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==T)
              ts_j[active_j] <- length(not_match)
              # if no tools are to be replaced because required tools are already loaded
              if (length(yes_match) == length(req_t[[active_j]])) {
                load_t[[active_j]] <- load_t[[previous_j]]
                aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                ct[active_j] <- ct[previous_j] + p_m[[m]][active_j]
              }
              # if tools need to be replaced
              else if (length(yes_match) < length(req_t[[active_j]])) {
                # if all tools need to be replaced 
                if (ts_j[active_j] == cap_m[[m]]) {
                  load_t[[active_j]] <- req_t[[active_j]]
                  aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                  ct[active_j] <- ct[previous_j]+p_m[[m]][active_j]+ts_j[active_j]*sw_m[[m]][1]
                }
                # if any tools need to be replaced (less than magazine capacity)
                else if (ts_j[active_j] < cap_m[[m]]) {
                  load_t[[active_j]] <- req_t[[active_j]]
                  aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                  # which tools are already loaded and later required
                  inta <- intersect(load_t[[previous_j]],aux_all_req[[m]])
                  # which tools can theoretically be kept
                  inta <- setdiff(inta,load_t[[active_j]])
             
                  # if there are more free slots than tools required in the future and not in magazine
                  if ((cap_m[[m]]-length(req_t[[active_j]])) > length(inta)) {
                    # replace the free slots with tools needed soonest (and not in the magazin) 
                    load_t[[active_j]] <- append(load_t[[active_j]],inta)
                    # randomly keep old tools until magazin is full
                    old_t <- setdiff(load_t[[previous_j]],union(load_t[[active_j]],aux_all_req[[m]]))
                    load_t[[active_j]] <- append(load_t[[active_j]],old_t[1:(cap_m[[m]]-length(load_t[[active_j]]))])
                    ct[active_j] <- ct[previous_j]+p_m[[m]][active_j] + ts_j[active_j]*sw_m[[m]][1]
                    remove(old_t)
                  }
                  # if job needs exactly cap tool
                  else if ((cap_m[[m]]-length(req_t[[active_j]])) == 0 ) {
                    load_t[[active_j]] = req_t[[active_j]]
                    ct[active_j] = ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                    
                  } 
                  # if there are less free slots than tools required in future and not in magazine 
                  else if ((cap_m[[m]]-length(req_t[[active_j]])) <= length(inta) & (cap_m[[m]]-length(req_t[[active_j]])) > 0) {
                    # replace the free slots with tools needed soonest (and not in the magazin)
                    load_t[[active_j]] <- append(load_t[[active_j]],inta[1:(cap_m[[m]] - length(req_t[[active_j]]))])
                    ct[active_j] <- ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                  }
                }
              }
            }
          }
        }
        remove(m)
        remove(active_j_pos)
        
        # ========================================
        # Result Perturbation
        # ========================================

        ct_pert <- ct
        ts_pert <- ts_j
        load_pert <- array()
        for(j in 1:max_j){
          load_pert[j] <- load_t[j]
        }
        rm(j)
        
        if (obj == "flowtime"){
          obj_pert <- Reduce("+",ct_pert)
          bks_pert <- obj_pert
        }
        if (obj == "makespan"){
          obj_pert <- Reduce(max,ct_pert)
          bks_pert <- obj_pert
        }
        # if (obj == "switches"){
        #   obj_pert <- sum(ts_j[!is.na(ts_pert)])
        #   bks_pert <- obj_pert
        # }
      
        # if the solution is better than the best known solution
        if (obj_pert < bks){
          # save new bks
          bks <- obj_pert
          ### overwrite (current) best solutions
          ct_opt <- ct_pert
          ts_opt <- ts_pert
          loads_opt <- array()
          for(j in 1:max_j){
            loads_opt[j] <- load_t[j]
          }
          remove(j)
          seq_opt <- seq_pert
          switch_opt <- sum(ts_j[!is.na(ts_j)])
          fmax_opt <- Reduce(max,ct)
          tft_opt <- Reduce("+",ct)
        }
        #######################################
   
        # =======================================
        # Local Search (2) around initial solution
        # =======================================
        
        # Do local search on perturbed solution
        # The best solution of the local search is retained
        # If no better solution is found restart the perturbation unless the max perturbation count is reached
        
        #### Set improvement flag as TRUE
        improved <- T
        ls_count <- 0

        # in any case use new perturbed sequence and new loading
        seqR <- seq_pert
        loadsR <- load_pert
        bksR <- bks_pert
      
        foreach(ma=1:max_m) %do% {
          
          improved <- T
          
          while (improved == T){

            improved <- F
            # new sequence is similar to old sequence
            seq2 <- seqR
            # retain improved best known solution
            bks_pert <- bksR
            # for each job on that machine
            for(j1_pos in 1:(length(seqR[[ma]])-1)){
              # and any other job on that machine
              for(j2_pos in (j1_pos+1):length(seqR[[ma]])){
                
                seq2[[ma]][j1_pos] <- seqR[[ma]][j2_pos]
                seq2[[ma]][j2_pos] <- seqR[[ma]][j1_pos]
                # ========================================
                # Tool Loading and keep tool needed soones (KTNS)
                # ========================================
                # sequence of unique tools that are still required
                t_req_m=list()
                for (m in 1:max_m) {
                  t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[seq2[[m]][-1]]))))
                  if(length(t_req_m[[m]]) < cap_m[[m]]){
                    t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
                  }
                }
                remove(m)
                
                # sequence of all tools that are still required
                all_t_req_m <- list()
                for (m in 1:max_m) {
                  all_t_req_m[[m]] <- unlist(req_t[seq2[[m]][-1]])
                }
                remove(m)
                
                ### auxiliary parameters for the loading
                load_t <- req_t
                aux_req <- t_req_m
                aux_all_req <- all_t_req_m
                
                ### completion time of job j
                ct <- list()
                # number of tools that are required and not in the magazine
                ts_j <- array(dim <- max_j)
                
                ### initial loading (first jobs)
                for (m in 1:max_m) {
                  active_j <- seq2[[m]][1]
                  ts_j[active_j] <- 0
                  # if no free slot
                  if (length(req_t[[active_j]]) == cap_m[[m]]) {
                    ct[active_j] <- p_m[[m]][active_j]
                    # if free slot available
                  } else if (length((req_t[[active_j]])) < cap_m[[m]]) {
                    hu <- match(req_t[[active_j]],aux_req[[m]])
                    hu <- hu[!is.na(hu)]
                    if (sum(hu) > 0) {
                      aux_req[[m]] <- aux_req[[m]][-hu]
                    }
                    for (free_slot in (length(req_t[[active_j]])+1):cap_m[[m]]) {
                      # put next tool in free slot that is required earliest 
                      # but only if it is not in use already by first job
                      load_t[[active_j]][free_slot] <- aux_req[[m]][1]
                      # remove not inserted
                      aux_req[[m]] <- aux_req[[m]][-1]
                      ct[active_j] <- p_m[[m]][active_j]
                    }
                  }
                }
                remove(hu)
                remove(m)
                remove(active_j)
                remove(free_slot)
                
                ### tool optimization 
                for (m in 1:max_m) {
                  if (length(seq2[[m]]) == 1) {
                    active_j <- seq2[[m]][1]
                    ct[active_j] <- p_m[[m]][active_j]
                  } else {
                    for (active_j_pos in 2:length(seq2[[m]])) {
                      active_j <- seq2[[m]][active_j_pos]
                      previous_j <- seq2[[m]][active_j_pos-1]
                      not_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==F)
                      yes_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==T)
                      ts_j[active_j] <- length(not_match)
                      # if no tools are to be replaced because required tools are already loaded
                      if (length(yes_match) == length(req_t[[active_j]])) {
                        load_t[[active_j]] <- load_t[[previous_j]]
                        aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                        ct[active_j] <- ct[previous_j] + p_m[[m]][active_j]
                      }
                      # if tools need to be replaced
                      else if (length(yes_match) < length(req_t[[active_j]])) {
                        # if all tools need to be replaced 
                        if (ts_j[active_j] == cap_m[[m]]) {
                          load_t[[active_j]] <- req_t[[active_j]]
                          aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                          ct[active_j] <- ct[previous_j]+p_m[[m]][active_j]+ts_j[active_j]*sw_m[[m]][1]
                        }
                        # if any tools need to be replaced (less than magazine capacity)
                        else if (ts_j[active_j] < cap_m[[m]]) {
                          load_t[[active_j]] <- req_t[[active_j]]
                          aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
                          # which tools are already loaded and later required
                          inta <- intersect(load_t[[previous_j]],aux_all_req[[m]])
                          # which tools can theoretically be kept
                          inta <- setdiff(inta,load_t[[active_j]])
                          
                          remove(hu)
                          # if there are more free slots than tools required in the future and not in magazine
                          if ((cap_m[[m]]-length(req_t[[active_j]])) > length(inta)) {
                            # replace the free slots with tools needed soonest (and not in the magazin) 
                            load_t[[active_j]] <- append(load_t[[active_j]],inta)
                            # randomly keep old tools until magazin is full
                            old_t <- setdiff(load_t[[previous_j]],union(load_t[[active_j]],aux_all_req[[m]]))
                            load_t[[active_j]] <- append(load_t[[active_j]],old_t[1:(cap_m[[m]]-length(load_t[[active_j]]))])
                            ct[active_j] <- ct[previous_j]+p_m[[m]][active_j] + ts_j[active_j]*sw_m[[m]][1]
                            remove(old_t)
                          }
                          # if job needs exactly cap tool
                          else if ((cap_m[[m]]-length(req_t[[active_j]])) == 0 ) {
                            load_t[[active_j]] = req_t[[active_j]]
                            ct[active_j] = ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                            
                          } 
                          # if there are less free slots than tools required in future and not in magazine 
                          else if ((cap_m[[m]]-length(req_t[[active_j]])) <= length(inta) & (cap_m[[m]]-length(req_t[[active_j]])) > 0) {
                            # replace the free slots with tools needed soonest (and not in the magazin)
                            load_t[[active_j]] <- append(load_t[[active_j]],inta[1:(cap_m[[m]] - length(req_t[[active_j]]))])
                            ct[active_j] <- ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
                          }
                        }
                      }
                    }
                  }
                }
                remove(m)
                remove(active_j_pos)
                
                ####################################
                
                if (obj == "flowtime") {
                  obj_new <- Reduce("+",ct)
                }
                if (obj == "makespan") {
                  obj_new <- Reduce(max,ct)
                }

                if (obj_new < bks_pert) {
                  # restart local search with new solution
                  bksR <- obj_new
                  bks_pert <- obj_new
                  seqR <- seq2
                  
                  # overwrite solution for perturbation
                  loadsR=array()
                  for(j in 1:max_j){
                    loadsR[j] <- load_t[j]
                  }
                  remove(j)
                  ctR <- ct
                  tsR <- ts_j
                  
                  # set improved to TRUE
                  improved <- T
                  break
                } else {
                ### otherwise use old sequence and old solution
                seq2 <- seqR
                }
              }
              if (obj_new < bks_pert) break
            }
            if(obj_new < bks_pert)  break
          }
          # falls Lösung nicht verbessert, aber schon 3 Perturbationen

        }
        remove(ma)
        remove(j1)
        remove(j2)
        remove(active_j)    
        
        ##############################################
        ### Result Local Search (2)
        ##############################################
        
        
        # falls Lösung verbessert
        if(bks_pert < bks){
          pert_again <- T
          # seqR, ctR, tsR, loadsR already oK
          # update best solution
          bks <- bksR
          seq_opt <- seqR
          switch_opt <- sum(ts_j[!is.na(tsR)])
          fmax_opt <- Reduce(max,ctR)
          tft_opt <- Reduce("+",ctR)
         
          ct_opt <- ctR
          ts_opt <- tsR
          loads_opt <- array()
          for(j in 1:max_j){
            loads_opt[j] <- load_t[j]
          }
          rm(j)
        }
        
        time_pert <- c(seq(from = 1, to = 10, by =1),seq(from=20, to =100, by=10)#,seq(from=200,to = 1000,by = 100)
                       )
        if(pert_count%in%time_pert){
          toc(log=T,quiet = T)
          timep <- tic.log(format=T)
          bs_pert[which(time_pert==pert_count),1] <- switch_opt
          bs_pert[which(time_pert==pert_count),2] <- tft_opt
          bs_pert[which(time_pert==pert_count),3] <- fmax_opt
          bs_pert[which(time_pert==pert_count),4] <- paste0(timep[[1]])
          tic.clearlog()
        }

        # falls nicht verbessert, aber noch nicht 3 Perturbationen durchgeführt
        # beste Lösung aus Pert+LS wird Ausgangslösung
        if(bks_pert >= bks && pert_count < pert_iter) {
          pert_again <- T
          # use best solution from local search or perturbation
          # seqR and loadsR already oK
          ctR <- ct_pert
          tsR <- ts_pert
          loadsR <- load_pert
          seqR <- seq_pert
          bksR <- bks_pert
        }

        # falls Lösung nicht verbessert, aber schon 3 Perturbationen
        if (bks_pert >= bks && pert_count >= pert_iter) break
      }
      
      ##############################################
      
      toc(log = T,quiet = T)
      c_time <- tic.log(format = T)
      tic.clear()
      tic.clearlog()
  
      # =====================================
      # Output
      # =====================================
      # transform lists to writable format
      seq_opt <- capture.output(cat(paste0(strwrap(seq_opt))))
      loads_opt <- capture.output(cat(paste0(strwrap(loads_opt))))
      
      # Summary of result to file
      #setwd("E:/Dorothea/EURO/Results_Pert-I/")
      f2 <- paste0("./Pert_bs",obj,ins,".csv",sep=";")
      write.table(bs_pert,paste("./Pert_bs",obj,ins,".csv",sep = ""),sep=";",row.names = T,col.names = F)
      
      
      # generate solution
      mat <- as.matrix(rbind(switch_opt,tft_opt,fmax_opt,c_time[1],seq_opt,loads_opt))
      f3 <- paste0("./Pert",pert_iter,"_",obj,heur,ins,".csv",sep = "")
      # write output to file
      if (file.exists(f3) == T) {
        print("file already exists")
      }
      if (file.exists(f3) == F) {
        write.table(mat,paste("./Pert",pert_iter,"_",obj,heur,ins,".csv",sep = ""),sep=",",row.names = T,col.names = F)
      }
      
      # =====================================
      # remove all variables and parameters except instances and instance counter
      rm(list=ls()[! ls() %in% c("data.list","csv_files","ins","obj","resamp")])
    
    }
}
#}




  