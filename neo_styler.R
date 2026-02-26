#### Function for creating NEO style graphs

neostyler <- function(x, domains="", scoretype="", vectors=F) {
    if (domains %in% "") {
        warning("WARNING: Default is for domains to be in order N, E, O, A, and C. If domains are not in the order or fewer domains are used, use the domains option.")
        domains="NEOAC"
    }

    if (scoretype %in% "") {
        warning("WARNING: Default is for scores to be T-scores (mean = 50, SD = 10)")
        scoretype="tscores"
    }

### Preliminaries
    names(x) <- dvec <- toupper(unlist(strsplit(domains, split=""))) # Gets list and order of domains to be converted into styles and names columns using this list

    if (scoretype=="tscores") { # Converts domain or factor T-scores into z-scores
        x <- (x-50)/10
    }
    else if (scoretype=="rawscores") { # Converts raw scores to z-scores
        warning("WARNING: Raw scores are not standardized using NEO-PI-R/NEO-PI-3 norms.")
        x <- as.data.frame(as.numeric(scale(x)))
    }
    else if (scoretype=="zscores") { # Nothing done to existing z-scores
    }
    else {
        stop("Invalid option")
    }

### Create styles
    cmp1 <- vector() # Sets up two vectors to capture all possible pairs of styles
    cmp2 <- vector()

    for (i in 1:(length(dvec)-1)) {
    for (j in i+1:(length(dvec)-i)) {
        cmp1 <- append(cmp1, dvec[i])
        cmp2 <- append(cmp2, dvec[j])
    }
}

### Populate a data frame with style vectors
    stylevecs <- as.data.frame(matrix(ncol=length(cmp1), nrow=nrow(x))) # Create empty data frame

    for (i in 1:(length(cmp1))) { # Populate it!
    stylevecs[,i] <- sqrt((x[cmp1[i]]^2) + (x[cmp2[i]]^2))
}

    names(stylevecs) <- vectornames <- paste("vec", cmp1, cmp2,sep="_") # Create names for style vectosr

### Populate data frame with each person's assigned style (if they have one)
    styleassd <- as.data.frame((matrix(ncol=length(cmp1), nrow=nrow(x), "0. Unstyled"))) # Create data frame for style assignment; all 0s!

for (i in 1:(length(cmp1))) { # Populate it!
    styleassd[(x[cmp1[i]] > 0) & (x[cmp2[i]] < 0) & (stylevecs[i] > .5), i] <- paste("1. ", cmp1[i],"+",cmp2[i],"-", sep="")

    styleassd[(x[cmp1[i]] > 0) & (x[cmp2[i]] > 0) & (stylevecs[i] > .5), i] <- paste("2. ", cmp1[i],"+",cmp2[i],"+", sep="")

    styleassd[(x[cmp1[i]] < 0) & (x[cmp2[i]] > 0) & (stylevecs[i] > .5), i] <- paste("3. ", cmp1[i],"-",cmp2[i],"+", sep="")

    styleassd[(x[cmp1[i]] < 0) & (x[cmp2[i]] < 0) & (stylevecs[i] > .5), i] <- paste("4. ", cmp1[i],"-",cmp2[i],"-", sep="")
}

    tempstylenames <- paste(cmp1, cmp2 ,sep="_") # Create names for style assignment vectors
    tempstylenames[grepl("N_E",tempstylenames)] <- "N_E_Style_of_Well_Being" # Changing style variable names to full names of styles
    tempstylenames[grepl("N_O",tempstylenames)] <- "N_O_Style_of_Defense"
    tempstylenames[grepl("N_A",tempstylenames)] <- "N_A_Style_of_Anger_Control"
    tempstylenames[grepl("N_C",tempstylenames)] <- "N_C_Style_of_Impulse_Control"
    tempstylenames[grepl("E_O",tempstylenames)] <- "E_O_Style_of_Interests"
    tempstylenames[grepl("E_A",tempstylenames)] <- "E_A_Style_of_Interactions"
    tempstylenames[grepl("E_C",tempstylenames)] <- "E_C_Style_of_Activity"
    tempstylenames[grepl("O_A",tempstylenames)] <- "O_A_Style_of_Attitudes"
    tempstylenames[grepl("O_C",tempstylenames)] <- "O_C_Style_of_Learning"
    tempstylenames[grepl("A_C",tempstylenames)] <- "A_C_Style_of_Altruism"

    names(styleassd) <- tempstylenames # Rename style assignments

    styleassd[styleassd=="1. N+E-"] <- "1. Gloomy Pessimists N+E-"
    styleassd[styleassd=="2. N+E+"] <- "2. Strongly Emotional N+E+"
    styleassd[styleassd=="3. N-E+"] <- "3. Upbeat Optimists N-E+"
    styleassd[styleassd=="4. N-E-"] <- "4. Low-keyed N-E-"

    styleassd[styleassd=="1. N+O-"] <- "1. Maladaptive N+O-"
    styleassd[styleassd=="2. N+O+"] <- "2. Hypersensitive N+O+"
    styleassd[styleassd=="3. N-O+"] <- "3. Adaptive N-O+"
    styleassd[styleassd=="4. N-O-"] <- "4. Hyposensitive N-O-"

    styleassd[styleassd=="1. N+A-"] <- "1. Temperamental N+A-"
    styleassd[styleassd=="2. N+A+"] <- "2. Timid N+A+"
    styleassd[styleassd=="3. N-A+"] <- "3. Easy-going N-A+"
    styleassd[styleassd=="4. N-A-"] <- "4. Cold-blooded N-A-"

    styleassd[styleassd=="1. N+C-"] <- "1. Undercontrolled N+C-"
    styleassd[styleassd=="2. N+C+"] <- "2. Overcontrolled N+C+"
    styleassd[styleassd=="3. N-C+"] <- "3. Directed N-C+"
    styleassd[styleassd=="4. N-C-"] <- "4. Relaxed N-C-"

    styleassd[styleassd=="1. E+O-"] <- "1. Mainstream Consumers E+O-"
    styleassd[styleassd=="2. E+O+"] <- "2. Creative Interactors E+O+"
    styleassd[styleassd=="3. E-O+"] <- "3. Introspectors E-O+"
    styleassd[styleassd=="4. E-O-"] <- "4. Homebodies E-O-"

    styleassd[styleassd=="1. E+A-"] <- "1. Leaders E+A-"
    styleassd[styleassd=="2. E+A+"] <- "2. Welcomers E+A+"
    styleassd[styleassd=="3. E-A+"] <- "3. Unassuming E-A+"
    styleassd[styleassd=="4. E-A-"] <- "4. Competitors E-A-"

    styleassd[styleassd=="1. E+C-"] <- "1. Fun-lovers E+C-"
    styleassd[styleassd=="2. E+C+"] <- "2. Go-getters E+C+"
    styleassd[styleassd=="3. E-C+"] <- "3. Plodders E-C+"
    styleassd[styleassd=="4. E-C-"] <- "4. Lethargic E-C-"

    styleassd[styleassd=="1. O+A-"] <- "1. Free-thinkers O+A-"
    styleassd[styleassd=="2. O+A+"] <- "2. Progressives O+A+"
    styleassd[styleassd=="3. O-A+"] <- "3. Traditionalists O-A+"
    styleassd[styleassd=="4. O-A-"] <- "4. Resolute Believers O-A-"

    styleassd[styleassd=="1. O+C-"] <- "1. Dreamers O+C-"
    styleassd[styleassd=="2. O+C+"] <- "2. Good Students O+C+"
    styleassd[styleassd=="3. O-C+"] <- "3. By-the-bookers O-C+"
    styleassd[styleassd=="4. O-C-"] <- "4. Reluctant Scholars O-C-"

    styleassd[styleassd=="1. A+C-"] <- "1. Well-Intentioned A+C-"
    styleassd[styleassd=="2. A+C+"] <- "2. Effective Altruists A+C+"
    styleassd[styleassd=="3. A-C+"] <- "3. Self-promoters A-C+"
    styleassd[styleassd=="4. A-C-"] <- "4. Undistinguished A-C-"

    ifelse(vectors==T, (return(cbind(stylevecs, styleassd))), (return(styleassd)))
}
