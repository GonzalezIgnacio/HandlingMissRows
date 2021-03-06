MIMFA <- function(object, ncomp=2, M=NULL, estimeNC=FALSE,
                    maxIter=500, tol=1e-10) {
    
    
    ##- checking general input arguments -------------------------------------#
    ##------------------------------------------------------------------------#
    
    ##- there are no missing individuals
    if (length(unlist(missingIndv(object))) == 0) {
        stop("no missing individuals in the data tables, MI is not useful.", 
            call.=FALSE)
    }
    
    ##- object
    if (class(object) != "MIDTList") {
        stop("'object' must be an object of class 'MIDTList'.", call.=FALSE)
    }
    
    ##- ncomp
    if (is.null(ncomp) || !is.numeric(ncomp) || (ncomp < 2) || 
        !is.finite(ncomp)) {
        stop("invalid number of components, 'ncomp'. It must be a positive", 
            " integer", call.=FALSE)
    }
    
    cond <- min(nrow(colData(object)) - 1,
                sum(vapply(assays(object), nrow, 1L)))
    
    if (ncomp > cond) {
        stop("'ncomp' must be a numeric value lower or equal to ",
            cond, call.=FALSE)
    }
    
    ncomp <- round(ncomp)
    
    ##- M (number of imputations)
    if (is.null(M)) { M <- 30 }
    
    if (!is.numeric(M) || M <= 1 || !is.finite(M)) {
        stop("invalid number of imputations, 'M'. It must be a positive",
            " integer", call.=FALSE)
    }
    
    M <- round(M)
    
    ##- estimeNC
    if (length(estimeNC) != 1 || !is.logical(estimeNC)) {
        stop("'estimeNC' must be either TRUE or FALSE", call.=FALSE)
    }
    
    ##- maxIter
    if (length(maxIter) != 1) {
        stop("'maxIter' must be a single value", call.=FALSE)
    }
    
    if (is.null(maxIter) || maxIter < 1 || !is.finite(maxIter) ||
        !is.numeric(maxIter)) {
        stop("invalid value for 'maxIter'. It must be a positive integer",
            call.=FALSE)
    }
    
    maxIter <- round(maxIter)
    
    ##- tol
    if (length(tol) != 1) {
        stop("'tol' must be a single value", call.=FALSE)
    }
    
    if (is.null(tol) || tol < 0 || !is.finite(tol)) {
        stop("invalid value for 'tol'. It must be a positive number",
            call.=FALSE)
    }
    
    ##- end checking ---------------------------------------------------------#
    
    
    ##- initialization of variables ------------------------------------------#
    ##------------------------------------------------------------------------#
    
    ##- internal function for inserting NA in missing columns ----#
    ##------------------------------------------------------------#
    insertCols <- function(mat, id) {
        
        mat <- as.data.frame(mat)
        
        for(i in seq_along(id)) {
            colSeq <- seq(from=id[i], to=ncol(mat))
            mat[, colSeq + 1] <- mat[, colSeq]
            mat[, id[i]] <- NA
        }
        
        return(as.matrix(mat))
    }
    ##------------------------------------------------------------#
    
    incompData <- assays(object)
    dfmap <- sampleMap(object)
    dfmap <- mapToList(dfmap, "assay")
    cnames <- rownames(colData(object))
    
    ##- inserting NA in missing columns
    for (i in names(incompData)) {
        colnames(incompData[[i]]) <- dfmap[[i]]$primary
        idx <- (cnames %in% colnames(incompData[[i]]))
        incompData[[i]] <- incompData[[i]][, cnames[idx]]
        missColId <- which(!idx)
        incompData[[i]] <- insertCols(incompData[[i]], missColId)
        colnames(incompData[[i]]) <- cnames
    }
    
    strt <- factor(colData(object)[, object@strata])
    missRows <- missingIndv(object)
    
    incompData <- lapply(incompData, t)
    nbCols <- vapply(incompData, ncol, 1L)
    nbTables <- length(incompData)
    nbRows <- length(strt)
    strLevels <- levels(strt)
    
    ##- creation of posible imputations in each stratum of each data ---------#
    ##------------------------------------------------------------------------#
    perm <- strMissRows <- strMiss <- vector("list", length(missRows))
    nmMissRows <- names(missRows)
    names(perm) <- names(strMissRows) <- names(strMiss) <- nmMissRows
    
    for (nm in names(strMissRows)) {
        perm[[nm]] <- strMissRows[[nm]] <- list()
        
        for (s in strLevels) {
            tmp <- apply(is.na(incompData[[nm]][strt == s, ]), 1, all)
            
            if (any(tmp)) {
                donors <- setdiff(names(tmp), names(tmp)[tmp])
                ## permutations per stratum
                perm[[nm]][[s]] <- t(permutations(length(donors), 
                                                    sum(tmp), donors))
                ## missing rows per stratum
                strMissRows[[nm]][[s]] <- names(tmp)[tmp]
            }
            
        }
    }
    
    rm(tmp)
    
    ##- number of posible imputations
    tmp <- lapply(perm, function(x) vapply(x, ncol, 1))
    nbStrMiss <- unlist(lapply(tmp, length))
    from <- 1
    
    for (i in seq_along(nbStrMiss)) {
        to <- sum(nbStrMiss[seq_len(i)])
        strMiss[[i]] <- seq(from, to)
        from <- to + 1
    }
    
    Mtotal <- prod(unlist(tmp))
    seqPermData <- alply(matrix(unlist(tmp)), 1, seq)
    
    M <- min(M, Mtotal)
    
    ##- selection of the donor indexes ---------------------------------------#
    ##------------------------------------------------------------------------#
    Midx <- sample.int(min(Mtotal, 1e15), M)
    idDonor <- NULL
    
    for (i in seq_along(Midx)) {
        idDonor <- rbind(idDonor, searchsComb(seqPermData, Midx[i]))
    }
    
    ##- realisation of the MFA on the imputed data ---------------------------#
    ##------------------------------------------------------------------------#
    U <- list()
    center <- sigma <- matrix(0, nrow=M, ncol=sum(nbCols))
    
    for (m in seq_len(M)) { ## nb. of imputations M
        completeData <- incompData
        
        for (nm in nmMissRows) {
            k <- 1
            
            for (s in strMiss[[nm]]) { ## nb. of strata with missing rows
                imputInd <- perm[[nm]][[k]][, idDonor[m, s]]
                
                ##- create imputate data
                completeData[[nm]][strMissRows[[nm]][[k]], ] <-
                    incompData[[nm]][imputInd, ]
                k <- k + 1
            }
        }
        
        ##- realisation of the MFA
        result <- MFA(completeData, ncomp, nbRows, nbTables, nbCols)
        U[[m]] <- data.frame(result$U)
        colnames(U[[m]]) <- paste0("comp ", seq_len(ncomp))
    }
    
    rm(perm)
    
    ##- calculation of the compromise space (STATIS method) ------------------#
    ##------------------------------------------------------------------------#
    tmp <- STATIS(U, nf=ncomp)
    colnames(tmp$Cli) <- paste0("comp ", seq_len(ncomp))
    
    ##- estimation of the number of components for data imputation -----------#
    ##------------------------------------------------------------------------#
    if (estimeNC) {
        ncp <- estimNC(tmp$Cro, minNC=2, ncomp)
        attr(ncp, "estimated") <- TRUE
    } else {
        ncp <- ncomp
        attr(ncp, "estimated") <- FALSE
    }
    
    ##- data imputation ------------------------------------------------------#
    ##------------------------------------------------------------------------#
    impD <- imputeDataMFA(incompData, tmp$Cli, missRows, comp=seq_len(ncp),
                            maxIter=maxIter, tol=tol)
    
    
    ##- results: MIDTList S4 class -------------------------------------------#
    ##------------------------------------------------------------------------#
    object <- new("MIDTList",
                    object,
                    compromise=tmp$Cli[, seq_len(ncp)],
                    configurations=U,
                    imputedIndv=impD,
                    MIparam=list(method="MFA",
                                    ncomp=ncp,
                                    M=M,
                                    Mtotal=Mtotal))

    return(object)
}
