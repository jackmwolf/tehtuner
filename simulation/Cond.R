#MSI version of VTSims code 4/6/20

source("/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu/Scripts/Rcode/Packages.R") #loading packages and CENIC data
source("/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu/Scripts/Rcode/Funcs.R")
source("/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu/Scripts/Rcode/Permute.R")

args=(commandArgs(TRUE))
for(i in 1:length(args)){eval(parse(text=args[[i]]))}
job = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

# Added paramaters for model tuning
alpha <- c(0.05, 0.2)
p_reps <- 100

set.seed(as.numeric(job))
seeds <- round(runif(n = sims, min = 1, max = 10000))

#create simulation wrapper
Cond <- function(p, seed){
  set.seed(seed)
  #generate data based on script input for dg
  dat <- dg(p)
  #VT1: getting individual treatment effects based on script input for vt1
  est <- vt1(dat$reg)
  #VT2: getting results from all 3 variations
  b.none <- c.none(dat, est)
  b.tree <- c.tree(dat, est)
  b.lin <- c.lin(dat, est)
  b.ctree <- c.ctree(dat, est)
  # Tuning parameters based on performance without TEH
  thetas <- tune_theta(dat, alpha = alpha, p_reps = p_reps, vt1_est = est)
  b.tuned_tree <- lapply(thetas$theta[, "tree"],
                         function(theta0) c.tuned_tree(dat, est, theta0)
  )
  names(b.tuned_tree) <- paste("tree_alpha", alpha, sep = "_")

  b.tuned_lasso <- lapply(thetas$theta[, "lasso"],
                          function(theta0) c.tuned_lasso(dat, est, theta0)
  )
  names(b.tuned_lasso) <- paste("lasso_alpha", alpha, sep = "_")

  b.tuned_ctree <- lapply(thetas$theta[, "ctree"],
                          function(theta0) c.tuned_ctree(dat, est, theta0)
  )
  names(b.tuned_ctree) <- paste("ctree_alpha", alpha, sep = "_")

  #putting all the results together
  models <- c(list(none = b.none, lin = b.lin, tree = b.tree, ctree = b.ctree),
              b.tuned_tree, b.tuned_lasso, b.tuned_ctree)

  nwgs <- sapply(models, function(.x) .x$nwg, simplify = TRUE)
  mses <- sapply(models, function(.x) .x$mse, simplify = TRUE)
  vars <- sapply(models, function(.x) .x$vars, simplify = TRUE)
  nvars <- sapply(vars, length, simplify = TRUE)
  vars <- sapply(vars, function(.x) paste(sort(.x), collapse = ", "), simplify = TRUE)

  re <- data.frame(nwgs = unname(nwgs), mses = unname(mses),
                   vars = unname(vars), nvars = unname(nvars))
  re$Stage2 <- names(models)

  return(re)

}

Cond <- possibly(eval(Cond),
                 otherwise = data.frame(nwgs = rep("error", 10),
                                        mses = rep("error", 10),
                                        vars = rep("error", 10),
                                        nvars = rep("error", 10),
                                        Stage2 = rep("error")))

#10 covariates:
list1 <- mapply(Cond, 10, seeds, SIMPLIFY = FALSE)
#20 covarites:
list2 <- mapply(Cond, 20, seeds, SIMPLIFY = FALSE)
#50 covariates:
list3 <- mapply(Cond, 50, seeds, SIMPLIFY = FALSE)

res <- list(P10 = list1, P20 = list2, P50 = list3)
saveRDS(res, paste0("sol.", job, ".rds"))
