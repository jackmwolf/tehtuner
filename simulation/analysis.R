package_dir <- "/panfs/roc/groups/11/koopmein/wolfx681/RPackages"
library(dplyr, lib.loc = package_dir)
library(tidyr, lib.loc = package_dir)
library(readr)
library(stringr)


cnums <- expand.grid("J", 1:2, 0:2, 1:4) %>%
  do.call(what = str_c, args = .)

# Reference table to convert C#### to to settings
cnum_lookup <- tibble(cnum = cnums) %>%
  mutate(.h = str_sub(cnum, 2, 2),
         .g = str_sub(cnum, 3, 3),
         .Stage1 = str_sub(cnum, 4, 4),
         h = ifelse(.h == "1", "Linear", "Nonlinear"),
         g = case_when(
           .g == "0" ~ "Null",
           .g == "1" ~ "Linear",
           .g == "2" ~ "Nonlinear"
         ),
         Stage1 = case_when(
           .Stage1 == "1" ~ "las",
           .Stage1 == "2" ~ "rf",
           .Stage1 == "3" ~ "mars",
           .Stage1 == "4" ~ "super"
         )
  ) %>%
  select(-starts_with("."))


get_n_hits <- function(x, hits) {
  sapply(x, function(x) {
    x <- str_split(x, pattern = ", ")[[1]]
    x <- x %in% hits
    return(sum(x))
  })
}

# Read in a directory of simulation results and aggregate them into one tibble
compact_results <- function(cnum, work_dir = file.path(), verbose = TRUE) {

  dir <- file.path(work_dir, cnum)

  if (verbose) {
    message(paste("Compacting simulations in", dir))
  }
  files <- list.files(dir, pattern = "*.rds$")
  re <- lapply(files, function(.x) readRDS(file.path(dir, .x)))

  re_long <-
    lapply(re, function(.x) {
    lapply(.x, function(.y) {
      # un-simplify output if .y is NOT a list of data frames
      if(is.matrix(.y)) {
        .y <- apply(.y, 2, as.data.frame)
      }
      .y <- lapply(.y, function(.z) mutate_if(.z, is.factor, as.character))
      .y <- do.call(rbind, .y)
      .y
    })
  })

  re_long <- apply(do.call(rbind, re_long), 2, function(.x) do.call(rbind, .x))
  for (i in 1:length(re_long)) {
    .name <- names(re_long)[[i]]
    P <- parse_number(.name)
    re_long[[i]]$P <- P
  }

  re_out <- do.call(rbind, re_long)
  re_out$cnum <- str_extract(dir, "J\\d\\d\\d$")
  re_out <- mutate_at(re_out, c("nwgs", "mses", "nvars"), as.numeric)

  info <- cnum_lookup[cnum_lookup$cnum == cnum, ]

  # Variables with TEH
  if (info$g == "Null") {
    hits <- c()
    n_teh <- length(hits)

    re_out <- re_out %>%
      mutate(n_hits = get_n_hits(vars, hits),
             prop_correct_marginal = n_hits / n_teh,
             prop_correct_conditional = ifelse(nvars > 0, n_hits / nvars, NA), #If no variables are selected, return NA
             sensitivity = n_hits / n_teh,
             specificity = 1 - (nvars - n_hits) / (P - n_teh)
      )

  } else {
    hits_10 <- paste0("V", c(1, 9))
    hits_20 <- paste0("V", c(1, 2, 10, 18))
    hits_50 <- paste0("V", c(1, 2, 10, 18))

    n_teh <- length(hits_10)
    re_10 <- re_out %>%
      filter(P == 10) %>%
      mutate(n_hits = get_n_hits(vars, hits_10),
             prop_correct_marginal = n_hits / n_teh,
             prop_correct_conditional = ifelse(nvars > 0, n_hits / nvars, NA), #If no variables are selected, return NA
             sensitivity = n_hits / n_teh,
             specificity = 1 - (nvars - n_hits) / (P - n_teh)
      )

    n_teh <- length(hits_20)
    re_20 <- re_out %>%
      filter(P == 20) %>%
      mutate(n_hits = get_n_hits(vars, hits_20),
             prop_correct_marginal = n_hits / n_teh,
             prop_correct_conditional = ifelse(nvars > 0, n_hits / nvars, NA), #If no variables are selected, return NA
             sensitivity = n_hits / n_teh,
             specificity = 1 - (nvars - n_hits) / (P - n_teh)
      )

    n_teh <- length(hits_50)
    re_50 <- re_out %>%
      filter(P == 50) %>%
      mutate(n_hits = get_n_hits(vars, hits_50),
             prop_correct_marginal = n_hits / n_teh,
             prop_correct_conditional = ifelse(nvars > 0, n_hits / nvars, NA), #If no variables are selected, return NA
             sensitivity = n_hits / n_teh,
             specificity = 1 - (nvars - n_hits) / (P - n_teh)
      )

    re_out <- rbind(re_10, re_20, re_50)

  }

  return(re_out)
}

# READ RESULTS =================================================================

# Safe to ignore warnings of the form:
#   NAs introduced by coercion
#   Input `nvars` is `.Primitive("as.double")(nvars)`
# This just happens when convering the character "error" to NA

work_dir <- "/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu"
re <- lapply(cnums, compact_results, work_dir = work_dir)


# Put all c#### objects into one tibble
re.df <- do.call(bind_rows, re)

# Remove rows with "error"
re.df <- re.df %>%
  filter(Stage2 != "error")

re.df <- re.df %>%
  left_join(cnum_lookup, by = "cnum") %>%
  mutate(N = 1000)

# TABLES =======================================================================
table_dir <- "/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu/Results"

# Function to summarise based on grouping variables
summarise_vt <- function(x) {
  summarise(x,
            Mean_X = mean(nvars),
            Pr_Any_X = mean(nvars != 0 ),
            sensitivity = mean(sensitivity),
            specificity = mean(specificity),
            Pr_Hit_Marginal = mean(prop_correct_marginal),
            Pr_Hit_Conditional = mean(prop_correct_conditional, na.rm = TRUE),
            wg = mean(nwgs/N),
            mse = mean(mses, na.rm = TRUE),
            N_Sims = n()
            )
}

# Save results as a Rds
re.df %>%
  group_by(Stage1, Stage2, g, h, P) %>%
  summarise_vt %>%
  saveRDS(file.path(table_dir, paste0("vt_results_", Sys.Date(), ".Rds")))
