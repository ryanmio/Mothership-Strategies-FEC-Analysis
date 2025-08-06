library(ggplot2)
library(dplyr)
library(scales)
library(httr)
library(jsonlite)
library(purrr)
library(lubridate)
library(stringr)
library(tidyr)

# ----------------------------------------------------------------------------
# Helper to flatten list-columns before writing CSVs
# ----------------------------------------------------------------------------
flatten_lists <- function(df) {
  df %>% mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = ", "))))
}




# =============================================================================
# SETUP AND DATA LOADING
# =============================================================================

# It's best practice to store API keys as environment variables
# rather than hardcoding them directly in the script.
FEC_API_KEY <- Sys.getenv("FEC_API_KEY")

# =============================================================================
# API FUNCTIONS
# =============================================================================

BASE_URL <- "https://api.open.fec.gov/v1/"

# This function handles making the GET request, checking for errors, and
# parsing the JSON response.
fetch_fec_data <- function(path, query_params = list()) {
  # Add the API key to every request
  query_params$api_key <- FEC_API_KEY

  # Make the API request
  full_url <- paste0(BASE_URL, path)
  response <- GET(full_url, query = query_params)

  # Check for HTTP errors (e.g., 404 Not Found, 429 Too Many Requests)
  stop_for_status(response, task = paste("fetch data from", full_url))

  # Parse the JSON content from the response
  content <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  Sys.sleep(1)  # Be polite to the API
  # The actual data is usually in a list called 'results'
  return(content)
}


# This function handles paginating through results to get all data.
fetch_all_pages <- function(path, query_params, set_limit=10) {
  all_results <- list()

  # Set a high number for per_page to minimize the number of calls
  query_params$per_page <- 100

  limit = 0
  repeat {
    # Make the API call using our helper
    content <- fetch_fec_data(path, query_params)
    page_results <- content$results

    # Add the retrieved page of results to our list
    if (length(page_results) > 0 && (is.data.frame(page_results) || is.list(page_results))) {
      all_results[[length(all_results) + 1]] <- page_results
    }

    # Check if we are on the last page. If so, exit the loop.
    if (length(page_results) == 0 || nrow(page_results) < query_params$per_page) {
         break
    }

    # Get the `last_indexes` cursor from the pagination info for the next loop iteration
    last_indexes <- content$pagination$last_indexes
    if (is.null(last_indexes) || is.null(last_indexes$last_index)) {
        break
    }

    # Update query_params for the next page request
    for (name in names(last_indexes)) {
        query_params[[name]] <- last_indexes[[name]]
    }

    limit = limit + 1
    if(limit >= set_limit){
        break
    }

    cat(".")
flush.console() # Print a dot for progress
  }
  cat("\n")

  # Combine the list of data frames from all pages into one big data frame
  return(bind_rows(all_results))
}


# --- Get Committee Leadership and Treasurer Info ---
get_committee_details <- function(committee_id) {
  Sys.sleep(1.0) # Be polite to the API by pausing between requests
  cat("Fetching details for:", committee_id, "\n")
flush.console()

  path <- file.path("committee", committee_id)
  # The fetch_fec_data function returns the full content object,
  # so we extract the 'results' from it.
  data <- fetch_fec_data(path)$results

  # Select and rename relevant columns for clarity
  if (length(data) > 0 && is.data.frame(data)) {
    data %>%
      select(
        committee_id,
        committee_name = name,
        treasurer_name,
        custodian_name_full,
        city,
        state,
        party_full,
        committee_type_full,
        designation_full
      )
  } else {
    # Return an empty tibble with correct columns if no data is found
    tibble()
  }
}

# --- Get Incoming Transfers from Other Committees ---
get_incoming_transfers <- function(committee_id) {
  Sys.sleep(1.0)
  cat("Fetching incoming transfers for:", committee_id, "\n")
flush.console()

  path = str_glue("schedules/schedule_a/")
  query <- list(
      min_amount=10000,
      committee_id=committee_id,
      contributor_type="committee",
      per_page=100,
      sort="-contribution_receipt_date",
      sort_hide_null=FALSE,
      sort_null_only=FALSE
  )
  data <- fetch_all_pages(path, query, set_limit=10)
  return(data)
}

# --- Get Outgoing Transfers to Other Committees ---
get_outgoing_transfers <- function(committee_id) {
    Sys.sleep(1.0)
    cat("Fetching ALL outgoing transfers for:", committee_id, "\n")
flush.console()
    path <- "schedules/schedule_b/"
    query <- list(
        committee_id = committee_id,
        recipient_committee_id_null = FALSE, # Only get transfers to other committees
        sort = "-disbursement_amount",
        min_amount=1000,
        per_page=100
    )
    data <- fetch_all_pages(path, query, set_limit=10)

    # Clean up the resulting dataframe
    if (nrow(data) > 0) {
        data$recipient_committee.affiliated_committee_name <- ifelse(is.null(data$recipient_committee.affiliated_committee_name),data$recipient_name,data$recipient_committee.affiliated_committee_name)
        data2 <- data %>%
                    mutate(transfer_amount = as.numeric(disbursement_amount)) %>%
                    select(
                    donor_committee_id = committee_id,
                    donor_committee_name = committee.name,
                    cycle = fec_election_year,
                    transfer_date = disbursement_date,
                    transfer_amount,
                    recipient_committee_name = recipient_name,
                    beneficiary_committee_name,
                    recipient_committee_id,
                    affiliated_comm_name = recipient_committee.affiliated_committee_name,
                    disbursement_description,
                    line_number_label = line_number_label,
                    disbursement_purpose_category,
                    disbursement_type_description
                )
        return(data2)
    } else {
        return(tibble()) # Return empty tibble if no transfers found
    }
}


# --- Get Contributions Made by a Committee ---
get_spending <- function(committee_id) {
  Sys.sleep(1.0)
  cat("Fetching ALL contributions for:", committee_id, "\n")
flush.console()
  path <- "schedules/schedule_a/"
  query <- list(
      contributor_id = committee_id,
      sort = "-contribution_receipt_date",
      min_amount=1000,
      per_page=100
  )
  data <- fetch_all_pages(path, query, set_limit=20)
  return(data)
}

# --- Get Independent Expenditures by a Committee ---
get_independent_expenditures <- function(committee_id) {
  Sys.sleep(1.0)
  cat("Fetching independent expenditures for:", committee_id, "\n")
flush.console()
  path <- "schedules/schedule_e/"
  query <- list(
      committee_id = committee_id,
      sort = "-expenditure_amount",
      per_page = 100
  )
  data <- fetch_all_pages(path, query, set_limit = 20)
  return(data)
}


# =============================================================================
#  Find all committees that have paid a specific vendor
# =============================================================================
get_clients_by_vendor <- function(vendor_names, cycles_to_fetch) {
  cat("Searching for clients who paid the following vendors:", paste(vendor_names, collapse=", "), "\n")
  cat("Across the following cycles:", paste(cycles_to_fetch, collapse=", "), "\n")


  # Use purrr::map_dfr to apply the search to each vendor name and row-bind the results
  all_disbursements <- map_dfr(vendor_names, function(name) {
    cat("Fetching disbursements for vendor:", name, "\n")
    path <- "schedules/schedule_b/"
    query <- list(
      recipient_name = name,
      cycle = paste(cycles_to_fetch, collapse = ","), # Convert vector to comma-separated string
      sort = "-disbursement_date",
      per_page = 100,
      sort_hide_null = "false", # Added based on user feedback
      sort_null_only = "false"  # Added based on user feedback
    )
    # Using fetch_all_pages to ensure we get all records across multiple pages
    fetch_all_pages(path, query, set_limit = 500) # Increased limit for more data
  })

  # Remove duplicate records that might have been fetched
  all_disbursements <- all_disbursements %>% distinct()

  cat("Found a total of", nrow(all_disbursements), "disbursements to specified vendors.\n")
  return(all_disbursements)
}


##
## Fetch FEC data and perform analysis
##

# ---- Fetch committees that have paid Mothership directly from the API ----
# Define the election cycles to query
election_cycles <- c(2016, 2018, 2020, 2022, 2024, 2026)
# Changed to a broader search term to catch more variations
mothership_vendor_names <- c("MOTHERSHIP")
ms_mothership <- get_clients_by_vendor(mothership_vendor_names, election_cycles)


# Add the cycle ID for grouping, if the data was found
if (nrow(ms_mothership) > 0) {
    ms_mothership$cyc_id = paste0(ms_mothership$committee_id,'_',ms_mothership$two_year_transaction_period)

    # --- FIX: Clean data before writing to CSV ---
    # The API can return list-columns which write.csv cannot handle.
    # This converts any list columns to comma-separated character strings.
    ms_mothership_for_csv <- ms_mothership %>%
      mutate(across(where(is.list), ~sapply(., function(x) paste(unlist(x), collapse = ", "))))

    # Save the cleaned disbursement data to a file.
    write.csv(ms_mothership_for_csv, file='fec_disbursements_to_mothership.csv', row.names = FALSE)

    # Aggregate disbursements by committee for Mothership only (using original dataframe)
    mothership_payments <- ms_mothership %>%
      # The column from the API is `committee.name`, not `committee_name`.
      group_by(committee_id, committee.name) %>%
      summarise(
        mothership_total = sum(disbursement_amount, na.rm = TRUE),
        num_payments = n(),
        .groups = 'drop'
      ) %>%
      # Rename for consistency with the rest of the script
      rename(committee_name = committee.name)
      write.csv(mothership_payments, file='total_payments_to_mothership.csv', row.names = FALSE)
} else {
    mothership_payments <- tibble()
    warning("No disbursements found for the specified vendors. Subsequent analysis may fail.")
}

# Load committee summary data from local files
comms_files <- c('committee_summary_2018.csv', 'committee_summary_2020.csv',
                 'committee_summary_2022.csv', 'committee_summary_2024.csv',
                 'committee_summary_2026.csv')
comms.all <- do.call(rbind, lapply(comms_files, function(f) {
  if(file.exists(f)) read.csv(f) else data.frame()
}))

if(nrow(comms.all) > 0){
    comms.all$cyc_id = paste0(comms.all$CMTE_ID,'_',comms.all$FEC_ELECTION_YR)
    comms.all$committee_id = comms.all$CMTE_ID

    # Merge with committee summary data
    merged_data <- left_join(mothership_payments, comms.all, by = c("committee_id"))

    # Fix column names and handle numeric conversion
    merged_data$indv_contrib <- as.numeric(merged_data$INDV_CONTB)
    merged_data$total_receipts <- as.numeric(merged_data$TTL_RECEIPTS)

    # Calculate proportions
    merged_data$pct_of_indv_fundraising <- ifelse(merged_data$indv_contrib > 0,
                                                 merged_data$mothership_total / merged_data$indv_contrib, NA)
    merged_data$pct_of_total_receipts <- ifelse(merged_data$total_receipts > 0,
                                               merged_data$mothership_total / merged_data$total_receipts, NA)

    # Clean committee names
    merged_data$committee_name_clean <- ifelse(is.na(merged_data$CMTE_NM) | merged_data$CMTE_NM == "",
                                              merged_data$committee_name, merged_data$CMTE_NM)

    top_clients <- merged_data %>%
      arrange(desc(mothership_total)) %>%
      filter(mothership_total > 1000) %>%
      mutate(
        indv_contrib_millions = indv_contrib / 1000000,
        mothership_millions = mothership_total / 1000000,
        pct_of_indv_display = ifelse(is.na(pct_of_indv_fundraising) | is.infinite(pct_of_indv_fundraising),
                                    NA, pct_of_indv_fundraising)
      )
    write.csv(top_clients,file='mothership_top_clients.csv', row.names=FALSE)

    # Use committee IDs from the analysis for the next steps
    cmte_ids <- unique(na.omit(top_100_clients$CMTE_ID))
} else {
    cmte_ids <- unique(na.omit(mothership_payments$committee_id))
    warning("Could not load local committee summary files. Proceeding with limited data.")
}


cat("Loaded base data for", nrow(mothership_payments), "mothership clients\n")
cat("Will attempt API collection for", length(cmte_ids), "committees\n")


# =============================================================================
# DATA COLLECTION - Fetch details for all identified committees
# =============================================================================

mothership_connected_pacs <- rbind(
    c('C00884510','BATTLEGROUND DEMOCRATS'),
    c('C00889808','DEM TURNOUT FUND'),
    c('C00853481','DEMOCRATS WHO WIN'),
    c('C00894881','DEMOCRATS WIN FUND'),
    c('C00889808','DEM TURNOUT 2024'),
    c('C00745786','DEMOCRATIC STRATEGY INSTITUTE'),
    c('C00808998','DEMOCRACYFIRST PAC'),    
    c('C00764233','DEFEND THE VOTE'),


    c('C00880062','ELECT DEMS NOW PAC'),
    c('C00573261','END CITIZENS UNITED'),
    c('C00750802','END CITIZENS UNITED 2020'),
    c('C00742734','END CITIZENS UNITED - PRIORITY'),
    c('C00742767','END CITIZENS UNITED - URGENT'),
    c('C00743666','END CITIZENS UNITED - NOW'),
    c('C00825117','END CITIZENS UNITED: SENATE FUND'),
    c('C00685552','MEDICARE FOR ALL'),
    c('C00580068','PROGRESSIVE TURNOUT PROJECT'),
    c('C00659599','PROGRESSIVE TAKEOVER'),
    
    c('C00819912','STOP GUN VIOLENCE PAC'),
    c('C00847673','STOP TRUMP'),
    c('C00884874','STOP PROJECT 2025'),
    c('C00633404','STOP REPUBLICANS'),
    c('C00483883','RETIRED AMERICANS PAC'),

    c('C00864363','PRO-CHOICE MAJORITY 2024'),
    c('C00840702','PRO-CHOICE WOMEN'),

    c('C00603084','NDTC'),
    c('C00880401','RETIRED DEMS 2024'),
    c('C00895193','RETIRED DEMS 2026'),
    c('C00483883','RETIRED AMERICANS PAC'),
    c('C00745786','DEMOCRATIC STRATEGY INSTITUTE'),
    c('C00853481','DEMOCRATS WHO WIN'),
    c('C00751982','VOTING RIGHTS DEFENSE FUND'),
    c('C00652594','MORE LIKE AMERICA'),

    c('C00685297','ELECT DEMOCRATIC WOMEN'),
    c('C00651042','MOMS FED UP')
)
mothership_connected_pacs <- as.data.frame(mothership_connected_pacs)
colnames(mothership_connected_pacs) <- c('CMTE_ID','Committee Name')


mothership_clients <- rbind(
    c('C00696153', 'JAIME HARRISON FOR US SENATE'),
    c('C00630426', 'JON OSSOFF FOR CONGRESS'),
    c('C00718866', 'JON OSSOFF FOR SENATE'),
    c('C00640623', 'DOUG JONES FOR SENATE COMMITTEE'),
    c('C00635888', 'AMMAR CAMPA NAJJAR FOR CONGRESS'),
    c('C00667964', 'OCONNOR FOR CONGRESS'),
    c('C00632232', 'ROB QUIST FOR MONTANA'),

    c('C00603084', 'NATIONAL DEMOCRATIC TRAINING COMMITTEE PAC'),
    c('C00365536', 'CHC BOLD PAC'),
    c('C00495028', 'HOUSE MAJORITY PAC'),
    c('C00271338', 'AMERIPAC: THE FUND FOR A GREATER AMERICA'),
    c('C00147512', 'CONGRESSIONAL BLACK CAUCUS PAC'),
    c('C00674093', 'BRADY PAC'),
    c('C00550970', 'EQUALITY PAC'),
    c('C00760827','DEMAND JUSTICE PAC'),
    c('C00840702', 'PRO-CHOICE WOMEN'),
    c('C00513176', 'CONGRESSIONAL PROGRESSIVE CAUCUS PAC')
)
mothership_clients <- as.data.frame(mothership_clients)
colnames(mothership_clients) <- c('CMTE_ID','Committee Name')


cmte_ids <- unique(c(mothership_connected_pacs$CMTE_ID, mothership_clients$CMTE_ID))

# Use purrr::map_dfr to apply the function to each ID and row-bind the results with error handling
cat("Will attempt API collection for", length(cmte_ids), "committees\n")
committee_details_list <- map(cmte_ids, function(cmte_id) {
  tryCatch({
    get_committee_details(cmte_id)
  }, error = function(e) {
    cat("Warning: Failed to fetch committee details for", cmte_id, ":", e$message, "\n")
    return(data.frame())
  })
})
committee_details_df <- bind_rows(committee_details_list)
cat("Successfully collected details for", nrow(committee_details_df), "committees\n")
write.csv(committee_details_df,file='mothership_connected_committee_details.csv')


cat("\n=== COLLECTING INCOMING TRANSFERS ===\n")
cat("Processing", length(cmte_ids), "committees...\n")
flush.console()
# Try to execute - with error handling for rate limits
tryCatch({
  incoming_transfers_df <- map_dfr(cmte_ids, get_incoming_transfers)
  cat("Total incoming transfer records collected:", nrow(incoming_transfers_df), "\n")
  incoming_transfers_df_clean <- flatten_lists(incoming_transfers_df)
  write.csv(incoming_transfers_df_clean,file='ms_incoming_transfers.csv', row.names=FALSE)  
  }, error = function(e) {
  cat("API collection for incoming transfers failed:", e$message, "\n")
  incoming_transfers_df <- data.frame()
})


cat("\n=== COLLECTING OUTGOING TRANSFERS ===\n")
cat("Processing", length(cmte_ids), "committees...\n")
flush.console()
# Try to execute outgoing transfers collection
tryCatch({
  outgoing_transfers_df <- map_dfr(cmte_ids, get_outgoing_transfers)
  cat("Total outgoing transfer records collected:", nrow(outgoing_transfers_df), "\n")
  write.csv(outgoing_transfers_df,file='disbursements_by_ms_connected_pacs.csv', row.names=FALSE)
}, error = function(e) {
  cat("API collection for outgoing transfers failed:", e$message, "\n")
  outgoing_transfers_df <- data.frame()
})

cat("\n=== COLLECTING INDEPENDENT EXPENDITURES ===\n")
cat("Processing", length(cmte_ids), "committees...\n")
flush.console()
tryCatch({
  # Collect independent expenditures with individual committee error handling
  independent_exp_list <- map(cmte_ids, function(cmte_id) {
    tryCatch({
      get_independent_expenditures(cmte_id)
    }, error = function(e) {
      cat("Warning: Failed to fetch independent expenditures for committee", cmte_id, ":", e$message, "\n")
      return(data.frame())
    })
  })
  independent_exp_df <- bind_rows(independent_exp_list)
  cat("Total independent expenditure records collected (raw):", nrow(independent_exp_df), "\n")
  # Dump raw IE data for offline debugging (flatten list-columns first)
  write.csv(flatten_lists(independent_exp_df), file='raw_independent_exp.csv', row.names=FALSE)
  # ----------------------------------------------------------------------------
  # Filter to core network and 2018+ cycles, de-duplicate on sub_id
  # ----------------------------------------------------------------------------
  core_ids_vec <- mothership_connected_pacs$CMTE_ID
  # Flatten two_year_transaction_period if it's a list-column
  if ("two_year_transaction_period" %in% names(independent_exp_df) &&
      is.list(independent_exp_df$two_year_transaction_period)) {
    independent_exp_df <- independent_exp_df %>%
        mutate(two_year_transaction_period = sapply(two_year_transaction_period, function(x) if(length(x)>0) x[[1]] else NA))
  }
  # --- Robust cycle calculation replacing previous mutate block ---
  independent_exp_df <- independent_exp_df %>%
      mutate(exp_year = ifelse(!is.na(expenditure_date) & nchar(as.character(expenditure_date)) >= 4,
                               as.integer(substr(as.character(expenditure_date),1,4)), NA))

  # Convert two_year_transaction_period to numeric scalar
  if ("two_year_transaction_period" %in% names(independent_exp_df)) {
      if (is.list(independent_exp_df$two_year_transaction_period)) {
          independent_exp_df <- independent_exp_df %>% mutate(
              two_year_tp_num = sapply(two_year_transaction_period, function(x) if(length(x) > 0) as.integer(x[[1]]) else NA_integer_)
          )
      } else {
          independent_exp_df <- independent_exp_df %>% mutate(two_year_tp_num = as.integer(two_year_transaction_period))
      }
  } else {
      independent_exp_df$two_year_tp_num <- NA_integer_
  }

  independent_exp_df <- independent_exp_df %>%
      mutate(cycle_calculated = ifelse(!is.na(two_year_tp_num),
                                       two_year_tp_num,
                                       ifelse(!is.na(exp_year),
                                              ifelse(exp_year %% 2 == 0, exp_year, exp_year + 1),
                                              NA_integer_))) %>%
      filter(committee_id %in% core_ids_vec,
             !is.na(cycle_calculated) & cycle_calculated >= 2018) %>%
      distinct(sub_id, .keep_all = TRUE)
  cat("Independent expenditure records after filters:", nrow(independent_exp_df), "\n")
  independent_exp_df_clean <- flatten_lists(independent_exp_df)
  write.csv(independent_exp_df_clean, file='independent_expenditures_by_ms_connected_pacs.csv', row.names=FALSE)  # Final IE output
}, error = function(e) {
  cat("API collection for independent expenditures failed:", e$message, "\n")
  independent_exp_df <- data.frame()
})

cat("\n=== COLLECTING CONTRIBUTIONS MADE BY MOTHERSHIP PACS ===\n")
tryCatch({
  contributions_made <- map_dfr(unique(mothership_connected_pacs$CMTE_ID), get_spending)
  cat("Total contributions recorded:", nrow(contributions_made), "\n")
  if(nrow(contributions_made) > 0) {
      library(readr)
      write.csv(flatten_lists(contributions_made), file='fec_contributions_made_by_mothership_pacs.csv', row.names=FALSE)
      cat("Total amount of contributions made: $", sum(contributions_made$contribution_receipt_amount), "\n")
  }
}, error = function(e) {
  cat("API collection for contributions failed:", e$message, "\n")
  contributions_made <- data.frame()
})


# =============================================================================
# ANALYSIS: TRACK SELF-DEALING AND VENDOR SPENDING
# =============================================================================

cat("\n--- Starting Analysis ---\n")

mothership_ids <- mothership_connected_pacs[,1]
mothership_ids <- unique(c(mothership_connected_pacs$CMTE_ID, mothership_clients$CMTE_ID))


# Proceed with analysis only if we have outgoing transfer data
if (exists("outgoing_transfers_df") && nrow(outgoing_transfers_df) > 0) {

  # --- Task 1: Generate a table of transfers between mothership PACs ---
  self_dealing_df <- outgoing_transfers_df %>%
    filter(
      donor_committee_id %in% mothership_ids,
      recipient_committee_id %in% mothership_ids
    )

  cat("Self-dealing transfers found:", nrow(self_dealing_df), "\n")
  cat("Total self-dealing amount:", dollar(sum(self_dealing_df$transfer_amount, na.rm = TRUE)), "\n")

  # --- Task 2: Analyze vendor spending to Mothership from the API data ---
  mothership_spending <- outgoing_transfers_df %>%
      filter(str_detect(recipient_committee_name, regex("mothership|trilogy", ignore_case = TRUE))) %>%
      group_by(donor_committee_id) %>%
      summarise(
          mothership_total = sum(transfer_amount, na.rm = TRUE),
          num_payments = n(),
          .groups = 'drop'
      )
  mnames <- outgoing_transfers_df$donor_committee_name[match(mothership_spending$donor_committee_id,outgoing_transfers_df$donor_committee_id)]
  mothership_spending <- cbind(committee_name=mnames,mothership_spending)


  cat("Mothership spending from API transfers:", dollar(sum(mothership_spending$mothership_total, na.rm = TRUE)), "\n")
  print("Top mothership spending from API:")
  print(as.data.frame((mothership_spending)))

  # Export the collected data
  write.csv(self_dealing_df, "mothership_self_dealing_api_complete.csv", row.names = FALSE)
  write.csv(mothership_spending, "mothership_api_spending_complete.csv", row.names = FALSE)

} else {
  warning("No outgoing transfer data was collected, skipping analysis section.")
}

# =============================================================================
# FINAL SUMMARY AND POST-ANALYSIS
# =============================================================================

cat("\n=== MOTHERSHIP API ANALYSIS SUMMARY ===\n")
cat("Committees analyzed:", length(cmte_ids), "\n")

if (exists("incoming_transfers_df") && nrow(incoming_transfers_df) > 0) {
  cat("Incoming transfer records:", nrow(incoming_transfers_df), "\n")
}

if (exists("outgoing_transfers_df") && nrow(outgoing_transfers_df) > 0) {
  cat("Outgoing transfer records:", nrow(outgoing_transfers_df), "\n")
}

if (exists("self_dealing_df") && nrow(self_dealing_df) > 0) {
  cat("Self-dealing transfers from API:", nrow(self_dealing_df), "\n")
  cat("Self-dealing amount from API:", dollar(sum(self_dealing_df$transfer_amount, na.rm = TRUE)), "\n")
}


##Since 2018, this core network of Mothership-linked PACs has raised approximately $678 million 
if (file.exists('mothership_top_clients.csv')) {
  top.clients <- read.csv('mothership_top_clients.csv')
  cat("Total Fees paid to Mothership:", sum(top.clients$mothership_total[!duplicated(top.clients$committee_id)]), "\n")
  ms.connected <- top.clients %>% filter(committee_id %in%  mothership_connected_pacs$CMTE_ID)
} else {
  cat("Top-client summary skipped – mothership_top_clients.csv not present.\n")
  ms.connected <- data.frame()
}
# ms.connected <- ms.connected[!duplicated(ms.connected$cyc_id),]
cat("Mothership Connected PACs Total Indv Fundraising:", dollar(sum(ms.connected$indv_contrib, na.rm = TRUE)), "\n")

##159 million
ms.connected2 <- ms.connected[!duplicated(ms.connected$committee_id),]
cat("Mothership Connected PACs Payments to Mothership:", dollar(sum(ms.connected2$mothership_total, na.rm = TRUE)), "\n")

ms.disb <- read.csv('disbursements_by_ms_connected_pacs.csv') %>%
            filter(donor_committee_id %in% mothership_connected_pacs$CMTE_ID,
                   cycle >= 2018)

##fundraising
adfund.keywords <-paste("donor","banquet",'messaging','digital','consult','text','email',
                        "fundraising","fundraising consulting","printing","catering",
                        "raffle","direct mail","donation","fundraiser","gifts", sep="|")
idrows <- grep(adfund.keywords, ms.disb$disbursement_description, perl = TRUE,ignore.case= TRUE, value = FALSE)
ms.idrows <- grep('mothership', ms.disb$recipient_committee_name, perl = TRUE,ignore.case= TRUE, value = FALSE)
idrows <- idrows[!(idrows %in% ms.idrows)]
cat("Mothership Connected PACs Disb|other fundraising/consulting:",dollar(sum(ms.disb$transfer_amount[idrows])))

keywords <-paste("payroll","paychex","salar", sep="|")
idrows <- grep(keywords, ms.disb$disbursement_description, perl = TRUE,ignore.case= TRUE, value = FALSE)
ms.idrows <- grep('mothership', ms.disb$recipient_committee_name, perl = TRUE,ignore.case= TRUE, value = FALSE)
idrows <- idrows[!(idrows %in% ms.idrows)]
cat("Mothership Connected PACs Disb|payroll:",dollar(sum(ms.disb$transfer_amount[idrows])))



# --- Additional Analysis on generated files ---
# This part assumes the CSVs were created successfully in the steps above.
unq_cmte_ids <- c(mothership_connected_pacs$CMTE_ID, mothership_clients$CMTE_ID)
pac.details <- read.csv('mothership_connected_committee_details.csv')
pac.details <- pac.details %>%
  mutate(
    'Relationship_to_Mothership' = ifelse(committee_id %in% mothership_connected_pacs$CMTE_ID,'Connected Pac','Client')
  ) %>%
  arrange(desc(Relationship_to_Mothership))
write.csv(pac.details,file='fec_mothership_pac_connections.csv', row.names=FALSE)

spending <- read.csv("disbursements_by_ms_connected_pacs.csv")
ms.spending <- spending %>% filter(donor_committee_id %in% mothership_connected_pacs$CMTE_ID)

# Contributions to candidates/committees inside and outside the network
ttc <- ms.spending %>% filter(disbursement_purpose_category  == 'CONTRIBUTIONS', !is.na(recipient_committee_id))
in_network_contribs <- ttc %>% filter(recipient_committee_id %in% unq_cmte_ids)
out_network_contribs <- ttc %>% filter(!(recipient_committee_id %in% unq_cmte_ids))

# Transfers to committees inside and outside the network
ttt <- ms.spending %>% filter(disbursement_purpose_category  == 'TRANSFERS', !is.na(recipient_committee_id))
trans_in_network <- ttt %>% filter(recipient_committee_id %in% unq_cmte_ids)
trans_out_network <- ttt %>% filter(!(recipient_committee_id %in% unq_cmte_ids))

total.spent.on.cands.and.comms <- sum(out_network_contribs$transfer_amount) + sum(trans_out_network$transfer_amount)

# Add independent expenditures to total if available
if (exists("independent_exp_df") && nrow(independent_exp_df) > 0) {
  candidate_ies <- independent_exp_df %>% filter(!is.na(candidate_id))
  total_candidate_ie <- sum(candidate_ies$expenditure_amount, na.rm = TRUE)
  total.spent.on.cands.and.comms <- total.spent.on.cands.and.comms + total_candidate_ie
  cat("Total independent expenditures (candidate related): ", dollar(total_candidate_ie), "\n")
}

cat("Total spent on outside candidates and committees (including IEs): ", dollar(total.spent.on.cands.and.comms), "\n")

# Correcting committee names for Sankey diagram
self_deal <- read.csv("mothership_self_dealing_api_complete.csv")
self_deal$donor_committee_name <- str_replace(self_deal$donor_committee_name, "STOP THESE", "STOP TRUMP")
self_deal$recipient_committee_name <- str_replace(self_deal$recipient_committee_name, "STOP THESE", "STOP TRUMP")
sankey_data = aggregate(self_deal$transfer_amount, list(self_deal$donor_committee_name, self_deal$recipient_committee_name), sum)
colnames(sankey_data) <- c('Donor Comm','Recipient Comm','Amount')
write.csv(sankey_data, file='self_dealing_sankey.csv', row.names=FALSE)

