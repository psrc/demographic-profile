library(magrittr)
library(psrccensus)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(labelled)
library(openxlsx)
#library(scales)

# 1. Setup: Person-level variables needed for Table 14 ----------------------
pvars <- c(
	"AGEP",                   # Age
	"ENG",                    # Ability to speak English
	"LANP"                    # Language spoken at home
)

# 2. Setup: Helper functions -------------------------------------------------

reg_pums_count <- purrr::partial(psrc_pums_count, rr="cv", incl_na=FALSE)       # Simplify repeated calls

ctyreg_pums_count <- function(so, groupvars=NULL){                              # County & Region stats in one call
	rs      <- list()
	rs[[1]] <- reg_pums_count(so, group_vars=groupvars)
	rs[[2]] <- reg_pums_count(so, group_vars=c("COUNTY", groupvars)) %>%
		filter(COUNTY != "Region")
	rs %<>% rbindlist() %>% arrange(DATA_YEAR, COUNTY)
	return(rs)
}

# 3. Main function: Table 14 only -------------------------------------------

get_tbl14_lep_languages <- function(dyear, pums_rds="C:/Users/mjensen/projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"){

	pp_df <- get_psrc_pums(5, dyear, "p", pvars, dir=pums_rds) %>%                 # Retrieve person-level data
        mutate(lep = factor(
			      case_when(AGEP < 5 ~ NA_character_,
        					    !str_detect(ENG, "^Very") ~ "Speak English less than 'very well'",
        					    TRUE ~ "Speak English 'very well'"))
	)

	languages <- psrc_pums_count(filter(pp_df, AGEP >= 5), group_vars=c("LANP", "lep")) # First total for pop & min speakers


	lep_languages	<- filter(languages, LANP != "Total" &
	                                   grepl("less than 'very well'", as.character(lep)) &
	                                   (count + count_moe) > 1000) %>%
		pull(LANP) %>% as.character() %>% unique()

	all5plus_pop <- max(languages$count)                                          # Regional population

	lep_stats <- filter(pp_df, AGEP >= 5 & LANP %in% lep_languages) %>%
		reg_pums_count(group_vars=c("LANP", "lep")) %>%
		filter(LANP != "Total" & lep != "Speak English 'very well'") %>%            # LEP share per language
		split(f=.$lep) %>% lapply(select, -lep)

	lep_stats$Total %<>% select(-contains("share")) %>%
		rename_with(~ paste0("total_", .x, recycle0 = TRUE),
								starts_with("count", ignore.case=FALSE))

	lep_stats$`Speak English less than 'very well'` %<>%
		rename_with(~ paste0("lep_", .x, recycle0 = TRUE),
								matches("count|share", ignore.case=FALSE))

	tbl14 <- inner_join(lep_stats$Total,                                          # Combine
	                    lep_stats$`Speak English less than 'very well'`,
											by=c("LANP", "DATA_YEAR", "COUNTY")) %>%
	  select(-matches("(YEAR|moe|x|y)$")) %>%
		arrange(desc(lep_count)) %>%
	  mutate(pop_share = lep_count / all5plus_pop)

	var_label(tbl14$LANP)        <- "Language Spoken at Home"
	var_label(tbl14$total_count) <- "All Speakers (Age 5+)"
	var_label(tbl14$lep_count)   <- "Limited English Proficient (LEP) Speakers (Age 5+)"
	var_label(tbl14$lep_share)   <- "LEP Speakers as % Share of All Speakers (Age 5+)"
	var_label(tbl14$pop_share)   <- "LEP Speakers as % Share of Total Population (Age 5+)"

	# Replace column names with labels for display
	display_tbl14 <- tbl14
	names(display_tbl14) <- sapply(display_tbl14, var_label)

	return(display_tbl14)
}

# 4. Write helper: Styled Excel output --------------------------------------

write_tbl14_xlsx <- function(tbl14,
										 file_path="tbl14_lep_languages.xlsx",
										 sheet_name="Tbl 14 LEP Languages"){

	if(is.null(tbl14) || nrow(tbl14) == 0){
		stop("`tbl14` is empty. Generate data with get_tbl14_lep_languages() before exporting.")
	}

	wb <- createWorkbook()
	addWorksheet(wb, sheet_name)
	writeData(wb, sheet=sheet_name, x=tbl14, startRow=1, startCol=1, headerStyle=NULL)

	n_cols <- ncol(tbl14)
	n_rows <- nrow(tbl14)

	header_style <- createStyle(
		fontName="Arial",
		fontSize=11,
		textDecoration="bold",
		fontColour="#FFFFFF",
		fgFill="#E67E22",
		wrapText=TRUE,
		halign="center",
		valign="center",
		border="Bottom",
		borderColour="#FFFFFF"
	)

	row_white_style <- createStyle(
		fontName="Arial",
		fontSize=10,
		fgFill="#FFFFFF"
	)

	row_gray_style <- createStyle(
		fontName="Arial",
		fontSize=10,
		fgFill="#F2F2F2"
	)

	count_fmt_style <- createStyle(
		fontName="Arial",
		fontSize=10,
		numFmt="#,##0"
	)

	percent_fmt_style <- createStyle(
		fontName="Arial",
		fontSize=10,
		numFmt="0.0%"
	)

	addStyle(wb, sheet=sheet_name, style=header_style,
			 rows=1, cols=1:n_cols, gridExpand=TRUE, stack=TRUE)

	if(n_rows > 0){
		data_rows <- 2:(n_rows + 1)
		odd_excel_rows <- data_rows[((seq_len(n_rows) %% 2) == 1)]
		even_excel_rows <- data_rows[((seq_len(n_rows) %% 2) == 0)]

		if(length(odd_excel_rows) > 0){
			addStyle(wb, sheet=sheet_name, style=row_white_style,
					 rows=odd_excel_rows, cols=1:n_cols, gridExpand=TRUE, stack=TRUE)
		}

		if(length(even_excel_rows) > 0){
			addStyle(wb, sheet=sheet_name, style=row_gray_style,
					 rows=even_excel_rows, cols=1:n_cols, gridExpand=TRUE, stack=TRUE)
		}

		# Numeric formats layered on top of row banding
		if("All Speakers (Age 5+)" %in% names(tbl14)){
			count_col <- which(names(tbl14) == "All Speakers (Age 5+)")
			addStyle(wb, sheet=sheet_name, style=count_fmt_style,
					 rows=data_rows, cols=count_col, gridExpand=TRUE, stack=TRUE)
		}

		if("Limited English Proficient (LEP) Speakers (Age 5+)" %in% names(tbl14)){
			count_col <- which(names(tbl14) == "Limited English Proficient (LEP) Speakers (Age 5+)")
			addStyle(wb, sheet=sheet_name, style=count_fmt_style,
					 rows=data_rows, cols=count_col, gridExpand=TRUE, stack=TRUE)
		}

		if("LEP Speakers as % Share of All Speakers (Age 5+)" %in% names(tbl14)){
			pct_col <- which(names(tbl14) == "LEP Speakers as % Share of All Speakers (Age 5+)")
			addStyle(wb, sheet=sheet_name, style=percent_fmt_style,
					 rows=data_rows, cols=pct_col, gridExpand=TRUE, stack=TRUE)
		}

		if("LEP Speakers as % Share of Total Population (Age 5+)" %in% names(tbl14)){
			pct_col <- which(names(tbl14) == "LEP Speakers as % Share of Total Population (Age 5+)")
			addStyle(wb, sheet=sheet_name, style=percent_fmt_style,
					 rows=data_rows, cols=pct_col, gridExpand=TRUE, stack=TRUE)
		}
	}

	setColWidths(wb, sheet=sheet_name, cols=1, widths="auto")
	if(n_cols > 1){
		setColWidths(wb, sheet=sheet_name, cols=2:n_cols, widths=20)
	}
	freezePane(wb, sheet=sheet_name, firstActiveRow=2)
	saveWorkbook(wb, file=file_path, overwrite=TRUE)

	invisible(file_path)
}

# Example --------------------------------------------------------------------
# display_tbl14 <- get_tbl14_lep_languages(2024)
# write_tbl14_xlsx(display_tbl14, file_path="tbl14_lep_languages.xlsx")
