# library(civicscienceR)
# library(dplyr)
# library(RMySQL)
# library(DBI)
# library(stringr)
# # library(xlsx)
# # library(lubridate)
# # library(httr)
# # library(parallel)
# # library(data.table)

scorecard_label_handler <- function(scorecard_uuid, bi.user, bi.password){

	scorecard_uuid <- gsub('-', '', scorecard_uuid)

	########################
	# CONNECT TO DATABASES #
	########################


	# establish connections to our databases
	cs.con <- dbConnect(RMySQL::MySQL(),
											username = bi.user,
											password = bi.password,
											host = "cs-nonwriter.civicscience.com",
											port = 3306,
											dbname = "civicscience")

	query_score_table_row <- paste0("SELECT DISTINCT sort_order, label, str.question_refid, qtext, ci.group_refid, score_uri FROM score_table_row str ",
																	"JOIN question q ON str.question_refid = q.question_id ",
																	"LEFT JOIN checkbox_item ci ON str.question_refid = ci.question_refid ",
																	"WHERE score_table_uuid = unhex('", scorecard_uuid, "')")

	query_score_table_multiscore_row <- paste0("SELECT sort_order, label, score_refid, `name` ",
																						 "FROM score_table_multiscore_row stmr ",
																						 "JOIN compound_score cs ON stmr.score_refid = cs.id ",
																						 "WHERE stmr.score_table_uuid = unhex('", scorecard_uuid, "')")

	str_output <- dbGetQuery(query_score_table_row, conn = cs.con)
	stmr_output <- dbGetQuery(query_score_table_multiscore_row, conn = cs.con)

	all_rows <- which(grepl("apct/", str_output$score_uri) == TRUE)
	checkbox_rows <- all_rows[which(!is.na(str_output$group_refid[all_rows]) == TRUE)]
	answer_rows <- all_rows[which(is.na(str_output$group_refid[all_rows]) == TRUE)]
	score_rows <- which(grepl("sql/", str_output$score_uri) == TRUE)

	if(length(checkbox_rows) > 0){
		checkbox_group_ids <- unique(str_output$group_refid[checkbox_rows])

		query_checkbox <- paste0("SELECT group_id, text FROM checkbox_group WHERE group_id IN (", paste(checkbox_group_ids, collapse=", "), ")")
		checkbox_output <- dbGetQuery(query_checkbox, conn = cs.con)

	}

	checkbox_answers <- gsub("apct/", "", str_output$score_uri[checkbox_rows]) %>%
		paste(., collapse = ", ")

	answer_choices <- gsub("apct/", "", str_output$score_uri[answer_rows]) %>%
		paste(., collapse = ", ")

	scores <- gsub("sql/", "", str_output$score_uri[score_rows]) %>%
		paste(., collapse = ", ")


	checkbox_table_query <- paste0("SELECT DISTINCT checked_answer_refid, `text` ",
																 "FROM checkbox_item ",
																 "WHERE checked_answer_refid IN (", checkbox_answers, ")")

	answer_table_query <- paste0("SELECT DISTINCT answer_choice_id, ac.`answer_choice` ",
															 "FROM answer_choice ac ",
															 "WHERE answer_choice_id IN (", answer_choices, ")")

	score_table_query <- paste0("SELECT DISTINCT `id`, `label` ",
															"FROM score ",
															"WHERE `id` IN (", scores, ")")

	checkbox_ref_table <- dbGetQuery(checkbox_table_query, conn = cs.con)
	answer_ref_table <- dbGetQuery(answer_table_query, conn = cs.con)
	score_ref_table <- dbGetQuery(score_table_query, conn = cs.con)

	# Disconnect from database - not necessary, but good practice
	dbDisconnect(cs.con)

	colnames(checkbox_ref_table) <- c("score_uri", "new label")
	colnames(answer_ref_table) <- colnames(checkbox_ref_table)
	colnames(score_ref_table) <- colnames(checkbox_ref_table)

	brand_rows <- which(grepl("brand/", str_output$score_uri) == TRUE)

	str_output$score_uri <- gsub("brand/aware", "Awareness", str_output$score_uri)
	str_output$score_uri <- gsub("brand/fav-all", "Favorability", str_output$score_uri)
	str_output$score_uri <- gsub("brand/fav-eff", "Effective Favorability", str_output$score_uri)

	str_output$score_uri <- gsub(".*/","", str_output$score_uri)

	checkbox_table <- str_output[checkbox_rows, ]
	checkbox_table$score_uri <- as.numeric(checkbox_table$score_uri)

	answer_table <- str_output[answer_rows, ]
	answer_table$score_uri <- as.numeric(answer_table$score_uri)

	score_table <- str_output[score_rows, ]
	score_table$score_uri <- as.numeric(score_table$score_uri)

	checkbox_table <- right_join(checkbox_table, checkbox_ref_table, by = "score_uri")
	answer_table <- right_join(answer_table, answer_ref_table, by = "score_uri")
	score_table <- right_join(score_table, score_ref_table, by = "score_uri")
	brand_table <- str_output[brand_rows, ]
	brand_table$`new label` <- brand_table$score_uri

	multiscore_table <- tibble(
		'sort_order' = stmr_output$sort_order,
		'label' = stmr_output$label,
		'question_refid' = stmr_output$name,
		'qtext' = stmr_output$name,
		'group_refid' = NA,
		'score_uri' = NA,
		'new label' = stmr_output$name
	)

	totalResponses_rows <- which(grepl("totalResponses", str_output$score_uri) == TRUE)
	totalResponses_table <- str_output[totalResponses_rows, ]

	totalResponses_table$`new label` <- 'Total Responses'

	scorecard <- rbind(checkbox_table, answer_table, score_table, brand_table, multiscore_table, totalResponses_table)

	supergroup_rows <- grep("Supergroup", scorecard$label)
	scorecard$Supergroup <- NA

	all_group_text <- NULL

	for(i in supergroup_rows){
		group_text <- str_extract_all(scorecard$label[i], '(?<=Supergroup\\s)\\w+') %>%
			unlist()
		group_text_length <- length(group_text)

		all_group_text <- c(all_group_text, paste0('Supergroup ', group_text), paste0('Supergroup ', group_text, ' '))

		if(group_text_length == 1){
			scorecard$Supergroup[i] <- group_text

		} else{
			scorecard$Supergroup[i] <- group_text[1]

			for(j in 2:group_text_length){
				next_row <- nrow(scorecard)+j-1
				scorecard[next_row, ] <- scorecard[i, ]
				scorecard$question_refid[next_row] <- NA_character_
				scorecard$Supergroup[next_row] <- group_text[j]
			}
		}
	}

	for(k in all_group_text){
		scorecard$label <- gsub(k, '', scorecard$label)
	}

	scorecard$label <- gsub(' > Awareness Index', ' > Awareness', scorecard$label)
	scorecard$label <- gsub(' > Effective Favorability Index', ' > Effective Favorability', scorecard$label)
	scorecard$label <- gsub(' > Favorability Index', ' > Favorability', scorecard$label)

	question_table <- scorecard[which(!is.na(scorecard$question_refid)), ]
	question_table <- question_table[which(is.na(question_table$group_refid)), ]

	checkbox_question_table <- scorecard[which(!is.na(scorecard$group_refid)), ]
	checkbox_question_table$question_refid <- paste0('CB ', checkbox_question_table$group_refid)
	checkbox_label_rows <- which(checkbox_question_table$label != '')
	if(length(checkbox_label_rows) > 0){
		checkbox_question_table$`new label`[checkbox_label_rows] <- checkbox_question_table$label[checkbox_label_rows]
	}
	checkbox_question_table <- left_join(checkbox_question_table, checkbox_output, by = c("group_refid" = "group_id"))
	checkbox_question_table$qtext <- checkbox_question_table$text
	checkbox_question_table <- checkbox_question_table[ , -which(colnames(checkbox_question_table) == 'text')]

	supergroup_table <- scorecard[which(!is.na(scorecard$Supergroup)), ]
	supergroup_table$question_refid <- paste0('SG ', supergroup_table$Supergroup)
	supergroup_table$qtext <- paste0('Supergroup ', supergroup_table$Supergroup)
	supergroup_label_rows <- which(supergroup_table$label != '')
	if(length(supergroup_label_rows) > 0){
		supergroup_table$`new label`[supergroup_label_rows] <- supergroup_table$label[supergroup_label_rows]
	}

	output <- rbind(question_table, checkbox_question_table, supergroup_table) %>%
		arrange(sort_order)

	output <- output[, c('sort_order', 'question_refid', 'qtext', 'new label')]
	colnames(output) <- c('sort_order', 'Question ID', 'Question Text', 'Answer Text')

	return(output)
}
