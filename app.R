library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

challenges <- read_rds("challenges.Rds")
test_cases <- read_rds("test_cases.Rds")
chat <- paste("Feel free to use this space to ask questions about the challenges,",
			  "but please be respectful of the others competing and don't give away",
			  "answers.")
global_vars <- reactiveValues(challenges=challenges, 
							  test_cases=test_cases,
							  scores=tibble(username="", completed=NA, last_submission=NA),
							  chat=chat,
							  users=NULL)

# Get the prefix for the line to be added to the chat window. Usually a newline
# character unless it's the first line.
line_prefix <- function(){
	if (is.null(isolate(global_vars$chat))){
		return("")
	}
	return("<br />")
}

header <- dashboardHeader(disable=TRUE)
sidebar <- dashboardSidebar(
	sidebarMenu(
		menuItem("Challenges", tabName="challenges", icon=icon("chess", class="fa-2x")),
		menuItem("Scoreboard", tabName="scoreboard", icon=icon("trophy", class="fa-2x")),
		menuItem("Chat Room", tabName="chat", icon=icon("comment", class="fa-2x")),
		menuItem("Cheatsheet", tabName="cheatsheet", icon=icon("file", class="fa-2x"))
	)
)
body <- dashboardBody(
	tags$head(tags$link(type="text/css", rel="stylesheet", href="main.css"),
			  tags$link(type="text/css", rel="stylesheet", href="chat.css"),
			  tags$script(src="chat.js")),
	
	tabItems(
		tabItem(tabName="challenges",
				fluidRow(
					column(width=5, offset=3, align="left", 
						   textInput("regex", label=NULL, placeholder="Enter regex pattern here", width="100%")
					),
					column(width=1, actionButton("submit", "Submit"))
				),
				fluidRow(style="height: 82vh; margin-top: 2vh; overflow: auto;",
					column(width=10, offset=1,
						   dataTableOutput("test_cases")
					)
				),
				fluidRow(style="margin-top: 1vh;",
					column(width=4, align="left", actionButton("prv", "Previous")),
					column(width=4, align="center", actionButton("question", "Show Challenge")),
					column(width=4, align="right", actionButton("nxt", "Next"))
				)
		),
		
		tabItem(tabName="scoreboard",
				dataTableOutput("scores")),
		
		tabItem(tabName="chat",
				fluidRow(
					column(width=8, style="margin-left: 20px;",
						   fluidRow(width=12, style="height: 85vh;", uiOutput("chat")),
						   fluidRow(
						   		column(width=10, id="message_input",
							   		 	textInput("entry", label=NULL, placeholder="Type your message...")
							   	),
							   	column(width=2, style="margin-top: 1vh;",
							   		   actionButton("send", "Send")
							   	)
						   )
					),
					column(width=3, align="right", style="height: 85vh; overflow: auto;", 
						   dataTableOutput("players"))
				),
				div(HTML(paste("The code for this chat room was adopted from the great", 
							   "work done <a href='https://github.com/trestletech/ShinyChat'>here</a>.")))
		),
		
		tabItem(tabName="cheatsheet",
				div(style="height: 96vh;",
					tags$iframe(style="height: 100%; width: 100%;", src="cheatsheet.pdf")
				)
		)
	)
)

ui <- dashboardPage(header, sidebar, body, title="Misec Regex CTF", skin="black")

server <- function(input, output, session) {
	session_vars <- reactiveValues(index=1, username="", solved=0)
	
	# signing in
	msg <- paste("Welcome to the #misec Regex CTF Event!",
				 "What follows is a series of 10 challenges",
				 "that require you to solve real-world security problems",
				 "using regex. The challenges will vary in difficulty and",
				 "can be solved in any order. If you have trouble, be sure",
				 "to check out the cheatsheet, or ask for help in #misec's",
				 "Slack in the #detroit channel.</br></br>To get started, enter a",
				 "username below.</br></br>")
	showModal(modalDialog(HTML(msg), 
						  textInput("username", label=NULL, placeholder="enter a username"),
						  title="Welcome!", 
						  easyClose=F, 
						  size="s", 
						  footer=actionButton("user_submit", "OK")))
	
	observeEvent(input$user_submit, {
		req(input$username)
		
		if (input$username %in% global_vars$user) {
			showNotification("That name is currently in use.", duration=5, closeButton=T, type="error")
		} else {
			session_vars$username <<- input$username
			df <- tibble(username=input$username, completed=0, last_submission=as.character(Sys.time()))
			scores <- bind_rows(global_vars$scores, df)
			
			# need to prevent people from simply refreshing the app and solving challenges they already
			# solved to move up the ranks. Always keep the record with the lowest score.
			scores <- scores %>%
				filter(!is.na(username) & username != "") %>%
				group_by(username) %>%
				arrange(completed) %>%
				slice(1) %>%
				ungroup() %>%
				arrange(desc(completed), desc(last_submission))
			write_rds(scores, "scores.Rds")
			global_vars$scores <<- scores
			removeModal(session=session)
			
			# show user in chat
			isolate({
				global_vars$chat <<- c(global_vars$chat, 
									   paste0(line_prefix(),
									   	   tags$span(class="user-enter",
									   	   		  session_vars$username,
									   	   		  "entered the room.")))
				global_vars$users <<- c(global_vars$users, session_vars$username)
			})
		}
	})
	
	# challenges
	
	output$test_cases <- renderDataTable({
		data <- global_vars$test_cases[[session_vars$index]]
		data <- data %>%
			mutate(matched = ifelse(matched, 
									as.character(icon("check")),
									as.character(icon("times"))),
				   include = ifelse(include, 
				   				 	as.character(icon("check")), 
				   				 	as.character(icon("times")))
			)
		datatable(data,
				  options=list(ordering=F, bLengthChange=F, searching=F, paging=F,
				  			 columnDefs=list(list(className="dt-center", targets=c(0, 2:4)),
				  			 				 list(visible=FALSE, targets=c(2)))),
				  colnames=c("No.", "Text", "Found", "Should Match", "Does Match"),
				  escape=F) %>%
			formatStyle("matched", 
						color=styleEqual(c(as.character(icon("check")), as.character(icon("times"))), 
										 c("green", "red"))) %>%
			formatStyle("include", 
						color=styleEqual(c(as.character(icon("check")), as.character(icon("times"))), 
										 c("green", "red")))
	})
	
	observeEvent(input$submit, {
		req(input$regex)
		
		if (session_vars$username == "") {
			msg <- paste("Congratulations. You found a way around submitting a username.",
						 "However, if you'd like to play, you'll need to create a username.",
						 "Refresh the app and try again.")
			showModal(modalDialog(msg, title="Tsk Tsk...", easyclose=F, footer=NULL, size="s"))
			return(NULL)
		}
		
		data <- global_vars$test_cases[[session_vars$index]]
		result <- str_extract(data$text, input$regex)
		result[is.na(result)] <- ""
		match <- result == data$found & result != ""
		data <- data %>%
			mutate(matched = ifelse(match,
									as.character(icon("check")),
									as.character(icon("times"))),
				   include = ifelse(include, 
				   				 	as.character(icon("check")), 
				   				 	as.character(icon("times"))))
		proxy <- dataTableProxy("test_cases", session=session)
		replaceData(proxy, data)
		
		if (all(data$include == data$matched)) {
			showModal(modalDialog(paste("Challenge", session_vars$index, "has been completed!"),
								  title="Congratulations!", easyClose=TRUE, size="s"))
			
			if (!session_vars$index %in% session_vars$solved) {
				scores <- global_vars$scores
				df1 <- filter(scores, username == session_vars$username)
				df2 <- filter(scores, username != session_vars$username)
				global_vars$scores <<- df1 %>%
					mutate(completed = completed + 1,
						  last_submission = as.character(Sys.time())) %>%
					bind_rows(df2) %>%
					filter(!is.na(username) & username != "") %>%
					arrange(desc(completed), desc(last_submission))
				session_vars$solved <<- c(session_vars$solved, session_vars$index)
				write_rds(global_vars$scores, "scores.Rds")
			}
		}
	})
	
	observeEvent(input$prv, {
		if (session_vars$index != 1) {
			session_vars$index <<- session_vars$index - 1
			updateTextInput(session, "regex", value="", placeholder="Enter regex pattern here")
		}
		return(NULL)
	})
	
	observeEvent(input$nxt, {
		if (session_vars$index != 10) {
			session_vars$index <<- session_vars$index + 1
			updateTextInput(session, "regex", value="", placeholder="Enter regex pattern here")
		}
		return(NULL)
	})
	
	observeEvent(input$question, {
		msg <- global_vars$challenges[[session_vars$index]]
		showModal(modalDialog(msg, title=paste("Challenge", session_vars$index),
							  size="s", easyClose=T))
	})
	
	# scoreboard
	output$scores <- renderDataTable({
		scores <- global_vars$scores
		datatable(scores, 
				  options=list(ordering=F, bLengthChange=F,
				  			 columnDefs=list(list(className='dt-center', targets=0:3))),
				  colnames=c("Rank", "Username", "Challenges Completed", "Latest Progress"))
	})
	
	# chat room
	# Dynamically create the UI for the chat window.
	output$chat <- renderUI({
		if (length(global_vars$chat) > 500) {
			# Too long, use only the most recent 500 lines
			n <- length(global_vars$chat)
			global_vars$chat <<- global_vars$chat[(n-500):n]
		}
		# Save the chat object so we can restore it later if needed.
		write_rds(global_vars$chat, "chat.Rds")
		
		# Pass the chat log through as HTML
		HTML(global_vars$chat)
	})
	
	output$players <- renderDataTable({
		data <- tibble(users=global_vars$users)
		datatable(data, options=list(paging=F, ordering=F, searching=F, dom="t"),
				  colnames="Current Players",
				  rownames=F)
	})
	
	# Listen for input$send changes (i.e. when the button is clicked)
	observeEvent(input$send, {
		isolate({
			# Add the current entry to the chat log.
			global_vars$chat <<- c(global_vars$chat, 
								   paste0(line_prefix(),
								   	   	  tags$span(class="username",
								   		  tags$abbr(title=Sys.time(), session_vars$username)
								   ),
								   ": ",
								   tagList(input$entry)))
		})
		# Clear out the text entry field.
		updateTextInput(session, "entry", value="")
	})
	
	# session end
	# When a session is ended, remove the user and note that they left the room. 
	session$onSessionEnded(function() {
		isolate({
			global_vars$users <<- global_vars$users[global_vars$users != session_vars$username]
			global_vars$chat <<- c(global_vars$chat, 
								   paste0(line_prefix(),
								  	      tags$span(class="user-exit",
										  session_vars$username,
										  "left the room.")))
		})
	})
}

shinyApp(ui = ui, server = server)

