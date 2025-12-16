library(shiny)

S3_ENDPOINT <- "s3.cl4.du.cesnet.cz"
S3_BUCKET <- "metasensors"

get_secret <- function(name) {
    path <- stringr::str_glue("/etc/secrets/{name}")
    if(file.exists(path)) {
        return(readLines(path, warn = FALSE)[1])
    }
    return(NULL)
}

S3_KEY_ID <- get_secret("access-key")
S3_SECRET_KEY <- get_secret("secret-key")

db_data <- new.env()

get_opts <- function() {
    option_list <- list(
        optparse::make_option(c("-p", "--port"), type = "integer", default = NULL,
                              help = "Port number", metavar = "PORT"),
        optparse::make_option(c("-d", "--db_file"), type = "character", default = NULL,
                              help = "Database file path", metavar = "FILE"),
        optparse::make_option(c("-a", "--auth"), type = "logical", default = FALSE,
                              help = "Enable authentication", action = "store_true"),
        optparse::make_option(c("--host"), type = "character", default = "127.0.0.1",
                              help = "Set host", metavar = "HOST"),
        optparse::make_option(c("-l", "--launch"), type = "logical", default = FALSE,
                              help = "Launch browser", action = "store_true")
    )

    opt_parser <- optparse::OptionParser(option_list = option_list)
    opts <- optparse::parse_args(opt_parser)
    return(opts)
}

opts <- get_opts()

www_dir <- file.path(getwd(), "www")
if (dir.exists(www_dir)) {
    shiny::addResourcePath("static", www_dir)
}

get_db_file <- function(opt_db_file) {
    if(!is.null(opt_db_file)) {
        return(opt_db_file)
    }
    if("date" %in% names(db_data) && (as.Date(db_data$date) == Sys.Date())) {
        return(NULL)
    }
    if("last_check" %in% names(db_data) && (lubridate::now() - db_data$last_check < lubridate::hours(3))) {
        return(NULL)
    }
    files <- aws.s3::get_bucket_df(
        bucket = S3_BUCKET,
        key = S3_KEY_ID,
        secret = S3_SECRET_KEY,
        base_url = S3_ENDPOINT,
        region = "") |>
        dplyr::filter(stringr::str_ends(.data$Key, ".rds"))
    last_modified <- max(files$LastModified)
    files <- dplyr::filter(files, .data$LastModified == last_modified)
    last_file <- files$Key[[1]]
    aws.s3::save_object(
        object = last_file,
        file = last_file,
        bucket = S3_BUCKET,
        key = S3_KEY_ID,
        secret = S3_SECRET_KEY,
        base_url = S3_ENDPOINT,
        region = ""
    )
    return(last_file)
}

get_db_date_text <- function() {
    if("date" %in% names(db_data)) {
        return(stringr::str_glue("DB date: {as.Date(db_data$date)}"))
    }
    return("DB date: unknown")
}

load_db_data <- function(opt_db_file=NULL, db_date_text_val=NULL) {
    file_path <- get_db_file(opt_db_file)
    if(is.null(file_path)) {
        return()
    }
    remote_data <- readRDS(file_path)
    purrr::walk(names(remote_data), function(name) {
        db_data[[name]] <- remote_data[[name]]
    })
    db_data$last_check <- lubridate::now()
    if(!is.null(db_date_text_val)) {
        db_date_text_val(get_db_date_text())
    }
}

load_db_data(opts$db_file)

get_projects_choices <- function() {
    names <- db_data$projects$project_name
    result <- as.list(db_data$projects$project_id)
    names(result) <- names
    result["NULL"] <- "NULL"
    return(result)
}

ui <- fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}")),
        tags$link(rel="icon", type="image/svg+xml", href="static/icon.svg")
    ),
    h1("GeoEko DB"),
    textOutput("db_date_text"),
    fluidRow(
        column(12, hr())
    ),
    h2("Search from all"),
    fluidRow(
        column(3, textInput("locality_text", "Locality id:", 
                            width="100%")),
        column(3, textInput("serial_number_text", "Serial number:", 
                            width="100%"))
    ),
    fluidRow(
        column(12, verbatimTextOutput("search_info"))
    ),
    fluidRow(
        column(12, hr())
    ),
    h2("Select items"),
    fluidRow(
        column(6, selectizeInput("project_select", "Project:", 
                                 choices = get_projects_choices(),
                                 selected = 5,
                                 width="100%"))
    ),
    fluidRow(
        column(6, selectizeInput("locality_select", "Locality:",
                                 choices = NULL, width="100%"))
    ),
    fluidRow(
        column(12, uiOutput("mapy_link"))
    ),
    fluidRow(
        column(12, verbatimTextOutput("locality_text"))
    ),
    fluidRow(
        column(6, checkboxInput("present_checkbox", "Present only",
                                value=TRUE, width="100%"))
    ),
    fluidRow(
        column(6, selectizeInput("logger_select", "Logger:",
                                 choices = NULL, width="100%"))
    ),
    fluidRow(
        column(12, verbatimTextOutput("logger_text"))
    ),
    fluidRow(
        column(12, DT::dataTableOutput("records_table"))
    ),
)

if(opts$auth) {
    ui <- shinymanager::secure_app(ui)
}

server <- function(input, output, session) {
    db_date_text_val <- reactiveVal(get_db_date_text())
    if(is.null(opts$db_file)) {
        load_db_data(db_date_text_val=db_date_text_val)
    }
    selected_locality_id_val <- reactiveVal(NULL)
    selected_serial_number_val <- reactiveVal(NULL)
    find_search_info_val <- reactiveVal(NULL)

    if(opts$auth) {
        credentials <- data.frame(
                user = get_secret("auth-user"),
                password = get_secret("auth-passwd"),
                stringsAsFactors = FALSE
            )
        res_auth <- shinymanager::secure_server(shinymanager::check_credentials(credentials))
    }

    observeEvent(input$project_select, {
        selected_project_id <- input$project_select
        choices <- .get_locality_choices(selected_project_id)
        updateSelectInput(session, "locality_select", choices = choices)
        if(!is.null(selected_locality_id_val())) {
            updateSelectInput(session, "locality_select", selected=selected_locality_id_val())
            selected_locality_id_val(NULL)
        }
    })
    observeEvent(input$locality_select, {
        selected_locality_id <- input$locality_select
        choices <- .get_logger_choices(selected_locality_id, input$present_checkbox)
        updateSelectInput(session, "logger_select", choices = choices)
        if(!is.null(selected_serial_number_val())) {
            updateSelectInput(session, "logger_select", selected=selected_serial_number_val())
            selected_serial_number_val(NULL)
        }
    })
    observeEvent(input$present_checkbox, {
        selected_locality_id <- input$locality_select
        choices <- .get_logger_choices(selected_locality_id, input$present_checkbox)
        selected_logger_id <- input$logger_select
        updateSelectInput(session, "logger_select", choices = choices)
        if(selected_logger_id %in% choices) {
            updateSelectInput(session, "logger_select", selected=selected_logger_id)
        }
    })
    observeEvent(input$serial_number_text, {
        serial_number <- input$serial_number_text
        result <- list(serial_number=serial_number)
        if(serial_number == "" || !(serial_number %in% db_data$loggers$serial_number)) {
            result[["found"]] <- FALSE
            find_search_info_val(result)
            return()
        }
        result[["found"]] <- TRUE
        records <- dplyr::filter(db_data$records, .data$serial_number_rec == serial_number)
        result[["count_records"]] <- nrow(records)
        if(nrow(records) == 0) {
            find_search_info_val(result)
            return()
        }
        result <- .set_all_by_serial_number(session, input, serial_number, records,
                                            selected_locality_id_val, selected_serial_number_val,
                                            result)
        find_search_info_val(result)
    })
    observeEvent(input$locality_text, {
        locality_id <- input$locality_text
        result <- list(locality_id=locality_id)
        if(locality_id == "" || !(locality_id %in% db_data$localities$locality_id)) {
            result[["found"]] <- FALSE
            find_search_info_val(result)
            return()
        }
        result[["found"]] <- TRUE
        .set_project_locality_logger(session, input, locality_id, NULL,
                                     selected_locality_id_val, selected_serial_number_val)
        find_search_info_val(result)
    })
    output$locality_text <- renderText(.get_locality_text(input$locality_select))
    output$mapy_link <- renderUI({
        return(.get_mapy_link(input$locality_select))
    })
    output$logger_text <- renderText(.get_logger_text(input$logger_select))
    output$records_table <- DT::renderDataTable({
        filter_locality_id <- NULL
        if (input$present_checkbox) {
            filter_locality_id <- input$locality_select
        }
        records <- .get_records_table(input$logger_select, filter_locality_id)
        DT::datatable(records, rownames=FALSE,
                      options=list(paging=FALSE))
    })
    output$search_info <- renderText(.get_serial_number_info(find_search_info_val()))
    output$db_date_text <- renderText({return(db_date_text_val())})
}

.get_locality_choices <- function(project_id) {
    if(project_id == "NULL") {
        assigned_localities <- unique(db_data$localities_projects$locality_id_locprj)
        localities <- dplyr::filter(db_data$localities, !(.data$locality_id %in% assigned_localities))
    } else {
        localities <- dplyr::filter(db_data$localities_projects, .data$project_id_locprj == project_id)
        colnames(localities) <- c("locality_id", "project_id")
        localities <- dplyr::left_join(localities, db_data$localities, by="locality_id")
    }

    name_function <- function(locality_id, date_terminated) {
        if(is.na(date_terminated)) {
            return(locality_id)
        }
        return(stringr::str_glue("{locality_id} (✝{date_terminated})"))
    }

    localities$name <- purrr::map2_chr(localities$locality_id, localities$date_terminated, name_function)
    result <- as.list(localities$locality_id)
    names(result) <- localities$name
    return(result)
}

.get_locality_text <- function(selected_locality_id) {
    selected_locality <- dplyr::filter(db_data$localities, .data$locality_id == selected_locality_id)
    if(nrow(selected_locality) != 1) {
        return(NULL)
    }
    params <- list(date_established="Date established",
                    date_terminated="Date terminated",
                    loc_description="Description",
                    loc_remarks="Remarks",
                    lat_wgs84="Latitude",
                    lon_wgs84="Longitude",
                    elevation="Elevation")
    result <- .get_verbatim_text(selected_locality, params)
    projects <- dplyr::filter(db_data$localities_projects, .data$locality_id_locprj == selected_locality_id)
    projects <- dplyr::left_join(projects, db_data$projects, by=c("project_id_locprj"="project_id"))
    result <- paste0(result, "\n", "Projects: ", stringr::str_c(projects$project_name, collapse=", "))
    return(result)
}

.get_mapy_link <- function(selected_locality_id) {
    selected_locality <- dplyr::filter(db_data$localities, .data$locality_id == selected_locality_id)
    if(nrow(selected_locality) != 1) {
        return(NULL)
    }
    x <- selected_locality$lon_wgs84
    y <- selected_locality$lat_wgs84
    url <- stringr::str_glue("https://mapy.cz/turisticka?x={x}&y={y}&z=16&source=coor&id={x},{y}")
    return(HTML(paste(a("Mapy.cz", href=url, target="_blank"))))
}

.get_logger_choices <- function(locality_id, present_only) {
    if(is.null(locality_id) || locality_id == "") {
        return(NULL)
    }
    records <- dplyr::filter(db_data$records, .data$locality_id_rec == locality_id)
    records <- dplyr::group_by(records, .data$serial_number_rec)

    logger_function <- function(.x, serial_number_df) {
        install_dates <- .x$datum[.x$action == "instalace" & !is.na(.x$datum)]
        disable_dates <- .x$datum[.x$action2 %in% c('zruseni', 'odpis', 'vymena') & !is.na(.x$datum)]
        last_install <- ifelse(length(install_dates) == 0, NA, max(install_dates))
        last_disable <- ifelse(length(disable_dates) == 0, NA, max(disable_dates))
        type <- db_data$loggers$logger_type[db_data$loggers$serial_number == serial_number_df$serial_number_rec]
        type_text <- ifelse(is.na(type), "Unknown", type)
        name <- stringr::str_glue("{serial_number_df$serial_number_rec} {type}")
        present <- TRUE
        if(!is.na(last_disable) && (is.na(last_install) || last_disable > last_install)) {
            last_disable_datetime <- as.POSIXct(last_disable, tz='UTC', origin='1970-01-01')
            name <- stringr::str_glue("{name} (away {last_disable_datetime})")
            present <- FALSE
        }
        result <- tibble::tibble(name=name,
                                 value=serial_number_df$serial_number_rec,
                                 present=present)
        return(result)
    }

    rows <- dplyr::group_map(records, logger_function)
    df <- dplyr::bind_rows(rows)
    if(present_only) {
        df <- dplyr::filter(df, .data$present)
    }
    result <- as.list(df$value)
    names(result) <- df$name
    return(result)
}

.get_logger_text <- function(selected_serial_number) {
    selected_logger <- dplyr::filter(db_data$loggers, .data$serial_number == selected_serial_number)
    if(nrow(selected_logger) != 1) {
        return(NULL)
    }
    params <- list(logger_project="Project",
                   date_deployed="Date deployed",
                   logger_remarks="Remarks",
                   decomissioned="Decomissioned",
                   kalibrace="Calibration",
                   moisture_error="Moisture error",
                   battery_last_replace="Last battery replace")
    return(.get_verbatim_text(selected_logger, params))
}

.get_verbatim_text <- function(row, params){
    text_parts <- character()
    for(param in names(params)) {
        value <- row[[param]]
        if(!is.na(value)) {
            text_parts <- c(text_parts, stringr::str_glue("{params[[param]]}: {value}"))
        }
    }
    return(stringr::str_c(text_parts, collapse="\n"))
}

.get_records_table <- function(selected_logger, filter_locality_id=NULL) {
    records <- dplyr::filter(db_data$records, .data$serial_number_rec == selected_logger)
    if(!is.null(filter_locality_id)) {
        records <- dplyr::filter(records, .data$locality_id_rec == filter_locality_id)
    }
    records <- dplyr::select(records, datum, locality_id_rec, action, action2, person, field_pc,
                             rec_remarks, out_of_soil, shield_missing, data_saved, battery_replace)
    records$datum <- format(records$datum, "%Y-%m-%d")
    return(records)
}

.set_project_locality_logger <- function(session, input, locality_id, serial_number,
                                        selected_locality_id_val, selected_serial_number_val) {
    projects <- dplyr::filter(db_data$localities_projects, .data$locality_id_locprj == locality_id)
    if(nrow(projects) == 0) {
        project_id <- "NULL"
    } else {
        project_id <- dplyr::first(projects$project_id_locprj)
    }
    if(input$project_select != project_id) {
        selected_locality_id_val(locality_id)
        selected_serial_number_val(serial_number)
        updateSelectInput(session, "project_select", selected=project_id)
    } else if (input$locality_select != locality_id) {
        selected_serial_number_val(serial_number)
        updateSelectInput(session, "locality_select", selected=locality_id)
    } else if (input$logger_select != serial_number) {
        updateSelectInput(session, "logger_select", selected=serial_number)
    }
}

.set_all_by_serial_number <- function(session, input, serial_number, records,
                                      selected_locality_id_val, selected_serial_number_val,
                                      find_info) {
    records <- dplyr::arrange(records, .data$datum)
    locality_id <- dplyr::last(records$locality_id_rec)
    find_info[["locality_id"]] <- locality_id
    .set_project_locality_logger(session, input, locality_id, serial_number,
                                 selected_locality_id_val, selected_serial_number_val)
    return(find_info)
}

.get_serial_number_info <- function(find_info) {
    params <- list(found="Found")
    info_names <- names(find_info)
    if("serial_number" %in% info_names) {
        params[["serial_number"]] <- "Serial number"
    }
    if("count_records" %in% info_names) {
        params[["count_records"]] <- "Count records"
    }
    if("locality_id" %in% info_names) {
        params[["locality_id"]] <- "Locality ID"
    }
    return(.get_verbatim_text(find_info, params))
}


# Run the application 
db_app <- shinyApp(ui = ui, server = server)
shiny::runApp(db_app, port=opts$port, host=opts$host, launch.browser=opts$launch)
