#' @rdname arango
#'
#' @name arango
#'
#' @title Internal interface to ArangoDB REST API
#'
#' @description
#'
#' Objects documented on this page are for developer
#' use. `arango_request()` formulates 'GET' or 'POST' to the ArangoDB
#' API.
#'
#' @param path character(1) path to the API end point. Note that
#'     database-specfic paths are prefixed with `/_db/igvf`.
#'
#' @param ... for `arango_request()`, named arguments to be used a
#'     query parameters. for `arango_cursor()`, named arguments to be
#'     interpreted as bind variables in the query.
#'
#' @param body if not NULL, formulate a POST request with JSON body.
#'
#' @param jwt_token character(1) JWT token obtained via
#'     `arango_auth()`.
#'
#' @return `arango_request()` returns the JSON response as a
#'     character(1) vector.
#'
#' @importFrom httr2 request req_url_path_append req_url_query
#'     req_auth_bearer_token req_body_raw req_perform resp_body_string
arango_request <-
    function(path, ..., body = NULL, jwt_token = NULL)
{
    rigvf_request(
        rigvf_config$get("db_host"), path,
        ...,
        body = body,
        jwt_token = jwt_token
    )
}

## endpoints

#' @rdname arango
#'
#' @description `arango_auth()` uses username and password to
#'     authenticate against the database.
#'
#' @param username character(1) ArangoDB user name. Default: "guest".
#'
#' @param password character(1) ArangoDB password. Default:
#'     "guestdaccigvfcatalog". A better practice is to use an environment
#'     variable to record the password, rather than encoding in a
#'     script, so `password = Sys.getenv("RIGVF_ARANGODB_PASSWORD")`.
#'
#' @details `arango_auth()` is 'memoized', so invoked only once per
#'     hour for a particular user and password. The memoised result
#'     can be cleared with `memoise::forget(arango_auth)`.
#'
#' @return `arango_auth()` returns a JWT token to be used for
#'     authentication in subsequent calls.
#'
#' @importFrom rjsoncons j_query
arango_auth <-
    function(
        username = rigvf_config$get("username"),
        password = rigvf_config$get("password"))
{
    body <- js_template(
        "arango_auth",
        username = username, password = password
    )
    json <- arango_request("/_open/auth", body = body)
    j_query(json, "jwt", as = "R")
}

#' @rdname arango
#'
#' @description `arango_collections()` implements the `_api/collection`
#'     endpoint for available collections in the IGVF database.
#'
#' @return `arango_collections()` returns a tibble with columns
#'     `name`, `type` (either 'node' or 'edge'), and `count`.
#'
#' @importFrom rjsoncons j_query
#'
#' @importFrom dplyr tibble .data bind_cols mutate filter select
#'     arrange case_match desc
#'
#' @importFrom tidyr unnest_wider
arango_collections <-
    function(
        username = rigvf_config$get("username"),
        password = rigvf_config$get("password"))
{
    ## authenticate
    jwt_token <- arango_auth(username, password)

    ## all collection names
    path <- "/_db/igvf/_api/collection"
    json <- arango_request(
        path,
        excludeSystem = "true",
        jwt_token = jwt_token
    )
    collection_names <- j_query(json, "result[].name", as = "R")

    ## collection information -- count and type
    responses <- lapply(collection_names, function(name) {
        path <- paste0("/_db/igvf/_api/collection/", name, "/count")
        json <- arango_request(path, jwt_token = jwt_token)
        info <- j_query(json, "{count: count, type: type}", as = "R")
        unlist(info) # character vector
    })

    ## tidy response, including recoding 'type' to 'node' or 'edge'
    tbl <-
        tibble(responses) |>
        unnest_wider("responses") |>
        bind_cols(tibble(name = collection_names)) |>
        mutate(
            type = case_match(type, 2 ~ "node", 3 ~ "edge")
        )

    ## return with 'name' in first column
    select(tbl, "name", "type", "count")
}

#' @rdname arango
#'
#' @description `arango_cursor()` implements the `_api/cursor`
#'     endpoint to allow a user-specified query of the IGVF database.
#'
#' @details
#'
#' `arango_cursor()` expects queries to be written in package system
#' files in the `inst/aql` directory. This allows rapid iteration
#' during query development (the package does not need to re-loaded
#' when the query is updated) and some opportunity for
#' language-specific highlighting if supported by the developer's text
#' editor.
#'
#' @param query character(1) the FILE NAME (without extension `.aql`
#'     of the query template.
#'
#' @return `arango_cursor()` returns the JSON character(1) 'result' of
#'     the query.
#'
#' @examples
#' ## available queries
#' templates <- system.file(package = "rigvf", "aql")
#' dir(templates)
arango_cursor <-
    function(
        query, ...,
        username = rigvf_config$get("username"),
        password = rigvf_config$get("password")
    )
{
    jwt_token <- arango_auth(username, password)

    lst <- list(...)
    if (length(lst) == 0L)
        ## force this to JSON object
        names(lst) <- character()
    bind_vars <- jsonlite::toJSON(lst, auto_unbox = TRUE)
    body <- js_template(
        "arango_cursor",
        query = query, bindVars = bind_vars
    )

    response <- arango_request(
        "/_db/igvf/_api/cursor",
        body = body, jwt_token = jwt_token
    )

    j_query(response, "result")
}
