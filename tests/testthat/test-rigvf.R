test_that("rigvf_config contains expected data", {
    keys <- c("catalog_host", "db_host", "db_name", "username", "password")
    expect_true(all(keys %in% rigvf_config$ls()))
    expect_identical(
        rigvf_config$get("catalog_host"),
        "https://api.catalogkg.igvf.org"
    )
    expect_identical(
        rigvf_config$get("db_host"),
        "https://db.catalog.igvf.org"
    )
    expect_identical(rigvf_config$get("db_name"), "igvf")

    bad_key <- basename(tempfile("key-"))
    expect_error(rigvf_config$get(bad_key))
})

test_that("oneof_is_scalar_character() works", {
    expect_true(oneof_is_scalar_character("a"))
    expect_true(oneof_is_scalar_character("a", NULL))
    expect_true(oneof_is_scalar_character(NULL, "a"))
    expect_false(oneof_is_scalar_character(c("a", "b")))
    expect_false(oneof_is_scalar_character("a", "b"))
    expect_false(oneof_is_scalar_character("a", NULL, "b"))
})

test_that("rigvf_request() works", {
    skip_if_offline()
    j_query <- rjsoncons::j_query

    ## GET
    base_url <- "https://httpbin.org"
    expect_silent(response <- rigvf_request(base_url, "anything"))
    expect_identical(j_query(response, "method"), "GET")
    expect_identical(j_query(response, "args"), '{}')
    expect_identical(j_query(response, "headers.Authorization"), "null")

    response <- rigvf_request(base_url, "anything", x = 1)
    expect_identical(j_query(response, "args"), '{"x":"1"}')

    response <- rigvf_request(base_url, "anything", x = 1, y = 2)
    expect_identical(j_query(response, "args"), '{"x":"1","y":"2"}')

    response <- rigvf_request(base_url, "anything", jwt_token = "jwt_token")
    expect_identical(
        j_query(response, "headers.Authorization"),
        paste("Bearer", "jwt_token")
    )

    ## POST
    response <- rigvf_request(base_url, "anything", body = "foo")
    expect_identical(j_query(response, "method"), "POST")
    expect_identical(j_query(response, "data"), "foo")
})
