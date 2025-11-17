test_that("catalog request works", {
    skip_if_offline()
    catalog_host <- rigvf_config$get("catalog_host")
    on.exit(rigvf_config$set("catalog_host", catalog_host))

    ## redirect catalog requests to httpbin.org.
    base_url <- "https://httpbin.org/anything"
    rigvf_config$set("catalog_host", base_url)

    expect_silent(response <- gene_variants("123"))
    ## response as a tibble; check specfic fields
    expect_identical(response$method, "GET")
    expect_identical(
        response$args,
        list(list(
            gene_id = "123", limit = "25", organism = "Homo sapiens", page = "0", verbose = "false"
        ))
    )
    expect_identical(
        response$url,
        paste0(
          "https://httpbin.org/anything/api/genes/variants",
          "?gene_id=123&organism=Homo sapiens&page=0&limit=25&verbose=false"
        )
    )
})
