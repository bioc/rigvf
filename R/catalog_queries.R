catalog_request <-
    function(query, ...)
{
    rigvf_request(
        paste0(rigvf_config$get("catalog_host"), "/api"),
        query, ...
    )
}

# for range queries, make e.g. chr1:101-200
# since the string is sent to IGVF, we convert from
# 1-based incoming position to 0-based outgoing
range_to_string <- 
    function(range)
{
    # output 0-based range
    paste0(
        seqnames(range),":",
        format(start(range) - 1, scientific=FALSE),
        "-",
        format(end(range), scientific=FALSE)
    )
}

#' @rdname catalog_queries
#' 
#' @name catalog_queries
#'
#' @title Query the IGVF Catalog via REST API
#'
#' @description This page documents functions using the IGVF REST
#'     API, documented at <https://api.catalogkg.igvf.org/#>.
#'
#' @description Note that functions will only return a limited number
#'     of responses, see `limit` and `page` arguments below for control
#'     over number of responses.
#'     
#' `gene_variants()` locates variants associated with a gene. 
#'      Only one of `gene_id`, `hgnc`, `gene_name`, or `alias` 
#'      should be specified.
#' 
#' @param gene_id character(1) Ensembl gene identifier, e.g., "ENSG00000106633"
#'
#' @param hgnc character(1) HGNC identifier
#'
#' @param gene_name character(1) Gene symbol, e.g., "GCK"
#'
#' @param alias character(1) Gene alias
#'
#' @param organism character(1) Either 'Homo sapiens' (default) or
#'     'Mus musculus'
#' 
#' @param log10pvalue character(1) The following can be used to set thresholds
#' on the negative log10pvalue: gt (>), gte (>=), lt (<), lte (<=), with a ":"
#' following and a value, e.g., "gt:5.0"
#' 
#' @param effect_size character(1) Optional string used for thresholding on 
#' the effect size of the variant on the gene. See 'log10pvalue'. E.g., "gt:0.5"
#' 
#' @param page integer(1) when there are more response items than `limit`, offers pagination.
#' starts on page 0L, next is 1L, ...
#'  
#' @param limit integer(1) the limit parameter controls the page size and can not exceed 1000
#'
#' @param verbose logical(1) return additional information about
#'     variants and genes
#'
#' @return `gene_variants()` returns a tibble describing variants
#'     associated with the gene; use `verbose = TRUE` to retrieve more
#'     extensive information.
#'
#' @examples
#' 
#' rigvf::gene_variants(gene_name = "GCK")
#' 
#' rigvf::gene_variants(gene_name = "GCK", effect_size="gt:0.5")
#'
#' rigvf::gene_variants(gene_name = "GCK", verbose = TRUE)
#' 
#' rigvf::variant_genes(spdi = "NC_000001.11:920568:G:A")
#' 
#' rigvf::gene_elements(gene_id = "ENSG00000187961")
#'
#' rng <- GenomicRanges::GRanges("chr1", IRanges::IRanges(1157520,1158189))
#' 
#' rigvf::elements(range = rng)
#' 
#' rigvf::element_genes(range = rng)
#' 
#' @export
gene_variants <-
    function(
        gene_id = NULL,
        hgnc = NULL,
        gene_name = NULL,
        alias = NULL,
        organism = "Homo sapiens",
        log10pvalue = NULL,
        effect_size = NULL,
        page = 0L,
        limit = 25L,
        verbose = FALSE)
{
    organism <- match.arg(organism)
    stopifnot(
        `only one of 'gene_id', 'hgnc', 'gene_name', 'alias' must be non-NULL` =
            oneof_is_scalar_character(gene_id, hgnc, gene_name, alias),
        is.null(log10pvalue) | is_scalar_character(log10pvalue),
        is.null(effect_size) | is_scalar_character(effect_size),
        is_scalar_integer(page),
        is_scalar_integer(limit),
        is_scalar_logical(verbose)
    )
        
    response <- catalog_request(
        "genes/variants",
        gene_id = gene_id,
        hgnc = hgnc,
        gene_name = gene_name,
        alias = alias,
        organism = organism,
        log10pvalue = log10pvalue,
        effect_size = effect_size,
        page = page,
        limit = limit,
        verbose = tolower(as.character(verbose))
    )
    j_pivot(response, as = "tibble")

}

#' @rdname catalog_queries
#' 
#' @param spdi character(1) SPDI of variant
#' 
#' @param hgvs character(1) HGVS of variant
#' 
#' @param rsid character(1) RSID of variant
#' 
#' @param variant_id character(1) IGVF variant ID
#' 
#' @param chr character(1) UCSC-style chromosome name of variant, e.g. "chr1"
#' 
#' @param position character(1) 0-based position of variant
#' 
#' @description `variant_genes()` locates genes
#'     associated with a variant.
#'     Only one of `spdi`, `hgvs`, `rsid`, `variant_id`,
#'     or `chr + position` should be specified.
#' @return `variant_genes()` returns a tibble describing genes
#'     associated with a variant; use `verbose = TRUE` to retrieve more
#'     extensive information.
#'
#' @export
variant_genes <-
    function(
        spdi = NULL,
        hgvs = NULL,
        rsid = NULL,
        variant_id = NULL,
        chr = NULL,
        position = NULL,
        organism = "Homo sapiens",
        log10pvalue = NULL,
        effect_size = NULL,
        page = 0L,
        limit = 25L,
        verbose = FALSE)
{
    organism <- match.arg(organism)
    
    chrpos <- if (!is.null(chr) & !is.null(position)) {
        paste0(chr, ":", position)
    } else {
        NULL
    }
    
    stopifnot(
        `'chr' and 'position' must be both non-NULL or NULL` = !xor(is.null(chr), is.null(position)),
        `only one of 'spdi', 'hgvs', 'rsid', 'variant_id', 'chr/position' must be non-NULL` =
            oneof_is_scalar_character(spdi, hgvs, rsid, variant_id, chrpos),
        is.null(log10pvalue) | is_scalar_character(log10pvalue),
        is.null(effect_size) | is_scalar_character(effect_size),
        is_scalar_integer(page),
        is_scalar_integer(limit),
        is_scalar_logical(verbose)
    )
    
    response <- catalog_request(
        "variants/genes",
        spdi = spdi, 
        hgvs = hgvs, 
        rsid = rsid, 
        variant_id = variant_id,
        chr = chr,
        position = position,
        organism = organism,
        log10pvalue = log10pvalue,
        effect_size = effect_size,
        page = page,
        limit = limit,
        verbose = tolower(as.character(verbose))
    )
    j_pivot(response, as = "tibble")

}

#' @rdname catalog_queries
#' 
#' @description `gene_elements()` locates elements
#'     associated with a gene.
#'
#' @return `gene_elements()` returns a tibble describing elements
#'     associated with the gene; use `verbose = TRUE` to retrieve more
#'     extensive information.
#'
#' @export
gene_elements <-
    function(
        gene_id = NULL,
        page = 0L,
        limit = 25L,
        verbose = FALSE)
{
    stopifnot(
        is_scalar_integer(page),
        is_scalar_integer(limit),
        is_scalar_logical(verbose)
    )
        
    response <- catalog_request(
        "genes/genomic-elements",
        gene_id = gene_id,
        page = page,
        limit = limit,
        verbose = tolower(as.character(verbose))
    )
    j_pivot(response, as = "tibble")

}

#' @rdname catalog_queries
#' 
#' @description `elements()` locates genomic elements
#'     based on a genomic range query.
#' 
#' @param range the query GRanges (expects 1-based start position)
#'
#' @return `elements()` returns a GRanges object describing elements.
#'
#' @importFrom GenomicRanges GRanges seqnames start end
#' @importFrom Seqinfo genome genome<-
#' @importFrom IRanges IRanges
#' @export
elements <-
    function(
        range = NULL,
        page = 0L,
        limit = 25L
    )
{
    
    igvf_genome <- "hg38" # IGVF uses this reference genome
        
    stopifnot(
        is(range, "GRanges"),
        length(range) == 1,
        all(is.na(genome(range))) | all(genome(range) == igvf_genome),
        is_scalar_integer(page),
        is_scalar_integer(limit)
    )

    response <- catalog_request(
        "genomic-elements",
        region = range_to_string(range), # converts to 0-based start
        page = page,
        limit = limit
    )
        
    tib <- j_pivot(response, as = "tibble")
    
    reserved <- c(
        "seqnames", "ranges", "strand", "seqlevels", "seqlengths",
        "isCircular", "start", "end", "width", "element"
    )
    meta <- tib[, !names(tib) %in% c("chr", "start", "end", reserved)]
    element_ranges <- GRanges(
        seqnames = tib$chr,
        ranges = IRanges(tib$start + 1, tib$end), # API gives 0-based start
        strand = "*",
        meta
    )
    genome(element_ranges) <- igvf_genome # IGVF reference genome
    element_ranges
    
}

#' @rdname catalog_queries
#' 
#' @description `element_genes()` locates genomic elements and associated genes
#'     based on a genomic range query.
#'
#' @return `element_genes()` returns a tibble describing genomic element and gene pairs.
#'
#' @importFrom methods is
#' 
#' @export
element_genes <-
    function(
        range = NULL,
        page = 0L,
        limit = 25L,
        verbose = FALSE
    )
{
    
    igvf_genome <- "hg38" # IGVF uses this reference genome
        
    stopifnot(
        is(range, "GRanges"),
        length(range) == 1,
        all(is.na(genome(range))) | all(genome(range) == igvf_genome),
        is_scalar_integer(page),
        is_scalar_integer(limit),
        is_scalar_logical(verbose)
    )
    
    response <- catalog_request(
        "genomic-elements/genes",
        region = range_to_string(range), # converts to 0-based start
        page = page,
        limit = limit,
        verbose = tolower(as.character(verbose))
    )
        
    j_pivot(response, as = "tibble")
    
}
