#' http://jason.bryer.org/posts/2012-12-10/Markdown_Jekyll_R_for_Blogging.html
#' This R script will process all R mardown files (those with in_ext file extention,
#' .rmd by default) in the current working directory. Files with a status of
#' 'processed' will be converted to markdown (with out_ext file extention, '.markdown'
#' by default). It will change the published parameter to 'true' and change the
#' status parameter to 'publish'.
#' 
#' @param path_site path to the local root storing the site files
#' @param dir_rmd the directory containing R Markdown files (inputs)
#' @param dir_md directory containing markdown files (outputs)
#' @param out_ext the file extention to use for processed files
#' @param in_ext the file extention of input files to process
#' @param recursive should rmd files in subdirectories be processed
#' @return nothing
#' @author Jason Bryer <jason@bryer.org>
convertRMarkdown <- function(path_site = getwd(), images.dir = path_site, images.url = '/images/', dir_rmd = "_drafts", dir_md = "_posts",
                             out_ext ='.md', in_ext ='.rmd', recursive = FALSE) {
  require(knitr, quietly=TRUE, warn.conflicts=FALSE)
  
  # GSV change
  files <- list.files(path = file.path(path_site, dir_rmd), pattern = in_ext, ignore.case = TRUE, recursive = recursive)
  
  for(f in files) {
    message(paste("Processing ", f, sep=''))
    content <- readLines(paste(dir_rmd, f, sep = "/")) # changed by GSV after putting .Rmd in _drafts
    frontMatter <- which(substr(content, 1, 3) == '---')
    if(length(frontMatter) >= 2 & 1 %in% frontMatter) {
      statusLine <- which(substr(content, 1, 7) == 'status:')
      publishedLine <- which(substr(content, 1, 10) == 'published:')
      if(statusLine > frontMatter[1] & statusLine < frontMatter[2]) {
        status <- unlist(strsplit(content[statusLine], ':'))[2]
        status <- sub('[[:space:]]+$', '', status)
        status <- sub('^[[:space:]]+', '', status)
        if(tolower(status) == 'process') {
          #This is a bit of a hack but if a line has zero length (i.e. a
          #black line), it will be removed in the resulting markdown file.
          #This will ensure that all line returns are retained.
          content[nchar(content) == 0] <- ' '
          message(paste('Processing ', f, sep=''))
          content[statusLine] <- 'status: publish'
          content[publishedLine] <- 'published: true'

          # GSV change to path
          outFile <- file.path(path_site, dir_md, paste(substr(f, 1, (nchar(f)-(nchar(in_ext)))), out_ext, sep = ""))
          
          render_markdown(strict=TRUE)
          
          # GSV render for jekyll
          render_jekyll(highlight = "pygments")
          
          opts_knit$set(out.format='markdown')
          
          # From Andy South:
          # The URL of an image is always base.url + fig.path"
          # https://groups.google.com/forum/#!topic/knitr/18aXpOmsumQ
          
          # when added, these do not work
          # opts_knit$set(base.url = path_site)
          # opts_chunk$set(fig.path = url_images) 
          
          opts_knit$set(base.dir=images.dir)
          opts_knit$set(base.url=images.url)
          
          try(knit(text=content, output=outFile), silent=FALSE)
          
        } else {
          warning(paste("Not processing ", f, ", status is '", status, 
                        "'. Set status to 'process' to convert.", sep=''))
        }
      } else {
        warning("Status not found in front matter.")
      }
    } else {
      warning("No front matter found. Will not process this file.")
    }
  }
  invisible()
}