#!/bin/bash

# http://jason.bryer.org/posts/2012-12-10/Markdown_Jekyll_R_for_Blogging.html
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
Rscript -e "source('$DIR/rmarkdown.r'); convertRMarkdown(images.dir='$DIR/images')"
