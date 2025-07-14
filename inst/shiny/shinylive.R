shinylive::export("inst/shiny", "docs/")
httpuv::runStaticServer("docs/", port=8008)
