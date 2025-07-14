shinylive::export("inst/shiny", "docs/shiny")
httpuv::runStaticServer("docs/shiny", port=8008)
