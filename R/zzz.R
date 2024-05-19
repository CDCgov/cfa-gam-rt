.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}

# Use CLI formatting in error messages automatically
# https://rlang.r-lib.org/reference/local_use_cli.html#usage
rlang::on_load(rlang::local_use_cli())
