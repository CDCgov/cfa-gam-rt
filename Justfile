alias docs := document
alias spell := spellcheck

export NOT_CRAN := "1"
export RCMDCHECK_ERROR_ON := "note"

prep: style document spellcheck lint check

spellcheck:
    Rscript -e "spelling::spell_check_package()"

test:
	Rscript -e "testthat::test_local()"

document:
	Rscript -e "roxygen2::roxygenize()"

check:
	Rscript -e "rcmdcheck::rcmdcheck()"

style:
	Rscript -e "styler::style_pkg()"

lint:
	Rscript -e "lintr::lint_package()"

update_wordlist:
    Rscript -e "spelling::update_wordlist(confirm = FALSE)"

clean:
	git restore --staged .
	git restore .
	git reset --hard HEAD
	git status

update:
    git stash push --include-untracked
    git switch main
    git pull --ff-only
    git switch -
    git rebase main
    git push --force-with-lease --force-if-includes

# Build and open vignette
vignette which='RtGam':
	Rscript -e "devtools::build_rmd('vignettes/{{which}}.Rmd')"
	open 'vignettes/{{which}}.html'
