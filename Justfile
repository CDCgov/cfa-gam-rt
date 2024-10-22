alias docs := document

prep: style document lint check

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

clean:
	git restore --staged .
	git restore .
	git reset --hard HEAD
	git status
