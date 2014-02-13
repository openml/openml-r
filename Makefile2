R	:= R 
RSCRIPT	:= Rscript
DELETE	:= rm -fR

.SILENT:
.PHONEY: clean package install test check

usage:
	echo "Available targets:"
	echo ""
	echo " clean         - Clean everything up"
	echo " roxygenize    - Convert roxygen docs to RD files"
	echo " package       - build source package"
	echo " install       - install the package"
	echo " test          - run tests"
	echo " check         - run R CMD check on the package"

clean:
	echo "Cleaning up ..."
	${DELETE} openML.Rcheck
	${DELETE} .RData .Rhistory

roxygenize: clean
	echo "Roxygenizing package ..."
	${RSCRIPT} ./tools/roxygenize
  
package: roxygenize
	echo "Building package file ..."
	${R} CMD build openML
  
install: roxygenize
	echo "Installing package ..."
	${R} CMD INSTALL openML

test: install
	echo "Testing package ..."
	${RSCRIPT} openML/test_all.R

check: install 
	echo "Running R CMD check ..."
	${R} CMD check openML

tutorial:
	echo "\nKnitting Rmd tutorial...\n"
	${DELETE} doc/knitted
	${DELETE} doc/figure
	mkdir doc/knitted
	${RSCRIPT} ./tools/generate-md-tutorial
#	sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/*.md
#	sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/tutorial/*.md
#	mv doc/figure doc/knitted/figure
