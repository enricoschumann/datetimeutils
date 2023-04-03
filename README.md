# datetimeutils

Utilities for handling dates and times, such as
selecting particular days of the week or month,
formatting timestamps as required by RSS feeds, or
converting timestamp representations of other software
(such as 'MATLAB' and 'Excel') to R. The package is
lightweight (no dependencies, pure R implementations)
and relies only on R's standard classes to represent
dates and times ('Date' and 'POSIXt'); it aims to
provide efficient implementations, through
vectorisation and the use of R's native numeric
representations of timestamps where possible.

[ [More] ](http://enricoschumann.net/R/packages/datetimeutils/)

## Installing the package

The latest released version is available from
http://enricoschumann.net. In an R session, just type:

    install.packages('datetimeutils', type = 'source',
                     repos = c('http://enricoschumann.net/R', getOption('repos')))


For the latest development version, check out the Git repository and
build it. In a shell (e.g. sh or bash):

    ## FIRST-TIME INSTALLATION
    #### cd to parent directory and ...
    $ git clone https://github.com/enricoschumann/datetimeutils.git

    #### build and install the package
    $ Rscript datetimeutils/Scripts/tznames.R 
    $ R CMD build datetimeutils
    $ R CMD INSTALL datetimeutils_0.1-0.tar.gz  ## adjust version number

    #### optionally check
    $ R CMD check datetimeutils_0.1-0.tar.gz    ## adjust version number



    ## UPDATING
    #### later: cd to parent directory and ...
    $ cd datetimeutils
	$ git pull
	$ cd ..
	$ Rscript datetimeutils/Scripts/tznames.R 
    $ R CMD build datetimeutils
    $ R CMD INSTALL datetimeutils_0.1-0.tar.gz  ## adjust version number
