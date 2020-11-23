========
Data set
========

The JSON data sets of the US presidential elections at 2020-11-03 can be found at `USElection2020-NYT-Results`_.
That Github project periodically retrieves the latest updates of those files, each one time-stamped with a date and time.

Here we have some of those data sets, as ZIP files containing election results per state, one file per state.

The ZIP file name shows the sub-directory of the data directory of `USElection2020-NYT-Results`_, from which the data was
copied. Then the data was locally prettified, by running program PrettifyJson (in this project). The resulting files were
then zipped into one of the ZIP files that are in this directory.

To use the data as input to one of the programs in this project, such as FindAnomalies or CreateReport, please unzip the ZIP
file to some local directory first.

Of course, one could run programs like FindAnomalies or CreateReport directly on the same data found in `USElection2020-NYT-Results`
(of any date-time found there). First a "git clone" is needed, because the programs in this project only run on local files.
Note that program FindAnomalies takes just one input file, and no directory, whereas program CreateReport can work on an
entire directory of JSON files.

Be careful to point to JSON directories where each JSON file represents one state, or else the programs do not work.

.. _`USElection2020-NYT-Results`: https://github.com/favstats/USElection2020-NYT-Results
