================
US-election-2020
================

This Scala project analyzes JSON data of the 2020-11-03 US presidential election.
For the data sets and more background information (and a Python script to expose fraudulous data), see `the-switc`_.

Not being a US citizen myself, why would I worry about the validity of the US presidential elections in 2020?

The thing is: I do not want to accept a tyrannical world in which truth has become meaningless. No matter where you stand
on the political spectrum, these elections were stolen. There is abundance of evidence to everyone paying attention,
and I will not go there (other than analyzing the same JSON voting data that many others have analyzed as well).

Of course the mainstream media prematurely declared a winner of the election, but they lie about everything of importance,
as every sufficiently awake person knows.

The election result data sets mentioned above were fed to the MSM, who used the data to their advantage, as if there are
no anomalies to see in the data. See for example `fraud proven`_. Of course anyone can inspect the same data, and find some
interesting things in there that the MSM failed to report on. Again, see `the-switc`_, where you can find a Python script
analyzing the data. For a Github project that uses a cron job to periodically retrieve the data sets from the NYT web site,
see `USElection2020-NYT-Results`_. That Github project is what was used as the source of data.

I wanted to have a look myself at the data, partly using code (written in Scala) doing the same as the Python script. Hence
this small project.

Indeed, the data shows massive fraud, and that's just in a JSON data set. Not to mention all other proof there is of the biggest
election fraud in US history.

Anyway, we should never be silent about the truth about everything of importance (now more than ever, with the CV19 crisis being
the biggest hoax ever perpetrated). There can be no liberty without truth, so truth must be defended vigorously.

Using the data
==============

To use the JSON input data, see `readme-data.rst`_. Program FindAnomalies works directly with this input data (just one file per run).
So does program CreateReport (one file or an entire directory). The latter program creates a "timeseries report view" of the same data,
in which each "data record" shows not only total votes and vote shares, but also total votes per candidate and the deltas compared
to the preceding "data record".

Taking these JSON reports as input, program SortReport sorts the JSON data according to some predefined sort criterium,
program CreateAnnotatedReport annotates the JSON with "anomaly information" (mainly for anomalies local to a "data record"),
and program ConvertReportToCsv turns the JSON reports into CSV files that contain the same information as the reports.

Some of those CSV files have been saved in this project as well (as ZIP files containing one CSV file per state).

Running the programs
====================

One way to run the programs in this project is to use `Coursier`_ to do that. Here it is assumed that indeed Coursier is used.

So first install Coursier, according to the installation instructions in the Coursier documentation.

The "cs launch" command can then be used to run the programs.

First prepare some local data directories. The JSON input files (one per state) are below assumed to be in directory <input-data>.
Below directories <report>, <csv-report>, <annotated-report> and <sorted-report> (at first empty) will be used as well.

Program FindAnomalies can be invoked as follows (here it is shown for michigan)::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.1.0 -M election.console.FindAnomalies -- <input-data>/michigan.json

Program FindAnomalies performs checks and extracts statistics similar to the Python script found at `the-switc`_.
Personally I find it hard to reliably compute the votes that can be considered lost or switched. That's why I do not consider
this the most interesting program here.

It would be nice to not just see the vote shares and total votes overall in the timeseries, but turn the data into a report
where each entry in the timeseries shows the total votes per candidate as well, along with the deltas compared to the preceding
timeseries entry. That's what program CreateReport does::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.1.0 -M election.console.CreateReport -- <input-data> <report>

Several other programs work on the output of this program, in the <report> directory. For example, program ConvertReportToCsv,
which transforms the report JSON into CSV files, containing exactly the same data::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.1.0 -M election.console.ConvertReportToCsv -- <report> <csv-report>

Of course, these CSV files can be manipulated (e.g. sorted) in any way possible, once loaded into a spreadsheet program.

To annotate the JSON reports with "anomalies" per "data record", run program CreateAnnotatedReport::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.1.0 -M election.console.CreateAnnotatedReport -- <report> <annotated-report>

To sort the JSON reports according to some predefined sort criterium, run program SortReport (here sorting on MaxDeltaVotes, largest first)::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.1.0 -M election.console.SortReport -- <report> <sorted-report> MaxDeltaVotes

Of course we could do the sorting on the CSV files that are output of the ConvertReportToCsv program instead, once loaded in
a spreadsheet program.

Some results
============

Many of the "anomalies" in the voting data that have been found and documented on the internet, can be reproduced here, using
the programs mentioned above. For the human reader it may be best to analyze the CSV files in a spreadsheet program.

For example, the impossible timeseries entries (for Michigan and Pennsylvania) mentioned in `Sarah Westall about voting fraud`_
or `natural news about voting fraud`_ are easy to reproduce here. Add this to all other proof of voting fraud, and we have a
huge fraud perpetrated against the American people. Personally, it is not about Republicans versus Democrats for me, but
about truth, no matter how hard the truth is suppresssed by the "media" and tech giants. Again, `there is no freedom without truth`_.


.. _`the-switc`: https://thedonald.win/p/11Q8XQIWRs/-happening-ive-updated-the-switc/
.. _`fraud proven`: https://sarahwestall.com/trump-won-fraud-proven-analysis-of-voting-data-shows-exactly-what-happened/
.. _`USElection2020-NYT-Results`: https://github.com/favstats/USElection2020-NYT-Results
.. _`readme-data.rst`: https://github.com/dvreeze/us-election-2020/tree/master/shared/src/main/scala/election/data/readme-data.rst
.. _`Coursier`: https://get-coursier.io/
.. _`Sarah Westall about voting fraud`: https://sarahwestall.com/trump-won-fraud-proven-analysis-of-voting-data-shows-exactly-what-happened/
.. _ `natural news about voting fraud`: https://www.naturalnews.com/2020-11-11-election-data-analyzed-votes-switched-biden-software.html
.. _`there is no freedom without truth`: https://www.paulcraigroberts.org/2016/02/02/there-is-no-freedom-without-truth-paul-craig-roberts/
