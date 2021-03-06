================
US-election-2020
================

This Scala project analyzes and helps analyze JSON data of the 2020-11-03 US presidential election.

For the data sets and more background information (and a Python script to expose fraudulous data), see `the-switc`_.
The availability of that Python script and JSON data set got this project started.

Not being a US citizen myself, why would I worry about the validity of the US presidential elections in 2020?

The thing is: I do not want to accept a tyrannical world in which truth has become meaningless. No matter where you stand
on the political spectrum, these elections were stolen (like countless previous ones, in the US and elsewhere). There is abundance
of evidence to everyone paying attention, and I will not go there, other than analyzing the same JSON voting data that many
others have analyzed as well.

Of course the mainstream media prematurely declared a winner of the election, but they lie about everything of importance
(don't get me started), so why would I believe anything they say?

The election result data sets mentioned above were fed to the MSM, who used the data to their advantage, as if there were
no anomalies to see in the data. See for example `fraud proven`_. Of course anyone can inspect the same data, and find some
interesting things in there that the MSM failed to report on. Again, see `the-switc`_, where you can find a Python script
analyzing the data. For a Github project that uses a cron job to periodically retrieve the data sets from the NYT web site,
see `USElection2020-EdisonResearch-Results`_. That Github project (or another repo from the same Github user) is what was used
as the source of data.

I wanted to have a look myself at the data, partly using code (written in Scala) doing the same as the Python script, and partly
reorganizing the data in order to see some interesting patterns. Hence this small project.

Indeed, the data shows massive fraud, and that's just in a JSON data set, analyzed in isolation. Not to mention all other proof
there is of the biggest election fraud in US history.

Anyway, we should never be silent about the truth about everything of importance (now more than ever, with the CV19 crisis being
the biggest hoax ever perpetrated). There can be no liberty without truth, so truth must be defended vigorously.

Analyzing (just) the spreadsheets
=================================

If you are only interested in some spreadsheets with voting data, and neither want to see any JSON data nor want to run any
of the programs, please have a look at the ZIP files in directory `report-csv`_. These ZIP files can be downloaded and unzipped,
resulting in a collection of CSV files, one per state, that can be loaded into a spreadsheet program.

See below, in section "Some results", for what can be found in the data, especially in states like Michigan, Pennsylvania, Wisconsin
and Georgia.

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

So first install Coursier if needed, according to the installation instructions in the Coursier documentation.

The "cs launch" command can then be used to run the programs.

First prepare some local data directories. The JSON input files (one per state) are below assumed to be in directory <input-data>.
Below directories <report>, <csv-report>, <annotated-report> and <sorted-report> (at first empty) will be used as well.

Program FindAnomalies can be invoked as follows (here it is shown for Michigan)::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.2.0 \
     -M election.console.FindAnomalies \
     -- <input-data>/michigan.json

Program FindAnomalies performs checks and extracts statistics similar to the Python script found at `the-switc`_.
Personally I find it hard to reliably compute the votes that can be considered lost or switched. That being said, I highly respect
the work done by the author of the Python script, without which I probably would not have started this small project.

It would be nice to not just see the vote shares and total votes overall in the timeseries, but turn the data into a report
where each entry in the timeseries shows the total votes per candidate as well, along with the deltas compared to the preceding
timeseries entry. That's what program CreateReport does::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.2.0 \
     -M election.console.CreateReport \
     -- <input-data> <report>

Several other programs work on the output of this program, in the <report> directory. For example, program ConvertReportToCsv,
which transforms the report JSON into CSV files, containing exactly the same data::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.2.0 \
     -M election.console.ConvertReportToCsv \
     -- <report> <csv-report>

Of course, these CSV files can be manipulated (e.g. sorted) in any way possible, once loaded into a spreadsheet program.

To annotate the JSON reports with "anomalies" per "data record" or across "data records", run program CreateAnnotatedReport::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.2.0 \
     -M election.console.CreateAnnotatedReport \
     -- <report> <annotated-report>

To sort the JSON reports according to some predefined sort criterium, run program SortReport (here sorting on MaxDeltaVotes, largest first)::

   cs launch eu.cdevreeze.us-election:us-election_2.13:0.2.0 \
     -M election.console.SortReport \
     -- <report> <sorted-report> MaxDeltaVotes

Of course we could do the sorting on the CSV files that are output of the ConvertReportToCsv program instead, once loaded in
a spreadsheet program.

Some results
============

Many of the "anomalies" in the voting data that have been found and documented on the internet can be reproduced here, using
the programs mentioned above. For the human reader, it may be best to analyze the CSV files in a spreadsheet program.

For example, the impossible timeseries entries (for Michigan and Pennsylvania) mentioned in `Sarah Westall about voting fraud`_
or `natural news about voting fraud`_ are easy to reproduce here. See also `Sharyl Attkisson about the voting fraud`_.

There is also a very interesting pattern that can be found (in the CSV files or JSON reports), especially for the battleground states.
Often, in many subsequent voting dumps, the percentage of votes in that dump for Trump and for Biden are constants. Now what are
the chances of that happening naturally? Over a longer period of time, those "constants" go down a bit for Trump, and go up a bit
for Biden, just enough to make Biden the winner (or to provide cover for Biden winning in other states). How convenient. These
patterns look very unlikely to me in a fair election. Instead, they look very much like software-generated data. I suspect that
experts in statistics can prove that these patterns are impossible in practice in an honest election (if that is even needed,
given that much better data must be available to the legal teams fighting this corruption). At least I can check for myself that
several things are off in the JSON/CSV data used here.

Add this to all other proof of voting fraud, and we have a huge crime perpetrated against the American people. For me personally,
it is not so much about Republicans versus Democrats, but it is about truth, no matter how hard the truth is suppresssed by the
"media" and tech giants. I mean, they even censor the US president. Let that sink in for a while. Again,
`there is no freedom without truth`_. How relevant the main message of that 2016 article is today (November 2020)!

Update 2020-12-05
=================

On December 2, 2020, US president Donald Trump made a speech on the voter fraud. See `Trump speech on voter fraud`_.
I checked a few things Donald Trump said against the (CSV) data in this project, in directory `report-csv`_.

According to Donald Trump, he suddenly started losing to Joe Biden in Wisconsin, at 3:42 in the morning, while being comfortably
in the lead up to that point. Let's check that against the CSV data. The time zone would be CST (Central Standard Time) in Wisconsin.
That's 9:42 Z (Zulu time zone), in the morning of 2020-11-04. As can be seen in the CSV file(s) for Wisconsin, at
2020-11-04T09:42:20Z, there is a big batch of 168386 votes, the vast majority of them for Biden, and the 3rd party even losing
some votes! Indeed, Trump went from a rather comfortable lead to a slight loss in that single batch. This batch of 168386 votes
happens to be the biggest batch of votes (deltaVotes in the CSV file) in the file. In that batch, less than 15% of that batch
(25163 votes) went to Trump and more than 85% (143379 votes) went to Biden, while the 3rd party miraculously lost some votes.
That's quite a deviation from the average vote shares of Trump and Biden, "coincidentally" in the largest vote dump.

There are even more peculiarities in the Wisconsin vote dumps leading up to that point, where the vote shares for Trump and Biden
per batch were constant to a large extent. This seems to have set the stage for the vote switching effect of the vote dump that
Mr. Trump talks about, as far as I am concerned. All in all, Mr. Trump's claim about the vote switch in Wisconsin is visible in the data.

The vote dump of 149772 votes coming in unexpectedly in Michigan that Donald Trump talks about is also visible in the CSV file.
It was at 2020-11-04T11:31:53Z in the CSV file. That's 06:31 EST, which is the local time in Detroit, Michigan, and that's the
local time mentioned by Donald Trump. This vote dump is the 3rd biggest vote dump in the file, and it gave only 4% of the votes
in the dump to Trump and 94.3% of the votes in the dump to Biden. Up to that point, Trump was leading comfortably, but that
changed a lot with this dump, to set the stage for a gradual switch from a Trump lead to a Biden one. Again, Donald Trump is right
in mentioning this highly suspect vote dump.

Of course, the MSM has had access to the same data all the time, and they could have found the same anomalies and reported about them.
Enough said about the MSM. To end with a positive note, this fraud awakens a lot of people to the nature of the old imploding reality
(based on control by fear and lies, to the benefit of a small elite and at the expense of everyone else). The US can do a lot better
than that, and I'm confident that many good courageous people will liberate and save their country, followed by many other countries.
Yet we are all needed in the information war, in particular by spreading truth.

.. _`the-switc`: https://thedonald.win/p/11Q8XQIWRs/-happening-ive-updated-the-switc/
.. _`fraud proven`: https://sarahwestall.com/trump-won-fraud-proven-analysis-of-voting-data-shows-exactly-what-happened/
.. _`USElection2020-EdisonResearch-Results`: https://github.com/favstats/USElection2020-EdisonResearch-Results
.. _`report-csv`: https://github.com/dvreeze/us-election-2020/blob/master/jvm/src/main/resources/report-csv
.. _`readme-data.rst`: https://github.com/dvreeze/us-election-2020/blob/master/jvm/src/main/resources/EdisonData/readme-data.rst
.. _`Coursier`: https://get-coursier.io/
.. _`Sarah Westall about voting fraud`: https://sarahwestall.com/trump-won-fraud-proven-analysis-of-voting-data-shows-exactly-what-happened/
.. _`natural news about voting fraud`: https://www.naturalnews.com/2020-11-11-election-data-analyzed-votes-switched-biden-software.html
.. _`Sharyl Attkisson about the voting fraud`: https://sharylattkisson.com/2020/11/what-youve-been-asking-for-a-fairly-complete-list-of-some-of-the-most-significant-claims-of-2020-election-miscounts-errors-or-fraud/
.. _`there is no freedom without truth`: https://www.paulcraigroberts.org/2016/02/02/there-is-no-freedom-without-truth-paul-craig-roberts/
.. _`Trump speech on voter fraud`: https://www.rev.com/blog/transcripts/donald-trump-speech-on-election-fraud-claims-transcript-december-2
