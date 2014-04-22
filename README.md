leo-senate-model
================

Leo combines polls with other information to predict how many Senate races Democrats and Republicans will win in 2014.

Leo uses:

Historical results from the [Open Elections](http://openelections.github.io/) project, CQ Press [Voting and Elections](http://library.cqpress.com/elections/) Collection, [Dave Leip’s Atlas of U.S. Presidential Elections](http://uselectionatlas.org/), and the [Federal Election Commission](http://www.fec.gov/pubrec/electionresults.shtml). 

Fund-raising data from the [Federal Election Commission](http://www.fec.gov/finance/disclosure/candcmte_info.shtml). Biographical data from [Project Vote Smart](http://votesmart.org/) and candidate websites. 

Polls aggregated by [Pollster](http://elections.huffingtonpost.com/pollster/api), the [Roper Center for Public Opinion Research](http://www.ropercenter.uconn.edu/CFIDE/cf/action/home/index.cfm), the [U.S. Officials’ Job Approval Ratings](http://www.unc.edu/~beyle/jars.html) project, [Polling Report](http://www.pollingreport.com/), Gallup, [Talking Points Memo](http://polltracker.talkingpointsmemo.com/), [The Argo Journal](http://www.argojournal.com/"), [Real Clear Politics](http://www.realclearpolitics.com/epolls/other/generic_congressional_vote-2170.html) and [FiveThirtyEight](http://fivethirtyeight.com/).

Brief instructions
------------------

   * Please make sure the following R packages have been installed:
     `gam`, `gtools`, `lubridate`, `maps`, `RJSONIO`, `gdata`,
     `plotrix`, `zoo`

   * Change directory to the top-level working directory of this Git
     repository.

   * Run: `Rscript master-public.R`

   * Prediction output can then be found in the
     `data-publisher/public/_big_assets/` subdirectory.
