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


### Sample output data in `/data-publisher/public/_big_assets`


##### histogram.tsv

| dem | chance |
|-----|--------|
|  44 |      0 |
|  45 |      0 |
|  46 |      0 |
|  47 |   0.05 |
|  48 |   0.25 |
|  49 |   0.15 |
|  50 |    0.3 |
|  51 |   0.15 |

##### matchups.tsv

| statePostal | secondary | probability | mean | scale | office | dem | rep |
|-------------|-----------|-------------|------|-------|--------|-----|-----|
|AK||0.0675992536643285|13.3371042222312|6.37706218633251|ak-senate-class-ii-2014|mark-begich|joe-miller
|AK||0.672480368777626|-0.369330147923164|6.40862522137136|ak-senate-class-ii-2014|mark-begich|daniel-s-sullivan
|AK||0.259920377558046|-1.42864965262853|6.43282051331378|ak-senate-class-ii-2014|mark-begich|mead-treadwell
|AL||1|-92.2586242817344|13.0668255800746|al-senate-class-ii-2014|NA|jeff-sessions
|AR||1|-0.588349828909989|5.43497477566511|ar-senate-class-ii-2014|mark-pryor|thomas-cotton-1
|CO||1|2.75130137738909|5.32999951709124|co-senate-class-ii-2014|mark-udall|cory-gardner
|DE||0.0485586168626867|22.1226995091985|12.2918575405219|de-senate-class-ii-2014|christopher-a-coons|tom-kovach
|DE||0.951441383137313|24.462299974418|12.2976359697371|de-senate-class-ii-2014|christopher-a-coons|christine-o'donnell

##### nyt-ratings-today.json

```json
{
  "date": "2014-04-22T13:19:49-0400",
  "data": [
    {
      "dem-pct": 49.428,
      "state": "AK",
      "office": "ak-senate-class-ii-2014",
      "fips": "2",
      "nyt-rating": 4
    },
    {
      "dem-pct": 4.1438E-10,
      "state": "AL",
      "office": "al-senate-class-ii-2014",
      "fips": "1",
      "nyt-rating": 7
    },
    {
      "dem-pct": 45.693,
      "state": "AR",
      "office": "ar-senate-class-ii-2014",
      "fips": "5",
      "nyt-rating": 4
    }
  ]
}
```


##### parameters.json

```json
{
  "continuingSeats": {
    "dem": 34,
    "rep": 30
  },
  "nationalErrorScale": 0.40797,
  "upsetProb": 40,
  "houseEffect": 3.5,
  "mt": 85,
  "mtNum": "17",
  "mtDenom": "20",
  "aan": "an"
}
```


##### senate-likelihood-all.tsv

| office | date | dem |
|--------|------|-----|
|ak-senate-class-ii-2014|1-Jan-14||57.0869004591178|
|ak-senate-class-ii-2014|25-Feb-14|51.4921956208518|
|ak-senate-class-ii-2014|22-Apr-14|49.4276015976854|
|al-senate-class-ii-2014|1-Jan-14||2.8079525308588e-09|
|al-senate-class-ii-2014|25-Feb-14|9.22057393777991e-10|
|al-senate-class-ii-2014|22-Apr-14|4.14383203099536e-10|

##### senate-likelihood.tsv
|    date   | dem |
|-----------|-----|
| 1-Jan-14  |  50 |
| 25-Feb-14 |  35 |
| 22-Apr-14 |  55 |


##### slug-lookup.tsv

|        slug       |   name   |           |
|-------------------|----------|-----------|
| mark-begich       | Mark     | Begich    |
| william-bryk      | William  | Bryk      |
| john-jaramillo    | John     | Jaramillo |
| joe-miller        | Joe      | Miller    |
| sarah-palin       | Sarah    | Palin     |
| daniel-s-sullivan | Dan      | Sullivan  |
| kathleen-tonn     | Kathleen | Tonn      |
| mead-treadwell    | Mead     | Treadwell |


