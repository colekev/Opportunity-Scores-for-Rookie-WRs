# Opportunity Scores for Rookie WRs

Included in this branch are the [R code](https://github.com/colekev/colekev.github.io/blob/Opportunity-Scores/OS_2016.R), the [plot](https://github.com/colekev/colekev.github.io/blob/Opportunity-Scores/qb_rec_adp_2016_2.png) of every teams' QB/Receiver relationship, and the [bar chart](https://github.com/colekev/colekev.github.io/blob/Opportunity-Scores/rook_wr_os_2016_3.png) of opportunity scores (or difference between data point and estimate) for every team.

This an analysis uses current [MyFantasyLeague MFL10](http://www54.myfantasyleague.com/2016/public#0) ADP to determine which teams have the most opportunity for rookie wide receivers. 

The [wide receiver analysis](http://rotoviz.com/2016/03/rookie-wide-receivers-fantasy/?hvid=17LjJu) is based on the quarterback/receiver ADP relationship to find undervalued players in fantasy football drafts.

A higher drafted quarterback, presumably, will throw for more yards and touchdowns than one drafted lower. Receivers are the ones catching the ball and accumulating those yards, touchdowns and fantasy points. By analyzing the relationship between our assessments of a team’s quarterback and receivers, we can see which part of the equation is undervalued versus the other.

There are a few adjustments that make this formula work:

1) The receiver value calculation is the inverse of ADP: the last pick in a 20-round, 12-team draft (240) minus ADP. You then add up all the values for wide receivers and tight ends to come up with the combined score.

2) The weighting of tight end ADP on the total receiver value calculation is cut down to one third of that of wide receivers. This gives a negative affect on opportunity for teams with an elite tight end, but recognizes that tight end and wide receiver production are not completely fungible.

3) The receiving stats accumulated by running backs in an offense are accounted for by discounting the receiver value calculation by the percentage of receiving fantasy points to running backs.

4) Quarterback rushing production is also accounted for by discounting the receiver value calculation by the percentage of quarterback fantasy points from rushing, not throwing.

If you assume that quarterbacks are generally more fairly valued than receivers according to ADP, a team below the relationship trendline has receivers that are undervalued, and should be a great landing spot for a rookie wide receiver.
