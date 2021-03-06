# What is MOMAM?
MOMAM6 (short for "Month of Mushrooms and Mayhem") is the 6th installment of an annual competition on Amazon's online streaming platform, twitch.tv, between two popular streamers, [iateyourpie](https://www.twitch.tv/iateyourpie) and [spikevegeta](https://www.twitch.tv/spikevegeta). Each day of the month, Spike and Pie compete in a different video game to see who can clear it the fastest. Thus the goal of this project is to:
1. Gather historical data on the results of previous MOMAM events, metadata on the games to be featured in the upcoming MOMAM, and information on what types of games each player tends to play throughout the year.
2. Determine which aspects of the overwhelming amount of collected metadata are most relevant for analysis.
3. Construct a model to predict the winner for each day of the event.
4. Submit those predictions to the [MOMAM6 Prediction Leaderboard](https://momam.streambig.tv/leaderboard) and (hopefully) perform well!

More information on MOMAM6 can be found on the [MOMAM website](https://www.streambig.net/momam6.html). Over 5400 people submitted predictions for the event this year, making it the most competitive MOMAM ever by a large margin! 

# Disclaimer
Although MOMAM6 has already begun and a model has already been chosen for leaderboard predictions, development on this project is still very much on-going. Below will provide an overview of how the initial predictions were generated, what's currently being modified, and what's being worked towards in hopes of making even more accurate predictions when MOMAM7 comes around.

# Step 1: The Data

In order to predict the winner for an upcoming game, we of course need information about about every game that's been played in previous years, as well as the upcoming games. igdb has an API that can be used to easily access information such as a game's genre, console, developer, and many more.

There is a missing file from the repository which contains functions to return a client_id, client_secret, and access_token for Twitch and igdb's APIs. Those who wish to run this repository on their own from scratch are advised to follow [this guide](https://api-docs.igdb.com/#about) in order to get your own credentials. However, all information that these API calls were used for should be contained within the `MOMAM.sqlite` file, thus reconstructing it would be superfluous.
