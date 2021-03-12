# What is MOMAM?
MOMAM6 (short for "Month of Mushrooms and Mayhem") is the 6th installment of an annual competition on Amazon's online streaming platform, twitch.tv, between two popular streamers, [iateyourpie](https://www.twitch.tv/iateyourpie) and [spikevegeta](https://www.twitch.tv/spikevegeta). Each day of the month, Spike and Pie compete in a different video game to see who can clear it the fastest. Thus the goal of this project is to:
1. Gather historical data on the results of previous MOMAM events, metadata on the games to be featured in the upcoming MOMAM, and information on what types of games each player tends to play throughout the year.
2. Determine which aspects of the overwhelming amount of collected metadata are most relevant for analysis.
3. Construct a model to predict the winner for each day of the event.
4. Submit those predictions to the [MOMAM6 Prediction Leaderboard](https://momam.streambig.tv/leaderboard) and (hopefully) perform well!

More information on MOMAM6 can be found on the [MOMAM website](https://www.streambig.net/momam6.html). Over 5400 people submitted predictions for the event this year, making it the most competitive MOMAM ever by a large margin! 

# Disclaimer
Although MOMAM6 has already begun and a model has already been chosen for leaderboard predictions, development on this project is still very much on-going. Below will provide an overview of how the initial predictions were generated, what's currently being modified, and what's being worked towards in hopes of making even more accurate predictions when MOMAM7 comes around.

# Step 1: Obtaining and Formatting the Data

In order to predict the winner for an upcoming game, we of course need information about about every game that's been played in previous years, as well as the upcoming games. igdb has an API that can be used to easily access information such as a game's genre, console, developer, and many more. The entire database building process is robut and automated. An in-depth description of the process is as follows:
1. A list of annual gameplay data csv files for each channel (obtained from SullyGnome.com) are imported. These files contain 2 important pieces of information: The name of the game and the length of time they've played it that year.
2. The list of games is parsed and transformed into a URL format ("Super Mario Bros." would become "super-mario-bros"). These transformed game titles are submitted as a post request to the igdb API where any pieces of metadata that *might* be useful predictors are returned. Columns include `genres`, `franchises`, `keywords`, `involved_companies`, as well as a few others.
3. Each of the categorical columns obtained from part 2 are replaced with many dummy variables.
4. Playtime is calculated for each column using the csv from part 1 and dummy variables from part 3. In other words, this step calculates how many hours each player has played each games under the "action" genre, or the "Mario" franchise, as their familiarity with each of these things can (and often does) impact the results of a race.

Do note that there is a missing file from the repository which contains functions to return a client_id, client_secret, and access_token for Twitch and igdb's APIs. Those who wish to run this repository on their own from scratch are advised to follow [this guide](https://api-docs.igdb.com/#about) in order to get your own credentials. However, all information that these API calls were used for should be contained within the `MOMAM.sqlite` file, thus reconstructing it would be superfluous.

# Step 2: Pre-processing

The process from step 1 resulted in thousands of extra variables, significantly increasing the dimensionality of the training data. However, many of these variables contain either no info or redundant info and need to be removed. As some examples:
1. Only one game in the training data has the `Spongebob` keyword, resulting in a zero-variance predictor. 
2. Some training data is repeated. For example, `Mario` is both a `franchise` and a `keyword` in igdb's API, resulting in the same data being repeated in multiple columns. A linear combination test is ran on the data and violating columns are removed.

# Step 3: Prepare for Training

Since the full set of training data is so small, choosing an appropriate train/test split and cross-validation method is important. As such, an 80/20 train/test split was used. The cross-validation choice was a little less straight-forward. Intuitively, the small training data points itself towards using LOOCV. However, the high dimensionality, some models still take an exceptionally long time to train. As such, an adaptive cross-validation method was used.

# Step 4: Model Training

If I was asked to choose precisely one method to model this data without training any others, I would absolutely gravitate towards a decision tree model (most likely a Boosted Tree). Though tree models are applicable across all sorts of datasets, this data's logical column paradigm implicity lends itself to a tree-based structure. "The game you'd like to predict is a platformer? If it's 2D, predict pie, but if it's 3D predict Spike." It's an extremely simple approach, while also being fast, effective, and interpretable.

The beauty of R's caret package, however, is that one can easily train and evaluate a plethora of models. As such, I ended up training approximately 100 different models, partially because I believe some of the models are appropriate, but mostly because I'm genuinely curously how well techniques I don't think are appropriate will perform, or because it's a technique I've never heard of before and (if it does well,) would like to learn more about it. Of course Naive Bayes is going perform poorly on this data - it violates every assumption the model makes. But training it is so fast that I might as well see how it performs (it's currently doing unphenomenially just as expected by the way - 5 correct prediction out of the 12 rounds that have happened so far.)

# Step 5: Model Selection, Current Performance, and Future Direction:

After training the nearly 100 models, it was time to select a model to use for actual leaderboard predictions. A combination of their test set accuracies and my own intution were used in selecting which model's predictions to trust. Disappointingly, but not unexpectedly, none of the models performed exceptionally well on the test set, peaking out at around 70% accuracy. This model - Random Forest by Randomization - is actually performing admirably in the actual competition, currently sitting at 9 correct predictions through the first 12 rounds.

While topping out at 70% accuracy seems like a poor result, I view this type of problem much like trying to predict to winners of a sports match. Unexpected things will happen. There are simply events that occur in a player's day to day life that simply can't be accounted for in a model like this. As such, I consider any models able to predict the winner correctly even 60% of the time to be performing exceptionally well. 

I do believe that these results can be improved further, however. Each year I expect the model to perform a little better as the amount of training data available will be larger. In addition, stricter dimensionality reduction of the training data (using something like PCA) as well as finer model tuning (recursively grid searching the hyperparameters of perspective models) are the first steps I'd like to take when designing another model for next year's MOMAM7.
