# data/ml-10m/import_data.R
# 100 review is added manually

# read movies
movie <- read.csv("movie.csv", stringsAsFactors=FALSE)

# id
movie$id <- as.character(movie$id)

# genre dummies
genre <- strsplit(movie$genre, "\\|")
genre.levels <- c("(no genres listed)", "Action", "Animation", "Adventure",
                  "Animation", "Children", "Comedy", "Crime", "Documentary",
                  "Drama", "Fantasy", "Film-Noir", "IMAX", "Horror", "Musical",
                  "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
genre.codes <- tolower(genre.levels)
genre.codes[match(c("(no genres listed)", "Film-Noir", "Sci-Fi"),
                  genre.levels)] <- c("unknown", "filmnoir", "scifi")

for (i in seq_along(genre.levels)) {
    g <- genre.levels[i]
    movie[,genre.codes[i]] <- sapply(genre, function(x) g %in% x)
}

## (if IMAX is the only listed genre, code as "unknown")
movie[sapply(genre, length) == 1 & movie[,"imax"], "unknown"] <- TRUE



# read ratings (and users)
rating <- read.csv("rating.csv")

# Count the number of reviews for each user
review_counts <- aggregate(rating$score, by=list(user=rating$user), FUN=length)
colnames(review_counts) <- c("user", "n_reviews")

# Filter for users with 100 or more reviews
users_with_100plus_reviews <- review_counts$user[review_counts$n_reviews >= 100]

# Filter the rating data to include only those users
rating <- rating[rating$user %in% users_with_100plus_reviews, ]

# If you need to filter the 'movie' data based on these users, you can do it as follows:
# This step is optional and depends on whether you want to sync the 'movie' data with the filtered 'rating' data
filtered_movie_ids <- unique(rating$movie)
movie <- movie[movie$id %in% filtered_movie_ids, ]
#Decreased from 10681 to 10667.

# users
user <- data.frame(id = as.character(sort(unique(rating$user))))


# ratings
rating$user <- match(rating$user, user$id)
rating$movie <- match(rating$movie, movie$id)
rating$score <- factor(rating$score, ordered=TRUE)
rating$time <- as.POSIXct(rating$timestamp, origin="1970-01-01", tz="UTC")
rating <- rating[,c("user", "movie", "score", "time")]
##### We do not need this part in our project#######
# tags
##tags <- read.csv("tag.csv")
# 
# rating.feature <- read.csv("rating-feature.csv")
# user.liked.last <- rating.feature$user_pos_len >= 1
# 
# logit <- function(p) log(p / (1 - p))
# 
# movie.logit.pop <- with(rating.feature,
#                         logit((movie_pos + 0.5) / (movie_pos + movie_neg + 1)))
#####################################################

# response and grouping factor
y <- as.numeric(rating$score >= 4)

# Convert the 'score' column from a factor to numeric
#scores_numeric <- as.numeric(as.character(rating$score))
#count <- scores_numeric * 2

group <- factor(rating$user, ordered=FALSE) #groups are the users


# covariate: genre
action <- rowSums(movie[,c("action", "adventure", "fantasy", "horror", "scifi",
                            "thriller")])
children <- rowSums(movie[,c("animation", "children")])
comedy <- rowSums(movie[,c("comedy"),drop=FALSE])
drama <- rowSums(movie[,c("crime", "documentary", "drama", "filmnoir",
                           "musical", "mystery", "romance", "war",
                           "western")])
genre <- cbind(action, children, comedy, drama)
genre.names <- colnames(genre)
genre <- genre / pmax(1, rowSums(genre))
contr.genre <- rbind(-1, diag(ncol(genre) - 1L))
genre <- genre %*% contr.genre
colnames(genre) <- genre.names[-1]
genre <- genre[rating$movie,,drop=FALSE]

# n.movie
n.movie.raw <- by(rep(1, length(y)), rating$movie, sum)
n.movie <- numeric(nrow(movie))
names(n.movie) <- seq_len(nrow(movie))
n.movie[names(n.movie.raw)] <- n.movie.raw

# y.movie
y.movie.raw <- by(y, rating$movie, sum)
y.movie <- numeric(nrow(movie))
names(y.movie) <- seq_len(nrow(movie))
y.movie[names(y.movie.raw)] <- y.movie.raw

# fixed effect covariate matrix
# fixef.mf <- model.frame(~ genre + movie.logit.pop + user.liked.last)
fixef.mf <- model.frame(~ genre)
fixef.mt <- attr(fixef.mf, "terms")
x <- model.matrix(fixef.mt, fixef.mf)

# random effect covariate matrix
# ranef.mf <- model.frame(~ genre)
# ranef.mt <- attr(ranef.mf, "terms")
#z <- model.matrix(ranef.mt, ranef.mf)
z <- x

data <- data.frame(x=I(x), z=I(z), y, group)

saveRDS(data, "data.rds")
