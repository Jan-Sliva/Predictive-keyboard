test_coll <- list(data.frame(ngram = c("i", "you", "the", "<>", "likes", "like", "united", "is", "states", "good"), prop = seq(1, 0, length.out = 10)),
                  data.frame(ngram = c("i like", "you likes", "the united", "<> is"), prop = c(0.1, 0.2, 0.3, 0.4)),
                  data.frame(ngram = c("i like you", "i like <>", "the united states", "<> is good"), prop = c(0.1, 0.2, 0.3, 0.4)))

test_tree <- CreateNGramTree(test_coll, 10, "ngram", " ", "prop", "<>")
