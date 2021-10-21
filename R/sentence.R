#' Create a sentence style identifier.  This uses the approach
#' described by Asana on their blog
#' <https://blog.asana.com/2011/09/6-sad-squid-snuggle-softly/>.
#' This approach encodes 32 bits of information (so 2^32 ~= 4 billion
#' possibilities) and in theory can be remapped to an integer if you
#' really wanted to.
#'
#' @title Sentence style identifiers
#'
#' @inheritParams ids
#'
#' @param past Use the past tense for verbs (e.g., slurped or jogged
#'   rather than slurping or jogging)
#'
#' @export
#' @author Rich FitzJohn
#' @examples
#' # Generate an identifier
#' ids::sentence()
#'
#' # Generate a bunch
#' ids::sentence(10)
#'
#' # As with adjective_animal, use "style" to control punctuation
#' ids::sentence(style = "Camel")
#' ids::sentence(style = "dot")
#' ids::sentence(style = "Title")
#'
#' # Change the tense of the verb:
#' set.seed(1)
#' ids::sentence()
#' set.seed(1)
#' ids::sentence(past = TRUE)
#'
#' # Pass n = NULL to bind arguments to a function
#' id <- ids::sentence(NULL, past = TRUE, style = "dot")
#' id()
#' id(10)
sentence <- function(n = 1, style = "snake", past = FALSE) {
  verbs <- if (past) asana_verbs_past else asana_verbs_present
  ids(n, asana_ids, asana_adjectives, asana_nouns, verbs, asana_adverbs,
      style = style)
}

asana_ids <- as.character(2:33)

asana_adjectives <- c(
  "adorable", "adventurous", "alluring", "amazing",
  "ambitious", "amusing", "astonishing", "attractive", "awesome",
  "bashful", "bawdy", "beautiful", "bewildered", "bizarre", "bouncy",
  "brainy", "brave", "brawny", "burly", "capricious", "careful",
  "caring", "cautious", "charming", "cheerful", "chivalrous",
  "classy", "clever", "clumsy", "colossal", "cool", "coordinated",
  "courageous", "cuddly", "curious", "cute", "daffy", "dapper",
  "dashing", "dazzling", "delicate", "delightful", "determined",
  "eager", "embarrassed", "enchanted", "energetic", "enormous",
  "entertaining", "enthralling", "enthusiastic", "evanescent",
  "excited", "exotic", "exuberant", "exultant", "fabulous", "fancy",
  "festive", "finicky", "flashy", "flippant", "fluffy", "fluttering",
  "funny", "furry", "fuzzy", "gaudy", "gentle", "giddy", "glamorous",
  "gleaming", "goofy", "gorgeous", "graceful", "grandiose", "groovy",
  "handsome", "happy", "hilarious", "honorable", "hulking",
  "humorous", "industrious", "incredible", "intelligent", "jazzy",
  "jolly", "joyous", "kind", "macho", "magnificent", "majestic",
  "marvelous", "mighty", "mysterious", "naughty", "nimble", "nutty",
  "oafish", "obnoxious", "outrageous", "pretty", "psychedelic",
  "psychotic", "puzzled", "quirky", "quizzical", "rambunctious",
  "remarkable", "sassy", "shaggy", "smelly", "sneaky", "spiffy",
  "swanky", "sweet", "swift", "talented", "thundering", "unkempt",
  "upbeat", "uppity", "wacky", "waggish", "whimsical", "wiggly",
  "zany")

asana_nouns <- c(
  "aardvarks", "alligators", "alpacas", "anteaters", "antelopes",
  "armadillos", "baboons", "badgers", "bears", "beavers",
  "boars", "buffalos", "bulls", "bunnies", "camels", "cats",
  "chameleons", "cheetahs", "centaurs", "chickens", "chimpanzees",
  "chinchillas", "chipmunks", "cougars", "cows", "coyotes", "cranes",
  "crickets", "crocodiles", "deers", "dinasaurs", "dingos", "dogs",
  "donkeys", "dragons", "elephants", "elves", "ferrets", "flamingos",
  "foxes", "frogs", "gazelles", "giraffes", "gnomes", "gnus", "goats",
  "gophers", "gorillas", "hamsters", "hedgehogs", "hippopotamus",
  "hobbits", "hogs", "horses", "hyenas", "ibexes", "iguanas",
  "impalas", "jackals", "jackalopes", "jaguars", "kangaroos",
  "kittens", "koalas", "lambs", "lemmings", "leopards", "lions",
  "ligers", "lizards", "llamas", "lynxes", "meerkat", "moles",
  "mongooses", "monkeys", "moose", "mules", "newts", "okapis",
  "orangutans", "ostriches", "otters", "oxes", "pandas", "panthers",
  "peacocks", "pegasuses", "phoenixes", "pigeons", "pigs",
  "platypuses", "ponies", "porcupines", "porpoises", "pumas",
  "pythons", "rabbits", "raccoons", "rams", "reindeers",
  "rhinoceroses", "salamanders", "seals", "sheep", "skunks",
  "sloths", "slugs", "snails", "snakes", "sphinxes", "sprites",
  "squirrels", "takins", "tigers", "toads", "trolls", "turtles",
  "unicorns", "walruses", "warthogs", "weasels", "wolves",
  "wolverines", "wombats", "woodchucks", "yaks", "zebras")

asana_verbs_past <- c(
  "ambled", "assembled", "burst", "babbled", "charged", "chewed",
  "clamored", "coasted", "crawled", "crept", "danced", "dashed",
  "drove", "flopped", "galloped", "gathered", "glided", "hobbled",
  "hopped", "hurried", "hustled", "jogged", "juggled", "jumped",
  "laughed", "marched", "meandered", "munched", "passed", "plodded",
  "pranced", "ran", "raced", "rushed", "sailed", "sang", "sauntered",
  "scampered", "scurried", "skipped", "slogged", "slurped", "spied",
  "sprinted", "spurted", "squiggled", "squirmed", "stretched",
  "strode", "strut", "swam", "swung", "traveled", "trudged",
  "tumbled", "twisted", "wade", "wandered", "whistled", "wiggled",
  "wobbled", "yawned", "zipped", "zoomed")

asana_verbs_present <- c(
  "ambling", "assembling", "bursting", "babbling", "charging", "chewing",
  "clamoring", "coasting", "crawling", "creeping", "dancing", "dashing",
  "driving", "flopping", "galloping", "gathering", "gliding", "hobbling",
  "hopping", "hurrying", "hustling", "jogging", "juggling", "jumping",
  "laughing", "marching", "meandering", "munching", "passing", "plodding",
  "prancing", "running", "racing", "rushing", "sailing", "singing",
  "sauntering", "scampering", "scurriing", "skipping", "slogging",
  "slurping", "spying", "sprinting", "spurting", "squiggling", "squirming",
  "stretching", "striding", "strutting", "swimming", "swinging", "traveling",
  "trudging", "tumbling", "twisting", "wading", "wandering", "whistling",
  "wiggling", "wobbling", "yawning", "zipping", "zooming")

asana_adverbs <- c(
  "absentmindedly", "adventurously", "angrily", "anxiously",
  "awkwardly", "bashfully", "beautifully", "bleakly", "blissfully",
  "boastfully", "boldly", "bravely", "briskly", "calmly",
  "carefully", "cautiously", "cheerfully", "cleverly", "cluelessly",
  "clumsily", "coaxingly", "colorfully", "coolly", "courageously",
  "curiously", "daintily", "defiantly", "deliberately",
  "delightfully", "diligently", "dreamily", "drudgingly", "eagerly",
  "effortlessly", "elegantly", "energetically", "enthusiastically",
  "excitedly", "fervently", "foolishly", "furiously", "gallantly",
  "gently", "gladly", "gleefully", "gracefully", "gratefully",
  "happily", "hastily", "haphazardly", "hungrily", "innocently",
  "inquisitively", "intensely", "jokingly", "joshingly", "joyously",
  "jovially", "jubilantly", "kiddingly", "knavishly", "knottily",
  "kookily", "lazily", "loftily", "longingly", "lovingly", "loudly",
  "loyally", "madly", "majestically", "merrily", "mockingly",
  "mysteriously", "nervously", "noisily", "obnoxiously", "oddly",
  "optimistically", "overconfidently", "outside", "owlishly",
  "patiently", "playfully", "politely", "powerfully", "purposefully",
  "quaintly", "quarrelsomely", "queasily", "quickly", "quietly",
  "quirkily", "quizzically", "rapidly", "reassuringly", "recklessly",
  "reluctantly", "reproachfully", "sadly", "scarily", "seriously",
  "shakily", "sheepishly", "shyly", "silently", "sillily",
  "sleepily", "slowly", "speedily", "stealthily", "sternly",
  "suspiciously", "sweetly", "tenderly", "tensely", "thoughtfully",
  "triumphantly", "unabashedly", "unaccountably", "urgently",
  "vainly", "valiantly", "victoriously", "warmly", "wearily",
  "youthfully", "zestfully")
