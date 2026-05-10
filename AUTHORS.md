# Amber Wang - aw896
# Claire Cheng - cqc6
# Monisha Bommu - mrb359
# Anshu Addanki - aa2863
# Neetu Matthews - nm734
# GenAI Usage:
# We used Claude to generate our JSON data. The hard difficulty puzzles come from BracketCity. We inputted the structure of the layout (for example, we told it what children, label, etc should represent)
# We asked ChatGPT to give examples of how certain methods Dream works. This was used in main.ml. We also used Claude to help figure out our errors when implementing the Dream server to host the game as we were running into issues during our initial implementation.
# Intially, some completed puzzles would reappear later in the game. We were confused whhy this happend so we asked Claude. It told us that the puzzle state is_solved (which lets us know whether or not this puzzle has been solved) is only being mutated locally. It gave us the idea of to use a hashtable to keep track of all of the puzzles we have already solved. We implemented this and this ensured that no puzzle was repeated again. 