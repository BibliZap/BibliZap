# BibliZap
Outil de recherche bibliographique par recursivité

## Principle
BibliZap aims to catalog articles similar to the source article based on both upward and downward citations. Downward citations correspond to the references of the articles (their bibliography). Upward citations correspond to the articles citing the source article. Here is a diagram summarizing the process:
[![Figure1.png](https://i.postimg.cc/tCGr2KQg/Figure1.png)](https://postimg.cc/3W9CwbwM)
At each level, the number of times each PMID appears is recorded. At the end of the process, the sum of occurrences provides the score. For instance, if an article is found once in the references of the source article, then is discovered 6 times in the articles cited by the articles that are cited by the source article, and is not found elsewhere, its score will be 7.

## Credits
BibliZap was created by Victor Leblanc, with the help of Bastien Le Guellec and Raphaël Bentegac (Univ Lille)
