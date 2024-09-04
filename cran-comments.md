## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* There are two words in the DESCRIPTION file being picked up as misspelled
  when they are not. The words are:
  Stefanski (10:5), which is the last name of the author of the algorithm.
  Sur (9:40), which is part of the name of the paper describing the algorithm.
  Both are spelled correctly and were added to the inst/WORDLIST file to
  prevent them from being flagged as misspelled in the future.
