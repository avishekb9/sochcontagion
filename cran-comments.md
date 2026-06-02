## Submission

This is the first submission of sochcontagion 0.1.0.

## Test environments

* local: Linux, R 4.1.2 (package floor; `Depends: R (>= 4.1.0)`)
* win-builder (R-devel and R-release)
* GitHub Actions: ubuntu-latest, macos-latest, windows-latest
  (R-release, R-devel, R-oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note.

The single note is the standard new-submission maintainer note, together with
a "possibly invalid URLs" list that is addressed below.

(On the local check machine a second note, "unable to verify current time"
/ "future file timestamps", is emitted because that machine has no access to
the external time-verification service. It does not occur on the CRAN check
farm and reflects the local environment only.)

## Notes on the flagged URLs

* The four DOI URLs
  (10.1103/PhysRevLett.85.461; 10.2307/1913643;
  10.1080/01621459.1999.10473882; 10.1080/01621459.1994.10476870)
  return HTTP 403 to the automated checker because the publishers (APS,
  JSTOR, Taylor & Francis) block non-browser requests. They are the
  canonical DOIs of the cited methods and resolve correctly in a browser.

* The GitHub URLs (repository, issue tracker, and CI badge) point to
  https://github.com/avishekb9/sochcontagion, the project's public home,
  which is made public at the time of submission so that these resolve.

## Notes on "Possibly misspelled words" in DESCRIPTION

All are correct: author surnames in the methodological references
(Koenker, Bassett, Schreiber, Theiler, Galdrikian, Politis, Romano,
Percival) and the present authors (Bhandari, Parida); domain terms
(Lorentzians, MODWT, WQTE, Nyquist). These are recorded in inst/WORDLIST.

## User state and parallelism

No function changes the user's options, par, or working directory; the
two plotting functions that set `par()` restore it with an immediate
`on.exit(graphics::par(oldpar))`. The package uses no parallel back-ends.

## Reverse dependencies

None -- first release.
