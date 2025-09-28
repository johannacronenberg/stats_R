Dies ist das GitHub Repo basic_R, das auf meiner Webseite verlinkt ist!

- beide index.Rmd files (deutsch und englisch) nacheinander knitten
- knit_books.R wird nicht benötigt!
- im bash terminal ausführen: python3 run_after_knit.py
  (muss angepasst werden, falls das output directory in den jeweiligen bookdown.yml files geändert wurden!)
- dann alle Neuerungen auf den branch main committen + pushen

- der branch gh-pages wurde als orphan branch erstellt:
  git checkout --orphan gh-pages
  git rm -rf .
  nano .gitignore (hier alle noch übrigen Dateien eintragen, siehe git status)
  git add .
  git commit -m "initial commit to gh-pages branch"
  git push origin gh-pages

- dann die richtigen Dateien vom branch main auf branch gh-pages übertragen:
  git status (man muss auf branch gh-pages sein!)
  git rm -rf .
  nano .gitignore (hier alle noch übrigen Dateien eintragen, siehe git status)
  git add .
  git commit -m "initial commit to gh-pages branch"
  git push origin gh-pages
  git checkout main -- deutsch/_book
  cp -r deutsch/_book/* .
  rm -rf deutsch/_book
  git checkout main -- english/_book
  cp -r english/_book/data-frames.html .
  cp -r english/_book/first-calculations-in-r.html .
  cp -r english/_book/introduction-to-the-tidyverse.html .
  cp -r english/_book/joining-with-dplyr.html .
  cp -r english/_book/manipulating-data-with-dplyr-continuation.html .
  cp -r english/_book/plotting-data-with-ggplot2.html .
  cp -r english/_book/pretty-plots.html .
  cp -r english/_book/setup.html .
  cp -r english/_book/summary-statistics.html .
  cp -r english/_book/tidying-data-with-tidyr.html .
  rm -rf english/_book
  git add .
  git commit -m "added german book as basis plus english htmls"
  git push origin gh-pages
- im GitHub repo basic_R sollte unter Settings > Pages der gh-pages branch als Basis angegeben sein,
  dann kann nämlich https://johannacronenberg.github.io/basic_R/ als Link zu der deutschen Version genutzt werden


