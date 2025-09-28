import fileinput
import sys
import os

def replacement(file, previousw, nextw):
   for line in fileinput.input(file, inplace=1):
	   if previousw in line:
		   line = line.replace(previousw, nextw)
	   sys.stdout.write(line)

chapters = {
  "einfache-lineare-regression": "simple-linear-regression",
  "einführung-in-die-inferenzstatistik": "introduction-to-inferential-statistics",
  "gemischte-lineare-regression": "mixed-linear-regression",
  "logistische-regression": "logistic-regression",
  "multiple-lineare-regression": "multiple-linear-regression",
  "index": "setup",
  "404": "404"
}

# for language in ["deutsch", "english"]:
# 	path = language + "/_book"
# 	for file in os.listdir(path):
# 		if file.endswith(".html"):
# 			if "deutsch" in path:
# 				replacement(os.path.join(path, file), '<a href="./">Programmieren in R: eine Einführung</a>', '<a href="./index.html">Programmieren in R: eine Einführung</a>')
# 				link = '<div class="book-header" role="navigation">\n\t\t\t<a class="btn pull-right js-toolbar-action" href="' + '../../english/_book/' + chapters[os.path.splitext(file)[0]] + '.html"><i class="fa fa-language"></i></a>'
# 				replacement(os.path.join(path, file), '<div class="book-header" role="navigation">', link)
# 			else:
# 				replacement(os.path.join(path, file), '<a href="./">Programming in R: An Introduction</a>', '<a href="./index.html">Programming in R: An Introduction</a>')
# 				link = '<div class="book-header" role="navigation">\n\t\t\t<a class="btn pull-right js-toolbar-action" href="' + '../../deutsch/_book/' + list(chapters.keys())[list(chapters.values()).index(os.path.splitext(file)[0])] + '.html"><i class="fa fa-language"></i></a>'
# 				replacement(os.path.join(path, file), '<div class="book-header" role="navigation">', link)


for language in ["deutsch", "english"]:
	if language == "deutsch":
		path = language + "/_book"
		for file in os.listdir(path):
			if file.endswith(".html"):
				replacement(os.path.join(path, file), '<a href="./">Statistik in R: eine Einführung</a>', '<a href="./index.html">Statistik in R: eine Einführung</a>')
				link = '<div class="book-header" role="navigation">\n\t\t\t<a class="btn pull-right js-toolbar-action" href="' + chapters[os.path.splitext(file)[0]] + '.html"><i class="fa fa-language"></i></a>'
				replacement(os.path.join(path, file), '<div class="book-header" role="navigation">', link)
	else:
		path = language + "/_book"
		for file in os.listdir(path):
			if file.endswith(".html"):
				replacement(os.path.join(path, file), '<a href="./">Statistics in R: An Introduction</a>', '<a href="./index.html">Statistics in R: An Introduction</a>')
				link = '<div class="book-header" role="navigation">\n\t\t\t<a class="btn pull-right js-toolbar-action" href="' + list(chapters.keys())[list(chapters.values()).index(os.path.splitext(file)[0])] + '.html"><i class="fa fa-language"></i></a>'
				replacement(os.path.join(path, file), '<div class="book-header" role="navigation">', link)
